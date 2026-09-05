;;; dired-git-status.el --- dired のファイル名に git の状態で色を付ける -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; treemacs-git-mode の代替。プロジェクトルートで `git status --porcelain=v1 -z'
;; を非同期に取り、path → state の表にして、変更を含むディレクトリにも伝播させ、
;; dired の各行 (dired-subtree の展開行を含む) のファイル名に overlay で face を当てる。
;;
;; - パース (`wamei/dired-git-status--parse') と伝播 (`--propagate') は純関数
;; - 取得はルート単位で 1 回。結果は `wamei/dired-git-status--cache' に置き、
;;   同じルートを見る全 dired バッファに配る
;; - 更新契機: バッファ表示 / revert (auto-revert・dired-tree・D&D を含む) /
;;   magit の refresh / Emacs でのファイル保存と外部変更の取り込み
;;   (after-save-hook・after-revert-hook) / .git の中身の変化 (index・HEAD 等を
;;   file-notify で監視)。macOS の kqueue はファイル内容の変更をディレクトリ監視で
;;   拾えないので、ファイル側の契機をここで補う
;; - フォルダの色: 中のファイルが全部同じ状態ならその色 (untracked だけなら
;;   untracked、added だけなら added)、混在なら modified、conflict があれば conflict
;;
;;; Code:

(require 'dired)
(require 'project)
(require 'subr-x)
(require 'cl-lib)
(require 'filenotify)

;; `wamei/dired-git-status-mode' は下の `define-minor-mode' で定義されるが、
;; それより前にある関数から参照するため前方宣言しておく。
(defvar wamei/dired-git-status-mode)

;;; face

(defgroup wamei/dired-git-status nil "dired のファイル名を git の状態で色分けする。" :group 'dired)

(defface wamei/dired-git-status-modified '((t (:foreground "#e5c07b")))
  "変更されたファイル、または変更を含むディレクトリ。")
(defface wamei/dired-git-status-added '((t (:foreground "#98c379")))
  "index に追加されたファイル。")
(defface wamei/dired-git-status-untracked '((t (:foreground "#56b6c2")))
  "未追跡のファイル。dired-rainbow の実行ファイル (緑) と見分けるためシアン系にする。")
(defface wamei/dired-git-status-renamed '((t (:foreground "#61afef")))
  "リネームされたファイル。")
(defface wamei/dired-git-status-conflict '((t (:foreground "#e06c75" :weight bold)))
  "コンフリクト中のファイル、またはそれを含むディレクトリ。")

(defconst wamei/dired-git-status--faces
  '((modified . wamei/dired-git-status-modified)
    (added . wamei/dired-git-status-added)
    (untracked . wamei/dired-git-status-untracked)
    (renamed . wamei/dired-git-status-renamed)
    (conflict . wamei/dired-git-status-conflict)))

;;; パース (純関数)

(defun wamei/dired-git-status--code-to-state (xy)
  "porcelain の 2 文字 XY を state に直す。無視するもの (!!) は nil。"
  (let ((x (aref xy 0)) (y (aref xy 1)))
    (cond ((and (eq x ??) (eq y ??)) 'untracked)
          ((and (eq x ?!) (eq y ?!)) nil)
          ((or (eq x ?U) (eq y ?U) (and (eq x ?A) (eq y ?A)) (and (eq x ?D) (eq y ?D))) 'conflict)
          ((memq x '(?R ?C)) 'renamed)
          ((eq x ?A) 'added)
          (t 'modified))))

(defun wamei/dired-git-status--parse (output root)
  "`git status --porcelain=v1 -z' の OUTPUT を ROOT からの絶対パス → state の hash にする。
エントリは \"XY path\\0\"、リネームは \"XY new\\0old\\0\"。"
  (let ((table (make-hash-table :test 'equal))
        (root (file-name-as-directory (expand-file-name root)))
        (fields (split-string output "\0" t)))
    (while fields
      (let* ((entry (pop fields))
             (xy (substring entry 0 2))
             (path (substring entry 3))
             (state (wamei/dired-git-status--code-to-state xy)))
        (when (memq (aref xy 0) '(?R ?C))
          (pop fields))                 ; 旧パスは捨てる
        (when state
          (puthash (directory-file-name (concat root path)) state table))))
    table))

(defun wamei/dired-git-status--summarize (states)
  "配下のファイルの STATES (リスト) からディレクトリの状態を決める。
conflict があれば conflict、全部同じならその状態、混在なら modified。"
  (cond ((memq 'conflict states) 'conflict)
        ((null (cdr (delete-dups (copy-sequence states)))) (car states))
        (t 'modified)))

(defun wamei/dired-git-status--propagate (table root)
  "TABLE の各パスの祖先 (ROOT 自身は除く) に状態を足した新しい hash。
祖先には配下の全ファイルの状態を集めて `wamei/dired-git-status--summarize' で決める:
untracked だけのフォルダは untracked、added だけなら added、混在なら modified、
conflict があれば conflict。"
  (let ((out (copy-hash-table table))
        (children (make-hash-table :test 'equal))
        (root (directory-file-name (expand-file-name root))))
    (maphash
     (lambda (path state)
       (let ((dir (directory-file-name (file-name-directory path))))
         (while (and (not (equal dir root))
                     (string-prefix-p (concat root "/") (concat dir "/")))
           (push state (gethash dir children))
           (setq dir (directory-file-name (file-name-directory dir))))))
     table)
    (maphash (lambda (dir states)
               (puthash dir (wamei/dired-git-status--summarize states) out))
             children)
    out))

;;; ルート

(defun wamei/dired-git-status--root ()
  "このバッファの git ルート (絶対パス、末尾 / なし)。git 管理外なら nil。
project-current を使い、その root に .git が無ければ nil。
リモート (TRAMP) では何もしない。"
  (unless (file-remote-p default-directory)
    (when-let* ((project (project-current nil))
                (root (directory-file-name (expand-file-name (project-root project)))))
      (when (file-exists-p (expand-file-name ".git" root))
        root))))

;;; 取得とキャッシュ

(defvar wamei/dired-git-status--cache (make-hash-table :test 'equal)
  "ルート → propagate 済みの path → state 表。")

(defvar wamei/dired-git-status--running (make-hash-table :test 'equal)
  "ルート → 実行中のプロセス。値が `again' 付きなら完了後にもう 1 回走らせる。")

(defun wamei/dired-git-status--buffers-for (root)
  "ROOT を見ている、mode 有効な dired バッファ。"
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (and (derived-mode-p 'dired-mode)
                       (bound-and-true-p wamei/dired-git-status-mode)
                       (equal (wamei/dired-git-status--root) root))))
              (buffer-list)))

(defun wamei/dired-git-status--distribute (root)
  "ROOT のキャッシュを、ROOT を見ている全バッファに描く。"
  (when-let* ((table (gethash root wamei/dired-git-status--cache)))
    (dolist (buf (wamei/dired-git-status--buffers-for root))
      (with-current-buffer buf
        (wamei/dired-git-status--decorate table)))))

(defun wamei/dired-git-status--fetch (root)
  "ROOT で git status を非同期に走らせ、終わったらキャッシュして配る。
実行中なら完了後にもう 1 回だけ走るよう印を付ける。"
  (wamei/dired-git-status--watch-git-dir root)
  (if (gethash root wamei/dired-git-status--running)
      (process-put (gethash root wamei/dired-git-status--running) 'again t)
    (let* ((buffer (generate-new-buffer " *dired-git-status*"))
           (default-directory (file-name-as-directory root))
           (process
            ;; git が無い / 実行できない環境でも dired を開くだけで signal しないよう、
            ;; プロセス起動の失敗はここで飲んでメッセージにする。
            (condition-case err
                (make-process
                 :name "dired-git-status"
                 :buffer buffer
                 :command '("git" "status" "--porcelain=v1" "-z" "--untracked-files=all")
                 :noquery t
                 :sentinel
                 (lambda (proc _event)
                   (unless (process-live-p proc)
                     (let ((again (process-get proc 'again)))
                       (remhash root wamei/dired-git-status--running)
                       (when (and (zerop (process-exit-status proc)) (buffer-live-p buffer))
                         (puthash root
                                  (wamei/dired-git-status--propagate
                                   (wamei/dired-git-status--parse
                                    (with-current-buffer buffer (buffer-string)) root)
                                   root)
                                  wamei/dired-git-status--cache)
                         (wamei/dired-git-status--distribute root))
                       (when (buffer-live-p buffer) (kill-buffer buffer))
                       (when again (wamei/dired-git-status--fetch root))))))
              (error
               (remhash root wamei/dired-git-status--running)
               (when (buffer-live-p buffer) (kill-buffer buffer))
               (message "dired-git-status: git status failed: %s" (error-message-string err))
               nil))))
      (when process
        (puthash root process wamei/dired-git-status--running)))))

;;; 描画

(defun wamei/dired-git-status--clear ()
  "このバッファの overlay を全部消す。"
  (remove-overlays (point-min) (point-max) 'wamei/dired-git-status-overlay t))

(defun wamei/dired-git-status--decorate (table)
  "TABLE に従い、このバッファの各行のファイル名に face を当てる。"
  (wamei/dired-git-status--clear)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let* ((file (dired-get-filename nil t))
                  (state (gethash (directory-file-name file) table))
                  (face (alist-get state wamei/dired-git-status--faces))
                  (beg (dired-move-to-filename))
                  (end (dired-move-to-end-of-filename t)))
        (let ((ov (make-overlay beg end)))
          (overlay-put ov 'wamei/dired-git-status-overlay t)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'face face)))
      (forward-line 1))))

;;; 更新契機

(defun wamei/dired-git-status-refresh ()
  "このバッファのルートの git 状態を再取得して、同じルートの全バッファに配る。"
  (interactive)
  (when-let* ((root (wamei/dired-git-status--root)))
    (wamei/dired-git-status--fetch root)))

(defun wamei/dired-git-status--redecorate ()
  "readin / subtree 展開のあと、キャッシュがあれば描き直す。
キャッシュが無い (初回) か、revert の途中 (auto-revert・dired-tree・D&D・g) なら
再取得もする。revert は「何か変わった」合図なので、キャッシュのままにしない。
初回は mode 有効化が既に取得を始めているので、実行中なら二重に起動しない。"
  (when wamei/dired-git-status-mode
    (when-let* ((root (wamei/dired-git-status--root)))
      (let ((table (gethash root wamei/dired-git-status--cache)))
        (when table
          (wamei/dired-git-status--decorate table))
        (when (or (bound-and-true-p revert-buffer-in-progress)
                  (and (null table)
                       (not (gethash root wamei/dired-git-status--running))))
          (wamei/dired-git-status--fetch root))))))

(defun wamei/dired-git-status--refresh-all-visible ()
  "表示中の dired バッファのルートを全部再取得する。magit の refresh 後に呼ぶ。"
  (let (roots)
    (dolist (win (window-list nil 'no-minibuf))
      (with-current-buffer (window-buffer win)
        (when (and (derived-mode-p 'dired-mode) (bound-and-true-p wamei/dired-git-status-mode))
          (when-let* ((root (wamei/dired-git-status--root)))
            (cl-pushnew root roots :test #'equal)))))
    (mapc #'wamei/dired-git-status--fetch roots)))

(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook #'wamei/dired-git-status--refresh-all-visible))

;;; ファイル側の契機 (A)

(defun wamei/dired-git-status--on-file-change ()
  "保存 / 外部変更の取り込みがあったファイルのルートを再取得する。
`after-save-hook' と `after-revert-hook' (global) 用。そのルートを見ている
dired バッファが無ければ何もしない。dired バッファ自身の revert は
`buffer-file-name' が nil なので対象外 (そちらは `--redecorate' が担う)。"
  (when-let* ((file buffer-file-name))
    (unless (file-remote-p file)
      (let ((default-directory (file-name-directory file)))
        (when-let* ((root (wamei/dired-git-status--root)))
          (when (or (gethash root wamei/dired-git-status--cache)
                    (wamei/dired-git-status--buffers-for root))
            (wamei/dired-git-status--fetch root)))))))

(add-hook 'after-save-hook #'wamei/dired-git-status--on-file-change)
(add-hook 'after-revert-hook #'wamei/dired-git-status--on-file-change)

;;; .git の監視 (B)

(defvar wamei/dired-git-status-git-dir-delay 0.3
  ".git の変化から再取得までの待ち時間 (秒)。index.lock の出入りなど連続する通知をまとめる。")

(defvar wamei/dired-git-status--git-watches (make-hash-table :test 'equal)
  "ルート → .git ディレクトリの file-notify descriptor。")

(defvar wamei/dired-git-status--git-timers (make-hash-table :test 'equal)
  "ルート → 予約中の再取得タイマー。")

(defun wamei/dired-git-status--git-dir-changed (root)
  ".git に変化があった。debounce してから ROOT を再取得する。"
  (when-let* ((timer (gethash root wamei/dired-git-status--git-timers)))
    (cancel-timer timer))
  (puthash root
           (run-at-time wamei/dired-git-status-git-dir-delay nil
                        (lambda ()
                          (remhash root wamei/dired-git-status--git-timers)
                          (if (wamei/dired-git-status--buffers-for root)
                              (wamei/dired-git-status--fetch root)
                            (wamei/dired-git-status--unwatch-git-dir root))))
           wamei/dired-git-status--git-timers))

(defun wamei/dired-git-status--watch-git-dir (root)
  "ROOT の .git ディレクトリを監視する (既に監視中なら何もしない)。
git は index や HEAD を lock ファイル経由の rename で書くので、ファイルではなく
ディレクトリを見ればエントリの出入りとして届く。.git がファイル (worktree) の
ときは監視しない。"
  (let ((gitdir (expand-file-name ".git" root)))
    (when (and (not (gethash root wamei/dired-git-status--git-watches))
               (file-directory-p gitdir))
      (ignore-errors
        (puthash root
                 (file-notify-add-watch
                  gitdir '(change)
                  (lambda (_event) (wamei/dired-git-status--git-dir-changed root)))
                 wamei/dired-git-status--git-watches)))))

(defun wamei/dired-git-status--unwatch-git-dir (root)
  "ROOT の .git の監視と予約中のタイマーを外す。"
  (when-let* ((desc (gethash root wamei/dired-git-status--git-watches)))
    (ignore-errors (file-notify-rm-watch desc))
    (remhash root wamei/dired-git-status--git-watches))
  (when-let* ((timer (gethash root wamei/dired-git-status--git-timers)))
    (cancel-timer timer)
    (remhash root wamei/dired-git-status--git-timers)))

(defun wamei/dired-git-status--release-root ()
  "このバッファが最後の利用者なら、そのルートの .git 監視を外す。
mode 無効化と kill-buffer 用。"
  (when-let* ((root (wamei/dired-git-status--root)))
    (unless (seq-remove (lambda (b) (eq b (current-buffer)))
                        (wamei/dired-git-status--buffers-for root))
      (wamei/dired-git-status--unwatch-git-dir root))))

;;; minor mode

(define-minor-mode wamei/dired-git-status-mode
  "dired のファイル名に git の状態で色を付ける。"
  :lighter nil
  (if wamei/dired-git-status-mode
      (progn
        (add-hook 'dired-after-readin-hook #'wamei/dired-git-status--redecorate 95 t)
        (add-hook 'dired-subtree-after-insert-hook #'wamei/dired-git-status--redecorate 95 t)
        ;; dired-tree や D&D の revert は revert-buffer を通るので --redecorate が再取得する
        (add-hook 'kill-buffer-hook #'wamei/dired-git-status--release-root nil t)
        (wamei/dired-git-status-refresh))
    (remove-hook 'dired-after-readin-hook #'wamei/dired-git-status--redecorate t)
    (remove-hook 'dired-subtree-after-insert-hook #'wamei/dired-git-status--redecorate t)
    (remove-hook 'kill-buffer-hook #'wamei/dired-git-status--release-root t)
    (wamei/dired-git-status--clear)
    (wamei/dired-git-status--release-root)))

(provide 'dired-git-status)
;;; dired-git-status.el ends here
