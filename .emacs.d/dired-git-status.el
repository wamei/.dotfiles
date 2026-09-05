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
;; - 更新契機: バッファ表示 / revert / magit の refresh / dired-tree の監視
;;
;;; Code:

(require 'dired)
(require 'project)
(require 'subr-x)
(require 'cl-lib)

;; `wamei/dired-git-status-mode' は下の `define-minor-mode' で定義されるが、
;; それより前にある関数から参照するため前方宣言しておく。
(defvar wamei/dired-git-status-mode)

;;; face

(defgroup wamei/dired-git-status nil "dired のファイル名を git の状態で色分けする。" :group 'dired)

(defface wamei/dired-git-status-modified '((t (:foreground "#e5c07b")))
  "変更されたファイル、または変更を含むディレクトリ。")
(defface wamei/dired-git-status-added '((t (:foreground "#98c379")))
  "index に追加されたファイル。")
(defface wamei/dired-git-status-untracked '((t (:foreground "#7ec699")))
  "未追跡のファイル。")
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

(defun wamei/dired-git-status--propagate (table root)
  "TABLE の各パスの祖先 (ROOT 自身は除く) に状態を足した新しい hash。
子に conflict があれば conflict、それ以外は modified。"
  (let ((out (copy-hash-table table))
        (root (directory-file-name (expand-file-name root))))
    (maphash
     (lambda (path state)
       (let ((dir (directory-file-name (file-name-directory path)))
             (mark (if (eq state 'conflict) 'conflict 'modified)))
         (while (and (not (equal dir root))
                     (string-prefix-p (concat root "/") (concat dir "/")))
           (unless (eq (gethash dir out) 'conflict)
             (puthash dir mark out))
           (setq dir (directory-file-name (file-name-directory dir))))))
     table)
    out))

;;; ルート

(defun wamei/dired-git-status--root ()
  "このバッファの git ルート (絶対パス、末尾 / なし)。git 管理外なら nil。
project-current を使い、その root に .git が無ければ nil。"
  (when-let* ((project (project-current nil))
              (root (directory-file-name (expand-file-name (project-root project)))))
    (when (file-exists-p (expand-file-name ".git" root))
      root)))

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
  (if (gethash root wamei/dired-git-status--running)
      (process-put (gethash root wamei/dired-git-status--running) 'again t)
    (let* ((buffer (generate-new-buffer " *dired-git-status*"))
           (default-directory (file-name-as-directory root))
           (process
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
                   (when again (wamei/dired-git-status--fetch root))))))))
      (puthash root process wamei/dired-git-status--running))))

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
  "readin / subtree 展開のあと、キャッシュがあれば描き直し、無ければ取得する。"
  (when wamei/dired-git-status-mode
    (when-let* ((root (wamei/dired-git-status--root)))
      (if-let* ((table (gethash root wamei/dired-git-status--cache)))
          (wamei/dired-git-status--decorate table)
        (wamei/dired-git-status--fetch root)))))

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

;;; minor mode

(define-minor-mode wamei/dired-git-status-mode
  "dired のファイル名に git の状態で色を付ける。"
  :lighter nil
  (if wamei/dired-git-status-mode
      (progn
        (add-hook 'dired-after-readin-hook #'wamei/dired-git-status--redecorate 95 t)
        (add-hook 'dired-subtree-after-insert-hook #'wamei/dired-git-status--redecorate 95 t)
        (add-hook 'wamei/dired-tree-refresh-hook #'wamei/dired-git-status-refresh nil t)
        (wamei/dired-git-status-refresh))
    (remove-hook 'dired-after-readin-hook #'wamei/dired-git-status--redecorate t)
    (remove-hook 'dired-subtree-after-insert-hook #'wamei/dired-git-status--redecorate t)
    (remove-hook 'wamei/dired-tree-refresh-hook #'wamei/dired-git-status-refresh t)
    (wamei/dired-git-status--clear)))

(provide 'dired-git-status)
;;; dired-git-status.el ends here
