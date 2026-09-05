;;; dired-tree.el --- dired-subtree を木として扱う -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; dired-subtree でディレクトリを展開する dired バッファに共通する振る舞い。
;;
;; 1. 展開記憶 (`wamei/dired-tree--expanded')
;;    dired-subtree-remove は範囲を削除して中の overlay をまとめて捨てるので、
;;    親を閉じると子孫の展開状態が消える。展開中ディレクトリの集合をバッファ
;;    ローカルに持ち、展開時に子孫を開き直す。閉じたときは自身だけ忘れる。
;;
;; 2. 展開ディレクトリの監視 (`wamei/dired-tree--reconcile-watches')
;;    いま overlay で展開中のディレクトリそれぞれに file-notify で監視をかけ、
;;    展開・折り畳みのたびに監視対象を合わせ直す。変更通知が来たら
;;    `wamei/dired-tree-revert-delay' 秒待ってからカーソル位置を保って revert
;;    し、`wamei/dired-tree-refresh-hook' を呼ぶ。
;; 3. path までの展開 (Task 4)
;; 4. D&D の drop 先
;;    別の dired バッファからファイルを subtree の行へ drop すると、その行の
;;    ディレクトリ (ファイル行なら親、行が無ければ top) へ移す。macOS の drop は
;;    常に private で届き修飾キー (Shift / Control / Meta) は伝わらない。dired 既定
;;    では private は copy 扱いだが、Finder と同じく既定は移動
;;    (`wamei/dired-tree-drop-action') にする。複数ファイルの drop は
;;    `dnd-multiple-handler' プロパティで 1 回にまとめて受け取り、落下先を 1 度だけ
;;    決めてから全部運ぶ。
;;
;;; Code:

(require 'dired)
(require 'dired-subtree)
(require 'dired-hacks-utils)
(require 'seq)
(require 'filenotify)
(require 'subr-x)
(require 'dired-aux)
(require 'dnd)

;; `wamei/dired-tree-mode' は下の `define-minor-mode' で定義されるが、
;; それより前にある関数から参照するため前方宣言しておく。
(defvar wamei/dired-tree-mode)

;;; 展開記憶

(defvar-local wamei/dired-tree--expanded nil
  "展開中 (または閉じた親の下で展開したまま) のディレクトリ。絶対パス、末尾 / なし。")

(defun wamei/dired-tree--normalize (dir)
  "DIR を絶対パス・末尾 / なしに正規化する。"
  (directory-file-name (expand-file-name dir)))

(defun wamei/dired-tree--expanded-add (expanded dir)
  "EXPANDED に DIR を加えた新しいリスト。既にあればそのまま。"
  (let ((d (wamei/dired-tree--normalize dir)))
    (if (member d expanded) expanded (cons d expanded))))

(defun wamei/dired-tree--expanded-remove (expanded dir)
  "EXPANDED から DIR だけを外した新しいリスト。子孫は残す。"
  (remove (wamei/dired-tree--normalize dir) expanded))

(defun wamei/dired-tree--children-to-reopen (expanded children)
  "CHILDREN (絶対パス) のうち EXPANDED に入っているものを出現順で返す。"
  (seq-filter (lambda (c) (member (wamei/dired-tree--normalize c) expanded))
              children))

(defun wamei/dired-tree--subdirs-in (ov)
  "subtree overlay OV の範囲にあるディレクトリ行の絶対パス。"
  (let (dirs)
    (save-excursion
      (goto-char (overlay-start ov))
      (while (< (point) (overlay-end ov))
        (when (and (dired-subtree--dired-line-is-directory-or-link-p)
                   (dired-utils-get-filename))
          (push (dired-utils-get-filename) dirs))
        (forward-line 1)))
    (nreverse dirs)))

(defun wamei/dired-tree--after-insert ()
  "`dired-subtree-after-insert-hook' 用。展開を記憶し、覚えている子孫を開き直す。"
  (when wamei/dired-tree-mode
    (when-let* ((ov (dired-subtree--get-ov)))
      (let ((dir (overlay-get ov 'dired-subtree-name)))
        (setq wamei/dired-tree--expanded
              (wamei/dired-tree--expanded-add wamei/dired-tree--expanded dir))
        (dolist (child (wamei/dired-tree--children-to-reopen
                        wamei/dired-tree--expanded
                        (wamei/dired-tree--subdirs-in ov)))
          (save-excursion
            (when (and (dired-utils-goto-line child)
                       (not (dired-subtree--is-expanded-p)))
              (dired-subtree-insert))))
        (wamei/dired-tree--reconcile-watches)))))

(defun wamei/dired-tree--before-remove (&rest _)
  "`dired-subtree-remove' の :before advice。閉じるディレクトリ自身だけ忘れる。"
  (when wamei/dired-tree-mode
    (when-let* ((ov (dired-subtree--get-ov)))
      (setq wamei/dired-tree--expanded
            (wamei/dired-tree--expanded-remove
             wamei/dired-tree--expanded (overlay-get ov 'dired-subtree-name))))))

;;; path までの展開

(defun wamei/dired-tree--inside-p (root file)
  "FILE が ROOT の配下 (ROOT 自身を除く) なら t。"
  (let ((root (file-name-as-directory (expand-file-name root)))
        (file (expand-file-name file)))
    (and (string-prefix-p root file)
         (not (equal (directory-file-name root) (directory-file-name file))))))

(defun wamei/dired-tree--ancestors (root file)
  "ROOT 直下から FILE の親までのディレクトリ列 (絶対パス、末尾 / なし)。
FILE が ROOT 直下なら nil。FILE が ROOT 外でも nil。FILE 自身の末尾 / は無視する。"
  (when (wamei/dired-tree--inside-p root file)
    (let* ((root (wamei/dired-tree--normalize root))
           (file (directory-file-name (expand-file-name file)))
           (dir (directory-file-name (file-name-directory file)))
           acc)
      (while (not (equal dir root))
        (push dir acc)
        (setq dir (directory-file-name (file-name-directory dir))))
      acc)))

(defun wamei/dired-tree-expand-to (file)
  "FILE までの祖先ディレクトリを展開し、FILE の行へ移動する。
FILE がこのバッファのルート外、または途中の行が見つからなければ nil で、
point は呼び出し前の位置に戻す。"
  (let ((root (expand-file-name default-directory))
        (start (point)))
    (if (wamei/dired-tree--inside-p root file)
        (or (catch 'missing
              (dolist (dir (wamei/dired-tree--ancestors root file))
                (unless (dired-utils-goto-line dir)
                  (throw 'missing nil))
                (unless (dired-subtree--is-expanded-p)
                  (dired-subtree-insert)))
              (dired-utils-goto-line (wamei/dired-tree--normalize file)))
            (progn (goto-char start) nil))
      nil)))

;;; カーソル保持

(defun wamei/dired-tree-revert ()
  "カーソル行のファイルを保って `revert-buffer' する。
dired 標準の復元は subtree 行では効かないので `dired-utils-goto-line' で戻す。

sidebar は選択されていない window に出る (`wamei/project-sidebar--reveal' は
window-point だけを動かす) ので、buffer point と window-point はずれる。
dired 標準の `dired-restore-positions' も window ごとの復元を持つが、subtree 行では
`dired-goto-file' が効かず行番号にフォールバックするため、行数が変わる revert で
別の行に飛ぶ。window ごとにも行のファイル名を控えて戻す。"
  (let ((file (dired-utils-get-filename))
        (window-files
         (mapcar (lambda (win)
                   (cons win (save-excursion
                               (goto-char (window-point win))
                               (dired-utils-get-filename))))
                 (get-buffer-window-list nil nil t))))
    (revert-buffer)
    (when file
      (or (dired-utils-goto-line file)
          (dired-goto-file file)))
    (pcase-dolist (`(,win . ,wfile) window-files)
      (when-let* ((wfile wfile)
                  (pos (save-excursion
                         (and (dired-utils-goto-line wfile) (point)))))
        (set-window-point win pos)))))

;;; 展開ディレクトリの監視

(defvar wamei/dired-tree-refresh-hook nil
  "監視による revert のあとに呼ぶ関数。dired-git-status が色の再取得に使う。")

(defvar wamei/dired-tree-revert-delay 0.3
  "file-notify の通知から revert までの待ち時間 (秒)。連続する通知をまとめる。")

(defvar-local wamei/dired-tree--watches nil
  "ディレクトリ → file-notify の descriptor。top ディレクトリは auto-revert が見るので含めない。")

(defvar-local wamei/dired-tree--revert-timer nil)

(defun wamei/dired-tree--watch-diff (current wanted)
  "CURRENT を WANTED に合わせるための (追加するもの . 外すもの)。"
  (cons (seq-remove (lambda (d) (member d current)) wanted)
        (seq-remove (lambda (d) (member d wanted)) current)))

(defun wamei/dired-tree--visible-expanded ()
  "overlay で展開中のディレクトリ (絶対パス、末尾 / なし)。"
  (delete-dups
   (mapcar (lambda (ov) (overlay-get ov 'dired-subtree-name))
           (dired-subtree--get-all-ovs))))

(defun wamei/dired-tree--schedule-revert (buffer)
  "BUFFER の revert を `wamei/dired-tree-revert-delay' 後に予約する。既存の予約は延ばす。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (timerp wamei/dired-tree--revert-timer)
        (cancel-timer wamei/dired-tree--revert-timer))
      (setq wamei/dired-tree--revert-timer
            (run-at-time wamei/dired-tree-revert-delay nil
                         (lambda ()
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (setq wamei/dired-tree--revert-timer nil)
                               (condition-case err
                                   (progn
                                     (wamei/dired-tree-revert)
                                     (run-hooks 'wamei/dired-tree-refresh-hook))
                                 (error
                                  (message "dired-tree: revert failed: %s"
                                           (error-message-string err))))))))))))

(defun wamei/dired-tree--watch-callback (buffer _event)
  "file-notify のコールバック。BUFFER の revert を予約する。"
  (wamei/dired-tree--schedule-revert buffer))

(defun wamei/dired-tree--reconcile-watches ()
  "監視対象を、いま見えている展開ディレクトリに合わせる。"
  (unless wamei/dired-tree--watches
    (setq wamei/dired-tree--watches (make-hash-table :test 'equal)))
  (let* ((current (hash-table-keys wamei/dired-tree--watches))
         (diff (wamei/dired-tree--watch-diff current (wamei/dired-tree--visible-expanded)))
         (buffer (current-buffer)))
    (dolist (dir (car diff))
      (when (file-directory-p dir)
        (ignore-errors
          (puthash dir
                   (file-notify-add-watch
                    dir '(change)
                    (lambda (event) (wamei/dired-tree--watch-callback buffer event)))
                   wamei/dired-tree--watches))))
    (dolist (dir (cdr diff))
      (ignore-errors (file-notify-rm-watch (gethash dir wamei/dired-tree--watches)))
      (remhash dir wamei/dired-tree--watches))))

(defun wamei/dired-tree--remove-all-watches ()
  "全ての監視と予約中の revert を外す。バッファ kill と mode 無効化用。"
  (when (timerp wamei/dired-tree--revert-timer)
    (cancel-timer wamei/dired-tree--revert-timer))
  (setq wamei/dired-tree--revert-timer nil)
  (when wamei/dired-tree--watches
    (maphash (lambda (_dir desc) (ignore-errors (file-notify-rm-watch desc)))
             wamei/dired-tree--watches)
    (clrhash wamei/dired-tree--watches)))

(defun wamei/dired-tree--after-remove ()
  "`dired-subtree-after-remove-hook' / `dired-after-readin-hook' 用。監視を現状に合わせる。"
  (when wamei/dired-tree-mode
    (wamei/dired-tree--reconcile-watches)))

;;; D&D の drop 先

(defvar wamei/dired-tree-drop-action 'move
  "drop が private / copy で届いたときの操作。move / copy のいずれか。
macOS の drop イベントは常に private で届き、dired 既定では copy になる。
Finder と同じく既定は移動にする。")

(defun wamei/dired-tree--drop-target (directory-p file parent top)
  "落下先ディレクトリ (末尾 / あり)。
行がディレクトリ (DIRECTORY-P) ならその FILE、ファイルなら PARENT、行に何も無ければ TOP。"
  (file-name-as-directory
   (cond ((and directory-p file) file)
         (file parent)
         (t top))))

(defun wamei/dired-tree--drop-destination (from target-dir)
  "FROM を TARGET-DIR に落としたときのフルパス。"
  (concat (file-name-as-directory target-dir)
          (file-name-nondirectory (directory-file-name from))))

(defun wamei/dired-tree--resolve-action (action)
  "dnd の ACTION を実際の操作に直す。private / copy は `wamei/dired-tree-drop-action'。"
  (if (memq action '(private copy)) wamei/dired-tree-drop-action action))

(defun wamei/dired-tree-drop-directory-at-point ()
  "point の行から落下先ディレクトリを決める。"
  (let* ((file (dired-utils-get-filename))
         (ov (dired-subtree--get-ov))
         (parent (if ov (overlay-get ov 'dired-subtree-name) (dired-current-directory))))
    (wamei/dired-tree--drop-target (and file (file-directory-p file))
                                   file parent (dired-current-directory))))

(defun wamei/dired-tree--drop-into-self-p (from to)
  "TO が FROM 自身か、FROM (ディレクトリ) の中なら t。
そのまま `rename-file' に渡すとエラーになるので、呼ぶ前に弾く。"
  (let ((from (expand-file-name from))
        (to (expand-file-name to)))
    (or (equal (directory-file-name from) (directory-file-name to))
        (and (file-directory-p from)
             (string-prefix-p (file-name-as-directory from)
                              (file-name-as-directory to))))))

(defun wamei/dired-tree--drop-one (from to action)
  "FROM を TO へ ACTION で運ぶ。運んだら t。自分自身の中や上書き拒否なら nil。"
  (cond
   ((wamei/dired-tree--drop-into-self-p from to)
    (message "dired-tree: skipping drop of %s into itself" from)
    nil)
   (t
    (let ((overwrite (and (file-exists-p to)
                          (y-or-n-p (format-message "Overwrite existing file `%s'? " to)))))
      (when (or overwrite (not (file-exists-p to)))
        (pcase action
          ('move (dired-rename-file from to overwrite))
          ('copy (dired-copy-file from to overwrite))
          ('link (make-symbolic-link from to overwrite)))
        t)))))

(defun wamei/dired-tree-dnd-handle-file (uris action)
  "URIS のローカルファイルを point の行の落下先へ ACTION で運ぶ。`dnd-protocol-alist' 用。
`dired-dnd-handle-file' は落下先を `dired-current-directory' (top) に固定するので、
subtree 行を見て決める版。終わったら revert して行を作り直す。

URIS は URI 1 本の文字列でもリストでもよい。`dnd-multiple-handler' プロパティを
付けてあるので `dnd-handle-multiple-urls' は複数ファイルの drop をリストで 1 回だけ
渡してくる。落下先は最初に 1 回だけ決め (revert で point が動いても drop 先が
ずれないように)、全部運んでから revert と `wamei/dired-tree-refresh-hook' を
1 回だけ回す。上書き確認はファイルごとに出す。"
  (let* ((uris (if (listp uris) uris (list uris)))
         (action (wamei/dired-tree--resolve-action action))
         (target (wamei/dired-tree-drop-directory-at-point))
         (handled nil)
         (changed nil))
    (dolist (uri uris)
      (when-let* ((from (dnd-get-local-file-name uri t)))
        (setq handled t)
        (when (wamei/dired-tree--drop-one
               from (wamei/dired-tree--drop-destination from target) action)
          (setq changed t))))
    (when changed
      (wamei/dired-tree-revert)
      (run-hooks 'wamei/dired-tree-refresh-hook))
    (and handled action)))

;; 複数ファイルの drop を 1 回で受け取る。これが無いと `dnd-handle-multiple-urls' が
;; ファイルごとに呼び、そのたびに revert が走って 2 つめ以降の落下先がずれる。
(put 'wamei/dired-tree-dnd-handle-file 'dnd-multiple-handler t)

(defun wamei/dired-tree--setup-dnd ()
  "このバッファの `dnd-protocol-alist' の先頭に自前のハンドラを置く。"
  (setq-local dnd-protocol-alist
              (cons '("^file:" . wamei/dired-tree-dnd-handle-file)
                    (default-value 'dnd-protocol-alist))))

;;; minor mode

(define-minor-mode wamei/dired-tree-mode
  "dired-subtree の展開を記憶し、展開ディレクトリを監視し、D&D の落下先を行から決める。"
  :lighter nil
  (if wamei/dired-tree-mode
      (progn
        (add-hook 'dired-subtree-after-insert-hook #'wamei/dired-tree--after-insert nil t)
        (add-hook 'dired-subtree-after-remove-hook #'wamei/dired-tree--after-remove nil t)
        (add-hook 'dired-after-readin-hook #'wamei/dired-tree--after-remove 90 t)
        (add-hook 'kill-buffer-hook #'wamei/dired-tree--remove-all-watches nil t)
        (advice-add 'dired-subtree-remove :before #'wamei/dired-tree--before-remove)
        (wamei/dired-tree--setup-dnd))
    (remove-hook 'dired-subtree-after-insert-hook #'wamei/dired-tree--after-insert t)
    (remove-hook 'dired-subtree-after-remove-hook #'wamei/dired-tree--after-remove t)
    (remove-hook 'dired-after-readin-hook #'wamei/dired-tree--after-remove t)
    (remove-hook 'kill-buffer-hook #'wamei/dired-tree--remove-all-watches t)
    (wamei/dired-tree--remove-all-watches)
    (kill-local-variable 'dnd-protocol-alist)))

(provide 'dired-tree)
;;; dired-tree.el ends here
