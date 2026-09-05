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
;; 4. D&D の drop 先 (Task 6)
;;
;;; Code:

(require 'dired)
(require 'dired-subtree)
(require 'dired-hacks-utils)
(require 'seq)
(require 'filenotify)
(require 'subr-x)

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
dired 標準の復元は subtree 行では効かないので `dired-utils-goto-line' で戻す。"
  (let ((file (dired-utils-get-filename)))
    (revert-buffer)
    (when file
      (or (dired-utils-goto-line file)
          (dired-goto-file file)))))

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
                               (wamei/dired-tree-revert)
                               (run-hooks 'wamei/dired-tree-refresh-hook)))))))))

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
  "全ての監視を外す。バッファ kill 用。"
  (when wamei/dired-tree--watches
    (maphash (lambda (_dir desc) (ignore-errors (file-notify-rm-watch desc)))
             wamei/dired-tree--watches)
    (clrhash wamei/dired-tree--watches)))

(defun wamei/dired-tree--after-remove ()
  "`dired-subtree-after-remove-hook' / `dired-after-readin-hook' 用。監視を現状に合わせる。"
  (when wamei/dired-tree-mode
    (wamei/dired-tree--reconcile-watches)))

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
        (advice-add 'dired-subtree-remove :before #'wamei/dired-tree--before-remove))
    (remove-hook 'dired-subtree-after-insert-hook #'wamei/dired-tree--after-insert t)
    (remove-hook 'dired-subtree-after-remove-hook #'wamei/dired-tree--after-remove t)
    (remove-hook 'dired-after-readin-hook #'wamei/dired-tree--after-remove t)
    (remove-hook 'kill-buffer-hook #'wamei/dired-tree--remove-all-watches t)
    (wamei/dired-tree--remove-all-watches)))

(provide 'dired-tree)
;;; dired-tree.el ends here
