;;; tty-image-mode.el --- tty の Emacs で画像ファイルを開く -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; tty の Emacs では `image-mode' が入口で失敗する (image-mode.el の
;; `(unless (display-images-p) (error "Display does not support images"))')。
;; ここでは kitty graphics protocol の Unicode placeholder で画像を描く major mode を
;; 用意し、tty のときだけ `image-mode' の代わりに使う。
;;
;; バッファのテキスト (ファイルの生データ) は壊さない。image-mode と同じく
;; `display' テキストプロパティとして placeholder を被せるだけなので、保存や
;; revert が壊れない。
;;
;;; Code:

(require 'kitty-graphics)
(require 'image-file)
(require 'seq)

(defvar-local wamei/tty-image--id nil
  "端末に置いている画像の ID。無ければ nil。")

(defvar-local wamei/tty-image--cells nil
  "最後に描いた大きさ (桁 . 行)。無ければ nil。")

;;;; 大きさ

(defun wamei/tty-image--window-cells (window)
  "WINDOW に収まる上限の (桁 . 行)。0 は転送が壊れるので 1 を下回らない。"
  (cons (max 1 (window-body-width window))
        (max 1 (window-body-height window))))

(defun wamei/tty-image--target-cells (file window)
  "FILE を WINDOW いっぱいに出すときの (桁 . 行)。
WINDOW が nil か、FILE の大きさを測れなければ nil。"
  (when window
    (let ((size (wamei/kitty-graphics-image-size file)))
      (when size
        (wamei/kitty-graphics-cell-count
         size (wamei/kitty-graphics-cell-size)
         (wamei/tty-image--window-cells window))))))

;;;; 表示

(defun wamei/tty-image--show (id cells)
  "今のバッファ全体に、画像 ID を CELLS (桁 . 行) で描く placeholder を被せる。
テキストは書き換えず `display' プロパティを載せるだけ。変更フラグは元に戻す。"
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (modified (buffer-modified-p)))
    (put-text-property (point-min) (point-max) 'display
                       (wamei/kitty-graphics-placeholder-string
                        id (car cells) (cdr cells)))
    (set-buffer-modified-p modified)))

(defun wamei/tty-image--forget ()
  "端末に置いた画像を解放し、記憶を捨てる。"
  (wamei/kitty-graphics-delete wamei/tty-image--id)
  (setq wamei/tty-image--id nil
        wamei/tty-image--cells nil))

(defun wamei/tty-image--render ()
  "今の window の大きさに合わせて画像を描き直す。
大きさが前と同じなら何もしない (redisplay のたびに転送しないため)。"
  (let* ((file buffer-file-name)
         (window (get-buffer-window (current-buffer)))
         (cells (and file (wamei/tty-image--target-cells file window))))
    (when (and cells (not (equal cells wamei/tty-image--cells)))
      (wamei/tty-image--forget)
      (let ((id (wamei/kitty-graphics-put file (car cells) (cdr cells))))
        (if id
            (progn
              (setq wamei/tty-image--id id
                    wamei/tty-image--cells cells)
              (wamei/tty-image--show id cells))
          ;; `error' は投げない。tty で debug-on-error t だとデバッガに入って操作不能になる。
          ;; 失敗した大きさも `--cells' に記憶しておく (`--id' は nil のまま)。
          ;; そうしないと window が変わるたびに同じ大きさで再送を試みて message を
          ;; 出し続けてしまう。手動での再試行は `--forget' が `--cells' を nil に
          ;; 戻すので塞がらない。
          (setq wamei/tty-image--cells cells)
          (message "画像を端末へ送れませんでした: %s" (file-name-nondirectory file)))))))

;;;; 次/前のファイル

(defun wamei/tty-image--image-files (dir)
  "DIR の中の画像ファイルの絶対パスを名前順に返す。"
  (let ((re (image-file-name-regexp)))
    (seq-filter (lambda (file)
                  (and (not (file-directory-p file))
                       (string-match-p re file)))
                (directory-files dir t))))

(defun wamei/tty-image--sibling (file files n)
  "FILES の中で FILE から N 個ずれた要素。端をはみ出すか FILE が無ければ nil。"
  (let ((i (seq-position files file #'equal)))
    (when i
      (let ((j (+ i n)))
        (when (and (>= j 0) (< j (length files)))
          (nth j files))))))

(defun wamei/tty-image-next-file (&optional n)
  "同じディレクトリの N 個あと (既定 1) の画像を開く。端なら何もしない。"
  (interactive "p" wamei/tty-image-mode)
  (let* ((file buffer-file-name)
         (next (and file
                    (wamei/tty-image--sibling
                     file (wamei/tty-image--image-files (file-name-directory file))
                     (or n 1)))))
    (if next
        (find-alternate-file next)
      (message "これ以上画像がありません"))))

(defun wamei/tty-image-previous-file (&optional n)
  "同じディレクトリの N 個まえ (既定 1) の画像を開く。端なら何もしない。"
  (interactive "p" wamei/tty-image-mode)
  (wamei/tty-image-next-file (- (or n 1))))

(defun wamei/tty-image-refresh ()
  "画像を送り直して描き直す。"
  (interactive nil wamei/tty-image-mode)
  (wamei/tty-image--forget)
  (wamei/tty-image--render))

;;;; モード

(defvar-keymap wamei/tty-image-mode-map
  :doc "`wamei/tty-image-mode' のキーマップ。"
  :parent special-mode-map
  "n" #'wamei/tty-image-next-file
  "p" #'wamei/tty-image-previous-file
  "g" #'wamei/tty-image-refresh)

(define-derived-mode wamei/tty-image-mode special-mode "TtyImage"
  "tty の Emacs で画像ファイルを見るためのモード。
kitty graphics protocol の Unicode placeholder で端末に描く。"
  (setq-local truncate-lines t
              cursor-type nil)
  ;; 行番号は見た目の問題ではない。桁を食われると placeholder の桁数が合わなくなる。
  (display-line-numbers-mode 0)
  (add-hook 'window-configuration-change-hook #'wamei/tty-image--render nil t)
  (add-hook 'kill-buffer-hook #'wamei/tty-image--forget nil t)
  (wamei/tty-image--render))

(provide 'tty-image-mode)
;;; tty-image-mode.el ends here
