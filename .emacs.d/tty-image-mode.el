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
      (when wamei/tty-image--id
        (wamei/tty-image--forget))
      (let ((id (wamei/kitty-graphics-put file (car cells) (cdr cells))))
        (if id
            (progn
              (setq wamei/tty-image--id id
                    wamei/tty-image--cells cells)
              (wamei/tty-image--show id cells))
          ;; `error' は投げない。tty で debug-on-error t だとデバッガに入って操作不能になる。
          (message "画像を端末へ送れませんでした: %s" (file-name-nondirectory file)))))))

(provide 'tty-image-mode)
;;; tty-image-mode.el ends here
