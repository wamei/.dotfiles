;;; dired-image-preview-kitty.el --- tty の Emacs で kitty graphics protocol により画像プレビューを出す -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; dired-image-preview の tty 用 backend。Ghostty など kitty graphics protocol の
;; Unicode placeholder (U=1) に対応した端末で、tty child frame に画像を描く。
;; 端末への転送と placeholder 文字列の組み立ては `kitty-graphics' に任せ、
;; ここでは child frame の配置・表示・後始末だけを担う。
;;
;; 座標は Emacs の redisplay が扱うので、tmux の pane 補正は要らない。
;;
;;; Code:

(require 'cl-lib)
(require 'dired-image-preview)
(require 'kitty-graphics)

(defgroup wamei/dired-image-preview-kitty nil
  "tty の dired 画像プレビュー (kitty graphics protocol)。"
  :group 'wamei/dired-image-preview)

;;;; 位置

(defun wamei/dired-image-preview-kitty--frame-position (anchor size frame-size gap)
  "child frame の (左 . 上) を桁・行で返す。
ANCHOR はアンカー文字の (桁 . 行)、SIZE は frame の (桁 . 行)、FRAME-SIZE は親 frame の
(桁 . 行)、GAP は空ける (桁 . 行)。枠 (undecorated nil) は frame の外側 1 文字に描かれる
ので、その分も空ける。下に収まらなければ上、どちらも無理なら下端に寄せる。"
  (let* ((cols (car size)) (rows (cdr size))
         (left (min (+ (car anchor) (car gap) 1)
                    (- (car frame-size) cols 1)))
         (below (+ (cdr anchor) 1 (cdr gap) 1))
         (above (- (cdr anchor) (cdr gap) 1 rows)))
    (cons (max 1 left)
          (cond ((<= (+ below rows 1) (cdr frame-size)) below)
                ((>= (1- above) 0) above)
                (t (max 1 (- (cdr frame-size) rows 1)))))))

;;;; 表示

(defvar wamei/dired-image-preview-kitty--buffer-name " *dired-image-preview-kitty*"
  "placeholder を並べるバッファの名前。")

(defvar wamei/dired-image-preview-kitty--frame nil
  "表示中の child frame。無ければ nil。")

(defvar wamei/dired-image-preview-kitty--shown-id nil
  "表示中の画像 ID。無ければ nil。")

(defun wamei/dired-image-preview-kitty--max-cells (frame)
  "FRAME に対するプレビューの上限 (桁 . 行)。GUI 版と同じ比率を使う。"
  (cons (max 1 (floor (* wamei/dired-image-preview-max-size-ratio (frame-width frame))))
        (max 1 (floor (* wamei/dired-image-preview-max-size-ratio (frame-height frame))))))

(defun wamei/dired-image-preview-kitty--fill-buffer (id cols rows)
  "placeholder バッファに画像 ID の COLS x ROWS を並べて返す。"
  (with-current-buffer (get-buffer-create wamei/dired-image-preview-kitty--buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (wamei/kitty-graphics-placeholder-string id cols rows))
      (goto-char (point-min)))
    (setq-local mode-line-format nil
                header-line-format nil
                cursor-type nil
                truncate-lines t
                show-trailing-whitespace nil
                cursor-in-non-selected-windows nil)
    (current-buffer)))

(defun wamei/dired-image-preview-kitty--make-frame (parent buffer position size)
  "PARENT の上に BUFFER を出す tty child frame を POSITION (左 . 上) に SIZE (桁 . 行) で作る。"
  (let* ((selected (selected-frame))
         (frame (make-frame `((parent-frame . ,parent)
                             (left . ,(car position)) (top . ,(cdr position))
                             (width . ,(car size)) (height . ,(cdr size))
                             (minibuffer . nil)
                             (no-accept-focus . t) (no-focus-on-map . t)
                             (no-other-frame . t)
                             ;; 枠は外側 1 文字。init.el の display table で罫線になる
                             (undecorated . nil)
                             (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                             (vertical-scroll-bars . nil)
                             (cursor-type . nil)
                             (visibility . t)))))
    ;; tty の `make-frame' は作った frame を選択する (GUI では選択しない)。そのままだと
    ;; dired の window が非選択になり、mode-line が非アクティブになってカーソルも消え、
    ;; 最初のキーがプレビュー側へ行ってしまう。元のフレームへ戻す。
    (unless (eq (selected-frame) selected)
      (select-frame selected 'norecord))
    (let ((window (frame-root-window frame)))
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t)
      (set-window-parameter window 'mode-line-format 'none)
      (set-window-parameter window 'header-line-format 'none))
    frame))

(defun wamei/dired-image-preview-kitty-available-p ()
  "この端末で kitty graphics のプレビューを出せるなら非 nil。
tty child frame が使え、端末が kitty graphics に対応していること。"
  (and (featurep 'tty-child-frames)
       (wamei/kitty-graphics-available-p)))

(defun wamei/dired-image-preview-kitty-show (target)
  "TARGET の画像を kitty graphics で child frame に表示する。"
  (wamei/dired-image-preview-kitty-hide)
  (let* ((window (wamei/dired-image-preview--target-window target))
         (frame (window-frame window))
         (file (wamei/dired-image-preview--target-file target))
         (image-px (wamei/kitty-graphics-image-size file)))
    (when image-px
      (let* ((cells (wamei/kitty-graphics-cell-count
                     image-px (wamei/kitty-graphics-cell-size)
                     (wamei/dired-image-preview-kitty--max-cells frame)))
             (id (wamei/kitty-graphics-put file (car cells) (cdr cells))))
        (when id
          (let* ((anchor (window-absolute-pixel-position
                          (wamei/dired-image-preview--target-anchor target) window))
                 (position (wamei/dired-image-preview-kitty--frame-position
                            (or anchor '(0 . 0)) cells
                            (cons (frame-width frame) (frame-height frame))
                            wamei/dired-image-preview-gap)))
            (setq wamei/dired-image-preview-kitty--shown-id id)
            (setq wamei/dired-image-preview-kitty--frame
                  (wamei/dired-image-preview-kitty--make-frame
                   frame (wamei/dired-image-preview-kitty--fill-buffer id (car cells) (cdr cells))
                   position cells))))))))

(defun wamei/dired-image-preview-kitty-hide ()
  "表示中のプレビューを消し、端末側の画像データも解放する。"
  (when (frame-live-p wamei/dired-image-preview-kitty--frame)
    (delete-frame wamei/dired-image-preview-kitty--frame t))
  (setq wamei/dired-image-preview-kitty--frame nil)
  (when wamei/dired-image-preview-kitty--shown-id
    (wamei/kitty-graphics-delete wamei/dired-image-preview-kitty--shown-id)
    (setq wamei/dired-image-preview-kitty--shown-id nil)))

(defun wamei/dired-image-preview-kitty-setup ()
  "dired-image-preview の backend をこの kitty graphics 版にする。"
  (setq wamei/dired-image-preview-display-function #'wamei/dired-image-preview-kitty-show
        wamei/dired-image-preview-hide-function #'wamei/dired-image-preview-kitty-hide
        wamei/dired-image-preview-available-predicate #'wamei/dired-image-preview-kitty-available-p))

(provide 'dired-image-preview-kitty)
;;; dired-image-preview-kitty.el ends here
