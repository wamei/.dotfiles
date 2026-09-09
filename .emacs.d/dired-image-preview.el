;;; dired-image-preview.el --- dired の画像ファイルにマウス/カーソルを当てるとプレビューを出す -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; `wamei/dired-image-preview-mode' を有効にした dired バッファで、画像ファイルの行に
;; マウスが止まる、または point が乗ると、その画像を行のそばにポップアップ表示する。
;; 行を離れると消える。
;;
;; - 対象の判定 (`--image-file-p' / `--target-at') と追跡 (`--track') は表示手段を知らない
;; - 表示は `wamei/dired-image-preview-display-function' と `-hide-function' に委ねる。
;;   既定は posframe (GUI の child frame)。tty 用の backend (kitty graphics など) は
;;   この 2 つと `wamei/dired-image-preview-available-predicate' を差し替えて足す
;; - マウス追跡は eldoc-mouse.el と同じ方式。`track-mouse' を立てて [mouse-movement] を
;;   受ける。point の追跡は post-command-hook で、マウス移動のコマンドでは動かない
;;   (両方が同時に動くとマウスで出した直後に point 側が消してしまう)
;; - 表示中は global な post-command-hook で見張り、他のバッファでキーを打ったり
;;   対象の window からバッファが消えたら閉じる。dired バッファのローカル hook だけでは
;;   他のバッファへ移った後の片付けができない
;;
;;; Code:

(require 'dired)
(require 'image)
(require 'svg)
(require 'cl-lib)

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-poshandler-point-bottom-left-corner "posframe")

(defgroup wamei/dired-image-preview nil
  "dired の画像ファイルのプレビューをポップアップ表示する。"
  :group 'dired)

(defcustom wamei/dired-image-preview-max-file-size (* 20 1024 1024)
  "この大きさ (バイト) を超える画像はプレビューしない。
巨大な画像のデコードで Emacs が固まるのを避ける。"
  :type 'integer)

(defcustom wamei/dired-image-preview-mouse-delay 0.5
  "マウスが画像の行で止まってから表示するまでの秒数。"
  :type 'number)

(defcustom wamei/dired-image-preview-point-delay 0.3
  "point が画像の行に乗ってから表示するまでの秒数。
行を連続で送っている間に点滅しないよう、少し待つ。"
  :type 'number)

(defcustom wamei/dired-image-preview-max-size-ratio 0.3
  "プレビューの最大の大きさ。フレームの幅・高さに対する比。"
  :type 'number)

(defcustom wamei/dired-image-preview-gap '(2 . 1)
  "プレビューをマウス/point の文字から離す距離 (桁 . 行)。
文字の右下にこの分だけ空けて置く。eldoc-box の `wamei/eldoc-box-at-point-gap' と揃える。"
  :type '(cons integer integer))

(defcustom wamei/dired-image-preview-checkerboard-colors '("#808080" . "#c0c0c0")
  "透過部分を見せるための市松模様の 2 色。"
  :type '(cons color color))

(defvar wamei/dired-image-preview-display-function
  #'wamei/dired-image-preview--posframe-show
  "ターゲット (`wamei/dired-image-preview--target') を受けてプレビューを表示する関数。")

(defvar wamei/dired-image-preview-hide-function
  #'wamei/dired-image-preview--posframe-hide
  "表示中のプレビューを消す関数。引数なし。")

(defvar wamei/dired-image-preview-available-predicate #'display-images-p
  "この環境でプレビューを出せるなら非 nil を返す関数。
nil を返す環境では `wamei/dired-image-preview-mode' は有効にならない。")

;;;; 判定

(defun wamei/dired-image-preview--image-file-p (file)
  "FILE がプレビューできる画像ファイルなら非 nil。
拡張子で画像型が決まり、その型をこの Emacs が描けて、通常ファイルで、
`wamei/dired-image-preview-max-file-size' 以下のものだけを対象にする。"
  (when-let* ((type (image-supported-file-p file))
              ((image-type-available-p type))
              ((file-regular-p file))
              ;; dired は -L で symlink を辿って表示するので、大きさもリンク先で見る
              (attrs (file-attributes (file-chase-links file))))
    (<= (file-attribute-size attrs) wamei/dired-image-preview-max-file-size)))

;;;; ターゲット

(cl-defstruct (wamei/dired-image-preview--target
               (:constructor wamei/dired-image-preview--make-target)
               (:copier nil))
  "プレビューの対象。dired の 1 行に対応する。
ANCHOR はマウス/point のあった位置で、ポップアップはこの文字の右下に置く。"
  window buffer file beg end anchor)

(defun wamei/dired-image-preview--target-at (window pos)
  "WINDOW のバッファの POS にある行が画像ファイルならターゲットを返す。それ以外は nil。"
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when (derived-mode-p 'dired-mode)
        (save-excursion
          (goto-char pos)
          (when-let* ((file (dired-get-filename nil t))
                      ((wamei/dired-image-preview--image-file-p file)))
            (wamei/dired-image-preview--make-target
             :window window :buffer (current-buffer) :file file
             :beg (line-beginning-position) :end (line-end-position)
             :anchor pos)))))))

(defun wamei/dired-image-preview--target-covers-p (target window pos)
  "TARGET が WINDOW の POS (同じバッファの同じ行) を指していれば非 nil。"
  (and target
       (window-live-p window)
       (eq (window-buffer window) (wamei/dired-image-preview--target-buffer target))
       (<= (wamei/dired-image-preview--target-beg target) pos)
       (<= pos (wamei/dired-image-preview--target-end target))))

;;;; 状態

(defvar wamei/dired-image-preview-mode)

(defvar wamei/dired-image-preview--active nil
  "いま追っているターゲット。追っていなければ nil。
表示待ちのタイマーとその発火はこのオブジェクトを `eq' で照合する。")

(defvar wamei/dired-image-preview--timer nil
  "表示待ちのタイマー。")

(defvar wamei/dired-image-preview--shown nil
  "プレビューを表示中なら非 nil。")

(defun wamei/dired-image-preview--cancel-timer ()
  "表示待ちのタイマーを取り消す。"
  (when wamei/dired-image-preview--timer
    (cancel-timer wamei/dired-image-preview--timer)
    (setq wamei/dired-image-preview--timer nil)))

(defun wamei/dired-image-preview--reset ()
  "追っているターゲットを捨て、タイマーと表示を片付ける。"
  (wamei/dired-image-preview--cancel-timer)
  (when wamei/dired-image-preview--shown
    (setq wamei/dired-image-preview--shown nil)
    (funcall wamei/dired-image-preview-hide-function))
  (setq wamei/dired-image-preview--active nil)
  (remove-hook 'post-command-hook #'wamei/dired-image-preview--post-command-global))

(defun wamei/dired-image-preview--show (target)
  "TARGET がまだ追っている対象ならプレビューを表示する。タイマーから呼ばれる。"
  (setq wamei/dired-image-preview--timer nil)
  (when (and (eq target wamei/dired-image-preview--active)
             (window-live-p (wamei/dired-image-preview--target-window target))
             (buffer-live-p (wamei/dired-image-preview--target-buffer target)))
    (setq wamei/dired-image-preview--shown t)
    (funcall wamei/dired-image-preview-display-function target)))

(defun wamei/dired-image-preview--track (window pos delay)
  "WINDOW の POS の行を追う。
同じ行を追っている最中なら何もしない。別の行なら今の表示を片付け、画像の行であれば
DELAY 秒後に表示を予約する。"
  (unless (wamei/dired-image-preview--target-covers-p
           wamei/dired-image-preview--active window pos)
    (wamei/dired-image-preview--reset)
    (when-let* ((target (wamei/dired-image-preview--target-at window pos)))
      (setq wamei/dired-image-preview--active target)
      (add-hook 'post-command-hook #'wamei/dired-image-preview--post-command-global)
      (setq wamei/dired-image-preview--timer
            (run-with-timer delay nil #'wamei/dired-image-preview--show target)))))

;;;; ハンドラ

(defun wamei/dired-image-preview--handle-motion (event)
  "マウス移動 EVENT を受けて追跡する。モードが有効なバッファの上だけ対象にする。"
  (interactive "e")
  ;; post-command-hook で `this-command' を見る側には、既定のマウス移動の束縛と
  ;; 同じ `ignore' に見せる。
  (setq this-command 'ignore)
  (let* ((posn (event-end event))
         (window (posn-window posn))
         (pos (posn-point posn)))
    (when (and (windowp window) (integerp pos))
      (if (buffer-local-value 'wamei/dired-image-preview-mode (window-buffer window))
          (wamei/dired-image-preview--track window pos wamei/dired-image-preview-mouse-delay)
        (wamei/dired-image-preview--reset)))))

(defun wamei/dired-image-preview--post-command ()
  "point の行を追跡する。モードが有効なバッファの post-command-hook (ローカル) 用。
マウス移動のコマンドでは動かない。選択中の window に出ているバッファだけを見る。"
  (unless (mouse-movement-p last-input-event)
    (when (eq (window-buffer) (current-buffer))
      (wamei/dired-image-preview--track (selected-window) (point)
                                        wamei/dired-image-preview-point-delay))))

(defun wamei/dired-image-preview--post-command-global ()
  "追跡中だけ global な post-command-hook に入れる見張り。
対象の window からバッファが消えたら閉じる。マウス移動以外のコマンドが対象以外の
バッファで走ったら閉じる (対象のバッファではローカル hook が point を見る)。"
  (let ((target wamei/dired-image-preview--active))
    (when (and target
               (or (not (eq (window-buffer (wamei/dired-image-preview--target-window target))
                            (wamei/dired-image-preview--target-buffer target)))
                   (not (or (mouse-movement-p last-input-event)
                            (eq (current-buffer)
                                (wamei/dired-image-preview--target-buffer target))))))
      (wamei/dired-image-preview--reset))))

;;;; 大きさと位置

(defun wamei/dired-image-preview--max-pixel-size (frame)
  "FRAME に対する `wamei/dired-image-preview-max-size-ratio' の大きさ (幅 . 高さ) をピクセルで返す。"
  (cons (floor (* wamei/dired-image-preview-max-size-ratio (frame-inner-width frame)))
        (floor (* wamei/dired-image-preview-max-size-ratio (frame-inner-height frame)))))

(defun wamei/dired-image-preview--fit-size (size max)
  "SIZE (幅 . 高さ) を縦横比を保って MAX (幅 . 高さ) に収めた大きさを返す。拡大はしない。"
  (let ((scale (min 1.0
                    (/ (float (car max)) (car size))
                    (/ (float (cdr max)) (cdr size)))))
    (cons (max 1 (round (* scale (car size))))
          (max 1 (round (* scale (cdr size)))))))

(defun wamei/dired-image-preview--max-image-size (frame)
  "FRAME を基準にした `max-image-size' の絶対値 (ピクセル) を返す。
既定の 10.0 (frame の 10 倍) は、画像を読み込む frame で判定される。posframe の
child frame は作られた直後 32px 程度なので、そのままだと 320px を超える画像が
\"Invalid image size\" で読めず、空白 1 文字分の小さな frame だけが残る。
親 frame の大きさで 10 倍を計算した整数にして、child frame でも同じ上限にする。"
  (if (integerp max-image-size)
      max-image-size
    (ceiling (* max-image-size (max (frame-pixel-width frame) (frame-pixel-height frame))))))

(defun wamei/dired-image-preview--pixel-gap (frame)
  "`wamei/dired-image-preview-gap' を FRAME の文字の大きさでピクセルにした (X . Y)。"
  (cons (* (car wamei/dired-image-preview-gap) (frame-char-width frame))
        (* (cdr wamei/dired-image-preview-gap) (frame-char-height frame))))

;;;; 描画

(defvar wamei/dired-image-preview--buffer-name " *dired-image-preview*"
  "プレビューを描くバッファの名前。")

(defconst wamei/dired-image-preview--alpha-types '(png gif webp svg)
  "透過 (アルファ) を持てる画像型。市松模様の上に合成して透過部分を見せる。")

(defun wamei/dired-image-preview--alpha-type-p (type)
  "画像型 TYPE が透過を持てるなら非 nil。"
  (memq type wamei/dired-image-preview--alpha-types))

(defun wamei/dired-image-preview--checkerboard-svg (file type size)
  "FILE (画像型 TYPE) を市松模様の上に SIZE (幅 . 高さ) で載せた SVG を返す。
macOS の Emacs は child frame の背景だけを透明にできない (alpha-background 非対応) ので、
透過部分は SVG の合成で見せる。Emacs 自身の PNG 読み込みはアルファを背景色に
焼き込んでしまうが、librsvg は <image> を正しく合成する。"
  (let* ((width (car size))
         (height (cdr size))
         (svg (svg-create width height))
         (defs (svg-node svg 'defs))
         (pattern (svg-node defs 'pattern :id "checkerboard" :width 16 :height 16
                            :patternUnits "userSpaceOnUse")))
    (svg-node pattern 'rect :width 16 :height 16
              :fill (car wamei/dired-image-preview-checkerboard-colors))
    (svg-node pattern 'rect :width 8 :height 8
              :fill (cdr wamei/dired-image-preview-checkerboard-colors))
    (svg-node pattern 'rect :x 8 :y 8 :width 8 :height 8
              :fill (cdr wamei/dired-image-preview-checkerboard-colors))
    (svg-rectangle svg 0 0 width height :fill "url(#checkerboard)")
    (svg-embed svg file (format "image/%s" (if (eq type 'svg) "svg+xml" type)) nil
               :width width :height height)
    svg))

(defun wamei/dired-image-preview--image (file max frame)
  "FILE の画像を MAX (幅 . 高さ) に収めた image spec を返す。FRAME で大きさを測る。
透過を持てる型は市松模様つきの SVG にする。それ以外は :max-width / :max-height で縮める。"
  (let* ((type (image-supported-file-p file))
         (image (create-image file type nil :max-width (car max) :max-height (cdr max))))
    (if (and (wamei/dired-image-preview--alpha-type-p type)
             (image-type-available-p 'svg))
        (svg-image (wamei/dired-image-preview--checkerboard-svg
                    file type
                    (wamei/dired-image-preview--fit-size
                     (image-size (create-image file type) t frame) max))
                   :scale 1)
      image)))

(defun wamei/dired-image-preview--render (image)
  "IMAGE をプレビュー用のバッファに描いてそのバッファを返す。"
  (with-current-buffer (get-buffer-create wamei/dired-image-preview--buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-image image)
      (goto-char (point-min)))
    (current-buffer)))

;;;; posframe backend

(defun wamei/dired-image-preview--posframe-show (target)
  "TARGET の画像を posframe でアンカー文字の右下に表示する。"
  (let* ((window (wamei/dired-image-preview--target-window target))
         (frame (window-frame window))
         (gap (wamei/dired-image-preview--pixel-gap frame))
         ;; 画像は posframe の fit-frame-to-buffer が child frame で読み込む。
         ;; その判定が child frame の大きさに縛られないよう、親 frame 基準の絶対値にする。
         (max-image-size (wamei/dired-image-preview--max-image-size frame))
         (buffer (wamei/dired-image-preview--render
                  (wamei/dired-image-preview--image
                   (wamei/dired-image-preview--target-file target)
                   (wamei/dired-image-preview--max-pixel-size frame)
                   frame))))
    ;; posframe は選択中の window を親として :position の文字を探すので、
    ;; マウスが別の window にあっても対象の window で計算させる。
    (with-selected-window window
      (posframe-show buffer
                     :position (wamei/dired-image-preview--target-anchor target)
                     :poshandler #'posframe-poshandler-point-bottom-left-corner
                     :x-pixel-offset (car gap)
                     :y-pixel-offset (cdr gap)
                     :border-width 1
                     :border-color (face-attribute 'wamei/popup-border :background nil t)
                     :accept-focus nil))))

(defun wamei/dired-image-preview--posframe-hide ()
  "posframe のプレビューを消す。"
  (when (get-buffer wamei/dired-image-preview--buffer-name)
    (posframe-hide wamei/dired-image-preview--buffer-name)))

;;;; モード

(defvar wamei/dired-image-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-movement] #'wamei/dired-image-preview--handle-motion)
    map)
  "`wamei/dired-image-preview-mode' のキーマップ。マウス移動だけを受ける。")

(defvar-local wamei/dired-image-preview--old-track-mouse nil
  "モードを有効にする前の `track-mouse' の値。")

(define-minor-mode wamei/dired-image-preview-mode
  "dired の画像ファイルにマウスか point を当てるとプレビューを出す。"
  :lighter nil
  :keymap wamei/dired-image-preview-mode-map
  (cond
   ((and wamei/dired-image-preview-mode
         (not (funcall wamei/dired-image-preview-available-predicate)))
    (setq wamei/dired-image-preview-mode nil))
   (wamei/dired-image-preview-mode
    (setq wamei/dired-image-preview--old-track-mouse track-mouse)
    ;; マウス移動イベントは `track-mouse' が非 nil のときだけ届く
    (setq-local track-mouse t)
    (add-hook 'post-command-hook #'wamei/dired-image-preview--post-command nil t))
   (t
    (remove-hook 'post-command-hook #'wamei/dired-image-preview--post-command t)
    (setq track-mouse wamei/dired-image-preview--old-track-mouse)
    (kill-local-variable 'track-mouse)
    (when (and wamei/dired-image-preview--active
               (eq (wamei/dired-image-preview--target-buffer wamei/dired-image-preview--active)
                   (current-buffer)))
      (wamei/dired-image-preview--reset)))))

(provide 'dired-image-preview)
;;; dired-image-preview.el ends here
