;;; tty-image-dired.el --- tty の Emacs で image-dired のサムネイルを出す -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; 組み込みの `image-dired' には `display-images-p' のガードが無く、tty では
;; `insert-image' がそのまま呼ばれて空白が並ぶだけになる。ここでは kitty graphics
;; protocol の Unicode placeholder でサムネイルをグリッド表示する。
;;
;; `image-dired-thumbnail-mode' から派生させるのが要点。image-dired の多くの
;; コマンドは `(unless (derived-mode-p 'image-dired-thumbnail-mode) (user-error ...))'
;; のガードを持つので、派生していれば点ベースのコマンド (マーク、dired 連動) が
;; そのまま動く。走査するコマンド (移動、x、RET) だけを添字ベースで置き換える。
;;
;; グリッドの座標は保持せず、添字から計算する:
;;
;;   段 r = i / columns、段の中の桁 c = i mod columns
;;   段の先頭行   = r * (箱の行数 + 1)   ; +1 はキャプション行
;;   箱の桁の起点 = c * (箱の桁数 + 1)   ; +1 は箱の間の空き
;;
;;; Code:

(require 'cl-lib)
(require 'image-dired)
(require 'kitty-graphics)

(defgroup wamei/tty-image-dired nil
  "tty の image-dired サムネイル一覧。"
  :group 'image-dired)

(defcustom wamei/tty-image-dired-box-size nil
  "サムネイル 1 枚を置く箱の (桁 . 行)。nil なら
`image-dired-thumb-size' と端末のセルの大きさから決める。"
  :type '(choice (const :tag "自動" nil) (cons integer integer)))

;;;; 幾何

(defun wamei/tty-image-dired--box-size (thumb-px cell-px)
  "THUMB-PX (サムネイルの最大ピクセル) を CELL-PX (幅 . 高さ) で表す箱の (桁 . 行)。
割り上げる。最低 1 桁 1 行。"
  (cons (max 1 (ceiling thumb-px (car cell-px)))
        (max 1 (ceiling thumb-px (cdr cell-px)))))

(defun wamei/tty-image-dired--columns (width box-cols)
  "本文 WIDTH 桁に BOX-COLS 桁の箱を何枚並べられるか。最低 1 枚。
箱 i は桁 [i*(box-cols+1), i*(box-cols+1)+box-cols-1] を占める (箱と箱の間の
1 桁は次の箱との区切りで、n 枚目のあとには置かない)。tty には fringe が無く
最終桁は truncation glyph に取られるが、この区切り分の空きがちょうどその 1 桁を
兼ねるので、WIDTH からあらためて引く必要はない。n*(box-cols+1) <= WIDTH を
満たす最大の n。"
  (max 1 (/ width (1+ box-cols))))

(defun wamei/tty-image-dired--move-index (index total columns direction)
  "INDEX から DIRECTION へ動いた添字。動けなければ nil (巡回しない)。
DIRECTION は forward / backward / down / up / line-beginning / line-end。
TOTAL は枚数、COLUMNS は段あたりの枚数。"
  (let ((next (pcase direction
                ('forward (1+ index))
                ('backward (1- index))
                ('down (+ index columns))
                ('up (- index columns))
                ('line-beginning (* columns (/ index columns)))
                ('line-end (min (1- total) (+ (* columns (/ index columns)) columns -1))))))
    (and next (>= next 0) (< next total) next)))

(defun wamei/tty-image-dired--visible-range (first-line window-lines band-lines columns total)
  "見えている添字の範囲 (最初 . 最後)。1 枚も無ければ nil。
FIRST-LINE は window の先頭がバッファの何行目か (0 起点)、WINDOW-LINES は
見えている行数、BAND-LINES は段の高さ (箱の行数 + キャプション 1 行)。"
  (when (> total 0)
    (let* ((first-band (/ first-line band-lines))
           (last-band (/ (+ first-line (max 0 (1- window-lines))) band-lines))
           (first-index (* first-band columns))
           (last-index (min (1- total) (+ (* last-band columns) columns -1))))
      (when (< first-index total)
        (cons first-index last-index)))))

;;;; バッファの状態

(defvar-local wamei/tty-image-dired--files nil
  "グリッド順に並べたファイルのベクタ。")

(defvar-local wamei/tty-image-dired--ids nil
  "各サムネイルの画像 ID のベクタ。端末に置いていなければその要素は nil。")

(defvar-local wamei/tty-image-dired--columns 1
  "段あたりの枚数。")

(defvar-local wamei/tty-image-dired--box '(1 . 1)
  "箱の (桁 . 行)。")

(defvar-local wamei/tty-image-dired--selected 0
  "選択中のサムネイルの添字。")

(defvar-local wamei/tty-image-dired--dired-buffer nil
  "元の dired バッファ。")

(defun wamei/tty-image-dired--band-lines ()
  "段の高さ (箱の行数 + キャプション 1 行)。"
  (1+ (cdr wamei/tty-image-dired--box)))

;;;; キャプション

(defun wamei/tty-image-dired--caption (file width selected marked)
  "FILE のキャプションを WIDTH 桁で返す。
SELECTED なら `highlight' face、MARKED なら頭に `*'。
placeholder のセルは前景色が画像 ID なので face を当てられない。選択と
マークの表示はこの行が担う。
日本語のファイル名では文字数と表示桁が食い違うので、`string-width' で数える。"
  (let* ((name (concat (if marked "*" "") (file-name-nondirectory file)))
         (text (truncate-string-to-width name width 0 ?\s)))
    (when selected
      (add-face-text-property 0 (length text) 'highlight nil text))
    text))

;;;; 位置

(defun wamei/tty-image-dired--goto-line (line)
  "バッファの LINE 行目 (0 起点) の先頭へ。"
  (goto-char (point-min))
  (forward-line line))

(defun wamei/tty-image-dired--box-line-region (index row)
  "サムネイル INDEX の箱の ROW 行目の領域 (開始 . 終了)。"
  (let* ((columns wamei/tty-image-dired--columns)
         (box-cols (car wamei/tty-image-dired--box))
         (band (/ index columns))
         (col (mod index columns)))
    (save-excursion
      (wamei/tty-image-dired--goto-line (+ (* band (wamei/tty-image-dired--band-lines)) row))
      (move-to-column (* col (1+ box-cols)))
      (let ((start (point)))
        (move-to-column (+ (* col (1+ box-cols)) box-cols))
        (cons start (point))))))

(defun wamei/tty-image-dired--caption-region (index)
  "サムネイル INDEX のキャプション行の領域 (開始 . 終了)。"
  (wamei/tty-image-dired--box-line-region index (cdr wamei/tty-image-dired--box)))

(defun wamei/tty-image-dired--goto-index (index)
  "サムネイル INDEX の箱の左上へ point を移し、選択を更新する。"
  (let ((old wamei/tty-image-dired--selected))
    (setq wamei/tty-image-dired--selected index)
    (wamei/tty-image-dired--redraw-caption old)
    (wamei/tty-image-dired--redraw-caption index))
  (let ((region (wamei/tty-image-dired--box-line-region index 0)))
    (goto-char (car region))))

;;;; 組み立て

(defun wamei/tty-image-dired--marked-p (file)
  "FILE が元の dired バッファでマークされていれば非 nil。"
  (let ((buffer wamei/tty-image-dired--dired-buffer))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (save-excursion
             (and (dired-goto-file file)
                  (image-dired-dired-file-marked-p)))))))

(defun wamei/tty-image-dired--put-properties (index)
  "サムネイル INDEX の箱とキャプションに image-dired と同じプロパティを載せる。"
  (let* ((file (aref wamei/tty-image-dired--files index))
         (props (list 'image-dired-thumbnail t
                      ;; 組み込みは 1 サムネ = 1 文字を前提にしたキーマップを
                      ;; 無効にしている。こちらも同じにする。
                      'keymap nil
                      'original-file-name file
                      'associated-dired-buffer wamei/tty-image-dired--dired-buffer
                      'tags (image-dired-list-tags file)
                      'mouse-face 'highlight
                      'comment (image-dired-get-comment file))))
    (dotimes (row (1+ (cdr wamei/tty-image-dired--box)))
      (let ((region (wamei/tty-image-dired--box-line-region index row)))
        (add-text-properties (car region) (cdr region) props)))))

(defun wamei/tty-image-dired--redraw-caption (index)
  "サムネイル INDEX のキャプションを描き直す。"
  (when (and wamei/tty-image-dired--files
             (< index (length wamei/tty-image-dired--files)))
    (let* ((file (aref wamei/tty-image-dired--files index))
           (region (wamei/tty-image-dired--caption-region index))
           (inhibit-read-only t)
           (text (wamei/tty-image-dired--caption
                  file (car wamei/tty-image-dired--box)
                  (= index wamei/tty-image-dired--selected)
                  (wamei/tty-image-dired--marked-p file))))
      (save-excursion
        (delete-region (car region) (cdr region))
        (goto-char (car region))
        (insert text))
      (wamei/tty-image-dired--put-properties index))))

(defun wamei/tty-image-dired--build (files dired-buffer columns box)
  "現在のバッファに FILES のグリッドを組み立てる。
COLUMNS は段あたりの枚数、BOX は箱の (桁 . 行)。箱は空白のままにし、
画像は可視範囲だけ後から送る (画像 ID は 1〜255 しかないため)。"
  (setq wamei/tty-image-dired--files files
        wamei/tty-image-dired--ids (make-vector (length files) nil)
        wamei/tty-image-dired--columns columns
        wamei/tty-image-dired--box box
        wamei/tty-image-dired--selected 0
        wamei/tty-image-dired--dired-buffer dired-buffer)
  (let* ((inhibit-read-only t)
         (box-cols (car box))
         (box-rows (cdr box))
         (total (length files))
         (bands (ceiling total columns))
         ;; 段の 1 行ぶんの幅。箱の間の 1 桁を含める
         (line-width (* columns (1+ box-cols))))
    (erase-buffer)
    (dotimes (_band bands)
      (dotimes (_row box-rows)
        (insert (make-string line-width ?\s) "\n"))
      (insert (make-string line-width ?\s) "\n"))
    (dotimes (index total)
      (wamei/tty-image-dired--redraw-caption index))
    (goto-char (point-min))))

;;;; サムネイルの実体

(defun wamei/tty-image-dired--make-thumb (src dst)
  "SRC のサムネイルを DST に sips で作る。作れたら非 nil。
組み込みの `image-dired-create-thumb' は非同期なので使わない。ファイルが
できるまで箱を空白にして後から描き直す、という面倒を避ける。"
  (make-directory (file-name-directory dst) t)
  (and (zerop (call-process "sips" nil nil nil
                            "-s" "format" "png"
                            "-Z" (number-to-string image-dired-thumb-size)
                            (expand-file-name src) "--out" dst))
       (file-exists-p dst)
       (> (file-attribute-size (file-attributes dst)) 0)))

(defun wamei/tty-image-dired--thumb-file (file)
  "FILE のサムネイルのパス。無いか古ければ作る。作れなければ nil。
置き場所は `image-dired-thumb-name' が返すパスで、GUI の image-dired と共有する。"
  (let* ((thumb (image-dired-thumb-name file))
         (thumb-attr (file-attributes thumb)))
    (if (and thumb-attr
             (not (time-less-p (file-attribute-modification-time thumb-attr)
                               (file-attribute-modification-time (file-attributes file)))))
        thumb
      (and (wamei/tty-image-dired--make-thumb file thumb) thumb))))

;;;; 送受

(defun wamei/tty-image-dired--fill-box (index lines)
  "サムネイル INDEX の箱に LINES (行ごとの文字列のリスト) を書き込む。
書き換えたあとプロパティを載せ直す (消えると点ベースのコマンドが動かなくなる)。"
  (let ((inhibit-read-only t))
    (save-excursion
      (cl-loop for row from 0 below (cdr wamei/tty-image-dired--box)
               for line in lines
               do (let ((region (wamei/tty-image-dired--box-line-region index row)))
                    (delete-region (car region) (cdr region))
                    (goto-char (car region))
                    (insert line))))
    (wamei/tty-image-dired--put-properties index)))

(defun wamei/tty-image-dired--show-box (index)
  "サムネイル INDEX を端末へ送り、箱に placeholder を書き込む。
サムネイルが作れない / 送れないときは箱を空白のままにする。`error' は投げない。"
  (unless (aref wamei/tty-image-dired--ids index)
    (let* ((file (aref wamei/tty-image-dired--files index))
           (thumb (wamei/tty-image-dired--thumb-file file))
           (px (and thumb (wamei/kitty-graphics-image-size thumb))))
      (when px
        (let* ((cells (wamei/kitty-graphics-cell-count
                       px (wamei/kitty-graphics-cell-size) wamei/tty-image-dired--box))
               (id (wamei/kitty-graphics-put thumb (car cells) (cdr cells) px)))
          (when id
            (aset wamei/tty-image-dired--ids index id)
            (wamei/tty-image-dired--fill-box
             index
             (cl-loop for row from 0 below (cdr wamei/tty-image-dired--box)
                      collect (let ((line (if (< row (cdr cells))
                                              (wamei/kitty-graphics-placeholder-line
                                               id (car cells) row)
                                            "")))
                                ;; placeholder は 1 セル = 3 文字の合成グリフなので、
                                ;; 箱の桁数に合わせるパディングは文字数ではなく表示桁で計算する
                                (concat line
                                        (make-string (- (car wamei/tty-image-dired--box)
                                                        (string-width line))
                                                     ?\s)))))))))))

(defun wamei/tty-image-dired--hide-box (index)
  "サムネイル INDEX を端末から解放し、箱を空白に戻す。"
  (when-let* ((id (aref wamei/tty-image-dired--ids index)))
    (wamei/kitty-graphics-delete id)
    (aset wamei/tty-image-dired--ids index nil)
    (wamei/tty-image-dired--fill-box
     index
     (make-list (cdr wamei/tty-image-dired--box)
                (make-string (car wamei/tty-image-dired--box) ?\s)))))

(defun wamei/tty-image-dired--window-visible-range ()
  "この window で見えているサムネイルの添字の範囲 (最初 . 最後)。無ければ nil。"
  (when-let* ((window (get-buffer-window (current-buffer))))
    (wamei/tty-image-dired--visible-range
     (save-excursion (goto-char (window-start window))
                     (count-lines (point-min) (line-beginning-position)))
     (window-body-height window)
     (wamei/tty-image-dired--band-lines)
     wamei/tty-image-dired--columns
     (length wamei/tty-image-dired--files))))

(defvar-local wamei/tty-image-dired--shown-range nil
  "最後に送った可視範囲 (最初 . 最後)。")

(defun wamei/tty-image-dired--sync-visible ()
  "見えているサムネイルだけが端末に置かれている状態にする。
範囲が前と同じなら何もしない。画像 ID は 1〜255 しかないので、画面外のぶんは解放する。"
  (let ((range (wamei/tty-image-dired--window-visible-range)))
    (unless (equal range wamei/tty-image-dired--shown-range)
      (setq wamei/tty-image-dired--shown-range range)
      (dotimes (index (length wamei/tty-image-dired--files))
        (if (and range (>= index (car range)) (<= index (cdr range)))
            (wamei/tty-image-dired--show-box index)
          (wamei/tty-image-dired--hide-box index))))))

(defun wamei/tty-image-dired--release-all ()
  "端末に置いたサムネイルをすべて解放する。"
  (when wamei/tty-image-dired--ids
    (dotimes (index (length wamei/tty-image-dired--ids))
      (wamei/tty-image-dired--hide-box index)))
  (setq wamei/tty-image-dired--shown-range nil))

(provide 'tty-image-dired)
;;; tty-image-dired.el ends here
