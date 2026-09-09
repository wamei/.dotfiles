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

(provide 'tty-image-dired)
;;; tty-image-dired.el ends here
