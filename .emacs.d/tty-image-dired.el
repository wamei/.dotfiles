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

;;;; 移動

(defun wamei/tty-image-dired--move (direction)
  "DIRECTION へ選択を動かす。端なら動かさずメッセージを出す。"
  (let ((next (wamei/tty-image-dired--move-index
               wamei/tty-image-dired--selected
               (length wamei/tty-image-dired--files)
               wamei/tty-image-dired--columns
               direction)))
    (if next
        (progn (wamei/tty-image-dired--goto-index next)
               (wamei/tty-image-dired--sync-visible))
      (message "これ以上サムネイルがありません"))))

(defun wamei/tty-image-dired-forward-image (&optional _n)
  "次のサムネイルへ。"
  (interactive "p" wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--move 'forward))

(defun wamei/tty-image-dired-backward-image (&optional _n)
  "前のサムネイルへ。"
  (interactive "p" wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--move 'backward))

(defun wamei/tty-image-dired-next-line ()
  "1 段下のサムネイルへ。"
  (interactive nil wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--move 'down))

(defun wamei/tty-image-dired-previous-line ()
  "1 段上のサムネイルへ。"
  (interactive nil wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--move 'up))

(defun wamei/tty-image-dired-move-beginning-of-line ()
  "段の先頭のサムネイルへ。"
  (interactive nil wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--move 'line-beginning))

(defun wamei/tty-image-dired-move-end-of-line ()
  "段の末尾のサムネイルへ。"
  (interactive nil wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--move 'line-end))

(defun wamei/tty-image-dired-first-image ()
  "最初のサムネイルへ。"
  (interactive nil wamei/tty-image-dired-mode)
  (when (> (length wamei/tty-image-dired--files) 0)
    (wamei/tty-image-dired--goto-index 0)
    (wamei/tty-image-dired--sync-visible)))

(defun wamei/tty-image-dired-last-image ()
  "最後のサムネイルへ。"
  (interactive nil wamei/tty-image-dired-mode)
  (when (> (length wamei/tty-image-dired--files) 0)
    (wamei/tty-image-dired--goto-index (1- (length wamei/tty-image-dired--files)))
    (wamei/tty-image-dired--sync-visible)))

(defun wamei/tty-image-dired-scroll-up ()
  "1 画面ぶんスクロールし、見えているサムネイルを送り直す。
選択 (`--selected') は動かさない。dired と同じで、選択は置いたまま画面だけ動く。"
  (interactive nil wamei/tty-image-dired-mode)
  (scroll-up-command)
  (wamei/tty-image-dired--sync-visible))

(defun wamei/tty-image-dired-scroll-down ()
  "1 画面ぶん戻り、見えているサムネイルを送り直す。選択は動かさない。"
  (interactive nil wamei/tty-image-dired-mode)
  (scroll-down-command)
  (wamei/tty-image-dired--sync-visible))

(defun wamei/tty-image-dired--scroll-sync (_window _start)
  "スクロールしたときに可視範囲を送り直す。`window-scroll-functions' 用。
匿名関数にすると `remove-hook' できず、モードに入り直すたびに溜まる。"
  (wamei/tty-image-dired--sync-visible))

;;;; 上書きするコマンド

(defun wamei/tty-image-dired-display-this ()
  "選択中の画像を開く。
組み込みの `image-dired-display-this' は `image-dired-image-mode'
\(`image-mode' 派生) を起こすが、それは Phase 1 の `image-mode' への advice と
噛み合わない。`find-file' で auto-mode-alist → image-mode → advice の経路に載せる。"
  (interactive nil wamei/tty-image-dired-mode)
  (let ((file (aref wamei/tty-image-dired--files wamei/tty-image-dired--selected)))
    (find-file file)))

(defun wamei/tty-image-dired-do-flagged-delete ()
  "dired 側で削除フラグの付いたファイルを消し、一覧を組み直す。
組み込みの `image-dired-do-flagged-delete' は 1 サムネ = 1 文字を前提に
バッファを走査するので使えない。"
  (interactive nil wamei/tty-image-dired-mode)
  (let ((dired-buffer wamei/tty-image-dired--dired-buffer))
    (when (buffer-live-p dired-buffer)
      (with-current-buffer dired-buffer (dired-do-flagged-delete))
      (wamei/tty-image-dired--release-all)
      (let ((files (cl-remove-if-not #'file-exists-p wamei/tty-image-dired--files)))
        (wamei/tty-image-dired--build files dired-buffer
                                      wamei/tty-image-dired--columns
                                      wamei/tty-image-dired--box))
      (wamei/tty-image-dired--goto-index 0)
      (wamei/tty-image-dired--sync-visible))))

(defun wamei/tty-image-dired--mark (action)
  "選択中のサムネイルの元ファイルを dired 側で ACTION する。
ACTION は `mark' / `unmark' / `flag'。処理のあとキャプションを描き直し、
次のサムネイルへ移る。
組み込みの `image-dired--do-mark-command' は使えない。あれはマークのあとに
`image-dired--thumb-update-mark-at-point' で placeholder のセルに face を貼り
\(画像が壊れる)、`image-dired-display-next' で 1 文字走査の移動をしたうえ画像まで
開いてしまう。どちらも関数呼び出しなので remap では止められない。"
  (let ((file (and wamei/tty-image-dired--files
                   (> (length wamei/tty-image-dired--files) 0)
                   (aref wamei/tty-image-dired--files
                         wamei/tty-image-dired--selected)))
        (dired-buffer wamei/tty-image-dired--dired-buffer))
    (cond
     ((not file) (message "サムネイルがありません"))
     ((not (buffer-live-p dired-buffer)) (message "dired バッファがありません"))
     (t
      (with-current-buffer dired-buffer
        (save-excursion
          (when (dired-goto-file file)
            (pcase action
              ('mark (dired-mark 1))
              ('unmark (dired-unmark 1))
              ('flag (dired-flag-file-deletion 1))))))
      (wamei/tty-image-dired--redraw-caption wamei/tty-image-dired--selected)
      ;; 組み込みは `image-dired-marking-shows-next' で毎回画像を開くが、
      ;; tty では全画面になって邪魔なので、次へ移るだけにする。
      (wamei/tty-image-dired--move 'forward)))))

(defun wamei/tty-image-dired-mark-thumb-original-file ()
  "選択中のサムネイルの元ファイルを dired でマークし、次へ移る。"
  (interactive nil wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--mark 'mark))

(defun wamei/tty-image-dired-unmark-thumb-original-file ()
  "選択中のサムネイルの元ファイルの dired のマークを外し、次へ移る。"
  (interactive nil wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--mark 'unmark))

(defun wamei/tty-image-dired-flag-thumb-original-file ()
  "選択中のサムネイルの元ファイルに dired で削除フラグを立て、次へ移る。"
  (interactive nil wamei/tty-image-dired-mode)
  (wamei/tty-image-dired--mark 'flag))

(defun wamei/tty-image-dired-unmark-all-marks ()
  "dired 側のマークを全部外し、キャプションを描き直す。選択は動かさない。"
  (interactive nil wamei/tty-image-dired-mode)
  (let ((dired-buffer wamei/tty-image-dired--dired-buffer))
    (if (not (buffer-live-p dired-buffer))
        (message "dired バッファがありません")
      (with-current-buffer dired-buffer (dired-unmark-all-marks))
      (dotimes (index (length wamei/tty-image-dired--files))
        (wamei/tty-image-dired--redraw-caption index)))))

(defun wamei/tty-image-dired--update-marks ()
  "マークの表示を更新する。`image-dired--thumb-update-marks' の差し替え先。
組み込みはサムネイルの枠の見た目で表すが、placeholder のセルには face を
当てられないのでキャプションで表す。
組み込みは自分の中でサムネイルバッファへ切り替えるので、advice として呼ばれる
時点の current buffer は呼び出し元 (dired バッファ) である。ここで明示的に
サムネイルバッファを見る。"
  (when-let* ((buffer (get-buffer image-dired-thumbnail-buffer)))
    (with-current-buffer buffer
      (when (derived-mode-p 'wamei/tty-image-dired-mode)
        (dotimes (index (length wamei/tty-image-dired--files))
          (wamei/tty-image-dired--redraw-caption index))))))

;;;; モード

(defvar-keymap wamei/tty-image-dired-mode-map
  :doc "`wamei/tty-image-dired-mode' のキーマップ。
組み込みの移動コマンドは 1 文字ずつ走査するので矩形の中で止まる。差し替える。
親の `image-dired-thumbnail-mode-map' は矢印キーや C-f / C-n / C-v を <remap> でも
組み込みへ飛ばしているので、リテラルのキーだけでなく remap も上書きする。"
  :parent image-dired-thumbnail-mode-map
  "f" #'wamei/tty-image-dired-forward-image
  "b" #'wamei/tty-image-dired-backward-image
  "n" #'wamei/tty-image-dired-next-line
  "p" #'wamei/tty-image-dired-previous-line
  "a" #'wamei/tty-image-dired-move-beginning-of-line
  "e" #'wamei/tty-image-dired-move-end-of-line
  "x" #'wamei/tty-image-dired-do-flagged-delete
  "RET" #'wamei/tty-image-dired-display-this
  "m" #'wamei/tty-image-dired-mark-thumb-original-file
  "u" #'wamei/tty-image-dired-unmark-thumb-original-file
  "d" #'wamei/tty-image-dired-flag-thumb-original-file
  "<delete>" #'wamei/tty-image-dired-flag-thumb-original-file
  "U" #'wamei/tty-image-dired-unmark-all-marks
  "<remap> <forward-char>"           #'wamei/tty-image-dired-forward-image
  "<remap> <right-char>"             #'wamei/tty-image-dired-forward-image
  "<remap> <backward-char>"          #'wamei/tty-image-dired-backward-image
  "<remap> <left-char>"              #'wamei/tty-image-dired-backward-image
  "<remap> <next-line>"              #'wamei/tty-image-dired-next-line
  "<remap> <previous-line>"          #'wamei/tty-image-dired-previous-line
  "<remap> <move-beginning-of-line>" #'wamei/tty-image-dired-move-beginning-of-line
  "<remap> <move-end-of-line>"       #'wamei/tty-image-dired-move-end-of-line
  "<remap> <beginning-of-buffer>"    #'wamei/tty-image-dired-first-image
  "<remap> <end-of-buffer>"          #'wamei/tty-image-dired-last-image
  "<remap> <scroll-up-command>"      #'wamei/tty-image-dired-scroll-up
  "<remap> <scroll-down-command>"    #'wamei/tty-image-dired-scroll-down)

(define-derived-mode wamei/tty-image-dired-mode image-dired-thumbnail-mode "TtyImageDired"
  "tty の Emacs で image-dired のサムネイルをグリッド表示するモード。
`image-dired-thumbnail-mode' から派生させることで、点ベースの組み込みコマンド
\(マーク、dired 連動) をそのまま使う。"
  (setq-local truncate-lines t
              cursor-type nil)
  ;; 行番号は見た目の問題ではない。桁を食われると placeholder の桁数が合わなくなる。
  (display-line-numbers-mode 0)
  (add-hook 'window-configuration-change-hook #'wamei/tty-image-dired--sync-visible nil t)
  (add-hook 'window-scroll-functions #'wamei/tty-image-dired--scroll-sync nil t)
  (add-hook 'kill-buffer-hook #'wamei/tty-image-dired--release-all nil t))

;;;; 入口

(defun wamei/tty-image-dired--show-thumbs (&optional arg _append _do-not-pop)
  "dired でマークされているファイルのサムネイルをグリッド表示する。
ARG があれば point のファイル 1 枚だけ。画像が無ければバッファは作らない。
段あたりの枚数は、表示先の window の実幅で決める。組み立ててから表示すると、
表示先が現在の window より狭いときにグリッドが壊れる。"
  (let* ((dired-buffer (current-buffer))
         (files (vconcat (dired-get-marked-files nil (and arg 1)))))
    (if (zerop (length files))
        (message "画像ファイルがありません")
      (let ((buffer (get-buffer-create image-dired-thumbnail-buffer)))
        (with-current-buffer buffer
          (unless (derived-mode-p 'wamei/tty-image-dired-mode)
            (wamei/tty-image-dired-mode))
          (wamei/tty-image-dired--release-all))
        (let ((window (display-buffer buffer)))
          (with-current-buffer buffer
            (let* ((box (or wamei/tty-image-dired-box-size
                            (wamei/tty-image-dired--box-size
                             image-dired-thumb-size (wamei/kitty-graphics-cell-size))))
                   (columns (wamei/tty-image-dired--columns
                             (window-body-width (or window (selected-window)))
                             (car box))))
              (wamei/tty-image-dired--build files dired-buffer columns box)
              (wamei/tty-image-dired--goto-index 0)
              (wamei/tty-image-dired--sync-visible))))))))

(defun wamei/tty-image-dired--display-thumbs-around (orig &rest args)
  "tty で `image-dired-display-thumbs' の代わりに呼ばれる。
端末が kitty graphics に対応していれば自分のグリッド、していなければ
元の実装をそのまま呼ぶ (空白が並ぶだけで害はない)。`error' は投げない。"
  (if (wamei/kitty-graphics-available-p)
      (apply #'wamei/tty-image-dired--show-thumbs args)
    (message "この端末は kitty graphics に対応していないのでサムネイルは出ません")
    (apply orig args)))

(defun wamei/tty-image-dired--update-marks-around (orig &rest args)
  "tty のグリッドでは、組み込みのマーク表示を走らせない。
組み込みは `image-dired-thumbnail-buffer' を 1 文字ずつ歩いて
`add-face-text-property' で face を貼るが、placeholder のセルは前景色が画像 ID
なので face を重ねると画像が壊れる。自分のモードのときはキャプションだけ更新する。"
  (let ((buffer (get-buffer image-dired-thumbnail-buffer)))
    (if (and buffer
             (with-current-buffer buffer
               (derived-mode-p 'wamei/tty-image-dired-mode)))
        (wamei/tty-image-dired--update-marks)
      (apply orig args))))

(defun wamei/tty-image-dired--update-mark-at-point-around (orig &rest args)
  "自分のモードでは組み込みのマーク描画を走らせない。
`add-face-text-property' で placeholder のセルに face を貼ると、前景色が画像 ID
なので画像が壊れる。自前のマークコマンド (`m'/`u'/`d'/`U') からはもう呼ばれないが、
組み込みの他のコマンド経由で呼ばれても壊さないようにする。"
  (unless (derived-mode-p 'wamei/tty-image-dired-mode)
    (apply orig args)))

(defun wamei/tty-image-dired-setup ()
  "tty で image-dired のサムネイルがグリッド表示されるようにする。
`M-x image-dired' も dired からの呼び出しも `image-dired-display-thumbs' を通る。"
  (advice-add 'image-dired-display-thumbs :around
              #'wamei/tty-image-dired--display-thumbs-around)
  (advice-add 'image-dired--thumb-update-marks :around
              #'wamei/tty-image-dired--update-marks-around)
  (advice-add 'image-dired--thumb-update-mark-at-point :around
              #'wamei/tty-image-dired--update-mark-at-point-around))

(provide 'tty-image-dired)
;;; tty-image-dired.el ends here
