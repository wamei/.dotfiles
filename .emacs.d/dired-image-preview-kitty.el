;;; dired-image-preview-kitty.el --- tty の Emacs で kitty graphics protocol により画像プレビューを出す -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; dired-image-preview の tty 用 backend。Ghostty など kitty graphics protocol の
;; Unicode placeholder (U=1) に対応した端末で、tty child frame に画像を描く。
;;
;; 仕組み:
;; - 画像を PNG にして端末へ送り (a=T,U=1,i=ID,c=COLS,r=ROWS)、仮想配置を作る
;; - U+10EEEE + 行の結合文字 + 桁の結合文字 を「前景色 = ID」で COLS x ROWS 並べた
;;   バッファを tty child frame に出す。端末はそのセルに画像を描く
;; - child frame を消すと Emacs が下の文字を描き直し、画像も消える。端末側の画像データは
;;   a=d,d=I で解放する
;;
;; 座標は Emacs の redisplay が扱うので、tmux の pane 補正は要らない。tmux の中では
;; エスケープシーケンスを passthrough (DCS tmux;) で包む (allow-passthrough on が必要)。
;;
;; tty の Emacs は合成 (composition) していない結合文字を 1 桁ずつ描くので、
;; 1 セル分の 3 文字は `compose-string' で必ず 1 グリフに合成する。これを忘れると
;; 1 セルが 3 桁になり、画像が縦縞になる。
;;
;; 端末への問い合わせ (a=q の対応確認、CSI 16 t のセル pixel) の応答はキー入力として
;; 届くので、送った直後に `read-event' で回収する。
;;
;;; Code:

(require 'cl-lib)
(require 'dired-image-preview)

(defgroup wamei/dired-image-preview-kitty nil
  "tty の dired 画像プレビュー (kitty graphics protocol)。"
  :group 'wamei/dired-image-preview)

(defcustom wamei/dired-image-preview-kitty-max-pixels 1200
  "端末へ送る前に画像の長辺をこのピクセル数まで縮める。転送量を抑える。"
  :type 'integer)

(defcustom wamei/dired-image-preview-kitty-response-timeout 0.5
  "端末からの応答を待つ秒数。"
  :type 'number)

(defconst wamei/dired-image-preview-kitty--chunk-size 4096
  "base64 の 1 チャンクの大きさ。protocol の上限。")

(defconst wamei/dired-image-preview-kitty--placeholder #x10EEEE
  "画像を描くセルに置く文字。")

(defconst wamei/dired-image-preview-kitty--diacritics
  [#x0305 #x030D #x030E #x0310 #x0312 #x033D #x033E #x033F #x0346 #x034A #x034B #x034C #x0350 #x0351 #x0352 #x0357
   #x035B #x0363 #x0364 #x0365 #x0366 #x0367 #x0368 #x0369 #x036A #x036B #x036C #x036D #x036E #x036F #x0483 #x0484
   #x0485 #x0486 #x0487 #x0592 #x0593 #x0594 #x0595 #x0597 #x0598 #x0599 #x059C #x059D #x059E #x059F #x05A0 #x05A1
   #x05A8 #x05A9 #x05AB #x05AC #x05AF #x05C4 #x0610 #x0611 #x0612 #x0613 #x0614 #x0615 #x0616 #x0617 #x0657 #x0658
   #x0659 #x065A #x065B #x065D #x065E #x06D6 #x06D7 #x06D8 #x06D9 #x06DA #x06DB #x06DC #x06DF #x06E0 #x06E1 #x06E2
   #x06E4 #x06E7 #x06E8 #x06EB #x06EC #x0730 #x0732 #x0733 #x0735 #x0736 #x073A #x073D #x073F #x0740 #x0741 #x0743
   #x0745 #x0747 #x0749 #x074A #x07EB #x07EC #x07ED #x07EE #x07EF #x07F0 #x07F1 #x07F3 #x0816 #x0817 #x0818 #x0819
   #x081B #x081C #x081D #x081E #x081F #x0820 #x0821 #x0822 #x0823 #x0825 #x0826 #x0827 #x0829 #x082A #x082B #x082C
   #x082D #x0951 #x0953 #x0954 #x0F82 #x0F83 #x0F86 #x0F87 #x135D #x135E #x135F #x17DD #x193A #x1A17 #x1A75 #x1A76
   #x1A77 #x1A78 #x1A79 #x1A7A #x1A7B #x1A7C #x1B6B #x1B6D #x1B6E #x1B6F #x1B70 #x1B71 #x1B72 #x1B73 #x1CD0 #x1CD1
   #x1CD2 #x1CDA #x1CDB #x1CE0 #x1DC0 #x1DC1 #x1DC3 #x1DC4 #x1DC5 #x1DC6 #x1DC7 #x1DC8 #x1DC9 #x1DCB #x1DCC #x1DD1
   #x1DD2 #x1DD3 #x1DD4 #x1DD5 #x1DD6 #x1DD7 #x1DD8 #x1DD9 #x1DDA #x1DDB #x1DDC #x1DDD #x1DDE #x1DDF #x1DE0 #x1DE1
   #x1DE2 #x1DE3 #x1DE4 #x1DE5 #x1DE6 #x1DFE #x20D0 #x20D1 #x20D4 #x20D5 #x20D6 #x20D7 #x20DB #x20DC #x20E1 #x20E7
   #x20E9 #x20F0 #x2CEF #x2CF0 #x2CF1 #x2DE0 #x2DE1 #x2DE2 #x2DE3 #x2DE4 #x2DE5 #x2DE6 #x2DE7 #x2DE8 #x2DE9 #x2DEA
   #x2DEB #x2DEC #x2DED #x2DEE #x2DEF #x2DF0 #x2DF1 #x2DF2 #x2DF3 #x2DF4 #x2DF5 #x2DF6 #x2DF7 #x2DF8 #x2DF9 #x2DFA
   #x2DFB #x2DFC #x2DFD #x2DFE #x2DFF #xA66F #xA67C #xA67D #xA6F0 #xA6F1 #xA8E0 #xA8E1 #xA8E2 #xA8E3 #xA8E4 #xA8E5
   #xA8E6 #xA8E7 #xA8E8 #xA8E9 #xA8EA #xA8EB #xA8EC #xA8ED #xA8EE #xA8EF #xA8F0 #xA8F1 #xAAB0 #xAAB2 #xAAB3 #xAAB7
   #xAAB8 #xAABE #xAABF #xAAC1 #xFE20 #xFE21 #xFE22 #xFE23 #xFE24 #xFE25 #xFE26 #x10A0F #x10A38 #x1D185 #x1D186 #x1D187
   #x1D188 #x1D189 #x1D1AA #x1D1AB #x1D1AC #x1D1AD #x1D242 #x1D243 #x1D244]
  "行・桁の番号を表す結合文字の表 (kitty の gen/rowcolumn-diacritics.txt、297 個)。添字が番号。")

;;;; エスケープシーケンス

(defun wamei/dired-image-preview-kitty--wrap (seq tmux-p)
  "SEQ を端末へ送る形にする。TMUX-P なら tmux の passthrough で包む。
包むときは中の ESC を二重にする (tmux の DCS の規約)。"
  (if tmux-p
      (concat "\ePtmux;" (replace-regexp-in-string "\e" "\e\e" seq t t) "\e\\")
    seq))

(defun wamei/dired-image-preview-kitty--chunks (string size)
  "STRING を SIZE 文字ずつのリストにする。空文字列は (\"\")。"
  (if (string-empty-p string)
      (list "")
    (let ((chunks nil) (i 0) (n (length string)))
      (while (< i n)
        (push (substring string i (min n (+ i size))) chunks)
        (setq i (+ i size)))
      (nreverse chunks))))

(defun wamei/dired-image-preview-kitty--transmit-sequences (b64 id cols rows &optional chunk-size)
  "PNG の base64 B64 を ID で送り COLS x ROWS の仮想配置を作るシーケンスのリスト。
先頭だけ制御キーを持ち、続きは m (続きがあるか) だけ。q=2 で応答を止める。"
  (let* ((chunks (wamei/dired-image-preview-kitty--chunks
                  b64 (or chunk-size wamei/dired-image-preview-kitty--chunk-size)))
         (last (1- (length chunks))))
    (cl-loop for chunk in chunks
             for i from 0
             collect (concat "\e_G"
                             (when (= i 0)
                               (format "a=T,U=1,f=100,i=%d,c=%d,r=%d,q=2," id cols rows))
                             (format "m=%d;%s\e\\" (if (< i last) 1 0) chunk)))))

(defun wamei/dired-image-preview-kitty--delete-sequence (id)
  "画像 ID の配置とデータを端末から消すシーケンス。"
  (format "\e_Ga=d,d=I,i=%d,q=2\e\\" id))

(defun wamei/dired-image-preview-kitty--query-sequence ()
  "端末が kitty graphics に対応しているかを訊くシーケンス。対応なら i=31;OK が返る。"
  "\e_Ga=q,i=31,s=1,v=1,f=24;AAAA\e\\")

(defun wamei/dired-image-preview-kitty--query-ok-p (response)
  "RESPONSE (端末からの応答) が対応を示していれば非 nil。"
  (and (string-match-p "_Gi=31;OK" response) t))

;;;; セルサイズ

(defun wamei/dired-image-preview-kitty--parse-cell-size (response)
  "CSI 16 t への応答 RESPONSE からセルの (幅 . 高さ) ピクセルを返す。無ければ nil。
応答は CSI 6 ; 高さ ; 幅 t の順。"
  (when (string-match "\\[6;\\([0-9]+\\);\\([0-9]+\\)t" response)
    (let ((height (string-to-number (match-string 1 response)))
          (width (string-to-number (match-string 2 response))))
      (when (and (> width 0) (> height 0))
        (cons width height)))))

(defun wamei/dired-image-preview-kitty--cell-count (image-px cell-px max-cells)
  "IMAGE-PX (幅 . 高さ) の画像を CELL-PX (幅 . 高さ) のセルで描くときの (桁 . 行)。
縦横比を保って MAX-CELLS (桁 . 行) に収める。拡大はしない。最低 1x1。"
  (let* ((cols (/ (float (car image-px)) (car cell-px)))
         (rows (/ (float (cdr image-px)) (cdr cell-px)))
         (scale (min 1.0 (/ (car max-cells) cols) (/ (cdr max-cells) rows))))
    (cons (max 1 (round (* cols scale)))
          (max 1 (round (* rows scale))))))

;;;; placeholder

(defun wamei/dired-image-preview-kitty--diacritic (n)
  "番号 N (0〜296) を表す結合文字。"
  (aref wamei/dired-image-preview-kitty--diacritics n))

(defun wamei/dired-image-preview-kitty--color (id color-cells)
  "画像 ID を前景色で表す色名。
24bit なら #0000ID、256 色なら color-ID (#0000NN は 256 色モードでは別の index に丸められる)。"
  (if (>= color-cells 16777216)
      (format "#%06X" id)
    (format "color-%d" id)))

(defun wamei/dired-image-preview-kitty--placeholder-line (id cols row color)
  "画像 ID の ROW 行目を COLS 桁ぶん描く placeholder の文字列。前景色は COLOR。
1 セル (基底 + 行 + 桁の結合文字) ずつ合成して 1 桁のグリフにする。"
  (ignore id)
  ;; make-string で作る文字列は unibyte になり多バイト文字を aset できないので、
  ;; 文字のリストから multibyte 文字列を組む
  (let ((line (apply #'string
                     (cl-loop for col below cols
                              collect wamei/dired-image-preview-kitty--placeholder
                              collect (wamei/dired-image-preview-kitty--diacritic row)
                              collect (wamei/dired-image-preview-kitty--diacritic col)))))
    (dotimes (col cols)
      (compose-string line (* col 3) (+ (* col 3) 3)))
    (add-face-text-property 0 (length line) `(:foreground ,color) nil line)
    line))

(defvar wamei/dired-image-preview-kitty--last-id 0
  "最後に使った画像 ID。")

(defun wamei/dired-image-preview-kitty--next-id ()
  "次の画像 ID。256 色モードでも前景色で表せるよう 1〜255 を巡回する。"
  (setq wamei/dired-image-preview-kitty--last-id
        (1+ (mod wamei/dired-image-preview-kitty--last-id 255))))

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

;;;; 画像の準備 (sips)

(defun wamei/dired-image-preview-kitty--parse-sips-size (output)
  "sips -g pixelWidth -g pixelHeight の OUTPUT から (幅 . 高さ) を返す。"
  (when (and (string-match "pixelWidth: \\([0-9]+\\)" output)
             (let ((w (string-to-number (match-string 1 output))))
               (and (string-match "pixelHeight: \\([0-9]+\\)" output)
                    (setq output (cons w (string-to-number (match-string 1 output)))))))
    output))

(defun wamei/dired-image-preview-kitty--resize-args (size max-pixels)
  "SIZE (幅 . 高さ) の長辺が MAX-PIXELS を超えるときだけ sips の縮小引数を返す。
sips の -Z は小さい画像を拡大してしまうので、必要なときにしか付けない。"
  (when (> (max (car size) (cdr size)) max-pixels)
    (list "-Z" (number-to-string max-pixels))))

(defun wamei/dired-image-preview-kitty--image-size (file)
  "sips で FILE の (幅 . 高さ) を測る。測れなければ nil。"
  (wamei/dired-image-preview-kitty--parse-sips-size
   (with-output-to-string
     (call-process "sips" nil standard-output nil
                   "-g" "pixelWidth" "-g" "pixelHeight" (expand-file-name file)))))

(defun wamei/dired-image-preview-kitty--prepare-png (file)
  "FILE を長辺 `wamei/dired-image-preview-kitty-max-pixels' 以下の PNG にして
\(一時ファイル . (幅 . 高さ)) を返す。失敗したら nil。呼び手が一時ファイルを消す。"
  (when-let* ((size (wamei/dired-image-preview-kitty--image-size file))
              (resize (or (wamei/dired-image-preview-kitty--resize-args
                           size wamei/dired-image-preview-kitty-max-pixels)
                          'none))
              (out (make-temp-file "dired-image-preview-" nil ".png")))
    (if (and (zerop (apply #'call-process "sips" nil nil nil
                           `("-s" "format" "png"
                             ,@(unless (eq resize 'none) resize)
                             ,(expand-file-name file) "--out" ,out)))
             (> (file-attribute-size (file-attributes out)) 0))
        (cons out (if (eq resize 'none)
                      size
                    (or (wamei/dired-image-preview-kitty--image-size out) size)))
      (delete-file out)
      nil)))

;;;; 端末との対話

(defun wamei/dired-image-preview-kitty--send (seq)
  "SEQ を端末へ送る。tmux の中なら passthrough で包む。"
  (send-string-to-terminal
   (wamei/dired-image-preview-kitty--wrap seq (and (getenv "TMUX") t))))

(defun wamei/dired-image-preview-kitty--read-response ()
  "端末からの応答を `wamei/dired-image-preview-kitty-response-timeout' 秒だけ集めて返す。
応答はキー入力として届く。フォーカスイベントなどが混ざることがある。"
  (let ((deadline (+ (float-time) wamei/dired-image-preview-kitty-response-timeout))
        (acc nil) ev)
    (while (and (< (float-time) deadline)
                (setq ev (read-event nil nil (max 0.05 (- deadline (float-time))))))
      (push ev acc))
    (mapconcat (lambda (e) (if (characterp e) (string e) "")) (nreverse acc) "")))

(defun wamei/dired-image-preview-kitty--query-terminal (param seq parse)
  "端末に SEQ を送り応答を PARSE で読んだ結果を、端末パラメータ PARAM に記憶して返す。
一度訊いたら訊き直さない。結果が nil のときは `none' を記憶する。"
  (let ((cached (terminal-parameter nil param)))
    (cond ((eq cached 'none) nil)
          (cached cached)
          (t (wamei/dired-image-preview-kitty--send seq)
             (let ((result (funcall parse (wamei/dired-image-preview-kitty--read-response))))
               (set-terminal-parameter nil param (or result 'none))
               result)))))

(defun wamei/dired-image-preview-kitty-available-p ()
  "この端末で kitty graphics のプレビューを出せるなら非 nil。
tty child frame が使え、端末が a=q に OK を返すこと。結果は端末ごとに記憶する。"
  (and (not (display-graphic-p))
       (featurep 'tty-child-frames)
       (wamei/dired-image-preview-kitty--query-terminal
        'wamei/dired-image-preview-kitty-supported
        (wamei/dired-image-preview-kitty--query-sequence)
        (lambda (response)
          (and (wamei/dired-image-preview-kitty--query-ok-p response) t)))))

(defun wamei/dired-image-preview-kitty--cell-size ()
  "この端末のセルの (幅 . 高さ) ピクセル。CSI 16 t で訊き、端末ごとに記憶する。
訊けなければ 8x16 とみなす。"
  (or (wamei/dired-image-preview-kitty--query-terminal
       'wamei/dired-image-preview-kitty-cell-size
       "\e[16t"
       #'wamei/dired-image-preview-kitty--parse-cell-size)
      '(8 . 16)))

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
  (let ((color (wamei/dired-image-preview-kitty--color id (display-color-cells))))
    (with-current-buffer (get-buffer-create wamei/dired-image-preview-kitty--buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dotimes (row rows)
          (unless (= row 0) (insert "\n"))
          (insert (wamei/dired-image-preview-kitty--placeholder-line id cols row color)))
        (goto-char (point-min)))
      (setq-local mode-line-format nil
                  header-line-format nil
                  cursor-type nil
                  truncate-lines t
                  show-trailing-whitespace nil
                  cursor-in-non-selected-windows nil)
      (current-buffer))))

(defun wamei/dired-image-preview-kitty--make-frame (parent buffer position size)
  "PARENT の上に BUFFER を出す tty child frame を POSITION (左 . 上) に SIZE (桁 . 行) で作る。"
  (let* ((selected (selected-frame))
         (frame (make-frame `((parent-frame . ,parent)
                             (left . ,(car position)) (top . ,(cdr position))
                             (width . ,(car size)) (height . ,(cdr size))
                             (minibuffer . nil)
                             (no-accept-focus . t) (no-focus-on-map . t)
                             (no-other-frame . t)
                             ;; 端末には fringe が無いので、`truncate-lines' の行が window の
                             ;; 幅ぴったりでも最終桁が truncation glyph に取られる。placeholder は
                             ;; 幅ぴったりに並べるため、そのままだと画像の右端 1 列が消える。
                             (no-special-glyphs . t)
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

(defun wamei/dired-image-preview-kitty-show (target)
  "TARGET の画像を kitty graphics で child frame に表示する。"
  (wamei/dired-image-preview-kitty-hide)
  (pcase-let* ((window (wamei/dired-image-preview--target-window target))
               (frame (window-frame window))
               (`(,png . ,image-px) (wamei/dired-image-preview-kitty--prepare-png
                                     (wamei/dired-image-preview--target-file target))))
    (when png
      (unwind-protect
          (let* ((cells (wamei/dired-image-preview-kitty--cell-count
                         image-px (wamei/dired-image-preview-kitty--cell-size)
                         (wamei/dired-image-preview-kitty--max-cells frame)))
                 (id (wamei/dired-image-preview-kitty--next-id))
                 (b64 (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert-file-contents-literally png)
                        (base64-encode-string (buffer-string) t)))
                 (anchor (window-absolute-pixel-position
                          (wamei/dired-image-preview--target-anchor target) window))
                 (position (wamei/dired-image-preview-kitty--frame-position
                            (or anchor '(0 . 0)) cells
                            (cons (frame-width frame) (frame-height frame))
                            wamei/dired-image-preview-gap)))
            (dolist (seq (wamei/dired-image-preview-kitty--transmit-sequences
                          b64 id (car cells) (cdr cells)))
              (wamei/dired-image-preview-kitty--send seq))
            (setq wamei/dired-image-preview-kitty--shown-id id)
            (setq wamei/dired-image-preview-kitty--frame
                  (wamei/dired-image-preview-kitty--make-frame
                   frame (wamei/dired-image-preview-kitty--fill-buffer id (car cells) (cdr cells))
                   position cells)))
        (delete-file png)))))

(defun wamei/dired-image-preview-kitty-hide ()
  "表示中のプレビューを消し、端末側の画像データも解放する。"
  (when (frame-live-p wamei/dired-image-preview-kitty--frame)
    (delete-frame wamei/dired-image-preview-kitty--frame t))
  (setq wamei/dired-image-preview-kitty--frame nil)
  (when wamei/dired-image-preview-kitty--shown-id
    (wamei/dired-image-preview-kitty--send
     (wamei/dired-image-preview-kitty--delete-sequence wamei/dired-image-preview-kitty--shown-id))
    (setq wamei/dired-image-preview-kitty--shown-id nil)))

(defun wamei/dired-image-preview-kitty-setup ()
  "dired-image-preview の backend をこの kitty graphics 版にする。"
  (setq wamei/dired-image-preview-display-function #'wamei/dired-image-preview-kitty-show
        wamei/dired-image-preview-hide-function #'wamei/dired-image-preview-kitty-hide
        wamei/dired-image-preview-available-predicate #'wamei/dired-image-preview-kitty-available-p))

(provide 'dired-image-preview-kitty)
;;; dired-image-preview-kitty.el ends here
