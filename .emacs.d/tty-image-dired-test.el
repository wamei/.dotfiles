;;; tty-image-dired-test.el --- tests for tty-image-dired -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l tty-image-dired-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "kitty-graphics.el" dir) nil t)
  (load (expand-file-name "tty-image-mode.el" dir) nil t)
  (load (expand-file-name "tty-image-dired.el" dir) nil t))

;;; 箱の大きさ

(ert-deftest wamei/tty-image-dired-box-size-rounds-up ()
  "サムネイルの最大ピクセルを、セルの大きさで割り上げた桁・行。"
  ;; 128px / 幅 8px = 16 桁、128px / 高さ 19px = 6.7 → 7 行
  (should (equal (wamei/tty-image-dired--box-size 128 '(8 . 19)) '(16 . 7))))

(ert-deftest wamei/tty-image-dired-box-size-is-at-least-one ()
  "セルが画像より大きくても 1 桁 1 行は確保する。"
  (should (equal (wamei/tty-image-dired--box-size 4 '(8 . 19)) '(1 . 1))))

;;; 段あたりの枚数

(ert-deftest wamei/tty-image-dired-columns-reserves-a-column-for-the-truncation-glyph ()
  "箱と箱の間は 1 桁空ける。tty には fringe が無く最終桁は truncation glyph に
取られるが、この区切りの空きがちょうどその 1 桁を兼ねる。"
  ;; 80 / (16 + 1) = 4.7 → 4 枚
  (should (= (wamei/tty-image-dired--columns 80 16) 4))
  ;; 35 / (16 + 1) = 2.05 → 2 枚
  (should (= (wamei/tty-image-dired--columns 35 16) 2)))

(ert-deftest wamei/tty-image-dired-columns-fits-an-exact-multiple ()
  "width が (箱の桁数 + 1) で割り切れるときも、入るだけ並べる。
箱 i は桁 [i*(b+1), i*(b+1)+b-1] を占め、最後の 1 桁は truncation glyph に
取られるだけなので、n*(b+1) <= width なら n 枚入る。"
  ;; 33 = 3 * 11。箱は 桁 0-9 / 11-20 / 22-31 を占め、桁 32 が glyph
  (should (= (wamei/tty-image-dired--columns 33 10) 3))
  ;; 34 = 2 * 17。箱は 桁 0-15 / 17-32 を占め、桁 33 が glyph
  (should (= (wamei/tty-image-dired--columns 34 16) 2))
  ;; 9 = 3 * 3
  (should (= (wamei/tty-image-dired--columns 9 2) 3)))

(ert-deftest wamei/tty-image-dired-columns-is-at-least-one ()
  "箱が入りきらない細い window でも 1 枚は並べる (0 だと段が作れない)。"
  (should (= (wamei/tty-image-dired--columns 10 16) 1))
  (should (= (wamei/tty-image-dired--columns 1 16) 1)))

;;; 移動

(ert-deftest wamei/tty-image-dired-move-index-forward-and-backward ()
  (should (= (wamei/tty-image-dired--move-index 0 5 3 'forward) 1))
  (should (= (wamei/tty-image-dired--move-index 4 5 3 'backward) 3)))

(ert-deftest wamei/tty-image-dired-move-index-stops-at-the-ends ()
  "端では巡回せず nil を返す。"
  (should-not (wamei/tty-image-dired--move-index 4 5 3 'forward))
  (should-not (wamei/tty-image-dired--move-index 0 5 3 'backward)))

(ert-deftest wamei/tty-image-dired-move-index-down-and-up ()
  "段をまたぐ移動は添字 ± 段あたりの枚数。"
  ;; 7 枚 3 列: 0 1 2 / 3 4 5 / 6
  (should (= (wamei/tty-image-dired--move-index 0 7 3 'down) 3))
  (should (= (wamei/tty-image-dired--move-index 3 7 3 'up) 0))
  ;; 5 + 3 = 8 は範囲外
  (should-not (wamei/tty-image-dired--move-index 5 7 3 'down))
  (should-not (wamei/tty-image-dired--move-index 1 7 3 'up)))

(ert-deftest wamei/tty-image-dired-move-index-line-beginning-and-end ()
  "段の端へ。最終段が半端でも溢れない。"
  ;; 7 枚 3 列: 0 1 2 / 3 4 5 / 6
  (should (= (wamei/tty-image-dired--move-index 4 7 3 'line-beginning) 3))
  (should (= (wamei/tty-image-dired--move-index 3 7 3 'line-end) 5))
  ;; 最終段は 6 の 1 枚だけ
  (should (= (wamei/tty-image-dired--move-index 6 7 3 'line-end) 6))
  (should (= (wamei/tty-image-dired--move-index 6 7 3 'line-beginning) 6)))

;;; 可視範囲

(ert-deftest wamei/tty-image-dired-visible-range-covers-the-shown-bands ()
  "window の先頭行と行数から、見えている段に載っている添字の範囲を出す。"
  ;; 段の高さ 8 行 (箱 7 行 + キャプション 1 行)、3 列、20 枚 (段は 0..6)
  ;; 先頭行 0、24 行見える → 段 0..2 (行 0..23) → 添字 0..8
  (should (equal (wamei/tty-image-dired--visible-range 0 24 8 3 20) '(0 . 8)))
  ;; 先頭行 8 (段 1 から)、16 行 → 段 1..2 → 添字 3..8
  (should (equal (wamei/tty-image-dired--visible-range 8 16 8 3 20) '(3 . 8))))

(ert-deftest wamei/tty-image-dired-visible-range-clamps-to-the-last-image ()
  "最終段が半端でも、存在しない添字を返さない。"
  ;; 7 枚 3 列 (段 0..2、最終段は 6 のみ)、先頭行 8、24 行 → 段 1..3 だが 7 枚しかない
  (should (equal (wamei/tty-image-dired--visible-range 8 24 8 3 7) '(3 . 6))))

(ert-deftest wamei/tty-image-dired-visible-range-nil-when-nothing-shown ()
  "1 枚も無ければ nil。"
  (should-not (wamei/tty-image-dired--visible-range 0 24 8 3 0)))

;;; キャプション

(ert-deftest wamei/tty-image-dired-caption-truncates-to-the-box-width ()
  "ファイル名は箱の幅に切り詰める。"
  (let ((s (wamei/tty-image-dired--caption "/d/very-long-name.png" 8 nil nil)))
    (should (= (string-width s) 8))))

(ert-deftest wamei/tty-image-dired-caption-pads-to-the-box-width ()
  "短くても箱の幅ぶんの桁を占める (グリッドがずれないように)。"
  (let ((s (wamei/tty-image-dired--caption "/d/a.png" 12 nil nil)))
    (should (= (string-width s) 12))
    (should (string-prefix-p "a.png" s))))

(ert-deftest wamei/tty-image-dired-caption-marks-with-an-asterisk ()
  "dired でマークされていれば頭に * を付ける。"
  (let ((s (wamei/tty-image-dired--caption "/d/a.png" 12 nil t)))
    (should (string-prefix-p "*a.png" s))
    (should (= (string-width s) 12))))

(ert-deftest wamei/tty-image-dired-caption-highlights-when-selected ()
  "選択中は face を付ける。placeholder のセルには face を当てられないので、
選択の表示はキャプションが担う。"
  (let ((plain (wamei/tty-image-dired--caption "/d/a.png" 12 nil nil))
        (sel (wamei/tty-image-dired--caption "/d/a.png" 12 t nil)))
    (should-not (get-text-property 0 'face plain))
    (should (get-text-property 0 'face sel))))

(ert-deftest wamei/tty-image-dired-caption-counts-display-width-not-characters ()
  "日本語のファイル名は 1 文字が 2 桁。文字数で切ると段がずれる。"
  (let ((s (wamei/tty-image-dired--caption "/d/写真の名前.png" 8 nil nil)))
    (should (= (string-width s) 8))))

;;; 組み立て

(defvar wamei/tty-image-dired-test--tags-db
  (make-temp-file "tty-image-dired-test-tags-")
  "テスト用のタグ DB。ユーザの `image-dired-tags-db-file' を読み書きしないため。")

(defun wamei/tty-image-dired-test--build (files columns box)
  "テスト用に FILES を COLUMNS 列・BOX の箱で組み立てたバッファを作る。"
  (let ((buf (generate-new-buffer " *tty-image-dired-test*"))
        (image-dired-tags-db-file wamei/tty-image-dired-test--tags-db))
    (with-current-buffer buf
      (wamei/tty-image-dired--build (vconcat files) nil columns box))
    buf))

(ert-deftest wamei/tty-image-dired-build-lays-out-bands ()
  "段は 箱の行数 + キャプション 1 行。5 枚 3 列なら 2 段。"
  (let ((buf (wamei/tty-image-dired-test--build
              '("/d/a.png" "/d/b.png" "/d/c.png" "/d/d.png" "/d/e.png") 3 '(4 . 2))))
    (unwind-protect
        (with-current-buffer buf
          (should (= wamei/tty-image-dired--columns 3))
          (should (equal wamei/tty-image-dired--box '(4 . 2)))
          (should (= (wamei/tty-image-dired--band-lines) 3))
          ;; 2 段 x 3 行 = 6 行
          (should (= (count-lines (point-min) (point-max)) 6)))
      (kill-buffer buf))))

(ert-deftest wamei/tty-image-dired-build-puts-image-dired-properties ()
  "点ベースの組み込みコマンドが動くよう、箱とキャプションの全体に
image-dired と同じテキストプロパティを載せる。"
  (let ((buf (wamei/tty-image-dired-test--build '("/d/a.png" "/d/b.png") 2 '(4 . 2))))
    (unwind-protect
        (with-current-buffer buf
          (dolist (row '(0 1))
            (let ((region (wamei/tty-image-dired--box-line-region 0 row)))
              (should (get-text-property (car region) 'image-dired-thumbnail))
              (should (equal (get-text-property (car region) 'original-file-name) "/d/a.png"))))
          (let ((region (wamei/tty-image-dired--caption-region 0)))
            (should (equal (get-text-property (car region) 'original-file-name) "/d/a.png")))
          ;; 2 枚目は別のファイル
          (let ((region (wamei/tty-image-dired--box-line-region 1 0)))
            (should (equal (get-text-property (car region) 'original-file-name) "/d/b.png"))))
      (kill-buffer buf))))

(ert-deftest wamei/tty-image-dired-box-line-region-is-the-box-width ()
  "箱の 1 行ぶんの領域は、箱の桁数と同じ長さ。"
  (let ((buf (wamei/tty-image-dired-test--build '("/d/a.png" "/d/b.png") 2 '(4 . 2))))
    (unwind-protect
        (with-current-buffer buf
          (let ((r0 (wamei/tty-image-dired--box-line-region 0 0))
                (r1 (wamei/tty-image-dired--box-line-region 1 0)))
            (should (= (- (cdr r0) (car r0)) 4))
            (should (= (- (cdr r1) (car r1)) 4))
            ;; 2 枚目は 1 枚目の右、間に 1 桁空く
            (should (= (car r1) (+ (cdr r0) 1)))))
      (kill-buffer buf))))

(ert-deftest wamei/tty-image-dired-goto-index-moves-point-into-the-thumbnail ()
  "移動先の point には original-file-name が載っている (組み込みコマンドの前提)。"
  (let ((buf (wamei/tty-image-dired-test--build '("/d/a.png" "/d/b.png" "/d/c.png") 2 '(4 . 2))))
    (unwind-protect
        (with-current-buffer buf
          (wamei/tty-image-dired--goto-index 2)
          (should (equal (get-text-property (point) 'original-file-name) "/d/c.png"))
          (should (= wamei/tty-image-dired--selected 2)))
      (kill-buffer buf))))

;;; サムネイルの実体

(ert-deftest wamei/tty-image-dired-thumb-file-reuses-a-fresh-cache ()
  "キャッシュが元ファイルより新しければ sips を起こさない。"
  (let ((dir (make-temp-file "tty-image-dired-test-" t)) (sips 0))
    (unwind-protect
        (let ((orig (expand-file-name "a.png" dir))
              (thumb (expand-file-name "thumb.png" dir)))
          (write-region "orig" nil orig)
          (write-region "thumb" nil thumb)
          (cl-letf (((symbol-function 'image-dired-thumb-name) (lambda (_f) thumb))
                    ((symbol-function 'wamei/tty-image-dired--make-thumb)
                     (lambda (&rest _) (setq sips (1+ sips)) t)))
            (should (equal (wamei/tty-image-dired--thumb-file orig) thumb))
            (should (= sips 0))))
      (delete-directory dir t))))

(ert-deftest wamei/tty-image-dired-thumb-file-creates-a-missing-cache ()
  "キャッシュが無ければ作る。作れなければ nil。"
  (let ((dir (make-temp-file "tty-image-dired-test-" t)))
    (unwind-protect
        (let ((orig (expand-file-name "a.png" dir))
              (thumb (expand-file-name "thumb.png" dir))
              (made nil))
          (write-region "orig" nil orig)
          (cl-letf (((symbol-function 'image-dired-thumb-name) (lambda (_f) thumb))
                    ((symbol-function 'wamei/tty-image-dired--make-thumb)
                     (lambda (_src dst) (setq made t) (write-region "t" nil dst) t)))
            (should (equal (wamei/tty-image-dired--thumb-file orig) thumb))
            (should made))
          (delete-file thumb)
          (cl-letf (((symbol-function 'image-dired-thumb-name) (lambda (_f) thumb))
                    ((symbol-function 'wamei/tty-image-dired--make-thumb)
                     (lambda (&rest _) nil)))
            (should-not (wamei/tty-image-dired--thumb-file orig))))
      (delete-directory dir t))))

;;; 送受

(defmacro wamei/tty-image-dired-test--with-grid (files columns box &rest body)
  "FILES を COLUMNS 列・BOX の箱で組み立てたバッファで BODY を評価する。"
  (declare (indent 3))
  `(let ((buf (wamei/tty-image-dired-test--build ,files ,columns ,box)))
     (unwind-protect (with-current-buffer buf ,@body)
       (kill-buffer buf))))

(ert-deftest wamei/tty-image-dired-show-box-writes-the-placeholder-and-keeps-properties ()
  "placeholder を書き込んでも、点ベースのコマンドが使うプロパティは残る。"
  (cl-letf (((symbol-function 'wamei/tty-image-dired--thumb-file) (lambda (f) f))
            ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(32 . 32)))
            ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
            ((symbol-function 'wamei/kitty-graphics-put) (lambda (&rest _) 7))
            ((symbol-function 'wamei/kitty-graphics-placeholder-line)
             (lambda (_id cols _row &optional _color) (make-string cols ?P))))
    (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
      (wamei/tty-image-dired--show-box 0)
      (should (= (aref wamei/tty-image-dired--ids 0) 7))
      (let ((region (wamei/tty-image-dired--box-line-region 0 0)))
        (should (equal (buffer-substring-no-properties (car region) (cdr region)) "PPPP"))
        (should (equal (get-text-property (car region) 'original-file-name) "/d/a.png"))))))

(ert-deftest wamei/tty-image-dired-hide-box-releases-the-id-and-blanks-the-box ()
  (let ((deleted nil))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--thumb-file) (lambda (f) f))
              ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(32 . 32)))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/kitty-graphics-put) (lambda (&rest _) 7))
              ((symbol-function 'wamei/kitty-graphics-delete) (lambda (id) (push id deleted)))
              ((symbol-function 'wamei/kitty-graphics-placeholder-line)
               (lambda (_id cols _row &optional _color) (make-string cols ?P))))
      (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
        (wamei/tty-image-dired--show-box 0)
        (wamei/tty-image-dired--hide-box 0)
        (should (equal deleted '(7)))
        (should-not (aref wamei/tty-image-dired--ids 0))
        (let ((region (wamei/tty-image-dired--box-line-region 0 0)))
          (should (equal (buffer-substring-no-properties (car region) (cdr region)) "    "))
          ;; プロパティは残す (点ベースのコマンドが動かなくなるため)
          (should (equal (get-text-property (car region) 'original-file-name) "/d/a.png")))))))

(ert-deftest wamei/tty-image-dired-sync-visible-sends-only-what-is-shown ()
  "可視範囲の外は送らない。範囲が変わらなければ送り直さない。"
  (let ((puts nil) (deleted nil) (range '(0 . 1)))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--thumb-file) (lambda (f) f))
              ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(32 . 32)))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/kitty-graphics-put)
               (lambda (file &rest _) (push file puts) (length puts)))
              ((symbol-function 'wamei/kitty-graphics-delete) (lambda (id) (push id deleted)))
              ((symbol-function 'wamei/kitty-graphics-placeholder-line)
               (lambda (_id cols _row &optional _color) (make-string cols ?P)))
              ((symbol-function 'wamei/tty-image-dired--window-visible-range)
               (lambda () range)))
      (wamei/tty-image-dired-test--with-grid
          '("/d/a.png" "/d/b.png" "/d/c.png" "/d/d.png") 2 '(4 . 2)
        (wamei/tty-image-dired--sync-visible)
        (should (equal (nreverse (copy-sequence puts)) '("/d/a.png" "/d/b.png")))
        ;; 2 回目は範囲が同じなので何も起きない
        (wamei/tty-image-dired--sync-visible)
        (should (= (length puts) 2))
        (should-not deleted)
        ;; 範囲が動いたら、外れたものを解放して新しいものを送る
        (setq range '(2 . 3))
        (wamei/tty-image-dired--sync-visible)
        (should (= (length puts) 4))
        (should (= (length deleted) 2))))))

(ert-deftest wamei/tty-image-dired-release-all-frees-every-id ()
  (let ((deleted nil))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--thumb-file) (lambda (f) f))
              ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(32 . 32)))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/kitty-graphics-put) (lambda (&rest _) 7))
              ((symbol-function 'wamei/kitty-graphics-delete) (lambda (id) (push id deleted)))
              ((symbol-function 'wamei/kitty-graphics-placeholder-line)
               (lambda (_id cols _row &optional _color) (make-string cols ?P))))
      (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
        (wamei/tty-image-dired--show-box 0)
        (wamei/tty-image-dired--show-box 1)
        (wamei/tty-image-dired--release-all)
        (should (= (length deleted) 2))
        (should-not (aref wamei/tty-image-dired--ids 0))
        (should-not (aref wamei/tty-image-dired--ids 1))))))

(ert-deftest wamei/tty-image-dired-show-box-survives-a-failed-transfer ()
  "端末への転送に失敗しても、その箱が空白のままになるだけで error にしない。"
  (cl-letf (((symbol-function 'wamei/tty-image-dired--thumb-file) (lambda (f) f))
            ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(32 . 32)))
            ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
            ((symbol-function 'wamei/kitty-graphics-put) (lambda (&rest _) nil)))
    (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
      (wamei/tty-image-dired--show-box 0)
      (should-not (aref wamei/tty-image-dired--ids 0))
      (let ((region (wamei/tty-image-dired--box-line-region 0 0)))
        (should (equal (buffer-substring-no-properties (car region) (cdr region)) "    "))))))

(ert-deftest wamei/tty-image-dired-show-box-survives-a-failed-thumbnail ()
  "1 枚のサムネイルが作れなくても、その箱が空白のままになるだけで error にしない。"
  (cl-letf (((symbol-function 'wamei/tty-image-dired--thumb-file) (lambda (_f) nil)))
    (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
      (wamei/tty-image-dired--show-box 0)
      (should-not (aref wamei/tty-image-dired--ids 0))
      (let ((region (wamei/tty-image-dired--box-line-region 0 0)))
        (should (equal (buffer-substring-no-properties (car region) (cdr region)) "    "))))))

(ert-deftest wamei/tty-image-dired-show-box-pads-by-display-width ()
  "placeholder は 1 セル = 3 文字の合成なので、パディングは文字数ではなく表示桁で計算する。
文字数で計算すると負の長さを make-string に渡して error になる。"
  (cl-letf (((symbol-function 'wamei/tty-image-dired--thumb-file) (lambda (f) f))
            ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(16 . 32)))
            ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
            ((symbol-function 'wamei/kitty-graphics-put) (lambda (&rest _) 7)))
    ;; 実物と同じく 1 セル = 3 文字を合成した文字列を返す
    (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
      (wamei/tty-image-dired--show-box 0)
      (let ((region (wamei/tty-image-dired--box-line-region 0 0)))
        ;; 箱は 4 桁。合成済みなので表示桁で 4 になっていること
        (should (= (string-width (buffer-substring (car region) (cdr region))) 4))))))

(provide 'tty-image-dired-test)
;;; tty-image-dired-test.el ends here
