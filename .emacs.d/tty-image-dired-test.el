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
  "テスト用のタグ DB ファイル。ファイル全体で `image-dired-tags-db-file' を
これに向け、実ユーザの `~/.emacs.d/image-dired/' を読み書きしないようにする。
`--build' ヘルパの外で `--show-box' / `--mark' / `--redraw-caption' を直接呼ぶ
テストもあるため、ヘルパ内だけの束縛では足りない (M5)。")

(setq image-dired-tags-db-file wamei/tty-image-dired-test--tags-db)

(add-hook 'kill-emacs-hook
          (lambda ()
            (ignore-errors (delete-file wamei/tty-image-dired-test--tags-db))))

(ert-deftest wamei/tty-image-dired-test-tags-db-points-at-a-temp-file ()
  "テストが実ユーザの `~/.emacs.d/image-dired/' を汚さないこと (M5)。"
  (should (equal image-dired-tags-db-file wamei/tty-image-dired-test--tags-db))
  (should (file-in-directory-p image-dired-tags-db-file temporary-file-directory)))

(defun wamei/tty-image-dired-test--build (files columns box)
  "テスト用に FILES を COLUMNS 列・BOX の箱で組み立てたバッファを作る。"
  (let ((buf (generate-new-buffer " *tty-image-dired-test*")))
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

(ert-deftest wamei/tty-image-dired-goto-index-ignores-out-of-range-index ()
  "範囲外の INDEX は無視して何もしないこと (T2-2)。呼び手が clamp 済みの
値を渡す前提が I3 で破れた実例があるので、`--goto-index' 自身にもガードを
入れて同じクラスの再発を止める。"
  (let ((buf (wamei/tty-image-dired-test--build '("/d/a.png" "/d/b.png") 2 '(4 . 2))))
    (unwind-protect
        (with-current-buffer buf
          (wamei/tty-image-dired--goto-index 0)
          (should-not (wamei/tty-image-dired--goto-index 5))
          (should (= wamei/tty-image-dired--selected 0))
          (should-not (wamei/tty-image-dired--goto-index -1))
          (should (= wamei/tty-image-dired--selected 0)))
      (kill-buffer buf))))

(ert-deftest wamei/tty-image-dired-goto-index-does-nothing-on-empty-grid ()
  "0 枚のグリッドで `error' にならないこと。"
  (let ((buf (wamei/tty-image-dired-test--build '() 2 '(4 . 2))))
    (unwind-protect
        (with-current-buffer buf
          (should-not (wamei/tty-image-dired--goto-index 0)))
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

(ert-deftest wamei/tty-image-dired-put-properties-uses-band-lines ()
  "段の高さの定義を 2 箇所に持たない (T2-1)。`--band-lines' の代わりに
`(1+ (cdr --box))' を直書きしていると、`--band-lines' を差し替えても
反映されない。1 列のグリッドなら `--box-line-region' 自身の band 計算は
0 * band-lines = 0 で影響を受けないので、row の走査範囲だけを見られる。"
  (cl-letf (((symbol-function 'wamei/tty-image-dired--band-lines) (lambda () 1)))
    (wamei/tty-image-dired-test--with-grid '("/d/a.png") 1 '(4 . 3)
      ;; --band-lines を 1 に差し替えると、キャプション行 (box の 3 行の
      ;; あとの row 3) は put-properties の対象にならないはず
      (let ((caption-region (wamei/tty-image-dired--box-line-region 0 3)))
        (wamei/tty-image-dired--put-properties 0)
        (should-not (get-text-property (car caption-region) 'image-dired-thumbnail))))))

;;; モード

(ert-deftest wamei/tty-image-dired-mode-derives-from-image-dired-thumbnail-mode ()
  "組み込みのコマンドは derived-mode-p のガードを持つので、派生していないと使えない。"
  (with-temp-buffer
    (wamei/tty-image-dired-mode)
    (should (derived-mode-p 'image-dired-thumbnail-mode))))

(ert-deftest wamei/tty-image-dired-mode-turns-off-line-numbers ()
  "行番号が桁を食うと placeholder の桁数が合わなくなる。
`global-display-line-numbers-mode' が有効だと major mode 変更後に t になるので、
それを打ち消せているかを見る。"
  (global-display-line-numbers-mode 1)
  (unwind-protect
      (with-temp-buffer
        (wamei/tty-image-dired-mode)
        (should-not display-line-numbers-mode)
        (should truncate-lines))
    (global-display-line-numbers-mode -1)))

(ert-deftest wamei/tty-image-dired-mode-hooks-window-changes-and-cleanup ()
  (with-temp-buffer
    (wamei/tty-image-dired-mode)
    (should (memq #'wamei/tty-image-dired--sync-visible
                  (buffer-local-value 'window-configuration-change-hook (current-buffer))))
    (should (memq #'wamei/tty-image-dired--scroll-sync
                  (buffer-local-value 'window-scroll-functions (current-buffer))))
    (should (memq #'wamei/tty-image-dired--release-all
                  (buffer-local-value 'kill-buffer-hook (current-buffer))))))

(ert-deftest wamei/tty-image-dired-mode-is-not-interactive ()
  "組み込みの `image-dired-thumbnail-mode' と同じく `:interactive nil' と
すること (M9)。`define-derived-mode' は `kill-all-local-variables' を通る
ので、生きたグリッドバッファで `M-x wamei/tty-image-dired-mode' を再実行
すると `--ids' が失われ、その時点で live だった画像 ID が
`wamei/kitty-graphics--live-ids' に残り続ける (プールが恒久的に縮む)。
呼び口を `--show-thumbs' の初回だけに絞るため、M-x からは呼べなくする。"
  (should-not (commandp #'wamei/tty-image-dired-mode)))

;;; 移動コマンド

(ert-deftest wamei/tty-image-dired-forward-image-moves-the-selection ()
  (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore))
    (wamei/tty-image-dired-test--with-grid
        '("/d/a.png" "/d/b.png" "/d/c.png") 2 '(4 . 2)
      (wamei/tty-image-dired--goto-index 0)
      (wamei/tty-image-dired-forward-image)
      (should (= wamei/tty-image-dired--selected 1))
      (should (equal (get-text-property (point) 'original-file-name) "/d/b.png")))))

(ert-deftest wamei/tty-image-dired-forward-image-messages-at-the-end ()
  "端では動かず、error も投げない。"
  (let ((messages nil))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
        (wamei/tty-image-dired--goto-index 1)
        (wamei/tty-image-dired-forward-image)
        (should (= wamei/tty-image-dired--selected 1))
        (should messages)))))

(ert-deftest wamei/tty-image-dired-forward-image-honors-the-prefix-argument ()
  "`C-u 3 f' 相当で 3 つ進むこと (T4-1)。`(interactive \"p\")' と宣言しながら
中で使っていなかったので、宣言と実装が食い違っていた。"
  (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore))
    (wamei/tty-image-dired-test--with-grid
        '("/d/a.png" "/d/b.png" "/d/c.png" "/d/d.png") 4 '(4 . 2)
      (wamei/tty-image-dired--goto-index 0)
      (wamei/tty-image-dired-forward-image 3)
      (should (= wamei/tty-image-dired--selected 3)))))

(ert-deftest wamei/tty-image-dired-backward-image-honors-the-prefix-argument ()
  (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore))
    (wamei/tty-image-dired-test--with-grid
        '("/d/a.png" "/d/b.png" "/d/c.png" "/d/d.png") 4 '(4 . 2)
      (wamei/tty-image-dired--goto-index 3)
      (wamei/tty-image-dired-backward-image 3)
      (should (= wamei/tty-image-dired--selected 0)))))

(ert-deftest wamei/tty-image-dired-forward-image-stops-at-the-end-and-messages ()
  "3 進める指示でも端で 1 つしか動けないときは、そこで止まってメッセージが
出ること。"
  (let ((messages nil))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (wamei/tty-image-dired-test--with-grid
          '("/d/a.png" "/d/b.png") 2 '(4 . 2)
        (wamei/tty-image-dired--goto-index 0)
        (wamei/tty-image-dired-forward-image 3)
        (should (= wamei/tty-image-dired--selected 1))
        (should messages)))))

(ert-deftest wamei/tty-image-dired-next-line-moves-a-band-down ()
  (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore))
    (wamei/tty-image-dired-test--with-grid
        '("/d/a.png" "/d/b.png" "/d/c.png" "/d/d.png") 2 '(4 . 2)
      (wamei/tty-image-dired--goto-index 0)
      (wamei/tty-image-dired-next-line)
      (should (= wamei/tty-image-dired--selected 2))
      (wamei/tty-image-dired-previous-line)
      (should (= wamei/tty-image-dired--selected 0)))))

(ert-deftest wamei/tty-image-dired-move-to-line-ends ()
  (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore))
    (wamei/tty-image-dired-test--with-grid
        '("/d/a.png" "/d/b.png" "/d/c.png" "/d/d.png") 2 '(4 . 2)
      (wamei/tty-image-dired--goto-index 2)
      (wamei/tty-image-dired-move-end-of-line)
      (should (= wamei/tty-image-dired--selected 3))
      (wamei/tty-image-dired-move-beginning-of-line)
      (should (= wamei/tty-image-dired--selected 2)))))

;;; キーマップ: 親の remap を潰しているか

(ert-deftest wamei/tty-image-dired-mode-map-overrides-every-parent-remap ()
  "親の <remap> が 1 つでも残っていると、そのキーで組み込みの 1 文字走査コマンドが動く。
親のマップを走査して、こちらのマップ越しに image-dired-* へ解決するものが無いことを見る。"
  (let ((leftovers nil))
    (map-keymap
     (lambda (key def)
       (when (eq key 'remap)
         (map-keymap
          (lambda (command _target)
            (let ((resolved (lookup-key wamei/tty-image-dired-mode-map
                                        (vector 'remap command))))
              (when (and (symbolp resolved)
                         (string-prefix-p "image-dired-" (symbol-name resolved)))
                (push (cons command resolved) leftovers))))
          def)))
     image-dired-thumbnail-mode-map)
    (should-not leftovers)))

;;; キーマップ: 1 文字走査で壊れる組み込みコマンドを潰しているか

(ert-deftest wamei/tty-image-dired-mode-map-disables-the-line-up-prefix ()
  "`g' を nil で束縛すると、子マップでは `g' 自体が prefix key でなくなり、
親の line-up 系 (`g f' / `g g' / `g i') に lookup-key で辿り着けなくなること
(I1)。`image-dired-line-up' はバッファを 1 サムネ=1 文字前提で組み替え、
矩形のグリッドを不可逆に壊す。"
  (should-not (keymapp (lookup-key wamei/tty-image-dired-mode-map "g")))
  (dolist (key '("gf" "gg" "gi"))
    (should-not (commandp (lookup-key wamei/tty-image-dired-mode-map key)))))

(ert-deftest wamei/tty-image-dired-mode-map-disables-the-tag-prefix ()
  "`t' も同様に潰す (I2)。`image-dired-tag-thumbnail' / `-remove' は
`image-dired--with-marked' でバッファを 1 サムネ=1 文字前提に走査する。"
  (should-not (keymapp (lookup-key wamei/tty-image-dired-mode-map "t")))
  (dolist (key '("tt" "tr"))
    (should-not (commandp (lookup-key wamei/tty-image-dired-mode-map key)))))

(ert-deftest wamei/tty-image-dired-mode-map-replaces-destructive-scanning-commands ()
  "`C-d' (`image-dired-delete-char') / `L' / `R' (回転) は同じ 1 文字走査の
前提でバッファを壊すか、jpegtran が無ければ `error' になる (I1 / I2)。
潰した先の `wamei/tty-image-dired-unsupported' に置き換わっていること。"
  (should (eq (lookup-key wamei/tty-image-dired-mode-map (kbd "C-d"))
              #'wamei/tty-image-dired-unsupported))
  (should (eq (lookup-key wamei/tty-image-dired-mode-map "L")
              #'wamei/tty-image-dired-unsupported))
  (should (eq (lookup-key wamei/tty-image-dired-mode-map "R")
              #'wamei/tty-image-dired-unsupported)))

(ert-deftest wamei/tty-image-dired-unsupported-messages-and-does-not-error ()
  "非目標のキーは「押しても何も起きない」が正しい実装。メッセージだけ出す。"
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (wamei/tty-image-dired-unsupported)
      (should messages))))

;;; 先頭・末尾へ移動

(ert-deftest wamei/tty-image-dired-first-image-moves-to-index-zero ()
  (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore))
    (wamei/tty-image-dired-test--with-grid
        '("/d/a.png" "/d/b.png" "/d/c.png") 2 '(4 . 2)
      (wamei/tty-image-dired--goto-index 2)
      (wamei/tty-image-dired-first-image)
      (should (= wamei/tty-image-dired--selected 0)))))

(ert-deftest wamei/tty-image-dired-last-image-moves-to-the-last-index ()
  (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore))
    (wamei/tty-image-dired-test--with-grid
        '("/d/a.png" "/d/b.png" "/d/c.png") 2 '(4 . 2)
      (wamei/tty-image-dired--goto-index 0)
      (wamei/tty-image-dired-last-image)
      (should (= wamei/tty-image-dired--selected 2)))))

(ert-deftest wamei/tty-image-dired-first-and-last-image-do-nothing-when-empty ()
  "0 枚のときに error にならないこと。"
  (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore))
    (wamei/tty-image-dired-test--with-grid '() 2 '(4 . 2)
      (should-not (wamei/tty-image-dired-first-image))
      (should-not (wamei/tty-image-dired-last-image)))))

;;; スクロール

(ert-deftest wamei/tty-image-dired-scroll-up-does-not-move-the-selection ()
  (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
            ((symbol-function 'scroll-up-command) #'ignore))
    (wamei/tty-image-dired-test--with-grid
        '("/d/a.png" "/d/b.png" "/d/c.png") 2 '(4 . 2)
      (wamei/tty-image-dired--goto-index 1)
      (wamei/tty-image-dired-scroll-up)
      (should (= wamei/tty-image-dired--selected 1)))))

(ert-deftest wamei/tty-image-dired-scroll-down-does-not-move-the-selection ()
  (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
            ((symbol-function 'scroll-down-command) #'ignore))
    (wamei/tty-image-dired-test--with-grid
        '("/d/a.png" "/d/b.png" "/d/c.png") 2 '(4 . 2)
      (wamei/tty-image-dired--goto-index 1)
      (wamei/tty-image-dired-scroll-down)
      (should (= wamei/tty-image-dired--selected 1)))))

;;; RET

(ert-deftest wamei/tty-image-dired-display-this-opens-the-original-file ()
  "組み込みの image-dired-image-mode ではなく find-file で開き、
auto-mode-alist → image-mode → Phase 1 の advice の経路に載せる。"
  (let ((opened nil))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
              ((symbol-function 'find-file) (lambda (f) (setq opened f))))
      (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
        (wamei/tty-image-dired--goto-index 1)
        (wamei/tty-image-dired-display-this)
        (should (equal opened "/d/b.png"))))))

(ert-deftest wamei/tty-image-dired-display-this-messages-on-empty-grid ()
  "全部消えて 0 枚になったグリッドで RET を押しても `args-out-of-range' に
ならず、メッセージだけ出ること (I3)。"
  (let ((messages nil) (opened nil))
    (cl-letf (((symbol-function 'find-file) (lambda (f) (setq opened f)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (wamei/tty-image-dired-test--with-grid '() 2 '(4 . 2)
        (wamei/tty-image-dired-display-this)
        (should-not opened)
        (should messages)))))

(ert-deftest wamei/tty-image-dired-display-this-messages-when-not-built ()
  "`--build' していないバッファ (`--files' が nil) でも `args-out-of-range' に
ならないこと (I3)。"
  (let ((messages nil) (opened nil))
    (cl-letf (((symbol-function 'find-file) (lambda (f) (setq opened f)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (wamei/tty-image-dired-mode)
        (wamei/tty-image-dired-display-this))
      (should-not opened)
      (should messages))))

;;; マーク・削除フラグ

(ert-deftest wamei/tty-image-dired-mark-thumb-calls-dired-mark-on-the-selected-file ()
  "選択中のファイルに対して dired-goto-file → dired-mark を呼ぶこと。
`--mark' はマークのあと次のサムネイルへ移り、そこでも `--marked-p' 経由で
`dired-goto-file' が呼ばれるので、最初の呼び出しだけを捕まえる (dired-buffer は
位置決めが終わってから live にする、という手も考えたが、両方やっておくと
将来 `--goto-index' の実装が変わっても頑丈)。"
  (let ((dired-buf (generate-new-buffer " *tty-image-dired-mark-test-dired*"))
        (goto-file nil) (marked nil))
    (unwind-protect
        (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
                  ((symbol-function 'dired-goto-file)
                   (lambda (f) (unless goto-file (setq goto-file f)) t))
                  ((symbol-function 'dired-mark) (lambda (&rest _) (setq marked t))))
          (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
            (wamei/tty-image-dired--goto-index 1)
            (setq wamei/tty-image-dired--dired-buffer dired-buf)
            (wamei/tty-image-dired-mark-thumb-original-file)
            (should (equal goto-file "/d/b.png"))
            (should marked)))
      (kill-buffer dired-buf))))

(ert-deftest wamei/tty-image-dired-unmark-thumb-calls-dired-unmark-on-the-selected-file ()
  (let ((dired-buf (generate-new-buffer " *tty-image-dired-mark-test-dired*"))
        (goto-file nil) (unmarked nil))
    (unwind-protect
        (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
                  ((symbol-function 'dired-goto-file)
                   (lambda (f) (unless goto-file (setq goto-file f)) t))
                  ((symbol-function 'dired-unmark) (lambda (&rest _) (setq unmarked t))))
          (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
            (wamei/tty-image-dired--goto-index 0)
            (setq wamei/tty-image-dired--dired-buffer dired-buf)
            (wamei/tty-image-dired-unmark-thumb-original-file)
            (should (equal goto-file "/d/a.png"))
            (should unmarked)))
      (kill-buffer dired-buf))))

(ert-deftest wamei/tty-image-dired-flag-thumb-calls-dired-flag-file-deletion-on-the-selected-file ()
  (let ((dired-buf (generate-new-buffer " *tty-image-dired-mark-test-dired*"))
        (goto-file nil) (flagged nil))
    (unwind-protect
        (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
                  ((symbol-function 'dired-goto-file)
                   (lambda (f) (unless goto-file (setq goto-file f)) t))
                  ((symbol-function 'dired-flag-file-deletion)
                   (lambda (&rest _) (setq flagged t))))
          (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
            (wamei/tty-image-dired--goto-index 0)
            (setq wamei/tty-image-dired--dired-buffer dired-buf)
            (wamei/tty-image-dired-flag-thumb-original-file)
            (should (equal goto-file "/d/a.png"))
            (should flagged)))
      (kill-buffer dired-buf))))

(ert-deftest wamei/tty-image-dired-mark-thumb-moves-to-the-next-image ()
  (let ((dired-buf (generate-new-buffer " *tty-image-dired-mark-test-dired*")))
    (unwind-protect
        (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
                  ((symbol-function 'dired-goto-file) (lambda (_f) t))
                  ((symbol-function 'dired-mark) #'ignore))
          (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
            (setq wamei/tty-image-dired--dired-buffer dired-buf)
            (wamei/tty-image-dired--goto-index 0)
            (wamei/tty-image-dired-mark-thumb-original-file)
            (should (= wamei/tty-image-dired--selected 1))))
      (kill-buffer dired-buf))))

(ert-deftest wamei/tty-image-dired-mark-thumb-does-not-open-the-image ()
  "組み込みの `image-dired-marking-shows-next' に相当する副作用 (画像を開く) を
起こさないこと。"
  (let ((dired-buf (generate-new-buffer " *tty-image-dired-mark-test-dired*"))
        (opened nil))
    (unwind-protect
        (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
                  ((symbol-function 'dired-goto-file) (lambda (_f) t))
                  ((symbol-function 'dired-mark) #'ignore)
                  ((symbol-function 'find-file) (lambda (&rest _) (setq opened t)))
                  ((symbol-function 'image-dired-display-this)
                   (lambda (&rest _) (setq opened t))))
          (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
            (setq wamei/tty-image-dired--dired-buffer dired-buf)
            (wamei/tty-image-dired--goto-index 0)
            (wamei/tty-image-dired-mark-thumb-original-file)
            (should-not opened)))
      (kill-buffer dired-buf))))

(ert-deftest wamei/tty-image-dired-mark-thumb-does-not-put-face-on-the-placeholder ()
  "placeholder のセルは前景色が画像 ID なので、face を貼ると画像が壊れる。
組み込みの `image-dired--thumb-update-mark-at-point' は point (箱) に face を
貼るが、自前のマークコマンドはそこを経由しない。"
  (let ((dired-buf (generate-new-buffer " *tty-image-dired-mark-test-dired*")))
    (unwind-protect
        (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
                  ((symbol-function 'dired-goto-file) (lambda (_f) t))
                  ((symbol-function 'dired-mark) #'ignore))
          (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png") 2 '(4 . 2)
            (setq wamei/tty-image-dired--dired-buffer dired-buf)
            (wamei/tty-image-dired--goto-index 0)
            (wamei/tty-image-dired-mark-thumb-original-file)
            (let ((region (wamei/tty-image-dired--box-line-region 0 0)))
              (should-not (get-text-property (car region) 'face)))))
      (kill-buffer dired-buf))))

(ert-deftest wamei/tty-image-dired-mark-thumb-messages-when-dired-buffer-is-gone ()
  "dired バッファが無ければ error にせずメッセージだけ出す。"
  (let ((messages nil))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (wamei/tty-image-dired-test--with-grid '("/d/a.png") 1 '(4 . 2)
        (setq wamei/tty-image-dired--dired-buffer nil)
        (wamei/tty-image-dired-mark-thumb-original-file)
        (should messages)))))

(ert-deftest wamei/tty-image-dired-unmark-all-marks-redraws-without-moving-selection ()
  "全キャプションを描き直し、選択は動かさない。"
  (let ((dired-buf (generate-new-buffer " *tty-image-dired-mark-test-dired*"))
        (redrawn nil))
    (unwind-protect
        (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
                  ((symbol-function 'dired-unmark-all-marks) #'ignore)
                  ((symbol-function 'wamei/tty-image-dired--redraw-caption)
                   (lambda (index) (push index redrawn))))
          (wamei/tty-image-dired-test--with-grid '("/d/a.png" "/d/b.png" "/d/c.png") 2 '(4 . 2)
            (setq wamei/tty-image-dired--dired-buffer dired-buf)
            (wamei/tty-image-dired--goto-index 1)
            (setq redrawn nil)
            (wamei/tty-image-dired-unmark-all-marks)
            (should (equal (sort redrawn #'<) '(0 1 2)))
            (should (= wamei/tty-image-dired--selected 1))))
      (kill-buffer dired-buf))))

(ert-deftest wamei/tty-image-dired-unmark-all-marks-messages-when-dired-buffer-is-gone ()
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (wamei/tty-image-dired-test--with-grid '("/d/a.png") 1 '(4 . 2)
        (setq wamei/tty-image-dired--dired-buffer nil)
        (wamei/tty-image-dired-unmark-all-marks)
        (should messages)))))

;;; 削除フラグの回収

(ert-deftest wamei/tty-image-dired-do-flagged-delete-rebuilds-with-remaining-files ()
  "dired 側で消したファイルを除いて組み直すこと (T4-2)。"
  (let ((dired-buf (generate-new-buffer " *tty-image-dired-flagged-delete-test-dired*"))
        (dir (make-temp-file "tty-image-dired-flagged-delete-test-" t)))
    (unwind-protect
        (let* ((a (expand-file-name "a.png" dir))
               (b (expand-file-name "b.png" dir)))
          (write-region "a" nil a)
          ;; b は書き込まない = 「dired 側で既に消えたファイル」を模す
          (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
                    ((symbol-function 'dired-do-flagged-delete) #'ignore)
                    ((symbol-function 'dired-goto-file) (lambda (_f) nil)))
            (wamei/tty-image-dired-test--with-grid (list a b) 2 '(4 . 2)
              (setq wamei/tty-image-dired--dired-buffer dired-buf)
              (wamei/tty-image-dired-do-flagged-delete)
              (should (= (length wamei/tty-image-dired--files) 1))
              (should (equal (aref wamei/tty-image-dired--files 0) a)))))
      (kill-buffer dired-buf)
      (delete-directory dir t))))

(ert-deftest wamei/tty-image-dired-do-flagged-delete-empties-the-grid-and-ret-does-not-error ()
  "全部消えたときは空のグリッドになり、その状態で RET を押しても `error' に
ならないこと (T4-2)。ここにテストが無かったことが I3 の穴を隠していた。"
  (let ((dired-buf (generate-new-buffer " *tty-image-dired-flagged-delete-test-dired*"))
        (messages nil) (opened nil))
    (unwind-protect
        (cl-letf (((symbol-function 'wamei/tty-image-dired--sync-visible) #'ignore)
                  ((symbol-function 'dired-do-flagged-delete) #'ignore)
                  ((symbol-function 'dired-goto-file) (lambda (_f) nil))
                  ((symbol-function 'find-file) (lambda (f) (setq opened f)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          ;; 存在しないファイルとして組み立てる = 削除後に全部消える状況を模す
          (wamei/tty-image-dired-test--with-grid
              '("/nonexistent/a.png" "/nonexistent/b.png") 2 '(4 . 2)
            (setq wamei/tty-image-dired--dired-buffer dired-buf)
            (wamei/tty-image-dired-do-flagged-delete)
            (should (= (length wamei/tty-image-dired--files) 0))
            (setq messages nil)
            (wamei/tty-image-dired-display-this)
            (should-not opened)
            (should messages)))
      (kill-buffer dired-buf))))

(ert-deftest wamei/tty-image-dired-do-flagged-delete-messages-when-dired-buffer-is-gone ()
  "兄弟コマンド (`--mark' / `--unmark-all-marks') と同じく、dired バッファが
無ければメッセージだけ出すこと (T4-3)。"
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (wamei/tty-image-dired-test--with-grid '("/d/a.png") 1 '(4 . 2)
        (setq wamei/tty-image-dired--dired-buffer nil)
        (wamei/tty-image-dired-do-flagged-delete)
        (should messages)))))

;;; マークの表示更新

(ert-deftest wamei/tty-image-dired-update-marks-works-from-another-buffer ()
  "組み込みは自分の中でサムネイルバッファへ切り替えるので、advice は呼び出し元
\(dired バッファ) で走る。別のバッファから呼ばれてもキャプションが更新されること。
`wamei/tty-image-dired-mode' に入ると `kill-all-local-variables' で --build の
状態が消えるので、先にモードへ入ってから --build する。"
  (let ((redrawn nil))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--redraw-caption)
               (lambda (index) (push index redrawn))))
      (let ((buf (generate-new-buffer " *tty-image-dired-test*")))
        (unwind-protect
            (let ((image-dired-thumbnail-buffer (buffer-name buf)))
              (with-current-buffer buf
                (wamei/tty-image-dired-mode)
                (wamei/tty-image-dired--build
                 (vconcat '("/d/a.png" "/d/b.png")) nil 2 '(4 . 2)))
              ;; --build 自身も --redraw-caption を呼ぶので、その分は数えない
              (setq redrawn nil)
              ;; 別のバッファから呼ぶ
              (with-temp-buffer
                (wamei/tty-image-dired--update-marks))
              (should (equal (sort redrawn #'<) '(0 1))))
          (kill-buffer buf))))))

;;; 入口

(ert-deftest wamei/tty-image-dired-around-builds-the-grid-when-available ()
  "端末が対応していれば自分のグリッドを組む。"
  (let ((built nil) (orig-called nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-available-p) (lambda () t))
              ((symbol-function 'wamei/tty-image-dired--show-thumbs)
               (lambda (&rest _) (setq built t))))
      (wamei/tty-image-dired--display-thumbs-around
       (lambda (&rest _) (setq orig-called t)))
      (should built)
      (should-not orig-called))))

(ert-deftest wamei/tty-image-dired-around-falls-back-to-the-original ()
  "非対応端末では error を投げず、元の実装をそのまま呼ぶ (今日と同じ挙動)。"
  (let ((orig-args nil) (messages nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-available-p) (lambda () nil))
              ((symbol-function 'wamei/tty-image-dired--show-thumbs)
               (lambda (&rest _) (error "呼ばれてはいけない")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (wamei/tty-image-dired--display-thumbs-around
       (lambda (&rest args) (setq orig-args args)) 'arg 'append 'do-not-pop)
      (should (equal orig-args '(arg append do-not-pop)))
      (should messages))))

(ert-deftest wamei/tty-image-dired-show-thumbs-messages-when-there-are-no-images ()
  "画像が 1 枚も無ければメッセージだけ。バッファは作らず error も投げない。"
  (let* ((messages nil) (built nil)
         (name " *tty-image-dired-empty-test*")
         (image-dired-thumbnail-buffer name))
    (unwind-protect
        (cl-letf (((symbol-function 'dired-get-marked-files) (lambda (&rest _) nil))
                  ((symbol-function 'wamei/tty-image-dired--build)
                   (lambda (&rest _) (setq built t)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          (with-temp-buffer
            (wamei/tty-image-dired--show-thumbs))
          (should messages)
          (should-not built)
          ;; ここが今回の本題。バッファができていないこと
          (should-not (get-buffer name)))
      (when (get-buffer name) (kill-buffer name)))))

(ert-deftest wamei/tty-image-dired-setup-adds-the-advice ()
  (unwind-protect
      (progn
        (wamei/tty-image-dired-setup)
        (should (advice-member-p #'wamei/tty-image-dired--display-thumbs-around
                                 'image-dired-display-thumbs))
        (should (advice-member-p #'wamei/tty-image-dired--update-marks-around
                                 'image-dired--thumb-update-marks))
        (should (advice-member-p #'wamei/tty-image-dired--update-mark-at-point-around
                                 'image-dired--thumb-update-mark-at-point)))
    (advice-remove 'image-dired-display-thumbs
                   #'wamei/tty-image-dired--display-thumbs-around)
    (advice-remove 'image-dired--thumb-update-marks
                   #'wamei/tty-image-dired--update-marks-around)
    (advice-remove 'image-dired--thumb-update-mark-at-point
                   #'wamei/tty-image-dired--update-mark-at-point-around)))

(ert-deftest wamei/tty-image-dired-update-mark-at-point-around-skips-the-builtin-for-our-mode ()
  "組み込みは point (箱) に `add-face-text-property' で face を貼り、placeholder の
セルを壊す。自分のモードでは元を呼ばない。"
  (let ((orig-called nil))
    (let ((buf (generate-new-buffer " *tty-image-dired-mark-at-point-test*")))
      (unwind-protect
          (with-current-buffer buf
            (wamei/tty-image-dired-mode)
            (wamei/tty-image-dired--update-mark-at-point-around
             (lambda (&rest _) (setq orig-called t)))
            (should-not orig-called))
        (kill-buffer buf)))))

(ert-deftest wamei/tty-image-dired-update-mark-at-point-around-calls-the-builtin-otherwise ()
  "自分のモードでなければ組み込みをそのまま呼ぶ (GUI や非対応端末のため)。"
  (let ((orig-called nil))
    (with-temp-buffer
      (wamei/tty-image-dired--update-mark-at-point-around
       (lambda (&rest _) (setq orig-called t)))
      (should orig-called))))

(ert-deftest wamei/tty-image-dired-update-marks-around-skips-the-builtin-for-our-mode ()
  "組み込みは placeholder のセルに face を貼って画像を壊すので、自分のモードのときは
元を呼ばない。"
  (let ((orig-called nil) (ours-called nil))
    (cl-letf (((symbol-function 'wamei/tty-image-dired--update-marks)
               (lambda () (setq ours-called t))))
      (let ((buf (generate-new-buffer " *tty-image-dired-marks-test*")))
        (unwind-protect
            (let ((image-dired-thumbnail-buffer (buffer-name buf)))
              (with-current-buffer buf (wamei/tty-image-dired-mode))
              (wamei/tty-image-dired--update-marks-around
               (lambda (&rest _) (setq orig-called t)))
              (should ours-called)
              (should-not orig-called))
          (kill-buffer buf))))))

(ert-deftest wamei/tty-image-dired-update-marks-around-calls-the-builtin-otherwise ()
  "自分のモードでなければ組み込みをそのまま呼ぶ (GUI や非対応端末のため)。"
  (let ((orig-args nil))
    (let ((buf (generate-new-buffer " *tty-image-dired-marks-test*")))
      (unwind-protect
          (let ((image-dired-thumbnail-buffer (buffer-name buf)))
            (wamei/tty-image-dired--update-marks-around
             (lambda (&rest args) (setq orig-args (or args 'called))))
            (should orig-args))
        (kill-buffer buf)))))

(provide 'tty-image-dired-test)
;;; tty-image-dired-test.el ends here
