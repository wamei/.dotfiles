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

(provide 'tty-image-dired-test)
;;; tty-image-dired-test.el ends here
