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

(provide 'tty-image-dired-test)
;;; tty-image-dired-test.el ends here
