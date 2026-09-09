;;; dired-image-preview-kitty-test.el --- tests for dired-image-preview-kitty -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l dired-image-preview-kitty-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(package-initialize)
(require 'dired)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "dired-image-preview.el" dir) nil t)
  (load (expand-file-name "dired-image-preview-kitty.el" dir) nil t))

;;; エスケープシーケンス

(ert-deftest wamei/dired-image-preview-kitty-wrap-passes-through-outside-tmux ()
  (should (equal (wamei/dired-image-preview-kitty--wrap "\e_Ga=q\e\\" nil) "\e_Ga=q\e\\")))

(ert-deftest wamei/dired-image-preview-kitty-wrap-uses-tmux-passthrough ()
  "tmux の中では DCS tmux; で包み、中の ESC を二重にする。"
  (should (equal (wamei/dired-image-preview-kitty--wrap "\e_Ga=q\e\\" t)
                 "\ePtmux;\e\e_Ga=q\e\e\\\e\\")))

(ert-deftest wamei/dired-image-preview-kitty-chunks-splits-by-size ()
  (should (equal (wamei/dired-image-preview-kitty--chunks "abcdefghij" 4) '("abcd" "efgh" "ij")))
  (should (equal (wamei/dired-image-preview-kitty--chunks "abcd" 4) '("abcd")))
  (should (equal (wamei/dired-image-preview-kitty--chunks "" 4) '(""))))

(ert-deftest wamei/dired-image-preview-kitty-transmit-sequences-first-carries-control-keys ()
  "先頭だけ a=T,U=1 と大きさを持ち、m は最後だけ 0。q=2 で応答を止める。"
  (let ((seqs (wamei/dired-image-preview-kitty--transmit-sequences "AAAABBBBCC" 7 20 8 4)))
    (should (= 3 (length seqs)))
    (should (equal (nth 0 seqs) "\e_Ga=T,U=1,f=100,i=7,c=20,r=8,q=2,m=1;AAAA\e\\"))
    (should (equal (nth 1 seqs) "\e_Gm=1;BBBB\e\\"))
    (should (equal (nth 2 seqs) "\e_Gm=0;CC\e\\"))))

(ert-deftest wamei/dired-image-preview-kitty-transmit-sequences-single-chunk ()
  (should (equal (wamei/dired-image-preview-kitty--transmit-sequences "AAAA" 1 2 3 4096)
                 '("\e_Ga=T,U=1,f=100,i=1,c=2,r=3,q=2,m=0;AAAA\e\\"))))

(ert-deftest wamei/dired-image-preview-kitty-delete-sequence-frees-data ()
  (should (equal (wamei/dired-image-preview-kitty--delete-sequence 7) "\e_Ga=d,d=I,i=7,q=2\e\\")))

(ert-deftest wamei/dired-image-preview-kitty-query-sequence ()
  (should (equal (wamei/dired-image-preview-kitty--query-sequence)
                 "\e_Ga=q,i=31,s=1,v=1,f=24;AAAA\e\\")))

(ert-deftest wamei/dired-image-preview-kitty-query-ok-p ()
  (should (wamei/dired-image-preview-kitty--query-ok-p "\e_Gi=31;OK\e\\"))
  ;; フォーカスイベントなどが混ざっても拾う
  (should (wamei/dired-image-preview-kitty--query-ok-p "\e[I\e_Gi=31;OK\e\\"))
  (should-not (wamei/dired-image-preview-kitty--query-ok-p ""))
  (should-not (wamei/dired-image-preview-kitty--query-ok-p "\e_Gi=31;EINVAL:bad\e\\")))

;;; セルサイズ

(ert-deftest wamei/dired-image-preview-kitty-parse-cell-size-reads-height-then-width ()
  "CSI 16 t の応答は CSI 6 ; 高さ ; 幅 t。返り値は (幅 . 高さ)。"
  (should (equal (wamei/dired-image-preview-kitty--parse-cell-size "\e[6;19;8t") '(8 . 19)))
  (should (equal (wamei/dired-image-preview-kitty--parse-cell-size "\e[O\e[6;19;8t") '(8 . 19)))
  (should-not (wamei/dired-image-preview-kitty--parse-cell-size ""))
  (should-not (wamei/dired-image-preview-kitty--parse-cell-size "\e[6;0;8t")))

(ert-deftest wamei/dired-image-preview-kitty-cell-count-fits-max-keeping-aspect ()
  "画像 400x400px、セル 8x19px、上限 60x12 セル → 高さで縛られ 12 行、幅は 12*19/8 = 28.5 → 28 桁。"
  (should (equal (wamei/dired-image-preview-kitty--cell-count '(400 . 400) '(8 . 19) '(60 . 12)) '(28 . 12)))
  ;; 横長 1600x400 → 幅で縛られ 60 桁、高さ 60*8/19*400/1600 = 6.3 → 6 行
  (should (equal (wamei/dired-image-preview-kitty--cell-count '(1600 . 400) '(8 . 19) '(60 . 12)) '(60 . 6))))

(ert-deftest wamei/dired-image-preview-kitty-cell-count-does-not-upscale ()
  "160x38px はそのまま 20x2 セル。上限より小さければ拡大しない。"
  (should (equal (wamei/dired-image-preview-kitty--cell-count '(160 . 38) '(8 . 19) '(60 . 12)) '(20 . 2))))

(ert-deftest wamei/dired-image-preview-kitty-cell-count-is-at-least-one ()
  (should (equal (wamei/dired-image-preview-kitty--cell-count '(3 . 3) '(8 . 19) '(60 . 12)) '(1 . 1))))

;;; placeholder

(ert-deftest wamei/dired-image-preview-kitty-diacritic-table ()
  (should (= (wamei/dired-image-preview-kitty--diacritic 0) #x0305))
  (should (= (wamei/dired-image-preview-kitty--diacritic 1) #x030D))
  (should (= (wamei/dired-image-preview-kitty--diacritic 2) #x030E))
  ;; 表は 297 個 (kitty の gen/rowcolumn-diacritics.txt)
  (should (= (length wamei/dired-image-preview-kitty--diacritics) 297))
  (should (= (wamei/dired-image-preview-kitty--diacritic 296) #x1D244)))

(ert-deftest wamei/dired-image-preview-kitty-color-name-depends-on-color-depth ()
  (should (equal (wamei/dired-image-preview-kitty--color 7 16777216) "#000007"))
  (should (equal (wamei/dired-image-preview-kitty--color 255 16777216) "#0000FF"))
  (should (equal (wamei/dired-image-preview-kitty--color 7 256) "color-7")))

(ert-deftest wamei/dired-image-preview-kitty-placeholder-line-is-composed-cells ()
  "1 セル = U+10EEEE + 行 + 桁の結合文字 3 文字を合成した 1 桁。前景色は ID。"
  (let ((line (wamei/dired-image-preview-kitty--placeholder-line 7 3 2 "#000007")))
    (should (= (length line) 9))
    (should (= (aref line 0) #x10EEEE))
    (should (= (aref line 1) (wamei/dired-image-preview-kitty--diacritic 2)))
    (should (= (aref line 2) (wamei/dired-image-preview-kitty--diacritic 0)))
    (should (= (aref line 5) (wamei/dired-image-preview-kitty--diacritic 1)))
    (should (equal (get-text-property 0 'face line) '(:foreground "#000007")))
    ;; tty の Emacs は合成しないと結合文字を 1 桁ずつ描くので、3 文字ごとに composition が要る
    (dolist (i '(0 3 6))
      (should (get-text-property i 'composition line)))
    (with-temp-buffer
      (insert line)
      (should (= (string-width line) 3)))))

(ert-deftest wamei/dired-image-preview-kitty-next-id-cycles-in-8-bits ()
  "256 色モードでも使えるよう ID は 1〜255 を巡回する。"
  (let ((wamei/dired-image-preview-kitty--last-id 0))
    (should (= (wamei/dired-image-preview-kitty--next-id) 1))
    (should (= (wamei/dired-image-preview-kitty--next-id) 2)))
  (let ((wamei/dired-image-preview-kitty--last-id 255))
    (should (= (wamei/dired-image-preview-kitty--next-id) 1))))

;;; 位置

(ert-deftest wamei/dired-image-preview-kitty-frame-position-below-right-of-anchor ()
  "アンカー (桁 10 . 行 5) の右下に gap (2 . 1) と枠 1 を空けて置く。"
  (should (equal (wamei/dired-image-preview-kitty--frame-position '(10 . 5) '(20 . 8) '(200 . 50) '(2 . 1))
                 '(13 . 8))))

(ert-deftest wamei/dired-image-preview-kitty-frame-position-flips-above-when-no-room-below ()
  "下に収まらなければアンカーの上に置く (枠込みで)。"
  (should (equal (wamei/dired-image-preview-kitty--frame-position '(10 . 45) '(20 . 8) '(200 . 50) '(2 . 1))
                 '(13 . 35))))

(ert-deftest wamei/dired-image-preview-kitty-frame-position-clamps-to-right-edge ()
  (should (equal (wamei/dired-image-preview-kitty--frame-position '(190 . 5) '(20 . 8) '(200 . 50) '(2 . 1))
                 '(179 . 8))))

;;; sips

(ert-deftest wamei/dired-image-preview-kitty-resize-args-only-when-larger-than-limit ()
  "長辺が上限を超えるときだけ -Z を付ける。sips -Z は小さい画像を拡大してしまうので。"
  (should (equal (wamei/dired-image-preview-kitty--resize-args '(4000 . 3000) 1200) '("-Z" "1200")))
  (should (equal (wamei/dired-image-preview-kitty--resize-args '(300 . 1300) 1200) '("-Z" "1200")))
  (should-not (wamei/dired-image-preview-kitty--resize-args '(300 . 120) 1200))
  (should-not (wamei/dired-image-preview-kitty--resize-args '(1200 . 800) 1200)))

(ert-deftest wamei/dired-image-preview-kitty-parse-sips-size ()
  (should (equal (wamei/dired-image-preview-kitty--parse-sips-size
                  "/tmp/x.png\n  pixelWidth: 1600\n  pixelHeight: 400\n")
                 '(1600 . 400)))
  (should-not (wamei/dired-image-preview-kitty--parse-sips-size "garbage")))

;;; child frame

(ert-deftest wamei/dired-image-preview-kitty-make-frame-keeps-selected-frame ()
  "tty では `make-frame' が作った child frame を選択してしまう (GUI では選択しない)。
そのままだと dired の window が非選択になり、mode-line が非アクティブになって
カーソルも消え、最初のキーがプレビュー側へ行く。作成後に元のフレームへ戻す。"
  (let* ((parent 'parent-frame)
         (child 'child-frame)
         (selected parent)
         (select-calls nil))
    (cl-letf (((symbol-function 'selected-frame) (lambda () selected))
              ;; tty の make-frame と同じく、作った frame を選択する
              ((symbol-function 'make-frame) (lambda (_params) (setq selected child) child))
              ((symbol-function 'select-frame)
               (lambda (frame &optional norecord)
                 (push (list frame norecord) select-calls)
                 (setq selected frame)))
              ((symbol-function 'frame-root-window) (lambda (_f) 'window))
              ((symbol-function 'set-window-buffer) #'ignore)
              ((symbol-function 'set-window-dedicated-p) #'ignore)
              ((symbol-function 'set-window-parameter) #'ignore))
      (should (eq (wamei/dired-image-preview-kitty--make-frame parent 'buffer '(1 . 2) '(3 . 4))
                  child))
      (should (eq selected parent))
      (should (equal select-calls (list (list parent 'norecord)))))))

(ert-deftest wamei/dired-image-preview-kitty-make-frame-does-not-reselect-when-unchanged ()
  "選択が動かない環境 (GUI) では `select-frame' を呼ばない。"
  (let* ((parent 'parent-frame)
         (select-calls nil))
    (cl-letf (((symbol-function 'selected-frame) (lambda () parent))
              ((symbol-function 'make-frame) (lambda (_params) 'child-frame))
              ((symbol-function 'select-frame)
               (lambda (frame &optional norecord) (push (list frame norecord) select-calls)))
              ((symbol-function 'frame-root-window) (lambda (_f) 'window))
              ((symbol-function 'set-window-buffer) #'ignore)
              ((symbol-function 'set-window-dedicated-p) #'ignore)
              ((symbol-function 'set-window-parameter) #'ignore))
      (wamei/dired-image-preview-kitty--make-frame parent 'buffer '(1 . 2) '(3 . 4))
      (should-not select-calls))))

(provide 'dired-image-preview-kitty-test)
;;; dired-image-preview-kitty-test.el ends here
