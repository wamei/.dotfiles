;;; kitty-graphics-test.el --- tests for kitty-graphics -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l kitty-graphics-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "kitty-graphics.el" dir) nil t))

;;; エスケープシーケンス

(ert-deftest wamei/kitty-graphics-wrap-passes-through-outside-tmux ()
  (should (equal (wamei/kitty-graphics--wrap "\e_Ga=q\e\\" nil) "\e_Ga=q\e\\")))

(ert-deftest wamei/kitty-graphics-wrap-uses-tmux-passthrough ()
  "tmux の中では DCS tmux; で包み、中の ESC を二重にする。"
  (should (equal (wamei/kitty-graphics--wrap "\e_Ga=q\e\\" t)
                 "\ePtmux;\e\e_Ga=q\e\e\\\e\\")))

(ert-deftest wamei/kitty-graphics-chunks-splits-by-size ()
  (should (equal (wamei/kitty-graphics--chunks "abcdefghij" 4) '("abcd" "efgh" "ij")))
  (should (equal (wamei/kitty-graphics--chunks "abcd" 4) '("abcd")))
  (should (equal (wamei/kitty-graphics--chunks "" 4) '(""))))

(ert-deftest wamei/kitty-graphics-transmit-sequences-first-carries-control-keys ()
  "先頭だけ a=T,U=1 と大きさを持ち、m は最後だけ 0。q=2 で応答を止める。"
  (let ((seqs (wamei/kitty-graphics--transmit-sequences "AAAABBBBCC" 7 20 8 4)))
    (should (= 3 (length seqs)))
    (should (equal (nth 0 seqs) "\e_Ga=T,U=1,f=100,i=7,c=20,r=8,q=2,m=1;AAAA\e\\"))
    (should (equal (nth 1 seqs) "\e_Gm=1;BBBB\e\\"))
    (should (equal (nth 2 seqs) "\e_Gm=0;CC\e\\"))))

(ert-deftest wamei/kitty-graphics-transmit-sequences-single-chunk ()
  (should (equal (wamei/kitty-graphics--transmit-sequences "AAAA" 1 2 3 4096)
                 '("\e_Ga=T,U=1,f=100,i=1,c=2,r=3,q=2,m=0;AAAA\e\\"))))

(ert-deftest wamei/kitty-graphics-delete-sequence-frees-data ()
  (should (equal (wamei/kitty-graphics--delete-sequence 7) "\e_Ga=d,d=I,i=7,q=2\e\\")))

(ert-deftest wamei/kitty-graphics-query-sequence ()
  (should (equal (wamei/kitty-graphics--query-sequence)
                 "\e_Ga=q,i=31,s=1,v=1,f=24;AAAA\e\\")))

(ert-deftest wamei/kitty-graphics-query-ok-p ()
  (should (wamei/kitty-graphics--query-ok-p "\e_Gi=31;OK\e\\"))
  ;; フォーカスイベントなどが混ざっても拾う
  (should (wamei/kitty-graphics--query-ok-p "\e[I\e_Gi=31;OK\e\\"))
  (should-not (wamei/kitty-graphics--query-ok-p ""))
  (should-not (wamei/kitty-graphics--query-ok-p "\e_Gi=31;EINVAL:bad\e\\")))

;;; セルサイズ

(ert-deftest wamei/kitty-graphics-parse-cell-size-reads-height-then-width ()
  "CSI 16 t の応答は CSI 6 ; 高さ ; 幅 t。返り値は (幅 . 高さ)。"
  (should (equal (wamei/kitty-graphics--parse-cell-size "\e[6;19;8t") '(8 . 19)))
  (should (equal (wamei/kitty-graphics--parse-cell-size "\e[O\e[6;19;8t") '(8 . 19)))
  (should-not (wamei/kitty-graphics--parse-cell-size ""))
  (should-not (wamei/kitty-graphics--parse-cell-size "\e[6;0;8t")))

(ert-deftest wamei/kitty-graphics-cell-count-fits-max-keeping-aspect ()
  "画像 400x400px、セル 8x19px、上限 60x12 セル → 高さで縛られ 12 行、幅は 12*19/8 = 28.5 → 28 桁。"
  (should (equal (wamei/kitty-graphics-cell-count '(400 . 400) '(8 . 19) '(60 . 12)) '(28 . 12)))
  ;; 横長 1600x400 → 幅で縛られ 60 桁、高さ 60*8/19*400/1600 = 6.3 → 6 行
  (should (equal (wamei/kitty-graphics-cell-count '(1600 . 400) '(8 . 19) '(60 . 12)) '(60 . 6))))

(ert-deftest wamei/kitty-graphics-cell-count-does-not-upscale ()
  "160x38px はそのまま 20x2 セル。上限より小さければ拡大しない。"
  (should (equal (wamei/kitty-graphics-cell-count '(160 . 38) '(8 . 19) '(60 . 12)) '(20 . 2))))

(ert-deftest wamei/kitty-graphics-cell-count-is-at-least-one ()
  (should (equal (wamei/kitty-graphics-cell-count '(3 . 3) '(8 . 19) '(60 . 12)) '(1 . 1))))

;;; placeholder

(ert-deftest wamei/kitty-graphics-diacritic-table ()
  (should (= (wamei/kitty-graphics--diacritic 0) #x0305))
  (should (= (wamei/kitty-graphics--diacritic 1) #x030D))
  (should (= (wamei/kitty-graphics--diacritic 2) #x030E))
  ;; 表は 297 個 (kitty の gen/rowcolumn-diacritics.txt)
  (should (= (length wamei/kitty-graphics--diacritics) 297))
  (should (= (wamei/kitty-graphics--diacritic 296) #x1D244)))

(ert-deftest wamei/kitty-graphics-color-name-depends-on-color-depth ()
  (should (equal (wamei/kitty-graphics--color 7 16777216) "#000007"))
  (should (equal (wamei/kitty-graphics--color 255 16777216) "#0000FF"))
  (should (equal (wamei/kitty-graphics--color 7 256) "color-7")))

(ert-deftest wamei/kitty-graphics-placeholder-line-is-composed-cells ()
  "1 セル = U+10EEEE + 行 + 桁の結合文字 3 文字を合成した 1 桁。前景色は ID。"
  (let ((line (wamei/kitty-graphics-placeholder-line 7 3 2 "#000007")))
    (should (= (length line) 9))
    (should (= (aref line 0) #x10EEEE))
    (should (= (aref line 1) (wamei/kitty-graphics--diacritic 2)))
    (should (= (aref line 2) (wamei/kitty-graphics--diacritic 0)))
    (should (= (aref line 5) (wamei/kitty-graphics--diacritic 1)))
    (should (equal (get-text-property 0 'face line) '(:foreground "#000007")))
    ;; tty の Emacs は合成しないと結合文字を 1 桁ずつ描くので、3 文字ごとに composition が要る
    (dolist (i '(0 3 6))
      (should (get-text-property i 'composition line)))
    (with-temp-buffer
      (insert line)
      (should (= (string-width line) 3)))))

(ert-deftest wamei/kitty-graphics-next-id-cycles-in-8-bits ()
  "256 色モードでも使えるよう ID は 1〜255 を巡回する。
`--live-ids' は process global なので、他のテストへ漏らさないよう let で nil に束縛する。"
  (let ((wamei/kitty-graphics--live-ids nil))
    (let ((wamei/kitty-graphics--last-id 0))
      (should (= (wamei/kitty-graphics--next-id) 1))
      (should (= (wamei/kitty-graphics--next-id) 2)))
    (let ((wamei/kitty-graphics--last-id 255))
      (should (= (wamei/kitty-graphics--next-id) 1)))))

(ert-deftest wamei/kitty-graphics-next-id-skips-live-ids ()
  "既に生きている (まだ解放していない) ID は飛ばす。
既存 ID へ a=T を送ると端末側で置き換わり、先に出していた画像が壊れるため。"
  (let ((wamei/kitty-graphics--last-id 0)
        (wamei/kitty-graphics--live-ids '(2 3)))
    (should (= (wamei/kitty-graphics--next-id) 1))
    (should (= (wamei/kitty-graphics--next-id) 4))))

(ert-deftest wamei/kitty-graphics-next-id-does-not-loop-forever-when-all-live ()
  "255 個すべてが使用中でも無限ループせず、いちばん古い ID を諦めて返す。"
  (let* ((wamei/kitty-graphics--last-id 0)
         (wamei/kitty-graphics--live-ids (number-sequence 1 255)))
    (should (= (wamei/kitty-graphics--next-id) 255))))

(ert-deftest wamei/kitty-graphics-put-tracks-live-id-and-delete-releases-it ()
  "put が成功すると ID を `--live-ids' に載せ、delete で外れる。"
  (let ((wamei/kitty-graphics--last-id 0)
        (wamei/kitty-graphics--live-ids nil)
        (tmp (make-temp-file "kitty-graphics-test-" nil ".png")))
    (unwind-protect
        (progn
          (write-region "fake-png-bytes" nil tmp nil 'silent)
          (cl-letf (((symbol-function 'wamei/kitty-graphics--prepare-png) (lambda (&rest _) tmp))
                    ((symbol-function 'wamei/kitty-graphics--send) #'ignore))
            (let ((id (wamei/kitty-graphics-put "/tmp/a.png" 4 2)))
              (should (= id 1))
              (should (equal wamei/kitty-graphics--live-ids '(1)))
              (wamei/kitty-graphics-delete id)
              (should-not wamei/kitty-graphics--live-ids))))
      (when (file-exists-p tmp) (delete-file tmp)))))

;;; sips

(ert-deftest wamei/kitty-graphics-resize-args-only-when-larger-than-limit ()
  "長辺が上限を超えるときだけ -Z を付ける。sips -Z は小さい画像を拡大してしまうので。"
  (should (equal (wamei/kitty-graphics--resize-args '(4000 . 3000) 1200) '("-Z" "1200")))
  (should (equal (wamei/kitty-graphics--resize-args '(300 . 1300) 1200) '("-Z" "1200")))
  (should-not (wamei/kitty-graphics--resize-args '(300 . 120) 1200))
  (should-not (wamei/kitty-graphics--resize-args '(1200 . 800) 1200)))

(ert-deftest wamei/kitty-graphics-parse-sips-size ()
  (should (equal (wamei/kitty-graphics--parse-sips-size
                  "/tmp/x.png\n  pixelWidth: 1600\n  pixelHeight: 400\n")
                 '(1600 . 400)))
  (should-not (wamei/kitty-graphics--parse-sips-size "garbage")))

;;; placeholder-string (新設)

(ert-deftest wamei/kitty-graphics-placeholder-string-joins-rows-with-newline ()
  "ROWS 行ぶんの placeholder を改行で連ねる。各行は COLS 桁。"
  (cl-letf (((symbol-function 'display-color-cells) (lambda (&optional _) 256)))
    (let* ((s (wamei/kitty-graphics-placeholder-string 7 2 3))
           (lines (split-string s "\n")))
      (should (= (length lines) 3))
      (dolist (line lines)
        (should (= (string-width line) 2)))
      ;; 行番号の結合文字が行ごとに違う
      (should (= (aref (nth 0 lines) 1) (wamei/kitty-graphics--diacritic 0)))
      (should (= (aref (nth 1 lines) 1) (wamei/kitty-graphics--diacritic 1)))
      (should (= (aref (nth 2 lines) 1) (wamei/kitty-graphics--diacritic 2)))
      ;; 256 色モードでは前景色が color-N
      (should (equal (get-text-property 0 'face (nth 0 lines)) '(:foreground "color-7"))))))

(ert-deftest wamei/kitty-graphics-query-terminal-does-not-ask-during-startup ()
  "起動中は端末に訊かない。Emacs 自身の初期化シーケンスが流れていて応答を
拾い損ねるため。訊いていないので記憶もしない (次に呼ばれたら訊き直す)。"
  (let ((sent nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics--send)
               (lambda (seq) (push seq sent)))
              ((symbol-function 'wamei/kitty-graphics--read-response) (lambda () ""))
              ((symbol-function 'terminal-parameter) (lambda (&rest _) nil))
              ((symbol-function 'set-terminal-parameter)
               (lambda (&rest _) (error "起動中に記憶してはいけない")))
              (after-init-time nil))
      (should-not (wamei/kitty-graphics--query-terminal 'test "SEQ" #'identity))
      (should-not sent))))

(ert-deftest wamei/kitty-graphics-query-terminal-asks-after-startup ()
  "起動後は訊いて、結果を記憶する。"
  (let ((sent nil) (stored nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics--send)
               (lambda (seq) (push seq sent)))
              ((symbol-function 'wamei/kitty-graphics--read-response) (lambda () "OK"))
              ((symbol-function 'terminal-parameter) (lambda (&rest _) nil))
              ((symbol-function 'set-terminal-parameter)
               (lambda (_t key value) (setq stored (cons key value))))
              (after-init-time (current-time)))
      (should (equal (wamei/kitty-graphics--query-terminal 'test "SEQ" #'identity) "OK"))
      (should (equal sent '("SEQ")))
      (should (equal stored '(test . "OK"))))))

(ert-deftest wamei/kitty-graphics-query-terminal-remembers-a-real-failure ()
  "起動後に訊いて応答が無かったのは本当の非対応。`none' として記憶し、訊き直さない。"
  (let ((stored nil) (asked 0))
    (cl-letf (((symbol-function 'wamei/kitty-graphics--send) #'ignore)
              ((symbol-function 'wamei/kitty-graphics--read-response)
               (lambda () (setq asked (1+ asked)) ""))
              ((symbol-function 'terminal-parameter) (lambda (&rest _) (cdr stored)))
              ((symbol-function 'set-terminal-parameter)
               (lambda (_t key value) (setq stored (cons key value))))
              (after-init-time (current-time)))
      (should-not (wamei/kitty-graphics--query-terminal 'test "SEQ" (lambda (_) nil)))
      (should (equal stored '(test . none)))
      ;; 2 回目は記憶を見るだけ
      (should-not (wamei/kitty-graphics--query-terminal 'test "SEQ" (lambda (_) nil)))
      (should (= asked 1)))))

(provide 'kitty-graphics-test)
;;; kitty-graphics-test.el ends here
