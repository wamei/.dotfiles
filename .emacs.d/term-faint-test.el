;;; term-faint-test.el --- tests for term-faint -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l term-faint-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "term-faint.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; テスト用の定数

(defconst wamei/term-faint-test--params '("38" "2" "85" "85" "86")
  "テストで faint の代わりに入れる前景色の引数。`shadow' が #555556 のときの値。")

(defun wamei/term-faint-test--translate (input &optional state)
  "INPUT を書き換えた文字列だけを返す。STATE 省略時は初期状態から始める。"
  (car (wamei/term-faint--translate
        input (or state (wamei/term-faint--initial-state))
        wamei/term-faint-test--params)))

;;; 書き換え

(ert-deftest wamei/term-faint-test-replaces-faint-with-color ()
  "SGR 2 は前景色に置き換わる。"
  (should (equal (wamei/term-faint-test--translate "\e[2mhello\e[22m")
                 "\e[38;2;85;85;86mhello\e[22;39m")))

(ert-deftest wamei/term-faint-test-keeps-bold ()
  "SGR 1 と、それを閉じる SGR 22 はそのまま通す。"
  (should (equal (wamei/term-faint-test--translate "\e[1mbold\e[22mrest")
                 "\e[1mbold\e[22mrest")))

(ert-deftest wamei/term-faint-test-keeps-foreground-after-bold ()
  "色の中の太字を閉じても色は消さない。claude の \"Listed 1 directory\" の形。"
  (should (equal (wamei/term-faint-test--translate
                  "\e[38;5;246mListed \e[1m1\e[22m directory \e[39m")
                 "\e[38;5;246mListed \e[1m1\e[22m directory \e[39m")))

(ert-deftest wamei/term-faint-test-restores-foreground-after-faint ()
  "色の中の faint を閉じたら、その色に戻す。"
  (should (equal (wamei/term-faint-test--translate
                  "\e[38;5;246mdim \e[2mfaint\e[22m back\e[39m")
                 "\e[38;5;246mdim \e[38;2;85;85;86mfaint\e[22;38;5;246m back\e[39m")))

(ert-deftest wamei/term-faint-test-reset-clears-faint ()
  "SGR 0 は faint を閉じるので、後続の 22 に色を足さない。"
  (should (equal (wamei/term-faint-test--translate "\e[2mx\e[0m\e[1my\e[22m")
                 "\e[38;2;85;85;86mx\e[0m\e[1my\e[22m")))

(ert-deftest wamei/term-faint-test-empty-params-is-reset ()
  "引数なしの SGR は 0 と同じ扱い。"
  (should (equal (wamei/term-faint-test--translate "\e[2mx\e[m\e[22m")
                 "\e[38;2;85;85;86mx\e[m\e[22m")))

(ert-deftest wamei/term-faint-test-handles-combined-params ()
  "1 つの SGR に複数の引数が入っていても 2 だけを置き換える。"
  (should (equal (wamei/term-faint-test--translate "\e[1;2;4mx")
                 "\e[1;38;2;85;85;86;4mx")))

(ert-deftest wamei/term-faint-test-ignores-background-arguments ()
  "背景色の引数を前景色と取り違えない。"
  (should (equal (wamei/term-faint-test--translate
                  "\e[48;5;31m\e[2mx\e[22m")
                 "\e[48;5;31m\e[38;2;85;85;86mx\e[22;39m")))

(ert-deftest wamei/term-faint-test-tracks-truecolor-foreground ()
  "24bit の前景色も覚えて戻せる。"
  (should (equal (wamei/term-faint-test--translate
                  "\e[38;2;1;2;3m\e[2mx\e[22m")
                 "\e[38;2;1;2;3m\e[38;2;85;85;86mx\e[22;38;2;1;2;3m")))

(ert-deftest wamei/term-faint-test-tracks-basic-foreground ()
  "30-37 / 90-97 の前景色も覚えて戻せる。"
  (should (equal (wamei/term-faint-test--translate "\e[31m\e[2mx\e[22m")
                 "\e[31m\e[38;2;85;85;86mx\e[22;31m"))
  (should (equal (wamei/term-faint-test--translate "\e[91m\e[2mx\e[22m")
                 "\e[91m\e[38;2;85;85;86mx\e[22;91m")))

(ert-deftest wamei/term-faint-test-leaves-other-sequences-alone ()
  "SGR 以外の制御列と素のテキストは触らない。"
  (should (equal (wamei/term-faint-test--translate "\e[?2026h\e[30;1Hplain\e[K")
                 "\e[?2026h\e[30;1Hplain\e[K"))
  (should (equal (wamei/term-faint-test--translate "no escapes here")
                 "no escapes here")))

(ert-deftest wamei/term-faint-test-carries-state-across-calls ()
  "faint が開いたまま呼び出しが切れても、次の 22 で色を戻す。"
  (let* ((first (wamei/term-faint--translate
                 "\e[38;5;246m\e[2mx" (wamei/term-faint--initial-state)
                 wamei/term-faint-test--params))
         (second (wamei/term-faint--translate
                  "y\e[22m" (cdr first) wamei/term-faint-test--params)))
    (should (equal (car first) "\e[38;5;246m\e[38;2;85;85;86mx"))
    (should (equal (car second) "y\e[22;38;5;246m"))))

;;; 途中で切れた制御列の持ち越し

(ert-deftest wamei/term-faint-test-splits-partial-escape ()
  "末尾の未完成な SGR は次回へ持ち越す。"
  (should (equal (wamei/term-faint--split-carry "abc\e") '("abc" . "\e")))
  (should (equal (wamei/term-faint--split-carry "abc\e[") '("abc" . "\e[")))
  (should (equal (wamei/term-faint--split-carry "abc\e[38;5") '("abc" . "\e[38;5")))
  (should (equal (wamei/term-faint--split-carry "abc\e[2m") '("abc\e[2m" . nil)))
  (should (equal (wamei/term-faint--split-carry "abc") '("abc" . nil))))

(ert-deftest wamei/term-faint-test-does-not-carry-private-sequences ()
  "SGR になりえない私的パラメータの列は持ち越さない (vterm 側の分割処理に任せる)。"
  (should (equal (wamei/term-faint--split-carry "abc\e[?20") '("abc\e[?20" . nil))))

(ert-deftest wamei/term-faint-test-rejoins-carried-escape ()
  "持ち越した断片と次の入力をつなぐと書き換えが効く。"
  (let* ((split (wamei/term-faint--split-carry "abc\e["))
         (joined (concat (cdr split) "2mx")))
    (should (equal (car split) "abc"))
    (should (equal (wamei/term-faint-test--translate joined)
                   "\e[38;2;85;85;86mx"))))

;;; 色の解釈

(ert-deftest wamei/term-faint-test-color-params-from-hex ()
  "#RRGGBB は 24bit の前景色引数になる。"
  (should (equal (wamei/term-faint--color-params "#555556")
                 '("38" "2" "85" "85" "86")))
  (should (equal (wamei/term-faint--color-params "#000000")
                 '("38" "2" "0" "0" "0"))))

(ert-deftest wamei/term-faint-test-color-params-fallback ()
  "解釈できない色なら 256 色の灰に落とす。"
  (should (equal (wamei/term-faint--color-params nil) '("38" "5" "244")))
  (should (equal (wamei/term-faint--color-params "not-a-color") '("38" "5" "244"))))

(provide 'term-faint-test)
;;; term-faint-test.el ends here
