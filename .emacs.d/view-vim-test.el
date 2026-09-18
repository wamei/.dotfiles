;;; view-vim-test.el --- tests for view-vim -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l view-vim-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'view)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "view-vim.el" dir) nil t))

(wamei/view-vim-setup)

;;; フィクスチャ

(defmacro wamei/view-vim-test--with-buffer (contents &rest body)
  "CONTENTS を入れた view-mode のバッファを選択 window に映して BODY を評価する。

`move-to-window-line' と `recenter' は選択 window を見るので、
`with-temp-buffer' ではなく実際に window へ映す必要がある。"
  (declare (indent 1))
  `(let ((buf (generate-new-buffer " *view-vim-test*")))
     (unwind-protect
         (save-window-excursion
           (with-current-buffer buf
             (insert ,contents)
             (view-mode 1)
             (set-window-buffer (selected-window) buf)
             (goto-char (point-min))
             (set-window-start (selected-window) (point-min))
             (redisplay)
             ,@body))
       (kill-buffer buf))))

(defun wamei/view-vim-test--lines (n)
  "1 行目から N 行目まで \"line <i>\" が並ぶ文字列を返す。"
  (mapconcat (lambda (i) (format "line %d" i))
             (number-sequence 1 n)
             "\n"))

;;; キーマップの配線

(ert-deftest wamei/view-vim-test-movement-keys ()
  "移動キーが vim のコマンドに刺さっている。"
  (dolist (pair '(("h" . backward-char)
                  ("j" . next-line)
                  ("k" . previous-line)
                  ("l" . forward-char)
                  ("w" . forward-to-word)
                  ("b" . backward-word)
                  ("e" . forward-word)
                  ("0" . wamei/view-vim-beginning-of-line)
                  ("^" . back-to-indentation)
                  ("$" . end-of-line)
                  ("g g" . wamei/view-vim-goto-first-line)
                  ("G" . wamei/view-vim-goto-last-line)
                  ("{" . backward-paragraph)
                  ("}" . forward-paragraph)
                  ("%" . wamei/view-vim-match-paren)))
    (should (eq (keymap-lookup view-mode-map (car pair)) (cdr pair)))))

(ert-deftest wamei/view-vim-test-screen-keys ()
  "スクロールと画面内移動のキーが刺さっている。"
  (dolist (pair '(("C-d" . View-scroll-half-page-forward)
                  ("C-u" . View-scroll-half-page-backward)
                  ("H" . wamei/view-vim-window-top)
                  ("M" . wamei/view-vim-window-middle)
                  ("L" . wamei/view-vim-window-bottom)
                  ("z z" . wamei/view-vim-recenter-center)
                  ("z t" . wamei/view-vim-recenter-top)
                  ("z b" . wamei/view-vim-recenter-bottom)))
    (should (eq (keymap-lookup view-mode-map (car pair)) (cdr pair)))))

(ert-deftest wamei/view-vim-test-does-not-shadow-emacs-scroll-keys ()
  "C-f / C-b / C-e / C-y は view-mode でも素の Emacs のまま。"
  (dolist (key '("C-f" "C-b" "C-e" "C-y"))
    (should (null (keymap-lookup view-mode-map key)))))

(ert-deftest wamei/view-vim-test-search-keys ()
  "検索キーが vim の割り当てになっている。"
  (dolist (pair '(("/" . View-search-regexp-forward)
                  ("?" . View-search-regexp-backward)
                  ("n" . View-search-last-regexp-forward)
                  ("N" . View-search-last-regexp-backward)
                  ("*" . wamei/view-vim-search-symbol-forward)
                  ("#" . wamei/view-vim-search-symbol-backward)))
    (should (eq (keymap-lookup view-mode-map (car pair)) (cdr pair)))))

(ert-deftest wamei/view-vim-test-exit-keys ()
  "q で抜け、i で編集に戻る。"
  (should (eq (keymap-lookup view-mode-map "q") 'View-quit))
  (should (eq (keymap-lookup view-mode-map "i") 'View-exit-and-edit)))

(ert-deftest wamei/view-vim-test-drops-less-style-keys ()
  "vim で別の意味を持つ less 風のキーは外してある。"
  (dolist (key '("d" "u" "y" "o" "r" "s" "p" "\\"))
    (should (null (keymap-lookup view-mode-map key)))))

(ert-deftest wamei/view-vim-test-view-read-only ()
  "read-only バッファで自動的に view-mode に入る。"
  (should (eq view-read-only t)))

;;; 0 (行頭 / 数引数)

(ert-deftest wamei/view-vim-test-zero-moves-to-bol ()
  "数引数の途中でなければ 0 は行頭へ移動する。"
  (wamei/view-vim-test--with-buffer "  foo bar\n"
    (goto-char (point-min))
    (end-of-line)
    (let ((current-prefix-arg nil)
          (last-command-event ?0))
      (call-interactively #'wamei/view-vim-beginning-of-line))
    (should (= (point) (line-beginning-position)))))

(ert-deftest wamei/view-vim-test-zero-is-a-digit-while-counting ()
  "数引数の途中の 0 は桁として扱う (10j が 10 行進むように)。"
  (wamei/view-vim-test--with-buffer "  foo bar\n"
    (goto-char (point-max))
    (let ((current-prefix-arg 1)
          (last-command-event ?0))
      (call-interactively #'wamei/view-vim-beginning-of-line)
      (should (equal prefix-arg 10)))))

;;; gg / G

(ert-deftest wamei/view-vim-test-gg-goes-to-first-line ()
  "gg は 1 行目のインデント末へ。"
  (wamei/view-vim-test--with-buffer "    line 1\nline 2\nline 3\n"
    (goto-char (point-max))
    (call-interactively #'wamei/view-vim-goto-first-line)
    (should (= (line-number-at-pos) 1))
    (should (= (point) (save-excursion (back-to-indentation) (point))))))

(ert-deftest wamei/view-vim-test-gg-with-count ()
  "N gg は N 行目へ。"
  (wamei/view-vim-test--with-buffer (wamei/view-vim-test--lines 20)
    (goto-char (point-min))
    (let ((current-prefix-arg 7))
      (call-interactively #'wamei/view-vim-goto-first-line))
    (should (= (line-number-at-pos) 7))))

(ert-deftest wamei/view-vim-test-G-goes-to-last-line ()
  "G は最終行へ。末尾の改行で作られる空行には止まらない。"
  (wamei/view-vim-test--with-buffer "line 1\nline 2\n  line 3\n"
    (goto-char (point-min))
    (call-interactively #'wamei/view-vim-goto-last-line)
    (should (= (line-number-at-pos) 3))
    (should (= (point) (save-excursion (back-to-indentation) (point))))))

(ert-deftest wamei/view-vim-test-G-with-count ()
  "N G は N 行目へ。"
  (wamei/view-vim-test--with-buffer (wamei/view-vim-test--lines 20)
    (goto-char (point-min))
    (let ((current-prefix-arg 5))
      (call-interactively #'wamei/view-vim-goto-last-line))
    (should (= (line-number-at-pos) 5))))

;;; H / M / L

(ert-deftest wamei/view-vim-test-window-top-middle-bottom ()
  "H / M / L が window の上端・中央・下端の行へ移動する。"
  (wamei/view-vim-test--with-buffer (wamei/view-vim-test--lines 200)
    (let ((height (window-body-height)))
      (goto-char (point-min))
      (forward-line 10)
      (call-interactively #'wamei/view-vim-window-top)
      (should (= (line-number-at-pos) 1))
      (call-interactively #'wamei/view-vim-window-middle)
      (should (= (line-number-at-pos) (1+ (/ height 2))))
      (call-interactively #'wamei/view-vim-window-bottom)
      (should (= (line-number-at-pos) height)))))

(ert-deftest wamei/view-vim-test-window-top-respects-scroll-margin ()
  "scroll-margin があるときの H はその分だけ下の行に止まる。"
  (wamei/view-vim-test--with-buffer (wamei/view-vim-test--lines 200)
    (let ((scroll-margin 3))
      (goto-char (point-min))
      (forward-line 10)
      (call-interactively #'wamei/view-vim-window-top)
      (should (= (line-number-at-pos) 4)))))

;;; zz / zt / zb

(ert-deftest wamei/view-vim-test-recenter-top ()
  "zt はカーソル行を window の先頭に持ってくる。"
  (wamei/view-vim-test--with-buffer (wamei/view-vim-test--lines 200)
    (goto-char (point-min))
    (forward-line 50)
    (call-interactively #'wamei/view-vim-recenter-top)
    (should (= (line-number-at-pos (window-start)) 51))))

(ert-deftest wamei/view-vim-test-recenter-center ()
  "zz はカーソル行を window の中央に持ってくる。"
  (wamei/view-vim-test--with-buffer (wamei/view-vim-test--lines 200)
    (goto-char (point-min))
    (forward-line 50)
    (call-interactively #'wamei/view-vim-recenter-center)
    (should (= (line-number-at-pos (window-start))
               (- 51 (/ (window-body-height) 2))))))

(ert-deftest wamei/view-vim-test-recenter-bottom ()
  "zb はカーソル行を window の下端に持ってくる。"
  (wamei/view-vim-test--with-buffer (wamei/view-vim-test--lines 200)
    (goto-char (point-min))
    (forward-line 50)
    (call-interactively #'wamei/view-vim-recenter-bottom)
    (should (= (line-number-at-pos (window-start))
               (- 51 (1- (window-body-height)))))))

;;; %

(ert-deftest wamei/view-vim-test-match-paren-from-open ()
  "開き括弧の上で % を押すと対応する閉じ括弧の上に移る。"
  (wamei/view-vim-test--with-buffer "(abc def)\n"
    (goto-char (point-min))
    (call-interactively #'wamei/view-vim-match-paren)
    (should (eq (char-after) ?\)))))

(ert-deftest wamei/view-vim-test-match-paren-from-close ()
  "閉じ括弧の上で % を押すと対応する開き括弧の上に移る。"
  (wamei/view-vim-test--with-buffer "(abc def)\n"
    (goto-char (point-min))
    (end-of-line)
    (backward-char 1)
    (call-interactively #'wamei/view-vim-match-paren)
    (should (= (point) (point-min)))))

(ert-deftest wamei/view-vim-test-match-paren-scans-forward-on-line ()
  "括弧の上にいないときは行内の次の括弧まで進んでから飛ぶ。"
  (wamei/view-vim-test--with-buffer "foo (bar) baz\n"
    (goto-char (point-min))
    (call-interactively #'wamei/view-vim-match-paren)
    (should (eq (char-after) ?\)))))

(ert-deftest wamei/view-vim-test-match-paren-without-paren-errors ()
  "行内に括弧が無ければエラーにする (黙って動かないのを避ける)。"
  (wamei/view-vim-test--with-buffer "foo bar baz\n"
    (goto-char (point-min))
    (should-error (call-interactively #'wamei/view-vim-match-paren)
                  :type 'user-error)))

;;; * / #

(ert-deftest wamei/view-vim-test-search-symbol-forward ()
  "* はカーソル下のシンボルを前方検索し、n で繰り返せるよう記録する。"
  (wamei/view-vim-test--with-buffer "alpha\nbeta\nalpha\n"
    (goto-char (point-min))
    (call-interactively #'wamei/view-vim-search-symbol-forward)
    (should (= (line-number-at-pos) 3))
    (should (equal view-last-regexp "\\_<alpha\\_>"))))

(ert-deftest wamei/view-vim-test-search-symbol-backward ()
  "# はカーソル下のシンボルを後方検索する。"
  (wamei/view-vim-test--with-buffer "alpha\nbeta\nalpha\n"
    (goto-char (point-min))
    (forward-line 2)
    (call-interactively #'wamei/view-vim-search-symbol-backward)
    (should (= (line-number-at-pos) 1))))

(ert-deftest wamei/view-vim-test-search-symbol-without-symbol-errors ()
  "シンボルの上にいなければエラーにする。"
  (wamei/view-vim-test--with-buffer "   \nalpha\n"
    (goto-char (point-min))
    (should-error (call-interactively #'wamei/view-vim-search-symbol-forward)
                  :type 'user-error)))

(provide 'view-vim-test)
;;; view-vim-test.el ends here
