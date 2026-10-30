;;; magit-diff-line-numbers-test.el --- tests for magit-diff-line-numbers -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l magit-diff-line-numbers-test.el -f ert-run-tests-batch-and-exit
;;
;; magit 本体は読まない。`magit-diff-wash-diffs-hook' に渡る生の git の出力を
;; 普通のバッファに入れて、フックの関数を直接呼ぶ。
;;; Code:

(require 'ert)
(load (expand-file-name "magit-diff-line-numbers.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun magit-diff-line-numbers-test--prefixes (diff)
  "DIFF に行番号を付け、各行の (行の文字列 . line-prefix の文字列) を返す。"
  (with-temp-buffer
    (insert diff)
    (goto-char (point-min))
    (wamei/magit-diff-line-numbers)
    (goto-char (point-min))
    (let (lines)
      (while (not (eobp))
        (push (cons (buffer-substring-no-properties (point) (line-end-position))
                    (let ((prefix (get-text-property (point) 'line-prefix)))
                      (and prefix (substring-no-properties prefix))))
              lines)
        (forward-line))
      (nreverse lines))))

(ert-deftest magit-diff-line-numbers-numbers-each-side ()
  "文脈行は両側、削除行は旧だけ、追加行は新だけに番号を付ける。見出しには付けない。"
  (should (equal (magit-diff-line-numbers-test--prefixes
                  "diff --git a/f b/f
index 1..2 100644
--- a/f
+++ b/f
@@ -8,3 +8,3 @@ heading
 a
-b
+c
 d
")
                 '(("diff --git a/f b/f" . nil)
                   ("index 1..2 100644" . nil)
                   ("--- a/f" . nil)
                   ("+++ b/f" . nil)
                   ("@@ -8,3 +8,3 @@ heading" . nil)
                   (" a" . " 8  8 │")
                   ("-b" . " 9    │")
                   ("+c" . "    9 │")
                   (" d" . "10 10 │")))))

(ert-deftest magit-diff-line-numbers-width-follows-largest-number ()
  "桁数は diff 全体で最大の行番号に揃え、hunk ごとに番号を数え直す。"
  (should (equal (mapcar #'cdr
                         (magit-diff-line-numbers-test--prefixes
                          "@@ -1 +1 @@
-x
+y
@@ -998,2 +998,3 @@
 p
+q
 r
"))
                 '(nil "   1      │" "        1 │"
                   nil " 998  998 │" "      999 │" " 999 1000 │"))))

(ert-deftest magit-diff-line-numbers-skips-no-newline-marker ()
  "\"\\ No newline at end of file\" には番号を付けず、数えもしない。"
  (should (equal (mapcar #'cdr
                         (magit-diff-line-numbers-test--prefixes
                          "@@ -1 +1 @@
-x
\\ No newline at end of file
+y
"))
                 '(nil "1   │" nil "  1 │"))))

(ert-deftest magit-diff-line-numbers-leaves-combined-diffs ()
  "マージの combined diff (@@@) は列が 3 つになるので付けない。"
  (should-not (delq nil (mapcar #'cdr
                                (magit-diff-line-numbers-test--prefixes
                                 "@@@ -1,1 -1,1 +1,1 @@@
- a
 +b
")))))

(ert-deftest magit-diff-line-numbers-colors-by-line-kind ()
  "番号の face は文脈行・削除行・追加行で分ける (delta と同じく削除は赤、追加は緑)。"
  (with-temp-buffer
    (insert "@@ -1,2 +1,2 @@\n a\n-b\n+c\n")
    (goto-char (point-min))
    (wamei/magit-diff-line-numbers)
    (should (equal (mapcar (lambda (line)
                             (goto-char (point-min))
                             (forward-line line)
                             (get-text-property 0 'face (get-text-property (point) 'line-prefix)))
                           '(1 2 3))
                   '(wamei/magit-diff-line-numbers
                     wamei/magit-diff-line-numbers-removed
                     wamei/magit-diff-line-numbers-added)))))

(ert-deftest magit-diff-line-numbers-keeps-text ()
  "テキストは変えない (magit の stage は本文の行をそのまま patch にする)。"
  (let ((diff "@@ -1,2 +1,2 @@\n a\n-b\n+c\n"))
    (with-temp-buffer
      (insert diff)
      (goto-char (point-min))
      (wamei/magit-diff-line-numbers)
      (should (equal (buffer-substring-no-properties (point-min) (point-max)) diff)))))

(provide 'magit-diff-line-numbers-test)
;;; magit-diff-line-numbers-test.el ends here
