;;; claude-complete-test.el --- tests for claude-complete -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "claude-cli.el" dir) nil t)
  (load (expand-file-name "claude-complete.el" dir) nil t))

;;; フィクスチャ

(defun wamei/claude-complete-test--stub (script)
  "SCRIPT を本文とする実行可能なシェルスクリプトを作り、そのパスを返す。"
  (let ((path (make-temp-file "claude-stub-" nil ".sh"
                              (concat "#!/bin/sh\n" script "\n"))))
    (set-file-modes path #o755)
    path))

(defmacro wamei/claude-complete-test--with-stub (script &rest body)
  "SCRIPT のスタブを `wamei/claude-cli-program' にして BODY を評価する。"
  (declare (indent 1))
  `(let ((wamei/claude-cli-program (wamei/claude-complete-test--stub ,script)))
     (unwind-protect (progn ,@body)
       (delete-file wamei/claude-cli-program))))

(defun wamei/claude-complete-test--wait (process)
  "PROCESS が終了し sentinel が走り終わるまで待つ。"
  (while (process-live-p process)
    (accept-process-output process 0.1))
  (accept-process-output nil 0.1))

(defmacro wamei/claude-complete-test--with-buffer (text &rest body)
  "TEXT を入れた一時バッファで BODY を評価する。
TEXT 中の `|' を取り除いてその位置に点を置く。`|' が無ければ末尾。"
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,text)
     (goto-char (point-min))
     (if (search-forward "|" nil t)
         (delete-char -1)
       (goto-char (point-max)))
     ,@body))

;;; 文脈抽出

(ert-deftest wamei/claude-complete-language-strips-ts-mode-and-mode-suffix ()
  (with-temp-buffer
    (setq major-mode 'typescript-ts-mode)
    (should (equal (wamei/claude-complete--language) "typescript"))
    (setq major-mode 'emacs-lisp-mode)
    (should (equal (wamei/claude-complete--language) "emacs-lisp"))
    (setq major-mode 'tsx-ts-mode)
    (should (equal (wamei/claude-complete--language) "tsx"))))

(ert-deftest wamei/claude-complete-path-falls-back-to-buffer-name ()
  (with-temp-buffer
    (rename-buffer "*scratch-x*" t)
    (should (equal (wamei/claude-complete--path) (buffer-name)))))

(ert-deftest wamei/claude-complete-path-uses-file-name-without-project ()
  (let ((dir (make-temp-file "claude-complete-" t)))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory (file-name-as-directory dir))
          (setq buffer-file-name (expand-file-name "sub/util.ts" dir))
          (should (equal (wamei/claude-complete--path) "util.ts")))
      (delete-directory dir t))))

(ert-deftest wamei/claude-complete-path-is-relative-to-project-root ()
  (let ((dir (make-temp-file "claude-complete-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (make-directory (expand-file-name "src" dir))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "src/util.ts" dir))
            (setq default-directory (expand-file-name "src/" dir))
            (should (equal (wamei/claude-complete--path) "src/util.ts"))))
      (delete-directory dir t))))

(ert-deftest wamei/claude-complete-context-splits-at-point ()
  (wamei/claude-complete-test--with-buffer "abc|def"
    (let ((context (wamei/claude-complete--context)))
      (should (equal (plist-get context :prefix) "abc"))
      (should (equal (plist-get context :suffix) "def"))
      (should (equal (plist-get context :language) "emacs-lisp"))
      (should (stringp (plist-get context :path))))))

(ert-deftest wamei/claude-complete-context-limits-prefix-and-suffix-length ()
  (wamei/claude-complete-test--with-buffer "0123456789|abcdefghij"
    (let* ((wamei/claude-complete-prefix-chars 4)
           (wamei/claude-complete-suffix-chars 3)
           (context (wamei/claude-complete--context)))
      (should (equal (plist-get context :prefix) "6789"))
      (should (equal (plist-get context :suffix) "abc")))))

;;; プロンプト

(ert-deftest wamei/claude-complete-prompt-wraps-file-and-marks-cursor ()
  (let ((prompt (wamei/claude-complete--prompt
                 '(:prefix "abc" :suffix "def" :path "src/a.ts" :language "typescript")
                 nil)))
    (should (equal prompt
                   "<file path=\"src/a.ts\" language=\"typescript\">\nabc<CURSOR>def\n</file>\n"))))

(ert-deftest wamei/claude-complete-prompt-lists-identifiers-when-given ()
  (let ((prompt (wamei/claude-complete--prompt
                 '(:prefix "" :suffix "" :path "a.ts" :language "typescript")
                 '("clamp" "Math"))))
    (should (string-suffix-p "</file>\n<identifiers>\nclamp, Math\n</identifiers>\n" prompt))))

(ert-deftest wamei/claude-complete-prompt-omits-identifiers-block-when-empty ()
  (let ((prompt (wamei/claude-complete--prompt
                 '(:prefix "" :suffix "" :path "a.ts" :language "typescript")
                 nil)))
    (should-not (string-match-p "<identifiers>" prompt))))

(ert-deftest wamei/claude-complete-system-prompt-forbids-fences-and-mentions-cursor ()
  (should (string-match-p "<CURSOR>" wamei/claude-complete-system-prompt))
  (should (string-match-p "fence" wamei/claude-complete-system-prompt))
  (should (string-match-p "<identifiers>" wamei/claude-complete-system-prompt)))

;;; 出力整形

(defun wamei/claude-complete-test--clean (text prefix suffix)
  (wamei/claude-complete--clean text (list :prefix prefix :suffix suffix
                                           :path "a.ts" :language "typescript")))

(ert-deftest wamei/claude-complete-clean-strips-code-fences ()
  (should (equal (wamei/claude-complete-test--clean "```ts\nreturn 1;\n```" "" "")
                 "return 1;"))
  (should (equal (wamei/claude-complete-test--clean "```\nreturn 1;\nfoo();\n```\n" "" "")
                 "return 1;\nfoo();")))

(ert-deftest wamei/claude-complete-clean-keeps-text-without-fences ()
  (should (equal (wamei/claude-complete-test--clean "return 1;" "" "") "return 1;")))

(ert-deftest wamei/claude-complete-clean-drops-repeated-line-head ()
  ;; 点の手前が「  return 」で、モデルが行全体を返してきた場合
  (should (equal (wamei/claude-complete-test--clean "  return Math.max(a, b);"
                                                    "function f() {\n  return " "\n}")
                 "Math.max(a, b);"))
  ;; インデント無しで行を繰り返した場合
  (should (equal (wamei/claude-complete-test--clean "return Math.max(a, b);"
                                                    "function f() {\n  return " "\n}")
                 "Math.max(a, b);")))

(ert-deftest wamei/claude-complete-clean-keeps-output-when-line-head-is-blank ()
  (should (equal (wamei/claude-complete-test--clean "  return 1;" "function f() {\n  " "\n}")
                 "return 1;"))
  (should (equal (wamei/claude-complete-test--clean "return 1;" "function f() {\n" "\n}")
                 "return 1;")))

(ert-deftest wamei/claude-complete-clean-drops-overlap-with-suffix ()
  ;; foo(<CURSOR>) で「a, b)」が返った場合、末尾の ) は既にある
  (should (equal (wamei/claude-complete-test--clean "a, b)" "foo(" ")") "a, b"))
  ;; 閉じ括弧の行が既にある場合
  (should (equal (wamei/claude-complete-test--clean "return x;\n}" "{\n  " "\n}")
                 "return x;")))

(ert-deftest wamei/claude-complete-clean-ignores-suffix-beyond-first-line ()
  (should (equal (wamei/claude-complete-test--clean "return x;" "{\n  " "\n}\nreturn x;")
                 "return x;")))

(ert-deftest wamei/claude-complete-clean-trims-trailing-whitespace ()
  (should (equal (wamei/claude-complete-test--clean "return 1;  \n\n" "" "") "return 1;")))

(ert-deftest wamei/claude-complete-clean-returns-nil-when-empty ()
  (should-not (wamei/claude-complete-test--clean "" "" ""))
  (should-not (wamei/claude-complete-test--clean "   \n" "" ""))
  (should-not (wamei/claude-complete-test--clean "```\n```" "" ""))
  (should-not (wamei/claude-complete-test--clean ")" "foo(" ")")))

(provide 'claude-complete-test)
;;; claude-complete-test.el ends here
