;;; claude-cli-test.el --- tests for claude-cli -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l claude-cli-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(load (expand-file-name "claude-cli.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defun wamei/claude-cli-test--stub (script)
  "SCRIPT を本文とする実行可能なシェルスクリプトを作り、そのパスを返す。
claude の代わりに `wamei/claude-cli-program' へ設定して使う。"
  (let ((path (make-temp-file "claude-stub-" nil ".sh"
                              (concat "#!/bin/sh\n" script "\n"))))
    (set-file-modes path #o755)
    path))

(defun wamei/claude-cli-test--wait (process)
  "PROCESS が終了し sentinel が走り終わるまで待つ。"
  (while (process-live-p process)
    (accept-process-output process 0.1))
  ;; sentinel は終了後に配送されるので、もう一度だけ回す。
  (accept-process-output nil 0.1))

(defmacro wamei/claude-cli-test--with-stub (script &rest body)
  "SCRIPT のスタブを `wamei/claude-cli-program' にして BODY を評価する。"
  (declare (indent 1))
  `(let ((wamei/claude-cli-program (wamei/claude-cli-test--stub ,script)))
     (unwind-protect (progn ,@body)
       (delete-file wamei/claude-cli-program))))

;;; コマンド構築

(ert-deftest wamei/claude-cli-command-uses-print-mode-without-tools ()
  (let ((wamei/claude-cli-program "claude"))
    (should (equal (wamei/claude-cli--command "haiku")
                   '("claude" "-p" "--model" "haiku"
                     "--output-format" "text"
                     "--tools" ""
                     "--no-session-persistence"
                     "--strict-mcp-config"
                     "--setting-sources" "")))))

(ert-deftest wamei/claude-cli-command-appends-system-prompt-when-given ()
  (let ((wamei/claude-cli-program "claude"))
    (should (equal (last (wamei/claude-cli--command "haiku" "be terse") 2)
                   '("--system-prompt" "be terse")))
    (should-not (member "--system-prompt" (wamei/claude-cli--command "haiku")))))

;;; 非同期実行

(ert-deftest wamei/claude-cli-run-passes-system-prompt-to-process ()
  (wamei/claude-cli-test--with-stub "printf '%s\\n' \"$@\""
    (let* ((result nil)
           (process (wamei/claude-cli-run "haiku" ""
                                          (lambda (text) (setq result text))
                                          "be terse")))
      (wamei/claude-cli-test--wait process)
      (should (string-match-p "^--system-prompt\nbe terse$" result)))))

(ert-deftest wamei/claude-cli-run-disables-extended-thinking ()
  (wamei/claude-cli-test--with-stub "printf '%s' \"$MAX_THINKING_TOKENS\""
    (let* ((result nil)
           (process (wamei/claude-cli-run "haiku" ""
                                          (lambda (text) (setq result text)))))
      (wamei/claude-cli-test--wait process)
      (should (equal result "0")))))

(ert-deftest wamei/claude-cli-run-feeds-input-on-stdin-and-returns-stdout ()
  (wamei/claude-cli-test--with-stub "cat"
    (let* ((result nil)
           (process (wamei/claude-cli-run "haiku" "hello\nworld"
                                          (lambda (text) (setq result text)))))
      (wamei/claude-cli-test--wait process)
      (should (equal result "hello\nworld")))))

(ert-deftest wamei/claude-cli-run-trims-trailing-newline ()
  (wamei/claude-cli-test--with-stub "echo answer"
    (let* ((result nil)
           (process (wamei/claude-cli-run "haiku" ""
                                          (lambda (text) (setq result text)))))
      (wamei/claude-cli-test--wait process)
      (should (equal result "answer")))))

(ert-deftest wamei/claude-cli-run-skips-callback-on-failure ()
  (wamei/claude-cli-test--with-stub "echo boom >&2; exit 1"
    (let* ((called nil)
           (messages nil)
           (process
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (let ((p (wamei/claude-cli-run "haiku" ""
                                             (lambda (_) (setq called t)))))
                (wamei/claude-cli-test--wait p)
                p))))
      (ignore process)
      (should-not called)
      (should (cl-some (lambda (m) (string-match-p "boom" m)) messages)))))

;;; 汎用コマンド

(ert-deftest wamei/claude-cli-defines-a-command-per-model ()
  (dolist (model wamei/claude-cli-models)
    (should (commandp (intern (format "wamei/claude-%s" model))))))

(ert-deftest wamei/claude-cli-show-result-puts-text-in-claude-buffer ()
  (when (get-buffer "*claude*") (kill-buffer "*claude*"))
  (wamei/claude-cli--show-result "haiku" "some answer")
  (with-current-buffer "*claude*"
    (should (string-match-p "some answer" (buffer-string)))))

;;; コミットメッセージ

(ert-deftest wamei/claude-commit-message-prompt-asks-for-commit-tags ()
  (should (string-match-p "<commit>" (wamei/claude-commit-message--prompt "d" "l"))))

(ert-deftest wamei/claude-commit-message-extract-takes-tagged-part-only ()
  (should (equal (wamei/claude-commit-message--extract
                  "I'll write a message.\n<commit>\nfix foo\n\nbody line\n</commit>\nDone.")
                 "fix foo\n\nbody line")))

(ert-deftest wamei/claude-commit-message-extract-falls-back-to-whole-output ()
  (should (equal (wamei/claude-commit-message--extract "fix foo\n") "fix foo")))

(ert-deftest wamei/claude-commit-message-extract-strips-code-fences ()
  (should (equal (wamei/claude-commit-message--extract "```\nfix foo\n```") "fix foo")))

(ert-deftest wamei/claude-commit-message-prompt-includes-diff-and-log ()
  (let ((prompt (wamei/claude-commit-message--prompt
                 "+added line" "9fedd0b 表記崩れを修正")))
    (should (string-match-p "\\+added line" prompt))
    (should (string-match-p "表記崩れを修正" prompt))))

(ert-deftest wamei/claude-commit-message-first-line-empty-in-fresh-buffer ()
  (with-temp-buffer
    (insert "\n\n# Please enter the commit message.\n")
    (should (wamei/claude-commit-message--first-line-empty-p))))

(ert-deftest wamei/claude-commit-message-first-line-empty-in-empty-buffer ()
  (with-temp-buffer
    (should (wamei/claude-commit-message--first-line-empty-p))))

(ert-deftest wamei/claude-commit-message-first-line-empty-ignores-later-lines ()
  (with-temp-buffer
    (insert "\nnotes below\n\n# Please enter the commit message.\n")
    (should (wamei/claude-commit-message--first-line-empty-p))))

(ert-deftest wamei/claude-commit-message-first-line-not-empty-when-summary-present ()
  (with-temp-buffer
    (insert "fix foo\n\n# Please enter the commit message.\n")
    (should-not (wamei/claude-commit-message--first-line-empty-p))))

(ert-deftest wamei/claude-commit-message-first-line-not-empty-when-only-spaces-then-text ()
  (with-temp-buffer
    (insert "   \nfix foo\n# c\n")
    (should (wamei/claude-commit-message--first-line-empty-p))))

(ert-deftest wamei/claude-commit-message-insert-keeps-text-below-empty-first-line ()
  (with-temp-buffer
    (insert "\nnotes below\n\n# Please enter the commit message.\n")
    (wamei/claude-commit-message--insert "new message" "#")
    (should (equal (buffer-string)
                   "new message\n\nnotes below\n\n# Please enter the commit message.\n"))))

(ert-deftest wamei/claude-commit-message-insert-keeps-existing-comment-gap ()
  (with-temp-buffer
    (insert "\n\n# Please enter the commit message.\n")
    (wamei/claude-commit-message--insert "new message" "#")
    (should (equal (buffer-string)
                   "new message\n\n# Please enter the commit message.\n"))))

(ert-deftest wamei/claude-commit-message-insert-into-empty-buffer ()
  (with-temp-buffer
    (insert "\n# Please enter the commit message.\n")
    (wamei/claude-commit-message--insert "new message" "#")
    (should (equal (buffer-string)
                   "new message\n\n# Please enter the commit message.\n"))
    (should (= (point) (point-min)))))

(ert-deftest wamei/claude-commit-message-insert-replaces-existing-text ()
  (with-temp-buffer
    (insert "old summary\n\nold body\n\n# Please enter the commit message.\n")
    (wamei/claude-commit-message--insert "new message" "#")
    (should (equal (buffer-string)
                   "new message\n\n# Please enter the commit message.\n"))))

(ert-deftest wamei/claude-commit-message-insert-without-comment-block ()
  (with-temp-buffer
    (wamei/claude-commit-message--insert "new message" "#")
    (should (equal (buffer-string) "new message\n"))))

(ert-deftest wamei/claude-commit-message-prompt-truncates-long-diff ()
  (let* ((wamei/claude-commit-message-max-diff-chars 20)
         (prompt (wamei/claude-commit-message--prompt
                  (make-string 100 ?x) "log entry")))
    (should-not (string-match-p (make-string 21 ?x) prompt))
    (should (string-match-p (make-string 20 ?x) prompt))
    (should (string-match-p "truncated" prompt))))

;;; claude-cli-test.el ends here
