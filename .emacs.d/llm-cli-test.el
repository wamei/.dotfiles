;;; llm-cli-test.el --- tests for llm-cli -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l llm-cli-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(load (expand-file-name "llm-cli.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defun wamei/llm-cli-test--stub (script)
  "SCRIPT を本文とする実行可能なシェルスクリプトを作り、そのパスを返す。
CLI の代わりにコマンドの先頭へ置いて使う。"
  (let ((path (make-temp-file "llm-stub-" nil ".sh"
                              (concat "#!/bin/sh\n" script "\n"))))
    (set-file-modes path #o755)
    path))

(defun wamei/llm-cli-test--wait (process)
  "PROCESS が終了し sentinel が走り終わるまで待つ。"
  (while (process-live-p process)
    (accept-process-output process 0.1))
  ;; sentinel は終了後に配送されるので、もう一度だけ回す。
  (accept-process-output nil 0.1))

(defmacro wamei/llm-cli-test--with-stub (script command &rest body)
  "SCRIPT のスタブを COMMAND という変数に束縛して BODY を評価する。
COMMAND にはスタブ 1 つだけからなるコマンドリストが入る。"
  (declare (indent 2))
  `(let* ((stub (wamei/llm-cli-test--stub ,script))
          (,command (list stub)))
     (unwind-protect (progn ,@body)
       (delete-file stub))))

(defmacro wamei/llm-cli-test--capturing-messages (messages &rest body)
  "BODY 中の `message' 呼び出しを MESSAGES という変数のリストに集める。"
  (declare (indent 1))
  `(let ((,messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) ,messages)))))
       ,@body)
     ,messages))

;;; 非同期実行

(ert-deftest wamei/llm-cli-run-process-feeds-input-on-stdin-and-returns-stdout ()
  (wamei/llm-cli-test--with-stub "cat" command
    (let* ((result nil)
           (process (wamei/llm-cli-run-process
                     command "hello\nworld" (lambda (text) (setq result text)))))
      (wamei/llm-cli-test--wait process)
      (should (equal result "hello\nworld")))))

(ert-deftest wamei/llm-cli-run-process-trims-trailing-newline ()
  (wamei/llm-cli-test--with-stub "echo answer" command
    (let* ((result nil)
           (process (wamei/llm-cli-run-process
                     command "" (lambda (text) (setq result text)))))
      (wamei/llm-cli-test--wait process)
      (should (equal result "answer")))))

(ert-deftest wamei/llm-cli-run-process-passes-arguments-to-the-program ()
  (wamei/llm-cli-test--with-stub "printf '%s\\n' \"$@\"" command
    (let* ((result nil)
           (process (wamei/llm-cli-run-process
                     (append command '("run" "--pure")) ""
                     (lambda (text) (setq result text)))))
      (wamei/llm-cli-test--wait process)
      (should (equal result "run\n--pure")))))

(ert-deftest wamei/llm-cli-run-process-uses-given-environment ()
  (wamei/llm-cli-test--with-stub "printf '%s' \"$WAMEI_LLM_TEST\"" command
    (let* ((result nil)
           (process (wamei/llm-cli-run-process
                     command "" (lambda (text) (setq result text))
                     (cons "WAMEI_LLM_TEST=42" process-environment))))
      (wamei/llm-cli-test--wait process)
      (should (equal result "42")))))

(ert-deftest wamei/llm-cli-run-process-applies-filter-to-output ()
  (wamei/llm-cli-test--with-stub "echo noisy answer" command
    (let* ((result nil)
           (process (wamei/llm-cli-run-process
                     command "" (lambda (text) (setq result text)) nil
                     (lambda (output) (string-remove-prefix "noisy " output)))))
      (wamei/llm-cli-test--wait process)
      (should (equal result "answer")))))

(ert-deftest wamei/llm-cli-run-process-reports-failure-with-label ()
  (wamei/llm-cli-test--with-stub "exit 3" command
    (let ((messages (wamei/llm-cli-test--capturing-messages messages
                      (wamei/llm-cli-test--wait
                       (wamei/llm-cli-run-process command "" #'ignore nil nil "opencode")))))
      (should (cl-some (lambda (m) (string-match-p "opencode.*failed" m)) messages)))))

(ert-deftest wamei/llm-cli-run-process-stays-silent-when-cancelled ()
  (wamei/llm-cli-test--with-stub "sleep 5" command
    (let ((messages (wamei/llm-cli-test--capturing-messages messages
                      (let ((process (wamei/llm-cli-run-process command "" #'ignore)))
                        (process-put process 'wamei/llm-cli-cancelled t)
                        (delete-process process)
                        (wamei/llm-cli-test--wait process)))))
      (should-not messages))))

(ert-deftest wamei/llm-cli-run-process-skips-callback-on-failure ()
  (wamei/llm-cli-test--with-stub "echo boom >&2; exit 1" command
    (let* ((called nil)
           (messages (wamei/llm-cli-test--capturing-messages messages
                       (wamei/llm-cli-test--wait
                        (wamei/llm-cli-run-process
                         command "" (lambda (_) (setq called t)))))))
      (should-not called)
      (should (cl-some (lambda (m) (string-match-p "boom" m)) messages)))))

;;; モデルの解決

(ert-deftest wamei/llm-cli-models-offer-claude-and-opencode ()
  (should (equal (mapcar #'car wamei/llm-cli-models)
                 '("haiku" "sonnet" "opus" "opencode"))))

(ert-deftest wamei/llm-cli-resolve-gives-runner-and-model ()
  (should (equal (wamei/llm-cli--resolve "sonnet")
                 (cons 'wamei/claude-cli-run "sonnet")))
  (should (equal (wamei/llm-cli--resolve "opencode")
                 (cons 'wamei/opencode-cli-run nil))))

(ert-deftest wamei/llm-cli-resolve-falls-back-to-the-default-runner ()
  "一覧に無い名前は既定の backend にモデル名としてそのまま渡す。"
  (should (equal (wamei/llm-cli--resolve "sonnet[1m]")
                 (cons wamei/llm-cli-default-runner "sonnet[1m]"))))

;;; 汎用コマンド

(ert-deftest wamei/llm-cli-show-result-puts-text-in-the-result-buffer ()
  (when (get-buffer wamei/llm-cli-result-buffer)
    (kill-buffer wamei/llm-cli-result-buffer))
  (wamei/llm-cli--show-result "haiku" "some answer")
  (with-current-buffer wamei/llm-cli-result-buffer
    (should (string-match-p "## haiku" (buffer-string)))
    (should (string-match-p "some answer" (buffer-string)))))

;;; コミットメッセージ

(ert-deftest wamei/llm-commit-message-is-a-command ()
  (should (commandp 'wamei/llm-commit-message)))

(ert-deftest wamei/llm-commit-message-prompt-asks-for-commit-tags ()
  (should (string-match-p "<commit>" (wamei/llm-commit-message--prompt "d" "l"))))

(ert-deftest wamei/llm-commit-message-extract-takes-tagged-part-only ()
  (should (equal (wamei/llm-commit-message--extract
                  "I'll write a message.\n<commit>\nfix foo\n\nbody line\n</commit>\nDone.")
                 "fix foo\n\nbody line")))

(ert-deftest wamei/llm-commit-message-extract-falls-back-to-whole-output ()
  (should (equal (wamei/llm-commit-message--extract "fix foo\n") "fix foo")))

(ert-deftest wamei/llm-commit-message-extract-strips-code-fences ()
  (should (equal (wamei/llm-commit-message--extract "```\nfix foo\n```") "fix foo")))

(ert-deftest wamei/llm-commit-message-prompt-includes-diff-and-log ()
  (let ((prompt (wamei/llm-commit-message--prompt
                 "+added line" "9fedd0b 表記崩れを修正")))
    (should (string-match-p "\\+added line" prompt))
    (should (string-match-p "表記崩れを修正" prompt))))

(ert-deftest wamei/llm-commit-message-first-line-empty-in-fresh-buffer ()
  (with-temp-buffer
    (insert "\n\n# Please enter the commit message.\n")
    (should (wamei/llm-commit-message--first-line-empty-p))))

(ert-deftest wamei/llm-commit-message-first-line-empty-in-empty-buffer ()
  (with-temp-buffer
    (should (wamei/llm-commit-message--first-line-empty-p))))

(ert-deftest wamei/llm-commit-message-first-line-empty-ignores-later-lines ()
  (with-temp-buffer
    (insert "\nnotes below\n\n# Please enter the commit message.\n")
    (should (wamei/llm-commit-message--first-line-empty-p))))

(ert-deftest wamei/llm-commit-message-first-line-not-empty-when-summary-present ()
  (with-temp-buffer
    (insert "fix foo\n\n# Please enter the commit message.\n")
    (should-not (wamei/llm-commit-message--first-line-empty-p))))

(ert-deftest wamei/llm-commit-message-first-line-not-empty-when-only-spaces-then-text ()
  (with-temp-buffer
    (insert "   \nfix foo\n# c\n")
    (should (wamei/llm-commit-message--first-line-empty-p))))

(ert-deftest wamei/llm-commit-message-insert-keeps-text-below-empty-first-line ()
  (with-temp-buffer
    (insert "\nnotes below\n\n# Please enter the commit message.\n")
    (wamei/llm-commit-message--insert "new message" "#")
    (should (equal (buffer-string)
                   "new message\n\nnotes below\n\n# Please enter the commit message.\n"))))

(ert-deftest wamei/llm-commit-message-insert-keeps-existing-comment-gap ()
  (with-temp-buffer
    (insert "\n\n# Please enter the commit message.\n")
    (wamei/llm-commit-message--insert "new message" "#")
    (should (equal (buffer-string)
                   "new message\n\n# Please enter the commit message.\n"))))

(ert-deftest wamei/llm-commit-message-insert-into-empty-buffer ()
  (with-temp-buffer
    (insert "\n# Please enter the commit message.\n")
    (wamei/llm-commit-message--insert "new message" "#")
    (should (equal (buffer-string)
                   "new message\n\n# Please enter the commit message.\n"))
    (should (= (point) (point-min)))))

(ert-deftest wamei/llm-commit-message-insert-replaces-existing-text ()
  (with-temp-buffer
    (insert "old summary\n\nold body\n\n# Please enter the commit message.\n")
    (wamei/llm-commit-message--insert "new message" "#")
    (should (equal (buffer-string)
                   "new message\n\n# Please enter the commit message.\n"))))

(ert-deftest wamei/llm-commit-message-insert-without-comment-block ()
  (with-temp-buffer
    (wamei/llm-commit-message--insert "new message" "#")
    (should (equal (buffer-string) "new message\n"))))

(ert-deftest wamei/llm-commit-message-prompt-truncates-long-diff ()
  (let* ((wamei/llm-commit-message-max-diff-chars 20)
         (prompt (wamei/llm-commit-message--prompt
                  (make-string 100 ?x) "log entry")))
    (should-not (string-match-p (make-string 21 ?x) prompt))
    (should (string-match-p (make-string 20 ?x) prompt))
    (should (string-match-p "truncated" prompt))))

;;; llm-cli-test.el ends here
