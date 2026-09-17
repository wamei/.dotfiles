;;; claude-cli-test.el --- tests for claude-cli -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l claude-cli-test.el -f ert-run-tests-batch-and-exit
;; 共通部分 (プロセス配管・コミットメッセージ) の試験は llm-cli-test.el にある。
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

(ert-deftest wamei/claude-cli-run-reports-failure-with-the-model-in-the-label ()
  (wamei/claude-cli-test--with-stub "exit 3"
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) messages)))))
        (wamei/claude-cli-test--wait (wamei/claude-cli-run "haiku" "" #'ignore)))
      (should (cl-some (lambda (m) (string-match-p "claude (haiku) failed" m)) messages)))))

;;; 汎用コマンド

(ert-deftest wamei/claude-cli-defines-a-command-per-model ()
  (dolist (model wamei/claude-cli-models)
    (should (commandp (intern (format "wamei/claude-%s" model))))))

;;; claude-cli-test.el ends here
