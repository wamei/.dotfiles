;;; opencode-cli-test.el --- tests for opencode-cli -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l opencode-cli-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(load (expand-file-name "opencode-cli.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defun wamei/opencode-cli-test--stub (script)
  "SCRIPT を本文とする実行可能なシェルスクリプトを作り、そのパスを返す。
opencode の代わりに `wamei/opencode-cli-program' へ設定して使う。"
  (let ((path (make-temp-file "opencode-stub-" nil ".sh"
                              (concat "#!/bin/sh\n" script "\n"))))
    (set-file-modes path #o755)
    path))

(defun wamei/opencode-cli-test--wait (process)
  "PROCESS が終了し sentinel が走り終わるまで待つ。"
  (while (process-live-p process)
    (accept-process-output process 0.1))
  (accept-process-output nil 0.1))

(defmacro wamei/opencode-cli-test--with-stub (script &rest body)
  "SCRIPT のスタブを `wamei/opencode-cli-program' にして BODY を評価する。"
  (declare (indent 1))
  `(let ((wamei/opencode-cli-program (wamei/opencode-cli-test--stub ,script)))
     (unwind-protect (progn ,@body)
       (delete-file wamei/opencode-cli-program))))

;;; コマンド構築

(ert-deftest wamei/opencode-cli-command-runs-without-plugins ()
  (let ((wamei/opencode-cli-program "opencode"))
    (should (equal (wamei/opencode-cli--command nil)
                   '("opencode" "run" "--pure")))))

(ert-deftest wamei/opencode-cli-command-omits-model-when-not-given ()
  "モデル無しなら opencode.jsonc の既定モデルに任せる。"
  (let ((wamei/opencode-cli-program "opencode"))
    (should-not (member "--model" (wamei/opencode-cli--command nil)))))

(ert-deftest wamei/opencode-cli-command-appends-model-when-given ()
  (let ((wamei/opencode-cli-program "opencode"))
    (should (equal (last (wamei/opencode-cli--command "local/qwen3.8:27b") 2)
                   '("--model" "local/qwen3.8:27b")))))

;;; 出力の整形

(ert-deftest wamei/opencode-cli-clean-strips-ansi-and-header ()
  (should (equal (wamei/opencode-cli--clean
                  "\033[0m\n> build · qwen3.8:27b\n\033[0m\nfix foo\n")
                 "fix foo")))

(ert-deftest wamei/opencode-cli-clean-keeps-quoted-lines-in-the-body ()
  "ヘッダ行として落とすのは先頭の 1 行だけ。本文の引用は残す。"
  (should (equal (wamei/opencode-cli--clean
                  "\033[0m\n> build · qwen3.8:27b\n\033[0m\n> quoted\nplain\n")
                 "> quoted\nplain")))

(ert-deftest wamei/opencode-cli-clean-leaves-plain-output-alone ()
  (should (equal (wamei/opencode-cli--clean "fix foo\n") "fix foo")))

;;; 非同期実行

(ert-deftest wamei/opencode-cli-run-feeds-input-on-stdin-and-cleans-output ()
  (wamei/opencode-cli-test--with-stub "printf '\\033[0m\\n> build · m\\n\\033[0m\\n'; cat"
    (let* ((result nil)
           (process (wamei/opencode-cli-run
                     nil "hello" (lambda (text) (setq result text)))))
      (wamei/opencode-cli-test--wait process)
      (should (equal result "hello")))))

(ert-deftest wamei/opencode-cli-run-prepends-system-prompt-to-the-input ()
  "opencode に --system-prompt は無いので、入力の先頭に置く。"
  (wamei/opencode-cli-test--with-stub "cat"
    (let* ((result nil)
           (process (wamei/opencode-cli-run
                     nil "the task" (lambda (text) (setq result text)) "be terse")))
      (wamei/opencode-cli-test--wait process)
      (should (equal result "be terse\n\nthe task")))))

(ert-deftest wamei/opencode-cli-run-passes-the-model-to-the-program ()
  (wamei/opencode-cli-test--with-stub "printf '%s\\n' \"$@\""
    (let* ((result nil)
           (process (wamei/opencode-cli-run
                     "local/qwen3.8:27b" "" (lambda (text) (setq result text)))))
      (wamei/opencode-cli-test--wait process)
      (should (equal result "run\n--pure\n--model\nlocal/qwen3.8:27b")))))

;;; 汎用コマンド

(ert-deftest wamei/opencode-is-a-command ()
  (should (commandp 'wamei/opencode)))

;;; opencode-cli-test.el ends here
