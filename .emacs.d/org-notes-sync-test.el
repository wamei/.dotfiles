;;; org-notes-sync-test.el --- tests for org-notes-sync -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l org-notes-sync-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "org-notes-sync.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defconst org-notes-sync-test--print-running
  "gui/501/jp.wamei.org-notes-sync = {
\tactive count = 1
\tstate = running

\tprogram = /Users/u/.local/bin/org-notes-sync
\truns = 4
\tpid = 123
\tlast exit code = 0

\tendpoint = {
\t\tstate = active
\t}
}
")

(defconst org-notes-sync-test--print-exited
  "gui/501/jp.wamei.org-notes-sync = {
\tactive count = 0
\tstate = not running

\truns = 4
\tlast exit code = 2

\tendpoint = {
\t\tstate = active
\t}
}
")

(defmacro org-notes-sync-test--with-env (responses &rest body)
  "状態ディレクトリを一時ディレクトリにし、launchctl を RESPONSES で差し替えて BODY を評価する。
RESPONSES は (サブコマンド . (status . output)) の alist。呼ばれた引数は
`calls' に新しい順で溜まり、`message' の出力は `messages' に溜まる。"
  (declare (indent 1))
  `(let* ((dir (make-temp-file "org-notes-sync-test" t))
          (wamei/org-notes-sync-state-directory dir)
          (wamei/org-notes-sync--watch nil)
          (calls nil)
          (messages nil)
          (responses ,responses))
     (cl-letf (((symbol-function 'wamei/org-notes-sync--launchctl)
                (lambda (&rest args)
                  (push args calls)
                  (or (cdr (assoc (car args) responses)) '(0 . ""))))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) messages))))
       (unwind-protect (progn ,@body)
         (wamei/org-notes-sync--stop-watch)
         (delete-directory dir t)))))

(defun org-notes-sync-test--append (file text)
  "FILE に TEXT を追記する。"
  (write-region text nil file t 'silent))

;;; launchctl print の読み取り

(ert-deftest org-notes-sync-test-parse-print-running ()
  "最上位の state だけを読む (入れ子の endpoint の state は無視する)。"
  (let ((r (wamei/org-notes-sync--parse-print org-notes-sync-test--print-running)))
    (should (equal (plist-get r :state) "running"))
    (should (equal (plist-get r :exit-code) 0))))

(ert-deftest org-notes-sync-test-parse-print-exited ()
  (let ((r (wamei/org-notes-sync--parse-print org-notes-sync-test--print-exited)))
    (should (equal (plist-get r :state) "not running"))
    (should (equal (plist-get r :exit-code) 2))))

(ert-deftest org-notes-sync-test-parse-print-never-exited ()
  "まだ 1 回も終わっていなければ終了コードは nil。"
  (let ((r (wamei/org-notes-sync--parse-print
            "x = {\n\tstate = running\n\tlast exit code = (never exited)\n}\n")))
    (should (equal (plist-get r :state) "running"))
    (should (null (plist-get r :exit-code)))))

;;; stderr の警告行

(ert-deftest org-notes-sync-test-new-warnings-reads-only-appended ()
  "OFFSET より後に足された警告行だけを返す。警告でない行は捨てる。"
  (let ((file (make-temp-file "org-notes-sync-err")))
    (unwind-protect
        (progn
          (org-notes-sync-test--append file "org-notes-sync: 古い警告\n")
          (let ((offset (wamei/org-notes-sync--file-size file)))
            (org-notes-sync-test--append
             file "bun のゴミ\norg-notes-sync: 新しい警告\n")
            (should (equal (wamei/org-notes-sync--new-warnings file offset)
                           '("org-notes-sync: 新しい警告")))))
      (delete-file file))))

(ert-deftest org-notes-sync-test-new-warnings-after-truncation ()
  "ファイルが OFFSET より短くなっていたら (切り詰められた) 頭から読む。"
  (let ((file (make-temp-file "org-notes-sync-err")))
    (unwind-protect
        (progn
          (org-notes-sync-test--append file "org-notes-sync: 切り詰め後\n")
          (should (equal (wamei/org-notes-sync--new-warnings file 100000)
                         '("org-notes-sync: 切り詰め後"))))
      (delete-file file))))

(ert-deftest org-notes-sync-test-new-warnings-missing-file ()
  (should (null (wamei/org-notes-sync--new-warnings "/nonexistent/launchd.err" 0))))

;;; 起動

(ert-deftest org-notes-sync-test-kickstarts-launchd-job-after-pending ()
  "pending を置いてから launchd のジョブを kickstart する (バイナリを直接動かさない)。"
  (org-notes-sync-test--with-env nil
    (org-notes-sync)
    (should (file-exists-p (expand-file-name "pending" dir)))
    (should (equal (car (last calls))
                   (list "kickstart"
                         (format "gui/%d/jp.wamei.org-notes-sync" (user-uid)))))
    (should wamei/org-notes-sync--watch)))

(ert-deftest org-notes-sync-test-kickstart-failure-tells-how-to-enable ()
  "ジョブが読み込まれていなければ、読み込み方を知らせて見張らない。"
  (org-notes-sync-test--with-env
      '(("kickstart" . (113 . "Could not find service")))
    (org-notes-sync)
    (should (null wamei/org-notes-sync--watch))
    (should (cl-some (lambda (m) (string-match-p "org-notes-sync:enable" m))
                     messages))))

(ert-deftest org-notes-sync-test-second-call-keeps-watch ()
  "見張っている間に呼ばれても、見張りの起点 (stderr の位置) を作り直さない。"
  (org-notes-sync-test--with-env nil
    (let ((err (expand-file-name "launchd.err" dir)))
      (org-notes-sync)
      (let ((watch wamei/org-notes-sync--watch))
        (org-notes-sync-test--append err "org-notes-sync: 1 周目の警告\n")
        (org-notes-sync)
        (should (eq wamei/org-notes-sync--watch watch))))))

;;; 終了の見張り

(ert-deftest org-notes-sync-test-poll-keeps-watching-while-running ()
  (org-notes-sync-test--with-env
      `(("print" . (0 . ,org-notes-sync-test--print-running)))
    (org-notes-sync)
    (wamei/org-notes-sync--poll)
    (should wamei/org-notes-sync--watch)
    (should (null messages))))

(ert-deftest org-notes-sync-test-poll-reports-warnings-when-done ()
  "終わったら、起動後に stderr へ足された警告行を出して見張りをやめる。"
  (org-notes-sync-test--with-env
      `(("print" . (0 . ,org-notes-sync-test--print-exited)))
    (let ((err (expand-file-name "launchd.err" dir)))
      (org-notes-sync-test--append err "org-notes-sync: 前回の警告\n")
      (org-notes-sync)
      (org-notes-sync-test--append err "org-notes-sync: メモの DB を読めません\n")
      (wamei/org-notes-sync--poll)
      (should (null wamei/org-notes-sync--watch))
      (should (equal messages '("org-notes-sync: メモの DB を読めません"))))))

(ert-deftest org-notes-sync-test-poll-reports-silent-failure ()
  "警告行が無いのに失敗で終わったら、終了コードと stderr の場所を知らせる。"
  (org-notes-sync-test--with-env
      `(("print" . (0 . ,org-notes-sync-test--print-exited)))
    (org-notes-sync)
    (wamei/org-notes-sync--poll)
    (should (null wamei/org-notes-sync--watch))
    (should (= (length messages) 1))
    (should (string-match-p "exit 2" (car messages)))
    (should (string-match-p "launchd\\.err" (car messages)))))

(ert-deftest org-notes-sync-test-poll-quiet-on-success ()
  (org-notes-sync-test--with-env
      `(("print" . (0 . ,(replace-regexp-in-string
                          "last exit code = 2" "last exit code = 0"
                          org-notes-sync-test--print-exited))))
    (org-notes-sync)
    (wamei/org-notes-sync--poll)
    (should (null wamei/org-notes-sync--watch))
    (should (null messages))))

(ert-deftest org-notes-sync-test-poll-gives-up-after-deadline ()
  "期限を過ぎても終わらなければ、知らせて見張りをやめる。"
  (org-notes-sync-test--with-env
      `(("print" . (0 . ,org-notes-sync-test--print-running)))
    (org-notes-sync)
    (plist-put wamei/org-notes-sync--watch :deadline (- (float-time) 1))
    (wamei/org-notes-sync--poll)
    (should (null wamei/org-notes-sync--watch))
    (should (= (length messages) 1))))

(provide 'org-notes-sync-test)
;;; org-notes-sync-test.el ends here
