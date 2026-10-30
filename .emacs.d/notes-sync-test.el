;;; notes-sync-test.el --- tests for notes-sync -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l notes-sync-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(package-initialize)
(require 'markdown-mode)
(load (expand-file-name "notes-sync.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defconst notes-sync-test--print-running
  "gui/501/jp.wamei.notes-sync = {
\tactive count = 1
\tstate = running

\tprogram = /Users/u/.local/bin/notes-sync
\truns = 4
\tpid = 123
\tlast exit code = 0

\tendpoint = {
\t\tstate = active
\t}
}
")

(defconst notes-sync-test--print-exited
  "gui/501/jp.wamei.notes-sync = {
\tactive count = 0
\tstate = not running

\truns = 4
\tlast exit code = 2

\tendpoint = {
\t\tstate = active
\t}
}
")

(defmacro notes-sync-test--with-env (responses &rest body)
  "状態ディレクトリを一時ディレクトリにし、launchctl を RESPONSES で差し替えて BODY を評価する。
RESPONSES は (サブコマンド . (status . output)) の alist。呼ばれた引数は
`calls' に新しい順で溜まり、`message' の出力は `messages' に溜まる。"
  (declare (indent 1))
  `(let* ((dir (make-temp-file "notes-sync-test" t))
          (wamei/notes-sync-state-directory dir)
          (wamei/notes-sync--watch nil)
          (calls nil)
          (messages nil)
          (responses ,responses))
     (cl-letf (((symbol-function 'wamei/notes-sync--launchctl)
                (lambda (&rest args)
                  (push args calls)
                  (or (cdr (assoc (car args) responses)) '(0 . ""))))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) messages))))
       (unwind-protect (progn ,@body)
         (wamei/notes-sync--stop-watch)
         (delete-directory dir t)))))

(defun notes-sync-test--append (file text)
  "FILE に TEXT を追記する。"
  (write-region text nil file t 'silent))

;;; launchctl print の読み取り

(ert-deftest notes-sync-test-parse-print-running ()
  "最上位の state だけを読む (入れ子の endpoint の state は無視する)。"
  (let ((r (wamei/notes-sync--parse-print notes-sync-test--print-running)))
    (should (equal (plist-get r :state) "running"))
    (should (equal (plist-get r :exit-code) 0))))

(ert-deftest notes-sync-test-parse-print-exited ()
  (let ((r (wamei/notes-sync--parse-print notes-sync-test--print-exited)))
    (should (equal (plist-get r :state) "not running"))
    (should (equal (plist-get r :exit-code) 2))))

(ert-deftest notes-sync-test-parse-print-never-exited ()
  "まだ 1 回も終わっていなければ終了コードは nil。"
  (let ((r (wamei/notes-sync--parse-print
            "x = {\n\tstate = running\n\tlast exit code = (never exited)\n}\n")))
    (should (equal (plist-get r :state) "running"))
    (should (null (plist-get r :exit-code)))))

;;; stderr の警告行

(ert-deftest notes-sync-test-new-warnings-reads-only-appended ()
  "OFFSET より後に足された警告行だけを返す。警告でない行は捨てる。"
  (let ((file (make-temp-file "notes-sync-err")))
    (unwind-protect
        (progn
          (notes-sync-test--append file "notes-sync: 古い警告\n")
          (let ((offset (wamei/notes-sync--file-size file)))
            (notes-sync-test--append
             file "bun のゴミ\nnotes-sync: 新しい警告\n")
            (should (equal (wamei/notes-sync--new-warnings file offset)
                           '("notes-sync: 新しい警告")))))
      (delete-file file))))

(ert-deftest notes-sync-test-new-warnings-after-truncation ()
  "ファイルが OFFSET より短くなっていたら (切り詰められた) 頭から読む。"
  (let ((file (make-temp-file "notes-sync-err")))
    (unwind-protect
        (progn
          (notes-sync-test--append file "notes-sync: 切り詰め後\n")
          (should (equal (wamei/notes-sync--new-warnings file 100000)
                         '("notes-sync: 切り詰め後"))))
      (delete-file file))))

(ert-deftest notes-sync-test-new-warnings-missing-file ()
  (should (null (wamei/notes-sync--new-warnings "/nonexistent/launchd.err" 0))))

;;; 起動

(ert-deftest notes-sync-test-kickstarts-launchd-job-after-pending ()
  "pending を置いてから launchd のジョブを kickstart する (バイナリを直接動かさない)。"
  (notes-sync-test--with-env nil
    (notes-sync)
    (should (file-exists-p (expand-file-name "pending" dir)))
    (should (equal (car (last calls))
                   (list "kickstart"
                         (format "gui/%d/jp.wamei.notes-sync" (user-uid)))))
    (should wamei/notes-sync--watch)))

(ert-deftest notes-sync-test-kickstart-failure-tells-how-to-enable ()
  "ジョブが読み込まれていなければ、読み込み方を知らせて見張らない。"
  (notes-sync-test--with-env
      '(("kickstart" . (113 . "Could not find service")))
    (notes-sync)
    (should (null wamei/notes-sync--watch))
    (should (cl-some (lambda (m) (string-match-p "notes-sync:enable" m))
                     messages))))

(ert-deftest notes-sync-test-second-call-keeps-watch ()
  "見張っている間に呼ばれても、見張りの起点 (stderr の位置) を作り直さない。"
  (notes-sync-test--with-env nil
    (let ((err (expand-file-name "launchd.err" dir)))
      (notes-sync)
      (let ((watch wamei/notes-sync--watch))
        (notes-sync-test--append err "notes-sync: 1 周目の警告\n")
        (notes-sync)
        (should (eq wamei/notes-sync--watch watch))))))

;;; 終了の見張り

(ert-deftest notes-sync-test-poll-keeps-watching-while-running ()
  (notes-sync-test--with-env
      `(("print" . (0 . ,notes-sync-test--print-running)))
    (notes-sync)
    (wamei/notes-sync--poll)
    (should wamei/notes-sync--watch)
    (should (null messages))))

(ert-deftest notes-sync-test-poll-reports-warnings-when-done ()
  "終わったら、起動後に stderr へ足された警告行を出して見張りをやめる。"
  (notes-sync-test--with-env
      `(("print" . (0 . ,notes-sync-test--print-exited)))
    (let ((err (expand-file-name "launchd.err" dir)))
      (notes-sync-test--append err "notes-sync: 前回の警告\n")
      (notes-sync)
      (notes-sync-test--append err "notes-sync: メモの DB を読めません\n")
      (wamei/notes-sync--poll)
      (should (null wamei/notes-sync--watch))
      (should (equal messages '("notes-sync: メモの DB を読めません"))))))

(ert-deftest notes-sync-test-poll-reports-silent-failure ()
  "警告行が無いのに失敗で終わったら、終了コードと stderr の場所を知らせる。"
  (notes-sync-test--with-env
      `(("print" . (0 . ,notes-sync-test--print-exited)))
    (notes-sync)
    (wamei/notes-sync--poll)
    (should (null wamei/notes-sync--watch))
    (should (= (length messages) 1))
    (should (string-match-p "exit 2" (car messages)))
    (should (string-match-p "launchd\\.err" (car messages)))))

(ert-deftest notes-sync-test-poll-quiet-on-success ()
  (notes-sync-test--with-env
      `(("print" . (0 . ,(replace-regexp-in-string
                          "last exit code = 2" "last exit code = 0"
                          notes-sync-test--print-exited))))
    (notes-sync)
    (wamei/notes-sync--poll)
    (should (null wamei/notes-sync--watch))
    (should (null messages))))

(ert-deftest notes-sync-test-poll-gives-up-after-deadline ()
  "期限を過ぎても終わらなければ、知らせて見張りをやめる。"
  (notes-sync-test--with-env
      `(("print" . (0 . ,notes-sync-test--print-running)))
    (notes-sync)
    (plist-put wamei/notes-sync--watch :deadline (- (float-time) 1))
    (wamei/notes-sync--poll)
    (should (null wamei/notes-sync--watch))
    (should (= (length messages) 1))))

;;; 同期対象の判定

(defmacro notes-sync-test--with-note (spec &rest body)
  "SPEC = (VAR NAME CONTENT) のメモバッファを作って VAR に束縛し BODY を評価する。
メモの置き場 `wamei/notes-sync-directory' は一時ディレクトリ (`dir') にする。
ファイルは作らず、NAME を訪れているバッファに CONTENT を直接入れる (ファイルから
読むと CRLF が復号で落ち、行末に \\r が残る場合を試せない)。"
  (declare (indent 1))
  (let ((var (nth 0 spec)) (name (nth 1 spec)) (content (nth 2 spec)))
    `(let* ((dir (file-name-as-directory (make-temp-file "notes-sync-notes" t)))
            (wamei/notes-sync-directory dir)
            (,var (generate-new-buffer "note")))
       (unwind-protect
           (with-current-buffer ,var
             (setq buffer-file-name (expand-file-name ,name dir))
             (insert ,content)
             ,@body)
         (with-current-buffer ,var (set-buffer-modified-p nil))
         (kill-buffer ,var)
         (delete-directory dir t)))))

(ert-deftest notes-sync-test-buffer-p-front-matter ()
  "先頭の front matter に notes_sync: yes があれば対象。"
  (notes-sync-test--with-note (buf "a.md" "---\nnotes_sync: yes\n---\n# a\n本文\n")
    (should (wamei/notes-sync-buffer-p buf))))

(ert-deftest notes-sync-test-buffer-p-among-other-keys ()
  (notes-sync-test--with-note
      (buf "a.md" "---\ntags: [x]\nnotes_sync: yes\nid: 1\n---\n# a\n")
    (should (wamei/notes-sync-buffer-p buf))))

(ert-deftest notes-sync-test-buffer-p-ignores-case-and-crlf ()
  (notes-sync-test--with-note
      (buf "a.md" "---\r\nNotes_Sync: YES\r\n---\r\n# a\r\n")
    (should (wamei/notes-sync-buffer-p buf))))

(ert-deftest notes-sync-test-buffer-p-rejects-without-front-matter ()
  "front matter の外 (本文) に書いた notes_sync: yes は opt-in ではない。"
  (notes-sync-test--with-note (buf "a.md" "# a\nnotes_sync: yes\n")
    (should-not (wamei/notes-sync-buffer-p buf))))

(ert-deftest notes-sync-test-buffer-p-rejects-front-matter-not-at-top ()
  (notes-sync-test--with-note
      (buf "a.md" "# a\n\n---\nnotes_sync: yes\n---\n")
    (should-not (wamei/notes-sync-buffer-p buf))))

(ert-deftest notes-sync-test-buffer-p-rejects-after-front-matter ()
  "閉じた front matter の後ろにある行は見ない。"
  (notes-sync-test--with-note
      (buf "a.md" "---\ntags: [x]\n---\nnotes_sync: yes\n")
    (should-not (wamei/notes-sync-buffer-p buf))))

(ert-deftest notes-sync-test-buffer-p-rejects-unclosed-front-matter ()
  (notes-sync-test--with-note (buf "a.md" "---\nnotes_sync: yes\n# a\n")
    (should-not (wamei/notes-sync-buffer-p buf))))

(ert-deftest notes-sync-test-buffer-p-rejects-other-values ()
  (notes-sync-test--with-note (buf "a.md" "---\nnotes_sync: no\n---\n")
    (should-not (wamei/notes-sync-buffer-p buf))))

(ert-deftest notes-sync-test-buffer-p-rejects-org ()
  "org のメモ (旧書式の #+notes_sync:) はもう対象にしない。"
  (notes-sync-test--with-note (buf "a.org" "#+title: a\n#+notes_sync: yes\n")
    (should-not (wamei/notes-sync-buffer-p buf)))
  (notes-sync-test--with-note (buf "a.org" "---\nnotes_sync: yes\n---\n")
    (should-not (wamei/notes-sync-buffer-p buf))))

(ert-deftest notes-sync-test-buffer-p-rejects-outside-directory ()
  (notes-sync-test--with-note (buf "a.md" "---\nnotes_sync: yes\n---\n")
    (let ((wamei/notes-sync-directory (make-temp-file "notes-sync-other" t)))
      (unwind-protect
          (should-not (wamei/notes-sync-buffer-p buf))
        (delete-directory wamei/notes-sync-directory t)))))

(ert-deftest notes-sync-test-buffer-p-rejects-without-directory ()
  (notes-sync-test--with-note (buf "a.md" "---\nnotes_sync: yes\n---\n")
    (let ((wamei/notes-sync-directory nil))
      (should-not (wamei/notes-sync-buffer-p buf)))))

(ert-deftest notes-sync-test-directory-defaults-to-notes ()
  (should (equal (default-value 'wamei/notes-sync-directory) "~/notes/")))

;;; フック

(ert-deftest notes-sync-test-setup-hooks-markdown-mode ()
  (let ((after-save-hook nil)
        (markdown-mode-hook nil))
    (wamei/notes-sync-setup)
    (should (memq #'wamei/notes-sync--after-save after-save-hook))
    (should (memq #'wamei/notes-sync--setup-buffer markdown-mode-hook))))

(defmacro notes-sync-test--counting-kicks (&rest body)
  "`notes-sync' と表示の有効化を数えるだけにして BODY を評価する。
蹴った回数は `kicks' に入る。"
  `(let ((kicks 0))
     (cl-letf (((symbol-function 'notes-sync) (lambda () (cl-incf kicks)))
               ((symbol-function 'wamei/notes-sync--enable-view) #'ignore))
       ,@body)))

(ert-deftest notes-sync-test-after-save-kicks-in-gfm-mode ()
  "init.el は .md を gfm-mode で開く (markdown-mode の派生)。"
  (notes-sync-test--with-note (buf "a.md" "---\nnotes_sync: yes\n---\n")
    (let ((markdown-mode-hook nil) (gfm-mode-hook nil))
      (gfm-mode))
    (notes-sync-test--counting-kicks
     (wamei/notes-sync--after-save)
     (should (= kicks 1)))))

(ert-deftest notes-sync-test-after-save-ignores-other-modes ()
  (notes-sync-test--with-note (buf "a.md" "---\nnotes_sync: yes\n---\n")
    (notes-sync-test--counting-kicks
     (wamei/notes-sync--after-save)
     (should (= kicks 0)))))

;;; inline 画像

(ert-deftest notes-sync-test-display-images-resolves-from-file-directory ()
  "画像リンクはファイルからの相対なので、default-directory がプロジェクトの
root を向いていても (メモとして開いたとき) ファイルの場所を基準にする。
付け直しで overlay が重ならないよう、先に外してから出す。"
  (notes-sync-test--with-note (buf "a.md" "![](.notes-media/X.jpg)\n")
    (let ((default-directory temporary-file-directory)
          (calls nil))
      (cl-letf (((symbol-function 'markdown-remove-inline-images)
                 (lambda () (push 'remove calls)))
                ((symbol-function 'markdown-display-inline-images)
                 (lambda () (push (cons 'display default-directory) calls))))
        (wamei/notes-sync--display-images))
      (should (equal (reverse calls) `(remove (display . ,dir)))))))

(ert-deftest notes-sync-test-enable-view-limits-image-size-on-gui ()
  (notes-sync-test--with-note (buf "a.md" "---\nnotes_sync: yes\n---\n")
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'wamei/notes-sync--display-images) #'ignore))
      (wamei/notes-sync--enable-view)
      (should (local-variable-p 'markdown-max-image-size))
      (should (equal (car markdown-max-image-size) 480))
      (should (memq #'wamei/notes-sync--display-images after-revert-hook)))
    (auto-revert-mode -1)))

(provide 'notes-sync-test)
;;; notes-sync-test.el ends here
