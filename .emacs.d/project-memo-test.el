;;; project-memo-test.el --- tests for project-memo -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'project)
(package-initialize)
(require 'dired-subtree)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "project-tabs.el" dir) nil t)
  (load (expand-file-name "dired-tree.el" dir) nil t)
  (load (expand-file-name "project-sidebar.el" dir) nil t)
  (load (expand-file-name "project-memo.el" dir) nil t))

(wamei/project-sidebar-setup)

;;; フィクスチャ

(defmacro wamei/project-memo-test--with-project (var &rest body)
  "一時ディレクトリを transient プロジェクトにして VAR に束縛し BODY を評価する。

`wamei/project-memo-directory' も一時ディレクトリに差し替える。実ユーザーの
~/org/ にテストが書き込まないようにするため、この束縛は必須。
VAR は `file-truename' 済み (macOS では make-temp-file の結果が
/var/folders/… → /private/var/… と symlink 越しになる)。"
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (file-truename (make-temp-file "memo-proj-" t))))
          (wamei/project-memo-directory
           (file-name-as-directory (file-truename (make-temp-file "memo-org-" t))))
          (project-find-functions
           (list (lambda (dir)
                   (when (string-prefix-p ,var (file-truename (expand-file-name dir)))
                     (cons 'transient ,var))))))
     (unwind-protect
         (progn ,@body)
       ;; メモバッファを (未保存でも聞かれないように) 片付けてからディレクトリを消す
       (dolist (buf (buffer-list))
         (when-let* ((file (buffer-file-name buf))
                     ((string-prefix-p wamei/project-memo-directory file)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory ,var t)
       (delete-directory wamei/project-memo-directory t))))

(defun wamei/project-memo-test--project (root)
  "ROOT のプロジェクトオブジェクト。"
  (project-current nil root))

;;; パス解決

(ert-deftest wamei/project-memo-file-is-project-name-under-memo-directory ()
  (wamei/project-memo-test--with-project root
    (should (equal (wamei/project-memo-file (wamei/project-memo-test--project root))
                   (expand-file-name
                    (concat (file-name-nondirectory (directory-file-name root)) ".org")
                    wamei/project-memo-directory)))))

(ert-deftest wamei/project-memo-file-replaces-slash-in-name ()
  (wamei/project-memo-test--with-project root
    (cl-letf (((symbol-function 'project-name) (lambda (_project) "group/app")))
      (should (equal (wamei/project-memo-file (wamei/project-memo-test--project root))
                     (expand-file-name "group-app.org" wamei/project-memo-directory))))))

(ert-deftest wamei/project-memo-global-file-is-global-name ()
  (wamei/project-memo-test--with-project root
    (should (equal (wamei/project-memo-global-file)
                   (expand-file-name "global.org" wamei/project-memo-directory)))))

(ert-deftest wamei/project-memo-file-creates-memo-directory ()
  (wamei/project-memo-test--with-project root
    (delete-directory wamei/project-memo-directory t)
    (should-not (file-directory-p wamei/project-memo-directory))
    (wamei/project-memo-global-file)
    (should (file-directory-p wamei/project-memo-directory))))

;;; メモバッファの判定

(ert-deftest wamei/project-memo-buffer-p-matches-org-under-memo-directory ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (find-file-noselect (wamei/project-memo-global-file))))
      (should (wamei/project-memo-buffer-p buffer)))))

(ert-deftest wamei/project-memo-buffer-p-rejects-other-files ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (should-not (wamei/project-memo-buffer-p buffer))
        (kill-buffer buffer)))))

(ert-deftest wamei/project-memo-buffer-p-rejects-non-org-in-memo-directory ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (find-file-noselect
                   (expand-file-name "notes.txt" wamei/project-memo-directory))))
      (should-not (wamei/project-memo-buffer-p buffer)))))

;;; メモバッファ

(ert-deftest wamei/project-memo-buffer-inserts-title-for-new-file ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (wamei/project-memo-buffer (wamei/project-memo-test--project root))))
      (with-current-buffer buffer
        (should (string-prefix-p
                 (concat "#+title: " (file-name-nondirectory (directory-file-name root)))
                 (buffer-string)))))))

(ert-deftest wamei/project-memo-buffer-keeps-existing-content ()
  (wamei/project-memo-test--with-project root
    (let ((file (wamei/project-memo-file (wamei/project-memo-test--project root))))
      (with-temp-file file (insert "既存の中身\n"))
      (let ((buffer (wamei/project-memo-buffer (wamei/project-memo-test--project root))))
        (with-current-buffer buffer
          (should (equal (buffer-string) "既存の中身\n")))))))

(ert-deftest wamei/project-memo-buffer-overrides-project-for-project-memo ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (wamei/project-memo-buffer (wamei/project-memo-test--project root))))
      (with-current-buffer buffer
        (should (local-variable-p 'project-current-directory-override))
        (should (equal (project-root (project-current nil)) root))))))

(ert-deftest wamei/project-memo-buffer-does-not-override-project-for-global ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (wamei/project-memo-buffer nil)))
      (with-current-buffer buffer
        (should-not (local-variable-p 'project-current-directory-override))
        (should (equal (buffer-file-name) (wamei/project-memo-global-file)))))))

(ert-deftest wamei/project-memo-buffer-is-org-mode ()
  (wamei/project-memo-test--with-project root
    (with-current-buffer (wamei/project-memo-buffer nil)
      (should (derived-mode-p 'org-mode)))))

;;; 表示トグル

(ert-deftest wamei/project-memo-toggle-shows-project-memo-in-main-window ()
  (wamei/project-memo-test--with-project root
    (let ((main (selected-window)))
      (with-current-buffer (window-buffer main)
        (setq default-directory root))
      (wamei/project-memo-toggle)
      (should (eq (selected-window) main))
      (should (equal (buffer-file-name (window-buffer main))
                     (wamei/project-memo-file (wamei/project-memo-test--project root)))))))

(ert-deftest wamei/project-memo-toggle-returns-to-previous-buffer ()
  (wamei/project-memo-test--with-project root
    (let* ((main (selected-window))
           (work (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            (set-window-buffer main work)
            (wamei/project-memo-toggle)
            (should (wamei/project-memo-buffer-p (window-buffer main)))
            (wamei/project-memo-toggle)
            (should (eq (window-buffer main) work)))
        (kill-buffer work)))))

(ert-deftest wamei/project-memo-toggle-with-prefix-shows-global-memo ()
  (wamei/project-memo-test--with-project root
    (let ((main (selected-window)))
      (with-current-buffer (window-buffer main)
        (setq default-directory root))
      (wamei/project-memo-toggle '(4))
      (should (equal (buffer-file-name (window-buffer main))
                     (wamei/project-memo-global-file))))))

(ert-deftest wamei/project-memo-toggle-outside-project-shows-global-memo ()
  (wamei/project-memo-test--with-project root
    (let ((main (selected-window))
          (scratch (get-buffer-create "*memo-test-scratch*")))
      (unwind-protect
          (progn
            (with-current-buffer scratch (setq default-directory temporary-file-directory))
            (set-window-buffer main scratch)
            (wamei/project-memo-toggle)
            (should (equal (buffer-file-name (window-buffer main))
                           (wamei/project-memo-global-file))))
        (kill-buffer scratch)))))

(ert-deftest wamei/project-memo-toggle-from-global-to-project-keeps-back-buffer ()
  (wamei/project-memo-test--with-project root
    (let* ((main (selected-window))
           (work (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            (set-window-buffer main work)
            (wamei/project-memo-toggle '(4))   ; 全体メモ
            (wamei/project-memo-toggle)        ; プロジェクトメモ (メモ → メモ)
            (should (equal (buffer-file-name (window-buffer main))
                           (wamei/project-memo-file (wamei/project-memo-test--project root))))
            (wamei/project-memo-toggle)        ; 戻り先は work のまま
            (should (eq (window-buffer main) work)))
        (kill-buffer work)))))

(ert-deftest wamei/project-memo-toggle-follows-displayed-memo-despite-stale-back-buffer ()
  (wamei/project-memo-test--with-project root
    (let* ((root-b (file-name-as-directory (file-truename (make-temp-file "memo-proj-b-" t))))
           (project-find-functions
            (cons (lambda (dir)
                    (when (string-prefix-p root-b (file-truename (expand-file-name dir)))
                      (cons 'transient root-b)))
                  project-find-functions))
           (main (selected-window))
           (memo-a (wamei/project-memo-buffer (wamei/project-memo-test--project root)))
           (work-b (find-file-noselect (expand-file-name "other.el" root-b))))
      (unwind-protect
          (progn
            ;; window に root の project memo を出した状態で、back には別
            ;; プロジェクト (root-b) の普通のバッファが残っている、という
            ;; toggle を経由しない (find-file 等での) 差し替えを模す。
            (set-window-buffer main memo-a)
            (set-window-parameter main 'wamei/project-memo-back work-b)
            (wamei/project-memo-toggle)
            ;; 表示中の memo-a 自身が root のメモだと正しく自己判定され、
            ;; back の別プロジェクトに惑わされず「戻る」動作 (work-b へ)
            ;; になる。back を無条件に信用すると root-b のメモへ誤って
            ;; 切り替わってしまう。
            (should (eq (window-buffer main) work-b)))
        (kill-buffer work-b)
        (delete-directory root-b t)))))

(ert-deftest wamei/project-memo-toggle-skips-other-memo-buffers-when-no-back-recorded ()
  (wamei/project-memo-test--with-project root
    (let* ((main (selected-window))
           (project (wamei/project-memo-test--project root))
           (project-memo (wamei/project-memo-buffer project))
           (global-memo (wamei/project-memo-buffer nil))
           (other (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            ;; toggle を経由せず window に直接メモを出した状態 (desktop 復元
            ;; 直後など) を模す。back は明示的に未記録にする (この window は
            ;; 1 つの batch セッションを全テストで使い回すので、他のテスト
            ;; の toggle が残した値が残っていることがある)。window の履歴
            ;; (window-prev-buffers) を別のメモだけにしておき、
            ;; `switch-to-prev-buffer' 任せだとメモに留まってしまうことを
            ;; 確認する。
            (set-window-buffer main project-memo)
            (set-window-parameter main 'wamei/project-memo-back nil)
            (set-window-prev-buffers main (list (list global-memo (point-min) (point-min))))
            (wamei/project-memo-toggle)
            (should-not (wamei/project-memo-buffer-p (window-buffer main))))
        (kill-buffer other)))))

(provide 'project-memo-test)
;;; project-memo-test.el ends here
