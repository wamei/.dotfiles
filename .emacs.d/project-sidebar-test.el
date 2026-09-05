;;; project-sidebar-test.el --- tests for project-sidebar -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l project-sidebar-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'project)
(package-initialize)
(require 'dired-subtree)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "project-tabs.el" dir) nil t)
  (load (expand-file-name "dired-tree.el" dir) nil t)
  (load (expand-file-name "project-sidebar.el" dir) nil t))

(wamei/project-sidebar-setup)

;;; フィクスチャ

(defmacro wamei/project-sidebar-test--with-project (var &rest body)
  "一時ディレクトリを transient プロジェクトにして VAR に束縛し BODY を評価する。
中に src/main.el と README を作る。"
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (make-temp-file "sidebar-" t)))
          (project-find-functions
           (list (lambda (dir)
                   (when (string-prefix-p ,var (expand-file-name dir))
                     (cons 'transient ,var))))))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "src" ,var) t)
           (write-region "" nil (expand-file-name "src/main.el" ,var))
           (write-region "" nil (expand-file-name "README" ,var))
           ,@body)
       (dolist (buf (buffer-list))
         (when (string-prefix-p " *sidebar: " (buffer-name buf))
           (kill-buffer buf)))
       (delete-directory ,var t))))

;;; バッファ

(ert-deftest wamei/project-sidebar-buffer-name-uses-project-name ()
  (should (equal (wamei/project-sidebar--buffer-name "/tmp/proj-a/") " *sidebar: proj-a*")))

(ert-deftest wamei/project-sidebar-root-for-falls-back-to-dir ()
  (let ((project-find-functions nil))
    (should (equal (wamei/project-sidebar--root-for "/tmp/nowhere/") "/tmp/nowhere/"))))

(ert-deftest wamei/project-sidebar-buffer-is-dired-with-modes ()
  (wamei/project-sidebar-test--with-project root
    (let ((buf (wamei/project-sidebar-buffer root)))
      (with-current-buffer buf
        (should (derived-mode-p 'dired-mode))
        (should wamei/project-sidebar-mode)
        (should wamei/dired-tree-mode)
        (should dired-hide-details-mode)
        (should (equal (expand-file-name default-directory) root))
        ;; `format-mode-line' は `emacs -Q --batch' では常に "" を返す
        ;; (redisplay が走らないため。GUI/emacsclient では問題なく動く)。
        ;; header-line-format の中身を直接見て代用する。
        (should (string-match-p (file-name-nondirectory (directory-file-name root))
                                (car header-line-format)))))))

(ert-deftest wamei/project-sidebar-buffer-is-reused ()
  (wamei/project-sidebar-test--with-project root
    (should (eq (wamei/project-sidebar-buffer root) (wamei/project-sidebar-buffer root)))))

(ert-deftest wamei/project-sidebar-buffer-does-not-hijack-plain-dired ()
  (wamei/project-sidebar-test--with-project root
    (let ((sidebar (wamei/project-sidebar-buffer root))
          (plain (dired-noselect root)))
      (unwind-protect
          (should-not (eq sidebar plain))
        (kill-buffer plain)))))

;;; 表示とトグル

(ert-deftest wamei/project-sidebar-show-displays-in-left-side-window ()
  (wamei/project-sidebar-test--with-project root
    (let ((win (wamei/project-sidebar-show root)))
      (unwind-protect
          (progn
            (should (window-live-p win))
            (should (eq (window-parameter win 'window-side) 'left))
            (should (window-parameter win 'no-other-window))
            (should (eq (wamei/project-sidebar-window) win)))
        (delete-window win)))))

(ert-deftest wamei/project-sidebar-toggle-cycles-open-focus-back-close ()
  (wamei/project-sidebar-test--with-project root
    (let ((main (selected-window)))
      (with-current-buffer (window-buffer main)
        (setq default-directory root))
      ;; 非表示 → 開いてフォーカス
      (wamei/project-sidebar-toggle)
      (let ((win (wamei/project-sidebar-window)))
        (should win)
        (should (eq (selected-window) win))
        ;; フォーカス中 → 元へ戻る (開いたまま)
        (wamei/project-sidebar-toggle)
        (should (eq (selected-window) main))
        (should (window-live-p win))
        ;; 表示中で未フォーカス → フォーカス
        (wamei/project-sidebar-toggle)
        (should (eq (selected-window) win))
        ;; C-u → 閉じる
        (wamei/project-sidebar-toggle '(4))
        (should-not (wamei/project-sidebar-window))))))

(provide 'project-sidebar-test)
;;; project-sidebar-test.el ends here
