;;; consult-project-files-test.el --- tests for consult-project-files -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l consult-project-files-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'project)
(require 'recentf)
(package-initialize)
(require 'consult)
(load (expand-file-name "consult-project-files.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defun wamei/consult-project-files-test--git (dir &rest args)
  "DIR で git ARGS を実行する。"
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %s: %s" args (buffer-string))))))

(defmacro wamei/consult-project-files-test--with-repo (root &rest body)
  "tracked.el / untracked.el / ignored.log を持つ git リポジトリを作り BODY を評価する。
ROOT にリポジトリの絶対パス (末尾 /) を束縛する。"
  (declare (indent 1))
  `(let* ((,root (file-name-as-directory (make-temp-file "cpf-" t)))
          (default-directory ,root)
          (recentf-list nil)
          (project-list-file (expand-file-name "projects" ,root))
          (project--list nil)
          (consult-project-function #'consult--default-project-function))
     (unwind-protect
         (progn
           (wamei/consult-project-files-test--git ,root "init" "-q")
           (write-region "" nil (expand-file-name "tracked.el" ,root))
           (write-region "" nil (expand-file-name "untracked.el" ,root))
           (write-region "" nil (expand-file-name "ignored.log" ,root))
           (write-region "*.log\n" nil (expand-file-name ".gitignore" ,root))
           (wamei/consult-project-files-test--git ,root "add" "tracked.el" ".gitignore")
           ,@body)
       (delete-directory ,root t))))

(defun wamei/consult-project-files-test--names ()
  "候補の相対パスだけを返す。"
  (mapcar #'car (wamei/consult-project-files--items)))

;;; 候補

(ert-deftest wamei/consult-project-files-includes-untracked ()
  "git の tracked / untracked どちらも相対パスで出る。ignore されたものは出ない。"
  (wamei/consult-project-files-test--with-repo root
    (let ((names (wamei/consult-project-files-test--names)))
      (should (member "tracked.el" names))
      (should (member "untracked.el" names))
      (should (member ".gitignore" names))
      (should-not (member "ignored.log" names)))))

(ert-deftest wamei/consult-project-files-items-carry-absolute-path ()
  "候補の cdr は絶対パス。consult--file-action がそのまま開ける。"
  (wamei/consult-project-files-test--with-repo root
    (let ((item (assoc "untracked.el" (wamei/consult-project-files--items))))
      (should (equal (cdr item) (expand-file-name "untracked.el" root))))))

(ert-deftest wamei/consult-project-files-excludes-open-buffers ()
  "既に開いているファイルは Project Buffer ソース側に出るので重複させない。"
  (wamei/consult-project-files-test--with-repo root
    (let ((buf (find-file-noselect (expand-file-name "tracked.el" root))))
      (unwind-protect
          (let ((names (wamei/consult-project-files-test--names)))
            (should-not (member "tracked.el" names))
            (should (member "untracked.el" names)))
        (kill-buffer buf)))))

(ert-deftest wamei/consult-project-files-excludes-recentf ()
  "recentf にあるものは Project File ソース側に出るので重複させない。"
  (wamei/consult-project-files-test--with-repo root
    (let ((recentf-list (list (expand-file-name "untracked.el" root))))
      (let ((names (wamei/consult-project-files-test--names)))
        (should-not (member "untracked.el" names))
        (should (member "tracked.el" names))))))

(ert-deftest wamei/consult-project-files-nil-outside-project ()
  "プロジェクト外では候補なし。"
  (let ((default-directory temporary-file-directory)
        (project-find-functions nil)
        (consult-project-function #'consult--default-project-function))
    (should-not (wamei/consult-project-files--items))))

;;; ソース定義

(ert-deftest wamei/consult-project-files-source-shape ()
  "consult--multi に渡せる形をしている。"
  (should (equal (plist-get wamei/consult-source-project-files :category) 'file))
  (should (functionp (plist-get wamei/consult-source-project-files :items)))
  (should (functionp (plist-get wamei/consult-source-project-files :state)))
  (should (functionp (plist-get wamei/consult-source-project-files :new))))

(ert-deftest wamei/consult-project-files-source-disabled-without-project-function ()
  (let ((consult-project-function nil))
    (should-not (funcall (plist-get wamei/consult-source-project-files :enabled)))))

;;; consult-project-files-test.el ends here
