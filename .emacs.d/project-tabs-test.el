;;; project-tabs-test.el --- tests for project-tabs -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l project-tabs-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'project)
(require 'tab-bar)
(package-initialize)
(require 'treemacs)
(load (expand-file-name "project-tabs.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defmacro wamei/project-tabs-test--with-project (root &rest body)
  "ROOT 配下だけをプロジェクトとみなす環境で BODY を評価する。"
  (declare (indent 1))
  `(let ((project-find-functions
          (list (lambda (dir)
                  (when (string-prefix-p ,root (expand-file-name dir))
                    (cons 'transient ,root))))))
     ,@body))

(defmacro wamei/project-tabs-test--with-tab-bar (&rest body)
  "タブを 1 つ (未固定) にリセットした tab-bar-mode で BODY を評価する。"
  (declare (indent 0))
  `(let ((tab-bar-tab-name-function #'wamei/tab-bar-tab-name-project))
     (tab-bar-mode 1)
     (set-frame-parameter nil 'tabs nil)
     (unwind-protect (progn ,@body)
       (set-frame-parameter nil 'tabs nil))))

(defun wamei/project-tabs-test--show (buffer dir)
  "BUFFER の default-directory を DIR にして選択 window に出す。"
  (with-current-buffer buffer
    (setq default-directory dir))
  (set-window-buffer (selected-window) buffer)
  buffer)

;;; タブ名

(ert-deftest wamei/project-tabs-name-uses-project-name ()
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (with-temp-buffer
      (wamei/project-tabs-test--show (current-buffer) "/tmp/proj-a/src/")
      (should (equal (wamei/tab-bar-tab-name-project) "proj-a")))))

(ert-deftest wamei/project-tabs-name-falls-back-to-buffer-name ()
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (with-temp-buffer
      (wamei/project-tabs-test--show (current-buffer) "/tmp/elsewhere/")
      (should (equal (wamei/tab-bar-tab-name-project) (buffer-name))))))

(ert-deftest wamei/project-tabs-name-looks-past-side-window ()
  "選択 window が no-other-window なら直近の通常 window のバッファで決める。"
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (delete-other-windows)
    (let ((main (generate-new-buffer " main"))
          (side (generate-new-buffer " side")))
      (unwind-protect
          (progn
            (wamei/project-tabs-test--show main "/tmp/proj-a/")
            (let ((side-window (split-window nil nil 'left)))
              (set-window-buffer side-window side)
              (set-window-parameter side-window 'no-other-window t)
              (select-window side-window)
              (should (equal (wamei/tab-bar-tab-name-project) "proj-a"))))
        (delete-other-windows)
        (kill-buffer main)
        (kill-buffer side)))))

(ert-deftest wamei/project-tabs-main-window-skips-side-window ()
  (let* ((main (selected-window))
         (side (split-window main nil 'left)))
    (unwind-protect
        (progn
          (set-window-parameter side 'no-other-window t)
          (select-window side)
          (should (eq (wamei/project-tabs-main-window) main))
          (select-window main)
          (should (eq (wamei/project-tabs-main-window) main)))
      (delete-window side))))

;;; タブ名の固定

(ert-deftest wamei/project-tabs-pin-renames-unpinned-tab-to-project ()
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (wamei/project-tabs-test--with-tab-bar
      (with-temp-buffer
        (wamei/project-tabs-test--show (current-buffer) "/tmp/proj-a/")
        (should (equal (wamei/project-tabs-pin-name) "proj-a"))
        (let ((tab (tab-bar--current-tab)))
          (should (equal (alist-get 'name tab) "proj-a"))
          (should (alist-get 'explicit-name tab)))))))

(ert-deftest wamei/project-tabs-pin-keeps-explicit-name ()
  "既に固定されたタブは別プロジェクトのバッファを出しても変えない。"
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (wamei/project-tabs-test--with-tab-bar
      (tab-rename "pinned")
      (with-temp-buffer
        (wamei/project-tabs-test--show (current-buffer) "/tmp/proj-a/")
        (should-not (wamei/project-tabs-pin-name))
        (should (equal (alist-get 'name (tab-bar--current-tab)) "pinned"))))))

(ert-deftest wamei/project-tabs-pin-skips-buffer-outside-project ()
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (wamei/project-tabs-test--with-tab-bar
      (with-temp-buffer
        (wamei/project-tabs-test--show (current-buffer) "/tmp/elsewhere/")
        (should-not (wamei/project-tabs-pin-name))
        (should-not (alist-get 'explicit-name (tab-bar--current-tab)))))))

(ert-deftest wamei/project-tabs-pin-is-noop-without-tab-bar-mode ()
  (wamei/project-tabs-test--with-project "/tmp/proj-a/"
    (tab-bar-mode -1)
    (with-temp-buffer
      (wamei/project-tabs-test--show (current-buffer) "/tmp/proj-a/")
      (should-not (wamei/project-tabs-pin-name)))))

;;; treemacs のガード

(defun wamei/project-tabs-test--project (path)
  (treemacs-project->create! :name (file-name-nondirectory path)
                             :path path :path-status 'local-readable))

(ert-deftest wamei/project-tabs-treemacs-guard-skips-when-root-not-rendered ()
  "root ノードが DOM にないプロジェクトは探索せず nil を返す。"
  (with-temp-buffer
    (setq treemacs-dom (make-hash-table :test 'equal))
    (let ((called nil))
      (should-not (wamei/treemacs--find-file-node-guard
                   (lambda (&rest _) (setq called t))
                   "/tmp/proj-a/file"
                   (wamei/project-tabs-test--project "/tmp/proj-a")))
      (should-not called))))

(ert-deftest wamei/project-tabs-treemacs-guard-delegates-when-root-rendered ()
  (with-temp-buffer
    (setq treemacs-dom (make-hash-table :test 'equal))
    (treemacs-dom-node->insert-into-dom!
     (treemacs-dom-node->create! :key "/tmp/proj-a" :position 1))
    (let ((project (wamei/project-tabs-test--project "/tmp/proj-a")))
      (should (equal (wamei/treemacs--find-file-node-guard
                      (lambda (path proj) (list path proj))
                      "/tmp/proj-a/file" project)
                     (list "/tmp/proj-a/file" project))))))

(ert-deftest wamei/project-tabs-treemacs-guard-resolves-project-from-workspace ()
  "PROJECT 省略時は現在の workspace から探し、見つからなければ nil。"
  (with-temp-buffer
    (setq treemacs-dom (make-hash-table :test 'equal))
    (treemacs-dom-node->insert-into-dom!
     (treemacs-dom-node->create! :key "/tmp/proj-a" :position 1))
    (let* ((project (wamei/project-tabs-test--project "/tmp/proj-a"))
           (treemacs-override-workspace
            (treemacs-workspace->create! :name "test" :projects (list project))))
      ;; persist ファイルの読み込みを抑止する
      (put 'treemacs :state-is-restored t)
      (should (equal (wamei/treemacs--find-file-node-guard
                      (lambda (path proj) (list path proj))
                      "/tmp/proj-a/file")
                     (list "/tmp/proj-a/file" project)))
      (should-not (wamei/treemacs--find-file-node-guard
                   (lambda (&rest _) (error "should not be called"))
                   "/tmp/other/file")))))

(provide 'project-tabs-test)
;;; project-tabs-test.el ends here
