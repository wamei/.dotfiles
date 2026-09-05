;;; project-sidebar-test.el --- tests for project-sidebar -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l project-sidebar-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'project)
(require 'cl-lib)
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
中に src/main.el と README を作る。VAR は `file-truename' 済み
(macOS では /var/folders/… → /private/var/… のように make-temp-file の結果が
symlink 越しになるため、`wamei/project-sidebar--root-for' の正規化と揃える)。"
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (file-truename (make-temp-file "sidebar-" t))))
          (project-find-functions
           (list (lambda (dir)
                   (when (string-prefix-p ,var (file-truename (expand-file-name dir)))
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
    (should (equal (wamei/project-sidebar--root-for "/tmp/nowhere/")
                   (file-name-as-directory (file-truename "/tmp/nowhere/"))))))

(ert-deftest wamei/project-sidebar-root-for-resolves-symlink ()
  (wamei/project-sidebar-test--with-project root
    (let ((link (make-temp-name (expand-file-name "sidebar-link-" temporary-file-directory))))
      (make-symbolic-link (directory-file-name root) link)
      (unwind-protect
          (should (equal (wamei/project-sidebar--root-for link) root))
        (delete-file link)))))

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

(ert-deftest wamei/project-sidebar-invisibility-spec-added-once ()
  (wamei/project-sidebar-test--with-project root
    (let ((buf (wamei/project-sidebar-buffer root)))
      (with-current-buffer buf
        (revert-buffer)
        (revert-buffer)
        (should (= 1 (seq-count (lambda (e) (eq e 'wamei/project-sidebar-header))
                                buffer-invisibility-spec)))))))

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

;;; follow

(ert-deftest wamei/project-sidebar-follow-target ()
  (should (eq (wamei/project-sidebar--follow-target "/r/a.el" "/r/" "/r/") 'same))
  (should (eq (wamei/project-sidebar--follow-target "/o/a.el" "/r/" "/o/") 'switch))
  (should (eq (wamei/project-sidebar--follow-target "/o/a.el" "/r/" nil) 'none))
  (should (eq (wamei/project-sidebar--follow-target nil "/r/" "/r/") 'none)))

(ert-deftest wamei/project-sidebar-follow-target-ignores-git-dir ()
  "magit の COMMIT_EDITMSG など .git 配下のファイルには追従しない (.git を展開させない)。"
  (should (eq (wamei/project-sidebar--follow-target "/r/.git/COMMIT_EDITMSG" "/r/" "/r/") 'none))
  (should (eq (wamei/project-sidebar--follow-target "/r/.git/rebase-merge/git-rebase-todo" "/r/" "/r/") 'none))
  ;; worktree: 共通 gitdir は別プロジェクト扱いになるが、これも追従しない
  (should (eq (wamei/project-sidebar--follow-target "/o/.git/worktrees/x/COMMIT_EDITMSG" "/r/" "/o/") 'none))
  ;; .gitignore や .github は .git そのものではないので通常どおり
  (should (eq (wamei/project-sidebar--follow-target "/r/.gitignore" "/r/" "/r/") 'same))
  (should (eq (wamei/project-sidebar--follow-target "/r/.github/ci.yml" "/r/" "/r/") 'same)))

(ert-deftest wamei/project-sidebar-follow-mode-hooks-window-state-change ()
  "follow は `window-state-change-functions' で駆動する。
magit の q (`set-window-configuration') は `window-buffer-change-functions' を
走らせないが、state-change は走るため。"
  (let ((was wamei/project-sidebar-follow-mode))
    (unwind-protect
        (progn
          (wamei/project-sidebar-follow-mode 1)
          (should (memq #'wamei/project-sidebar--follow-soon window-state-change-functions))
          (wamei/project-sidebar-follow-mode -1)
          (should-not (memq #'wamei/project-sidebar--follow-soon window-state-change-functions))
          (should-not (memq #'wamei/project-sidebar--follow-soon window-buffer-change-functions))
          (should-not (memq #'wamei/project-sidebar--follow-soon window-selection-change-functions)))
      (wamei/project-sidebar-follow-mode (if was 1 -1)))))

(ert-deftest wamei/project-sidebar-follow-expands-to-visited-file ()
  (wamei/project-sidebar-test--with-project root
    (let* ((main (selected-window))
           (file (expand-file-name "src/main.el" root))
           (buf (find-file-noselect file)))
      (unwind-protect
          (progn
            (set-window-buffer main buf)
            (let ((win (wamei/project-sidebar-show root)))
              (wamei/project-sidebar--follow (selected-frame))
              (with-current-buffer (window-buffer win)
                (should (equal (save-excursion
                                 (goto-char (window-point win))
                                 (dired-utils-get-filename))
                               file)))
              (delete-window win)))
        (kill-buffer buf)))))

(ert-deftest wamei/project-sidebar-follow-switches-to-other-project ()
  (wamei/project-sidebar-test--with-project root-a
    (wamei/project-sidebar-test--with-project root-b
      (let* ((project-find-functions
              (list (lambda (dir)
                      (let ((dir (file-truename (expand-file-name dir))))
                        (cond ((string-prefix-p root-a dir) (cons 'transient root-a))
                              ((string-prefix-p root-b dir) (cons 'transient root-b)))))))
             (main (selected-window))
             (buf (find-file-noselect (expand-file-name "README" root-b))))
        (unwind-protect
            (let ((win (wamei/project-sidebar-show root-a)))
              (set-window-buffer main buf)
              (wamei/project-sidebar--follow (selected-frame))
              (should (eq (window-buffer win) (wamei/project-sidebar-buffer root-b)))
              (delete-window win))
          (kill-buffer buf))))))

(ert-deftest wamei/project-sidebar-follow-skips-remote-buffer ()
  (wamei/project-sidebar-test--with-project root
    (let* ((main (selected-window))
           (buf (generate-new-buffer " *sidebar-test-remote*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq buffer-file-name "/ssh:example.invalid:/tmp/x.el"))
            (let ((win (wamei/project-sidebar-show root)))
              (set-window-buffer main buf)
              (wamei/project-sidebar--follow (selected-frame))
              (should (eq (window-buffer win) (wamei/project-sidebar-buffer root)))
              (delete-window win)))
        (kill-buffer buf)))))

(ert-deftest wamei/project-sidebar-follow-switch-keeps-window-dedicated-on-error ()
  (wamei/project-sidebar-test--with-project root-a
    (wamei/project-sidebar-test--with-project root-b
      (let* ((project-find-functions
              (list (lambda (dir)
                      (let ((dir (file-truename (expand-file-name dir))))
                        (cond ((string-prefix-p root-a dir) (cons 'transient root-a))
                              ((string-prefix-p root-b dir) (cons 'transient root-b)))))))
             (main (selected-window))
             (buf (find-file-noselect (expand-file-name "README" root-b))))
        (unwind-protect
            (let ((win (wamei/project-sidebar-show root-a)))
              (set-window-buffer main buf)
              (cl-letf (((symbol-function 'wamei/project-sidebar-buffer)
                         (lambda (&rest _) (error "boom"))))
                (ignore-errors (wamei/project-sidebar--follow (selected-frame))))
              (should (window-dedicated-p win))
              (delete-window win))
          (kill-buffer buf))))))

;;; 開く

(ert-deftest wamei/project-sidebar-preview-shows-file-without-focus ()
  (wamei/project-sidebar-test--with-project root
    (let* ((main (selected-window))
           (win (wamei/project-sidebar-show root)))
      (unwind-protect
          (progn
            (select-window win)
            (wamei/dired-tree-expand-to (expand-file-name "README" root))
            (wamei/project-sidebar-preview)
            (should (eq (selected-window) win))
            (should (equal (buffer-file-name (window-buffer main))
                           (expand-file-name "README" root))))
        (kill-buffer (window-buffer main))
        (delete-window win)))))

(ert-deftest wamei/project-sidebar-open-selects-main-for-file ()
  (wamei/project-sidebar-test--with-project root
    (let* ((main (selected-window))
           (win (wamei/project-sidebar-show root)))
      (unwind-protect
          (progn
            (select-window win)
            (wamei/dired-tree-expand-to (expand-file-name "README" root))
            (wamei/project-sidebar-open)
            (should (eq (selected-window) main))
            (should (equal (buffer-file-name (window-buffer main))
                           (expand-file-name "README" root))))
        (kill-buffer (window-buffer main))
        (delete-window win)))))

(ert-deftest wamei/project-sidebar-open-toggles-directory ()
  (wamei/project-sidebar-test--with-project root
    (let ((win (wamei/project-sidebar-show root)))
      (unwind-protect
          (progn
            (select-window win)
            (dired-utils-goto-line (expand-file-name "src" root))
            (wamei/project-sidebar-open)
            (should (eq (selected-window) win))
            (should (dired-utils-goto-line (expand-file-name "src/main.el" root))))
        (delete-window win)))))

;;; q と現在行の強調

(ert-deftest wamei/project-sidebar-quit-closes-window ()
  (wamei/project-sidebar-test--with-project root
    (let* ((main (selected-window))
           (win (wamei/project-sidebar-show root)))
      (select-window win)
      (wamei/project-sidebar-quit)
      (should-not (wamei/project-sidebar-window))
      (should (eq (selected-window) main)))))

(ert-deftest wamei/project-sidebar-reveal-marks-current-row ()
  (wamei/project-sidebar-test--with-project root
    (let* ((win (wamei/project-sidebar-show root))
           (file (expand-file-name "src/main.el" root)))
      (unwind-protect
          (progn
            (wamei/project-sidebar--reveal win file)
            (with-current-buffer (window-buffer win)
              (let ((ov wamei/project-sidebar--row-overlay)
                    (pos (window-point win)))
                (should (overlayp ov))
                (should (= (overlay-start ov)
                           (save-excursion (goto-char pos) (line-beginning-position))))
                (should (= (overlay-end ov)
                           (save-excursion (goto-char pos) (line-beginning-position 2))))
                (should (eq (overlay-get ov 'face) 'wamei/project-sidebar-current-row))
                (should (overlay-get ov 'before-string)))))
        (delete-window win)))))

(ert-deftest wamei/project-sidebar-row-overlay-survives-revert ()
  (wamei/project-sidebar-test--with-project root
    (let* ((win (wamei/project-sidebar-show root))
           (file (expand-file-name "src/main.el" root)))
      (unwind-protect
          (progn
            (wamei/project-sidebar--reveal win file)
            (with-current-buffer (window-buffer win)
              (wamei/dired-tree-revert)
              (run-hooks 'wamei/dired-tree-refresh-hook)
              (let ((ov wamei/project-sidebar--row-overlay))
                (should (overlay-buffer ov))
                (should (= (overlay-start ov)
                           (save-excursion (goto-char (window-point win))
                                           (line-beginning-position)))))))
        (delete-window win)))))

(ert-deftest wamei/project-sidebar-hides-cursor-when-not-selected ()
  (wamei/project-sidebar-test--with-project root
    (with-current-buffer (wamei/project-sidebar-buffer root)
      (should (local-variable-p 'cursor-in-non-selected-windows))
      (should-not cursor-in-non-selected-windows))))

(provide 'project-sidebar-test)
;;; project-sidebar-test.el ends here
