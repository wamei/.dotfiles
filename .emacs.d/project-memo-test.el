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

;;; 自動保存

(ert-deftest wamei/project-memo-auto-save-p-only-for-memo-buffers ()
  (wamei/project-memo-test--with-project root
    (let ((memo (wamei/project-memo-buffer nil))
          (work (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            (should (with-current-buffer memo (wamei/project-memo--auto-save-p)))
            (should-not (with-current-buffer work (wamei/project-memo--auto-save-p))))
        (kill-buffer work)))))

(ert-deftest wamei/project-memo-save-all-writes-modified-memo ()
  (wamei/project-memo-test--with-project root
    (let ((memo (wamei/project-memo-buffer nil)))
      (with-current-buffer memo
        (goto-char (point-max))
        (insert "書きかけ\n")
        (should (buffer-modified-p)))
      (wamei/project-memo-save-all)
      (should-not (buffer-modified-p memo))
      (should (file-exists-p (wamei/project-memo-global-file)))
      (with-temp-buffer
        (insert-file-contents (wamei/project-memo-global-file))
        (should (string-match-p "書きかけ" (buffer-string)))))))

(ert-deftest wamei/project-memo-save-all-leaves-other-buffers-alone ()
  (wamei/project-memo-test--with-project root
    (let ((work (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            (with-current-buffer work (insert ";; 未保存\n"))
            (wamei/project-memo-save-all)
            (should (buffer-modified-p work))
            (should-not (file-exists-p (expand-file-name "main.el" root))))
        (with-current-buffer work (set-buffer-modified-p nil))
        (kill-buffer work)))))

(ert-deftest wamei/project-memo-save-all-accepts-hook-arguments ()
  (wamei/project-memo-test--with-project root
    ;; window-selection-change-functions は frame を渡す。
    (should-not (wamei/project-memo-save-all (selected-frame)))))

(ert-deftest wamei/project-memo-autosave-setup-installs-predicate-and-hooks ()
  (let* ((auto-save-visited-predicate nil)
         (window-selection-change-functions nil)
         (kill-emacs-hook nil)
         (base-calls 0)
         ;; #'ignore だと「呼ばれたかどうか」を見られないので、素の focus-change
         ;; 処理を模した副作用付きの関数にしておく。
         (after-focus-change-function (lambda () (setq base-calls (1+ base-calls))))
         (auto-save-visited-mode nil)
         (save-all-calls 0))
    (cl-letf (((symbol-function 'auto-save-visited-mode) (lambda (&rest _) t))
              ((symbol-function 'wamei/project-memo-save-all)
               (lambda (&rest _) (setq save-all-calls (1+ save-all-calls)))))
      (wamei/project-memo-autosave-setup)
      (should (eq auto-save-visited-predicate #'wamei/project-memo--auto-save-p))
      (should (memq #'wamei/project-memo-save-all window-selection-change-functions))
      (should (memq #'wamei/project-memo-save-all kill-emacs-hook))
      ;; after-focus-change-function への合成が :after であることを確認する。
      ;; advice-function-member-p は「含まれているか」しか見ないので、それだけだと
      ;; :override 等への取り違えを見逃す。base (素の focus-change 処理) の副作用と
      ;; wamei/project-memo-save-all の副作用が両方観測できることまで見て、
      ;; base を消してしまう合成方法ではないことを確かめる。
      (should (advice-function-member-p #'wamei/project-memo-save-all
                                        after-focus-change-function))
      (funcall after-focus-change-function)
      (should (= base-calls 1))
      (should (= save-all-calls 1)))))

(ert-deftest wamei/project-memo-autosave-setup-does-not-double-compose-after-focus-change-function ()
  (let* ((auto-save-visited-predicate nil)
         (window-selection-change-functions nil)
         (kill-emacs-hook nil)
         (after-focus-change-function #'ignore)
         (auto-save-visited-mode nil)
         (save-all-calls 0))
    (cl-letf (((symbol-function 'auto-save-visited-mode) (lambda (&rest _) t))
              ((symbol-function 'wamei/project-memo-save-all)
               (lambda (&rest _) (setq save-all-calls (1+ save-all-calls)))))
      ;; init.el を再評価するなどして 2 回呼ばれても、フォーカス変化のたびに
      ;; wamei/project-memo-save-all が 2 回走る (合成が二重になる) ことがない
      ;; ように、実際に 1 回だけ発火することを確認する。
      (wamei/project-memo-autosave-setup)
      (wamei/project-memo-autosave-setup)
      (funcall after-focus-change-function)
      (should (= save-all-calls 1)))))

;;; タブの初期画面

(ert-deftest wamei/project-memo-switch-setup-shows-sidebar-and-memo ()
  (wamei/project-memo-test--with-project root
    (let ((main (selected-window)))
      (with-current-buffer (window-buffer main)
        (setq default-directory root))
      (unwind-protect
          (progn
            ;; ERT は各テストの本体を `with-temp-buffer' で包むため、ここまでの
            ;; current-buffer は main のバッファではなく ERT の一時バッファになっている。
            ;; 実際の呼び出し (project-switch-project からの call-interactively) では
            ;; コマンドループが current-buffer を選択中 window のバッファに揃えてから
            ;; 呼ぶので、その前提をここで揃える (select-window はバッファもカレントにする)。
            (select-window main)
            (wamei/project-memo-switch-setup)
            ;; 本文 window にプロジェクトメモ
            (should (equal (buffer-file-name (window-buffer main))
                           (wamei/project-memo-file (wamei/project-memo-test--project root))))
            ;; 左に sidebar が出ていて、フォーカスは本文に残る
            (let ((side (wamei/project-sidebar-window)))
              (should side)
              (should (eq (window-parameter side 'window-side) 'left)))
            (should (eq (selected-window) main)))
        (when-let* ((side (wamei/project-sidebar-window)))
          (delete-window side))))))

(ert-deftest wamei/project-memo-switch-setup-uses-directory-override ()
  (wamei/project-memo-test--with-project root
    ;; project-switch-project と同じ状況: default-directory は別で、
    ;; project-current-directory-override だけが対象プロジェクトを指す。
    (let ((main (selected-window))
          (caller (get-buffer-create "*memo-test-caller*")))
      (unwind-protect
          (progn
            (with-current-buffer caller
              (setq default-directory temporary-file-directory)
              (setq-local project-current-directory-override root)
              (set-window-buffer main caller)
              (wamei/project-memo-switch-setup))
            (should (equal (buffer-file-name (window-buffer main))
                           (wamei/project-memo-file (wamei/project-memo-test--project root)))))
        (when-let* ((side (wamei/project-sidebar-window)))
          (delete-window side))
        (kill-buffer caller)))))

(provide 'project-memo-test)
;;; project-memo-test.el ends here
