;;; project-extra-files-test.el --- tests for project-extra-files -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l project-extra-files-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'project)
(require 'recentf)
(package-initialize)
(require 'consult)
(load (expand-file-name "project-extra-files.el"
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
  "git リポジトリを作り BODY を評価する。
tracked.el (add 済) / untracked.el / ignored.log と .env.local (ignore 対象) /
node_modules/pkg/index.js と .next/cache/x.js (ディレクトリ丸ごと ignore) /
.claude/settings.local.json (ディレクトリ丸ごと ignore だが開きたいもの) を持つ。
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
           (write-region "" nil (expand-file-name ".env.local" ,root))
           (make-directory (expand-file-name "node_modules/pkg" ,root) t)
           (write-region "" nil (expand-file-name "node_modules/pkg/index.js" ,root))
           (make-directory (expand-file-name ".next/cache" ,root) t)
           (write-region "" nil (expand-file-name ".next/cache/x.js" ,root))
           (make-directory (expand-file-name ".claude" ,root) t)
           (write-region "" nil (expand-file-name ".claude/settings.local.json" ,root))
           ;; 許可ディレクトリの中に入れ子の git リポジトリ (worktree 等) がある場合
           (make-directory (expand-file-name ".claude/worktrees/wt" ,root) t)
           (wamei/consult-project-files-test--git
            (expand-file-name ".claude/worktrees/wt" ,root) "init" "-q")
           (write-region "*.log\n*.local\nnode_modules/\n.next/\n.claude/\n"
                         nil (expand-file-name ".gitignore" ,root))
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

(ert-deftest wamei/consult-project-files-includes-open-buffers ()
  "開いているファイルも残す。Project Buffer ソースはバッファ名しか出さないので、
ここに残さないとパスで絞り込む手段が無くなる。"
  (wamei/consult-project-files-test--with-repo root
    (let ((buf (find-file-noselect (expand-file-name "tracked.el" root))))
      (unwind-protect
          (let ((names (wamei/consult-project-files-test--names)))
            (should (member "tracked.el" names))
            (should (member "untracked.el" names)))
        (kill-buffer buf)))))

(ert-deftest wamei/consult-project-files-excludes-recentf ()
  "recentf にあって開いていないものは Project File ソース側に出るので重複させない。"
  (wamei/consult-project-files-test--with-repo root
    (let ((recentf-list (list (expand-file-name "untracked.el" root))))
      (let ((names (wamei/consult-project-files-test--names)))
        (should-not (member "untracked.el" names))
        (should (member "tracked.el" names))))))

(ert-deftest wamei/consult-project-files-includes-open-buffers-in-recentf ()
  "recentf にあっても開いていれば残す。consult の Project File ソースは
開いているものを落とすので、そちらと重複しない。"
  (wamei/consult-project-files-test--with-repo root
    (let* ((file (expand-file-name "untracked.el" root))
           (recentf-list (list file))
           (buf (find-file-noselect file)))
      (unwind-protect
          (should (member "untracked.el" (wamei/consult-project-files-test--names)))
        (kill-buffer buf)))))

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

;;; ignore されたファイル

(ert-deftest wamei/project-ignored-files-lists-only-ignored ()
  "ignore 対象だけを絶対パスで返す。tracked / untracked は含めない。"
  (wamei/consult-project-files-test--with-repo root
    (let ((files (wamei/project-ignored-files (project-current nil root))))
      (should (member (expand-file-name ".env.local" root) files))
      (should (member (expand-file-name "ignored.log" root) files))
      (should-not (member (expand-file-name "tracked.el" root) files))
      (should-not (member (expand-file-name "untracked.el" root) files)))))

(ert-deftest wamei/project-ignored-files-skips-ignored-dirs ()
  "ディレクトリ丸ごと ignore されているものには降りない。
node_modules のような重いものも .next のような生成物も、名前を知らなくても落ちる。"
  (wamei/consult-project-files-test--with-repo root
    (let ((files (wamei/project-ignored-files (project-current nil root))))
      (should-not (seq-find (lambda (f) (string-search "node_modules" f)) files))
      (should-not (seq-find (lambda (f) (string-search ".next" f)) files))
      ;; ディレクトリ自体も候補にしない
      (should-not (seq-find (lambda (f) (string-suffix-p "/" f)) files)))))

(ert-deftest wamei/project-ignored-files-descends-into-allowed-dirs ()
  "許可リストにあるディレクトリだけは丸ごと ignore でも中を列挙する。"
  (wamei/consult-project-files-test--with-repo root
    (let* ((wamei/project-ignored-files-include-dirs '(".claude"))
           (files (wamei/project-ignored-files (project-current nil root))))
      (should (member (expand-file-name ".claude/settings.local.json" root) files))
      ;; 入れ子リポジトリは git がディレクトリとして報告する。候補にしない
      (should-not (seq-find (lambda (f) (string-search "worktrees" f)) files)))
    (let ((wamei/project-ignored-files-include-dirs nil))
      (should-not (member (expand-file-name ".claude/settings.local.json" root)
                          (wamei/project-ignored-files (project-current nil root)))))))

(ert-deftest wamei/project-ignored-files-default-allows-claude-dir ()
  (should (member ".claude" wamei/project-ignored-files-include-dirs)))

(ert-deftest wamei/project-ignored-files-nil-outside-git ()
  "git 管理でないプロジェクトでは空。"
  (let* ((root (file-name-as-directory (make-temp-file "cpf-nogit-" t)))
         (project (cons 'transient root)))
    (unwind-protect
        (should-not (wamei/project-ignored-files project))
      (delete-directory root t))))

;;; consult ソース (ignored)

(ert-deftest wamei/consult-project-ignored-files-items-relative ()
  (wamei/consult-project-files-test--with-repo root
    (let ((items (wamei/consult-project-ignored-files--items)))
      (should (equal (cdr (assoc ".env.local" items))
                     (expand-file-name ".env.local" root)))
      (should-not (assoc "tracked.el" items)))))

(ert-deftest wamei/consult-project-ignored-files-keeps-open-excludes-recent ()
  "ignore 済みでも、開いているものは残し、recentf にあって開いていないものは落とす。"
  (wamei/consult-project-files-test--with-repo root
    (let ((buf (find-file-noselect (expand-file-name ".env.local" root)))
          (recentf-list (list (expand-file-name "ignored.log" root))))
      (unwind-protect
          (let ((names (mapcar #'car (wamei/consult-project-ignored-files--items))))
            (should (member ".env.local" names))
            (should-not (member "ignored.log" names)))
        (kill-buffer buf)))))

(ert-deftest wamei/consult-project-ignored-files-source-shape ()
  (should (equal (plist-get wamei/consult-source-project-ignored-files :category) 'file))
  (should (functionp (plist-get wamei/consult-source-project-ignored-files :items)))
  (should (functionp (plist-get wamei/consult-source-project-ignored-files :state))))

;;; project-find-file

(ert-deftest wamei/project-files-include-ignored-only-when-flagged ()
  "フラグが立っているときだけ project-files が ignore 対象も返す。"
  (wamei/consult-project-files-test--with-repo root
    (let ((project (project-current nil root)))
      (should-not (member (expand-file-name ".env.local" root) (project-files project)))
      (let ((wamei/project-files--include-ignored t))
        (should (member (expand-file-name ".env.local" root) (project-files project)))
        (should (member (expand-file-name "tracked.el" root) (project-files project)))))))

(ert-deftest wamei/project-files-include-ignored-respects-dirs ()
  "DIRS が指定されたらその配下の ignore 対象だけ足す。"
  (wamei/consult-project-files-test--with-repo root
    (let ((project (project-current nil root))
          (sub (expand-file-name "sub/" root)))
      (make-directory sub)
      (write-region "" nil (expand-file-name "a.local" sub))
      (let* ((wamei/project-files--include-ignored t)
             (files (project-files project (list sub))))
        (should (member (expand-file-name "a.local" sub) files))
        (should-not (member (expand-file-name ".env.local" root) files))))))

(ert-deftest wamei/project-files-include-ignored-relative-names ()
  "project-files-relative-names が立っていれば ignore 対象も相対パスで足す。
project-find-file は相対パスで受け取るので、絶対パスを混ぜると共通接頭辞が
無くなり、ignore 対象だけフルパスで表示されてしまう。"
  (wamei/consult-project-files-test--with-repo root
    (let* ((project (project-current nil root))
           (project-files-relative-names t)
           (wamei/project-files--include-ignored t)
           (files (project-files project)))
      (should (member ".env.local" files))
      (should (member "tracked.el" files))
      (should-not (seq-find #'file-name-absolute-p files)))))

(ert-deftest wamei/project-files-include-ignored-relative-names-in-dir ()
  "DIRS が 1 つなら、そのディレクトリからの相対パスにする (project-vc と同じ)。"
  (wamei/consult-project-files-test--with-repo root
    (let ((project (project-current nil root))
          (sub (expand-file-name "sub/" root)))
      (make-directory sub)
      (write-region "" nil (expand-file-name "a.local" sub))
      (let* ((project-files-relative-names t)
             (wamei/project-files--include-ignored t)
             (files (project-files project (list sub))))
        (should (member "a.local" files))
        (should-not (member ".env.local" files))
        (should-not (seq-find #'file-name-absolute-p files))))))

(ert-deftest wamei/project-find-file-is-advised ()
  (should (advice-member-p #'wamei/project-find-file--include-ignored 'project-find-file)))

;;; project-extra-files-test.el ends here
