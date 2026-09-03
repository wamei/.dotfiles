;;; project-extra-files.el --- project の候補に未訪問・ignore 済みのファイルを足す -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; project.el と consult の候補から漏れる 2 種類のファイルを補う。
;;
;; 1. 未訪問のファイル (`wamei/consult-source-project-files')
;;    consult の "Project File" ソース (`consult-source-project-recent-file') は
;;    名前に反して recentf の中身しか見ないため、一度も開いていないファイルは
;;    `consult-project-buffer' (C-x p b) に出てこない。`project-files' を候補源に
;;    するソースを足す。project-vc は既定で git の untracked も列挙する
;;    (`project-vc-include-untracked') ので、まだ add していない新規ファイルも
;;    候補に入る。
;;
;; 2. ignore 済みのファイル (`wamei/project-ignored-files')
;;    .env.local や *.local.json のように .gitignore で外しているが日常的に開く
;;    ファイルは、project-files にも 1. にも出ない。`git ls-files -o -i` で
;;    ignore 対象を列挙し、consult には "Project Ignored" ソースとして、
;;    `project-find-file' には候補への追加として足す。node_modules や .next の
;;    ように丸ごと ignore されたディレクトリは依存物か生成物なので中に降りない
;;    (`--directory' で畳む)。例外は `wamei/project-ignored-files-include-dirs'。
;;
;; どのソースも、既に開いているものと recentf にあるものは他のソースが出すので
;; 除いて重複を避ける。
;;
;; project-find-file への追加は `project-files' を直接いじらず、
;; `project-find-file' の実行中だけフラグを立てて advice で足す。project-files
;; は project-find-regexp や project-search など多くのコマンドが共有しており、
;; そちらまで ignore 対象が混ざるのは望ましくないため。
;;
;;; Code:

(require 'project)
(require 'seq)

;; consult は起動時には読まない。関数は実行時にだけ呼ばれる。
(declare-function consult--project-root "consult")
(declare-function consult--buffer-file-hash "consult")
(declare-function consult--file-state "consult")
(declare-function consult--file-action "consult")
(defvar consult-project-function)

;;; 共通

(defun wamei/project-extra-files--visited-hash ()
  "開いているバッファのファイルと recentf のファイルを絶対パスで持つハッシュ。"
  (let ((table (consult--buffer-file-hash)))
    (dolist (file (bound-and-true-p recentf-list) table)
      (puthash (expand-file-name file) t table))))

(defun wamei/project-extra-files--relative-items (root files)
  "ROOT 配下で未訪問の FILES を (相対パス . 絶対パス) のリストにする。"
  (let ((len (length root))
        (visited (wamei/project-extra-files--visited-hash))
        items)
    (dolist (file files (nreverse items))
      (let ((file (expand-file-name file)))
        (when (and (string-prefix-p root file)
                   (not (gethash file visited)))
          (push (cons (substring file len) file) items))))))

;;; 未訪問のファイル

(defun wamei/consult-project-files--items ()
  "現在のプロジェクトのうち、開いても recentf にも無いファイルを返す。
各要素は (相対パス . 絶対パス)。プロジェクト外なら nil。"
  (when-let* ((root (consult--project-root))
              (project (project-current nil root)))
    (wamei/project-extra-files--relative-items root (project-files project))))

(defvar wamei/consult-source-project-files
  `( :name     "Project Files"
     :narrow   ?a
     :category file
     :face     consult-file
     :history  file-name-history
     :state    ,#'consult--file-state
     :new
     ,(lambda (file)
        (consult--file-action
         (expand-file-name file (consult--project-root))))
     :enabled  ,(lambda () consult-project-function)
     :items    ,#'wamei/consult-project-files--items)
  "`consult-project-buffer' 用の、未訪問のプロジェクトファイルのソース。")

;;; ignore 済みのファイル

(defvar wamei/project-ignored-files-include-dirs '(".claude" ".vscode" ".superpowers")
  "ディレクトリ丸ごと ignore されていても中のファイルを候補に入れるディレクトリ名。
node_modules や .next のように丸ごと ignore されたディレクトリは依存物か生成物で、
開きたいものは無いのが普通なので、既定では中に降りない。ここに挙げた名前
(ルートからの相対パスではなくディレクトリ名) だけ例外として列挙する。")

(defun wamei/project-ignored-files--git-ls (root &rest args)
  "ROOT で git ls-files -z -o -i --exclude-standard ARGS を実行し、相対パスを返す。
git 管理外や git が無いときは nil。"
  (with-temp-buffer
    (let ((default-directory root))
      (when (ignore-errors
              (zerop (apply #'process-file "git" nil '(t nil) nil
                            "ls-files" "-z" "-o" "-i" "--exclude-standard" args)))
        (split-string (buffer-string) "\0" t)))))

(defun wamei/project-ignored-files (project)
  "PROJECT で git に ignore されているファイルを絶対パスで返す。
丸ごと ignore されたディレクトリは --directory で 1 エントリに畳まれるので中を
走査しない。`wamei/project-ignored-files-include-dirs' にある名前のものだけ、
改めてそのディレクトリを対象に列挙する。git 管理でなければ nil。"
  (let ((root (project-root project))
        files)
    (dolist (entry (wamei/project-ignored-files--git-ls root "--directory")
                   (nreverse files))
      (if (not (string-suffix-p "/" entry))
          (push (expand-file-name entry root) files)
        (when (member (file-name-nondirectory (directory-file-name entry))
                      wamei/project-ignored-files-include-dirs)
          (dolist (file (wamei/project-ignored-files--git-ls root "--" entry))
            ;; 入れ子の git リポジトリ (worktree 等) はここでもディレクトリとして出る
            (unless (string-suffix-p "/" file)
              (push (expand-file-name file root) files))))))))

(defun wamei/consult-project-ignored-files--items ()
  "現在のプロジェクトで ignore されていて、開いても recentf にも無いファイル。"
  (when-let* ((root (consult--project-root))
              (project (project-current nil root)))
    (wamei/project-extra-files--relative-items
     root (wamei/project-ignored-files project))))

(defvar wamei/consult-source-project-ignored-files
  `( :name     "Project Ignored"
     :narrow   ?i
     :category file
     :face     consult-file
     :history  file-name-history
     :state    ,#'consult--file-state
     :enabled  ,(lambda () consult-project-function)
     :items    ,#'wamei/consult-project-ignored-files--items)
  "`consult-project-buffer' 用の、git に ignore されたファイルのソース。")

;;; project-find-file

(defvar wamei/project-files--include-ignored nil
  "non-nil のとき `project-files' が ignore 対象も返す。
`project-find-file' の実行中だけ動的に束縛する。")

(defun wamei/project-files--with-ignored (fn project &optional dirs)
  "`project-files' の :around advice。フラグが立っていれば ignore 対象を足す。
DIRS があればその配下のものだけ足す。`project-files-relative-names' が non-nil
なら project-vc と同じく、DIRS の唯一の要素 (無ければルート) からの相対パスに
する。project-find-file は相対パスで受け取るので、絶対パスを混ぜると共通接頭辞
が消えて ignore 対象だけフルパスで表示されてしまう。"
  (let ((files (funcall fn project dirs)))
    (if (not wamei/project-files--include-ignored)
        files
      (let* ((dirs (mapcar (lambda (dir) (file-name-as-directory (expand-file-name dir)))
                           dirs))
             (ignored (wamei/project-ignored-files project))
             (base (and project-files-relative-names
                        (<= (length dirs) 1)
                        (or (car dirs) (project-root project)))))
        (when dirs
          (setq ignored
                (seq-filter (lambda (file)
                              (seq-some (lambda (dir) (string-prefix-p dir file)) dirs))
                            ignored)))
        (when base
          (setq ignored (mapcar (lambda (file) (file-relative-name file base)) ignored)))
        (append files ignored)))))

(defun wamei/project-find-file--include-ignored (fn &rest args)
  "`project-find-file' の :around advice。実行中は ignore 対象も候補に入れる。"
  (let ((wamei/project-files--include-ignored t))
    (apply fn args)))

(advice-add 'project-files :around #'wamei/project-files--with-ignored)
(advice-add 'project-find-file :around #'wamei/project-find-file--include-ignored)

(provide 'project-extra-files)
;;; project-extra-files.el ends here
