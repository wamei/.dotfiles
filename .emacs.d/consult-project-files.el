;;; consult-project-files.el --- consult-project-buffer に未訪問のファイルを足す -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; consult の "Project File" ソース (`consult-source-project-recent-file') は
;; 名前に反して recentf の中身しか見ないため、一度も開いていないファイルは
;; `consult-project-buffer' (C-x p b) に出てこない。
;;
;; ここでは `project-files' を候補源にするソースを足す。project-vc は既定で
;; git の untracked も列挙する (`project-vc-include-untracked') ので、まだ
;; add していない新規ファイルも候補に入る。既に開いているものと recentf に
;; あるものは他のソースが出すので除き、重複を避ける。
;;
;;; Code:

(require 'project)
(require 'consult)

(defun wamei/consult-project-files--items ()
  "現在のプロジェクトのうち、開いても recentf にも無いファイルを返す。
各要素は (相対パス . 絶対パス)。プロジェクト外なら nil。"
  (when-let* ((root (consult--project-root))
              (project (project-current nil root)))
    (let ((len (length root))
          (open (consult--buffer-file-hash))
          (recent (make-hash-table :test #'equal))
          items)
      (dolist (file (bound-and-true-p recentf-list))
        (puthash (expand-file-name file) t recent))
      (dolist (file (project-files project) (nreverse items))
        (let ((file (expand-file-name file)))
          (when (and (string-prefix-p root file)
                     (not (gethash file open))
                     (not (gethash file recent)))
            (push (cons (substring file len) file) items)))))))

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

(provide 'consult-project-files)
;;; consult-project-files.el ends here
