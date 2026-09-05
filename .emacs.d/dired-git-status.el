;;; dired-git-status.el --- dired のファイル名に git の状態で色を付ける -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; treemacs-git-mode の代替。プロジェクトルートで `git status --porcelain=v1 -z'
;; を非同期に取り、path → state の表にして、変更を含むディレクトリにも伝播させ、
;; dired の各行 (dired-subtree の展開行を含む) のファイル名に overlay で face を当てる。
;;
;; - パース (`wamei/dired-git-status--parse') と伝播 (`--propagate') は純関数
;; - 取得はルート単位で 1 回。結果は `wamei/dired-git-status--cache' に置き、
;;   同じルートを見る全 dired バッファに配る
;; - 更新契機: バッファ表示 / revert / magit の refresh / dired-tree の監視
;;
;;; Code:

(require 'dired)
(require 'project)
(require 'subr-x)

;;; face

(defgroup wamei/dired-git-status nil "dired のファイル名を git の状態で色分けする。" :group 'dired)

(defface wamei/dired-git-status-modified '((t (:foreground "#e5c07b")))
  "変更されたファイル、または変更を含むディレクトリ。")
(defface wamei/dired-git-status-added '((t (:foreground "#98c379")))
  "index に追加されたファイル。")
(defface wamei/dired-git-status-untracked '((t (:foreground "#7ec699")))
  "未追跡のファイル。")
(defface wamei/dired-git-status-renamed '((t (:foreground "#61afef")))
  "リネームされたファイル。")
(defface wamei/dired-git-status-conflict '((t (:foreground "#e06c75" :weight bold)))
  "コンフリクト中のファイル、またはそれを含むディレクトリ。")

(defconst wamei/dired-git-status--faces
  '((modified . wamei/dired-git-status-modified)
    (added . wamei/dired-git-status-added)
    (untracked . wamei/dired-git-status-untracked)
    (renamed . wamei/dired-git-status-renamed)
    (conflict . wamei/dired-git-status-conflict)))

;;; パース (純関数)

(defun wamei/dired-git-status--code-to-state (xy)
  "porcelain の 2 文字 XY を state に直す。無視するもの (!!) は nil。"
  (let ((x (aref xy 0)) (y (aref xy 1)))
    (cond ((and (eq x ??) (eq y ??)) 'untracked)
          ((and (eq x ?!) (eq y ?!)) nil)
          ((or (eq x ?U) (eq y ?U) (and (eq x ?A) (eq y ?A)) (and (eq x ?D) (eq y ?D))) 'conflict)
          ((memq x '(?R ?C)) 'renamed)
          ((eq x ?A) 'added)
          (t 'modified))))

(defun wamei/dired-git-status--parse (output root)
  "`git status --porcelain=v1 -z' の OUTPUT を ROOT からの絶対パス → state の hash にする。
エントリは \"XY path\\0\"、リネームは \"XY new\\0old\\0\"。"
  (let ((table (make-hash-table :test 'equal))
        (root (file-name-as-directory (expand-file-name root)))
        (fields (split-string output "\0" t)))
    (while fields
      (let* ((entry (pop fields))
             (xy (substring entry 0 2))
             (path (substring entry 3))
             (state (wamei/dired-git-status--code-to-state xy)))
        (when (memq (aref xy 0) '(?R ?C))
          (pop fields))                 ; 旧パスは捨てる
        (when state
          (puthash (directory-file-name (concat root path)) state table))))
    table))

(defun wamei/dired-git-status--propagate (table root)
  "TABLE の各パスの祖先 (ROOT 自身は除く) に状態を足した新しい hash。
子に conflict があれば conflict、それ以外は modified。"
  (let ((out (copy-hash-table table))
        (root (directory-file-name (expand-file-name root))))
    (maphash
     (lambda (path state)
       (let ((dir (directory-file-name (file-name-directory path)))
             (mark (if (eq state 'conflict) 'conflict 'modified)))
         (while (and (not (equal dir root))
                     (string-prefix-p (concat root "/") (concat dir "/")))
           (unless (eq (gethash dir out) 'conflict)
             (puthash dir mark out))
           (setq dir (directory-file-name (file-name-directory dir))))))
     table)
    out))

(provide 'dired-git-status)
;;; dired-git-status.el ends here
