;;; dired-tree.el --- dired-subtree を木として扱う -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; dired-subtree でディレクトリを展開する dired バッファに共通する振る舞い。
;;
;; 1. 展開記憶 (`wamei/dired-tree--expanded')
;;    dired-subtree-remove は範囲を削除して中の overlay をまとめて捨てるので、
;;    親を閉じると子孫の展開状態が消える。展開中ディレクトリの集合をバッファ
;;    ローカルに持ち、展開時に子孫を開き直す。閉じたときは自身だけ忘れる。
;;
;; 2. 展開ディレクトリの監視 (Task 5)
;; 3. path までの展開 (Task 4)
;; 4. D&D の drop 先 (Task 6)
;;
;;; Code:

(require 'dired)
(require 'dired-subtree)
(require 'dired-hacks-utils)
(require 'seq)

;;; 展開記憶

(defvar-local wamei/dired-tree--expanded nil
  "展開中 (または閉じた親の下で展開したまま) のディレクトリ。絶対パス、末尾 / なし。")

(defun wamei/dired-tree--normalize (dir)
  "DIR を絶対パス・末尾 / なしに正規化する。"
  (directory-file-name (expand-file-name dir)))

(defun wamei/dired-tree--expanded-add (expanded dir)
  "EXPANDED に DIR を加えた新しいリスト。既にあればそのまま。"
  (let ((d (wamei/dired-tree--normalize dir)))
    (if (member d expanded) expanded (cons d expanded))))

(defun wamei/dired-tree--expanded-remove (expanded dir)
  "EXPANDED から DIR だけを外した新しいリスト。子孫は残す。"
  (remove (wamei/dired-tree--normalize dir) expanded))

(defun wamei/dired-tree--children-to-reopen (expanded children)
  "CHILDREN (絶対パス) のうち EXPANDED に入っているものを出現順で返す。"
  (seq-filter (lambda (c) (member (wamei/dired-tree--normalize c) expanded))
              children))

(defun wamei/dired-tree--subdirs-in (ov)
  "subtree overlay OV の範囲にあるディレクトリ行の絶対パス。"
  (let (dirs)
    (save-excursion
      (goto-char (overlay-start ov))
      (while (< (point) (overlay-end ov))
        (when (and (dired-subtree--dired-line-is-directory-or-link-p)
                   (dired-utils-get-filename))
          (push (dired-utils-get-filename) dirs))
        (forward-line 1)))
    (nreverse dirs)))

(defun wamei/dired-tree--after-insert ()
  "`dired-subtree-after-insert-hook' 用。展開を記憶し、覚えている子孫を開き直す。"
  (when wamei/dired-tree-mode
    (when-let* ((ov (dired-subtree--get-ov)))
      (let ((dir (overlay-get ov 'dired-subtree-name)))
        (setq wamei/dired-tree--expanded
              (wamei/dired-tree--expanded-add wamei/dired-tree--expanded dir))
        (dolist (child (wamei/dired-tree--children-to-reopen
                        wamei/dired-tree--expanded
                        (wamei/dired-tree--subdirs-in ov)))
          (save-excursion
            (when (and (dired-utils-goto-line child)
                       (not (dired-subtree--is-expanded-p)))
              (dired-subtree-insert))))))))

(defun wamei/dired-tree--before-remove (&rest _)
  "`dired-subtree-remove' の :before advice。閉じるディレクトリ自身だけ忘れる。"
  (when wamei/dired-tree-mode
    (when-let* ((ov (dired-subtree--get-ov)))
      (setq wamei/dired-tree--expanded
            (wamei/dired-tree--expanded-remove
             wamei/dired-tree--expanded (overlay-get ov 'dired-subtree-name))))))

;;; minor mode

(define-minor-mode wamei/dired-tree-mode
  "dired-subtree の展開を記憶し、展開ディレクトリを監視し、D&D の落下先を行から決める。"
  :lighter nil
  (if wamei/dired-tree-mode
      (progn
        (add-hook 'dired-subtree-after-insert-hook #'wamei/dired-tree--after-insert nil t)
        (advice-add 'dired-subtree-remove :before #'wamei/dired-tree--before-remove))
    (remove-hook 'dired-subtree-after-insert-hook #'wamei/dired-tree--after-insert t)))

(provide 'dired-tree)
;;; dired-tree.el ends here
