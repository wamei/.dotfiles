;;; dired-tree-test.el --- tests for dired-tree -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l dired-tree-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(package-initialize)
(require 'dired-subtree)
(load (expand-file-name "dired-tree.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defmacro wamei/dired-tree-test--with-tree (var &rest body)
  "一時ディレクトリに a/b/c.txt と a/d.txt と e.txt を作り、VAR に束縛して BODY を評価する。"
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "dired-tree-" t))))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "a/b" ,var) t)
           (write-region "" nil (expand-file-name "a/b/c.txt" ,var))
           (write-region "" nil (expand-file-name "a/d.txt" ,var))
           (write-region "" nil (expand-file-name "e.txt" ,var))
           ,@body)
       (delete-directory ,var t))))

(defmacro wamei/dired-tree-test--with-dired (root var &rest body)
  "ROOT の dired バッファ (wamei/dired-tree-mode 有効) を VAR に束縛して BODY を評価し、後で kill する。"
  (declare (indent 2))
  `(let ((,var (dired-noselect ,root)))
     (unwind-protect
         (with-current-buffer ,var
           (wamei/dired-tree-mode 1)
           ,@body)
       (kill-buffer ,var))))

;;; 展開記憶 (純関数)

(ert-deftest wamei/dired-tree-normalize-strips-trailing-slash ()
  (should (equal (wamei/dired-tree--normalize "/tmp/x/") "/tmp/x"))
  (should (equal (wamei/dired-tree--normalize "/tmp/x") "/tmp/x")))

(ert-deftest wamei/dired-tree-expanded-add-is-idempotent ()
  (let ((e (wamei/dired-tree--expanded-add nil "/tmp/x/")))
    (should (equal e '("/tmp/x")))
    (should (equal (wamei/dired-tree--expanded-add e "/tmp/x") e))))

(ert-deftest wamei/dired-tree-expanded-remove-keeps-descendants ()
  (let ((e '("/tmp/x/y" "/tmp/x" "/tmp/z")))
    (should (equal (wamei/dired-tree--expanded-remove e "/tmp/x/")
                   '("/tmp/x/y" "/tmp/z")))))

(ert-deftest wamei/dired-tree-children-to-reopen-keeps-order ()
  (should (equal (wamei/dired-tree--children-to-reopen
                  '("/tmp/x/c" "/tmp/x/a")
                  '("/tmp/x/a" "/tmp/x/b" "/tmp/x/c"))
                 '("/tmp/x/a" "/tmp/x/c"))))

;;; 展開記憶 (dired バッファ)

(ert-deftest wamei/dired-tree-insert-records-and-remove-forgets-only-self ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-insert)
      (dired-utils-goto-line (expand-file-name "a/b" root))
      (dired-subtree-insert)
      (should (equal (sort (copy-sequence wamei/dired-tree--expanded) #'string<)
                     (list (expand-file-name "a" root) (expand-file-name "a/b" root))))
      ;; 親 a を閉じる
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-toggle)
      (should (equal wamei/dired-tree--expanded (list (expand-file-name "a/b" root)))))))

(ert-deftest wamei/dired-tree-reinsert-reopens-remembered-children ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-insert)
      (dired-utils-goto-line (expand-file-name "a/b" root))
      (dired-subtree-insert)
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-toggle)
      (should-not (dired-utils-goto-line (expand-file-name "a/b/c.txt" root)))
      ;; もう一度開くと a/b も開いた状態で戻る
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-insert)
      (should (dired-utils-goto-line (expand-file-name "a/b/c.txt" root))))))

;;; 祖先と展開

(ert-deftest wamei/dired-tree-inside-p ()
  (should (wamei/dired-tree--inside-p "/tmp/r/" "/tmp/r/a/b.txt"))
  (should-not (wamei/dired-tree--inside-p "/tmp/r" "/tmp/r"))
  (should-not (wamei/dired-tree--inside-p "/tmp/r" "/tmp/rx/a.txt"))
  (should-not (wamei/dired-tree--inside-p "/tmp/r" "/tmp/other/a.txt")))

(ert-deftest wamei/dired-tree-ancestors-lists-dirs-below-root ()
  (should (equal (wamei/dired-tree--ancestors "/tmp/r/" "/tmp/r/a/b/c.txt")
                 '("/tmp/r/a" "/tmp/r/a/b")))
  (should (equal (wamei/dired-tree--ancestors "/tmp/r" "/tmp/r/e.txt") nil)))

(ert-deftest wamei/dired-tree-expand-to-opens-ancestors-and-lands-on-file ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (should (wamei/dired-tree-expand-to (expand-file-name "a/b/c.txt" root)))
      (should (equal (dired-utils-get-filename) (expand-file-name "a/b/c.txt" root)))
      (should (equal (sort (copy-sequence wamei/dired-tree--expanded) #'string<)
                     (list (expand-file-name "a" root) (expand-file-name "a/b" root)))))))

(ert-deftest wamei/dired-tree-expand-to-returns-nil-outside-root ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (should-not (wamei/dired-tree-expand-to "/etc/hosts")))))

(ert-deftest wamei/dired-tree-expand-to-lands-on-directory-without-expanding-it ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (should (equal (wamei/dired-tree--ancestors
                       root (file-name-as-directory (expand-file-name "a/b" root)))
                     (list (expand-file-name "a" root))))
      (should (wamei/dired-tree-expand-to
               (file-name-as-directory (expand-file-name "a/b" root))))
      (should (equal (dired-utils-get-filename) (expand-file-name "a/b" root)))
      (should-not (dired-subtree--is-expanded-p)))))

(ert-deftest wamei/dired-tree-expand-to-returns-nil-and-keeps-point-when-ancestor-missing ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (goto-char (point-min))
      (let ((pos (point)))
        (should-not (wamei/dired-tree-expand-to (expand-file-name "zz/y.txt" root)))
        (should (equal (point) pos))))))

(ert-deftest wamei/dired-tree-revert-keeps-point-on-subtree-line ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (wamei/dired-tree-expand-to (expand-file-name "a/b/c.txt" root))
      (wamei/dired-tree-revert)
      (should (equal (dired-utils-get-filename) (expand-file-name "a/b/c.txt" root))))))

;;; 監視

(ert-deftest wamei/dired-tree-watch-diff ()
  (let ((diff (wamei/dired-tree--watch-diff '("/a" "/b") '("/b" "/c"))))
    (should (equal (car diff) '("/c")))
    (should (equal (cdr diff) '("/a")))))

(ert-deftest wamei/dired-tree-visible-expanded-follows-overlays ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (wamei/dired-tree-expand-to (expand-file-name "a/b/c.txt" root))
      (should (equal (sort (wamei/dired-tree--visible-expanded) #'string<)
                     (list (expand-file-name "a" root) (expand-file-name "a/b" root))))
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-toggle)
      (should-not (wamei/dired-tree--visible-expanded)))))

(ert-deftest wamei/dired-tree-reconcile-registers-watch-per-visible-dir ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (wamei/dired-tree-expand-to (expand-file-name "a/b/c.txt" root))
      (should (= (hash-table-count wamei/dired-tree--watches) 2))
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-toggle)
      (should (= (hash-table-count wamei/dired-tree--watches) 0)))))

(provide 'dired-tree-test)
;;; dired-tree-test.el ends here
