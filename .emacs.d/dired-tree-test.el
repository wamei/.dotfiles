;;; dired-tree-test.el --- tests for dired-tree -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l dired-tree-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
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

(defun wamei/dired-tree-test--contents (file)
  "FILE の中身を文字列で返す。"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

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

(ert-deftest wamei/dired-tree-revert-keeps-window-point-on-subtree-line ()
  "sidebar は選択されていない window に出るので window-point が buffer point とずれる。
revert のあとも window-point が元の行に戻ること。

- 選択中の window は window-point が buffer point と一致してしまうので、
  ずれを作るために別 window (非選択) に出す。
- dired 標準の `dired-restore-positions' は window ごとの復元も持つが、
  subtree 行では `dired-goto-file' が効かず行番号にフォールバックする。
  行数が変わらない revert では偶然当たってしまうので、revert の契機
  (外部でのファイル追加) を作って行がずれる状況で確かめる。"
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (let ((win (split-window)))
        (unwind-protect
            (progn
              (set-window-buffer win buf)
              (should (wamei/dired-tree-expand-to (expand-file-name "a/b/c.txt" root)))
              (set-window-point win (point))
              (goto-char (point-min))
              ;; a より前に並ぶファイルが増えて、以降の行が 1 行下にずれる
              (write-region "" nil (expand-file-name "0.txt" root))
              (wamei/dired-tree-revert)
              (should (equal (save-excursion
                               (goto-char (window-point win))
                               (dired-utils-get-filename))
                             (expand-file-name "a/b/c.txt" root))))
          (delete-window win))))))

(ert-deftest wamei/dired-tree-revert-keeps-marks-on-subtree-lines ()
  "revert のあとも subtree 行のマークが残ること。
dired 標準の `dired-mark-remembered' は `dired-goto-file' で行を探すので
subtree 行のマークだけ落ちる。top-level の行は標準の復元で残る。"
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (should (wamei/dired-tree-expand-to (expand-file-name "a/b/c.txt" root)))
      (dired-mark 1)
      (dired-utils-goto-line (expand-file-name "e.txt" root))
      (dired-mark 1)
      (wamei/dired-tree-revert)
      (should (equal (sort (dired-get-marked-files nil nil nil nil t) #'string<)
                     (sort (list (expand-file-name "a/b/c.txt" root)
                                 (expand-file-name "e.txt" root))
                           #'string<))))))

(ert-deftest wamei/dired-tree-revert-keeps-marker-characters ()
  "`*' 以外のマーカー文字 (`D' など) もそのまま戻ること。"
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (should (wamei/dired-tree-expand-to (expand-file-name "a/b/c.txt" root)))
      (let ((dired-marker-char ?D))
        (dired-mark 1))
      (wamei/dired-tree-revert)
      (should (dired-utils-goto-line (expand-file-name "a/b/c.txt" root)))
      (should (eq (char-after (line-beginning-position)) ?D)))))

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

(ert-deftest wamei/dired-tree-disable-cancels-pending-revert ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (wamei/dired-tree-expand-to (expand-file-name "a/b/c.txt" root))
      (wamei/dired-tree--schedule-revert (current-buffer))
      (should (timerp wamei/dired-tree--revert-timer))
      (wamei/dired-tree-mode -1)
      (should-not wamei/dired-tree--revert-timer)
      (should (= (hash-table-count wamei/dired-tree--watches) 0)))))

;;; drop 先

(ert-deftest wamei/dired-tree-drop-target-prefers-directory-line ()
  (should (equal (wamei/dired-tree--drop-target t "/r/a" "/r/" "/r/") "/r/a/"))
  (should (equal (wamei/dired-tree--drop-target nil "/r/a/x.txt" "/r/a" "/r/") "/r/a/"))
  (should (equal (wamei/dired-tree--drop-target nil nil nil "/r/") "/r/")))

(ert-deftest wamei/dired-tree-drop-destination-keeps-basename ()
  (should (equal (wamei/dired-tree--drop-destination "/src/x.txt" "/r/a/") "/r/a/x.txt"))
  (should (equal (wamei/dired-tree--drop-destination "/src/dir/" "/r/a/") "/r/a/dir")))

(ert-deftest wamei/dired-tree-resolve-action-maps-private-to-default ()
  (let ((wamei/dired-tree-drop-action 'move))
    (should (eq (wamei/dired-tree--resolve-action 'private) 'move))
    (should (eq (wamei/dired-tree--resolve-action 'copy) 'move))
    (should (eq (wamei/dired-tree--resolve-action 'link) 'link))))

(ert-deftest wamei/dired-tree-dnd-moves-file-into-subtree-directory ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (wamei/dired-tree-expand-to (expand-file-name "a/d.txt" root))
      ;; point は a/d.txt の行。落下先はその親 a/
      (let ((wamei/dired-tree-drop-action 'move))
        (wamei/dired-tree-dnd-handle-file
         (concat "file://" (expand-file-name "e.txt" root)) 'private))
      (should (file-exists-p (expand-file-name "a/e.txt" root)))
      (should-not (file-exists-p (expand-file-name "e.txt" root))))))

(ert-deftest wamei/dired-tree-dnd-copies-file-when-action-is-copy ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (wamei/dired-tree-expand-to (expand-file-name "a/d.txt" root))
      (let ((wamei/dired-tree-drop-action 'copy))
        (wamei/dired-tree-dnd-handle-file
         (concat "file://" (expand-file-name "e.txt" root)) 'private))
      (should (file-exists-p (expand-file-name "a/e.txt" root)))
      (should (file-exists-p (expand-file-name "e.txt" root))))))

(ert-deftest wamei/dired-tree-dnd-links-file-when-action-is-link ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (wamei/dired-tree-expand-to (expand-file-name "a/d.txt" root))
      (wamei/dired-tree-dnd-handle-file
       (concat "file://" (expand-file-name "e.txt" root)) 'link)
      (should (file-symlink-p (expand-file-name "a/e.txt" root)))
      (should (equal (file-truename (expand-file-name "a/e.txt" root))
                     (file-truename (expand-file-name "e.txt" root))))
      (should (file-exists-p (expand-file-name "e.txt" root))))))

(ert-deftest wamei/dired-tree-dnd-declined-overwrite-keeps-both-files ()
  (wamei/dired-tree-test--with-tree root
    (write-region "dest" nil (expand-file-name "a/e.txt" root))
    (write-region "src" nil (expand-file-name "e.txt" root))
    (wamei/dired-tree-test--with-dired root buf
      (wamei/dired-tree-expand-to (expand-file-name "a/d.txt" root))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (wamei/dired-tree-dnd-handle-file
         (concat "file://" (expand-file-name "e.txt" root)) 'private))
      (should (file-exists-p (expand-file-name "e.txt" root)))
      (should (equal (wamei/dired-tree-test--contents (expand-file-name "e.txt" root)) "src"))
      (should (equal (wamei/dired-tree-test--contents (expand-file-name "a/e.txt" root))
                     "dest")))))

(ert-deftest wamei/dired-tree-dnd-drop-on-self-is-noop ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      ;; point は top の e.txt 自身の行。落下先は top なので移動元と同じ
      (should (dired-utils-goto-line (expand-file-name "e.txt" root)))
      (wamei/dired-tree-dnd-handle-file
       (concat "file://" (expand-file-name "e.txt" root)) 'private)
      (should (file-exists-p (expand-file-name "e.txt" root))))))

(ert-deftest wamei/dired-tree-dnd-moves-every-file-of-a-multi-file-drop ()
  (should (get 'wamei/dired-tree-dnd-handle-file 'dnd-multiple-handler))
  (wamei/dired-tree-test--with-tree root
    (write-region "" nil (expand-file-name "f.txt" root))
    (wamei/dired-tree-test--with-dired root buf
      (wamei/dired-tree-expand-to (expand-file-name "a/d.txt" root))
      (let ((wamei/dired-tree-drop-action 'move))
        (wamei/dired-tree-dnd-handle-file
         (list (concat "file://" (expand-file-name "e.txt" root))
               (concat "file://" (expand-file-name "f.txt" root)))
         'private))
      (should (file-exists-p (expand-file-name "a/e.txt" root)))
      (should (file-exists-p (expand-file-name "a/f.txt" root)))
      (should-not (file-exists-p (expand-file-name "e.txt" root)))
      (should-not (file-exists-p (expand-file-name "f.txt" root))))))

(provide 'dired-tree-test)
;;; dired-tree-test.el ends here
