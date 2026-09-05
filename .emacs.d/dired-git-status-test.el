;;; dired-git-status-test.el --- tests for dired-git-status -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l dired-git-status-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(package-initialize)
(load (expand-file-name "dired-git-status.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun wamei/dired-git-status-test--alist (table)
  "TABLE をソート済み alist にする。"
  (let (acc)
    (maphash (lambda (k v) (push (cons k v) acc)) table)
    (sort acc (lambda (a b) (string< (car a) (car b))))))

;;; XY → state

(ert-deftest wamei/dired-git-status-code-to-state ()
  (should (eq (wamei/dired-git-status--code-to-state "??") 'untracked))
  (should (eq (wamei/dired-git-status--code-to-state " M") 'modified))
  (should (eq (wamei/dired-git-status--code-to-state "M ") 'modified))
  (should (eq (wamei/dired-git-status--code-to-state " D") 'modified))
  (should (eq (wamei/dired-git-status--code-to-state "A ") 'added))
  (should (eq (wamei/dired-git-status--code-to-state "AM") 'added))
  (should (eq (wamei/dired-git-status--code-to-state "R ") 'renamed))
  (should (eq (wamei/dired-git-status--code-to-state "UU") 'conflict))
  (should (eq (wamei/dired-git-status--code-to-state "AA") 'conflict))
  (should-not (wamei/dired-git-status--code-to-state "!!")))

;;; パース

(ert-deftest wamei/dired-git-status-parse-handles-z-separated-entries ()
  (let ((table (wamei/dired-git-status--parse
                (concat " M src/a.el\0?? new.txt\0A  b.el\0")
                "/r/")))
    (should (equal (wamei/dired-git-status-test--alist table)
                   '(("/r/b.el" . added)
                     ("/r/new.txt" . untracked)
                     ("/r/src/a.el" . modified))))))

(ert-deftest wamei/dired-git-status-parse-rename-uses-new-path ()
  ;; -z ではリネームは "R  新\0旧\0" の順で来る
  (let ((table (wamei/dired-git-status--parse "R  new.el\0old.el\0" "/r")))
    (should (equal (wamei/dired-git-status-test--alist table)
                   '(("/r/new.el" . renamed))))))

(ert-deftest wamei/dired-git-status-parse-ignores-empty-output ()
  (should (= (hash-table-count (wamei/dired-git-status--parse "" "/r")) 0)))

;;; 伝播

(ert-deftest wamei/dired-git-status-propagate-marks-ancestors-modified ()
  (let* ((table (wamei/dired-git-status--parse " M src/deep/a.el\0" "/r"))
         (out (wamei/dired-git-status--propagate table "/r")))
    (should (equal (wamei/dired-git-status-test--alist out)
                   '(("/r/src" . modified)
                     ("/r/src/deep" . modified)
                     ("/r/src/deep/a.el" . modified))))))

(ert-deftest wamei/dired-git-status-propagate-conflict-wins ()
  (let* ((table (wamei/dired-git-status--parse " M src/a.el\0UU src/b.el\0" "/r"))
         (out (wamei/dired-git-status--propagate table "/r")))
    (should (eq (gethash "/r/src" out) 'conflict))))

(ert-deftest wamei/dired-git-status-propagate-does-not-mark-root ()
  (let* ((table (wamei/dired-git-status--parse "?? x.txt\0" "/r"))
         (out (wamei/dired-git-status--propagate table "/r")))
    (should-not (gethash "/r" out))))

(provide 'dired-git-status-test)
;;; dired-git-status-test.el ends here
