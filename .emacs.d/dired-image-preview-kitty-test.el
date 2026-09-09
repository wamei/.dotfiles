;;; dired-image-preview-kitty-test.el --- tests for dired-image-preview-kitty -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l dired-image-preview-kitty-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(package-initialize)
(require 'dired)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "kitty-graphics.el" dir) nil t)
  (load (expand-file-name "dired-image-preview.el" dir) nil t)
  (load (expand-file-name "dired-image-preview-kitty.el" dir) nil t))

;;; 位置

(ert-deftest wamei/dired-image-preview-kitty-frame-position-below-right-of-anchor ()
  "アンカー (桁 10 . 行 5) の右下に gap (2 . 1) と枠 1 を空けて置く。"
  (should (equal (wamei/dired-image-preview-kitty--frame-position '(10 . 5) '(20 . 8) '(200 . 50) '(2 . 1))
                 '(13 . 8))))

(ert-deftest wamei/dired-image-preview-kitty-frame-position-flips-above-when-no-room-below ()
  "下に収まらなければアンカーの上に置く (枠込みで)。"
  (should (equal (wamei/dired-image-preview-kitty--frame-position '(10 . 45) '(20 . 8) '(200 . 50) '(2 . 1))
                 '(13 . 35))))

(ert-deftest wamei/dired-image-preview-kitty-frame-position-clamps-to-right-edge ()
  (should (equal (wamei/dired-image-preview-kitty--frame-position '(190 . 5) '(20 . 8) '(200 . 50) '(2 . 1))
                 '(179 . 8))))

;;; child frame

(ert-deftest wamei/dired-image-preview-kitty-make-frame-keeps-selected-frame ()
  "tty では `make-frame' が作った child frame を選択してしまう (GUI では選択しない)。
そのままだと dired の window が非選択になり、mode-line が非アクティブになって
カーソルも消え、最初のキーがプレビュー側へ行く。作成後に元のフレームへ戻す。"
  (let* ((parent 'parent-frame)
         (child 'child-frame)
         (selected parent)
         (select-calls nil))
    (cl-letf (((symbol-function 'selected-frame) (lambda () selected))
              ;; tty の make-frame と同じく、作った frame を選択する
              ((symbol-function 'make-frame) (lambda (_params) (setq selected child) child))
              ((symbol-function 'select-frame)
               (lambda (frame &optional norecord)
                 (push (list frame norecord) select-calls)
                 (setq selected frame)))
              ((symbol-function 'frame-root-window) (lambda (_f) 'window))
              ((symbol-function 'set-window-buffer) #'ignore)
              ((symbol-function 'set-window-dedicated-p) #'ignore)
              ((symbol-function 'set-window-parameter) #'ignore))
      (should (eq (wamei/dired-image-preview-kitty--make-frame parent 'buffer '(1 . 2) '(3 . 4))
                  child))
      (should (eq selected parent))
      (should (equal select-calls (list (list parent 'norecord)))))))

(ert-deftest wamei/dired-image-preview-kitty-make-frame-does-not-reselect-when-unchanged ()
  "選択が動かない環境 (GUI) では `select-frame' を呼ばない。"
  (let* ((parent 'parent-frame)
         (select-calls nil))
    (cl-letf (((symbol-function 'selected-frame) (lambda () parent))
              ((symbol-function 'make-frame) (lambda (_params) 'child-frame))
              ((symbol-function 'select-frame)
               (lambda (frame &optional norecord) (push (list frame norecord) select-calls)))
              ((symbol-function 'frame-root-window) (lambda (_f) 'window))
              ((symbol-function 'set-window-buffer) #'ignore)
              ((symbol-function 'set-window-dedicated-p) #'ignore)
              ((symbol-function 'set-window-parameter) #'ignore))
      (wamei/dired-image-preview-kitty--make-frame parent 'buffer '(1 . 2) '(3 . 4))
      (should-not select-calls))))

(provide 'dired-image-preview-kitty-test)
;;; dired-image-preview-kitty-test.el ends here
