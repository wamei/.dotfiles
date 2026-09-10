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

(ert-deftest wamei/dired-image-preview-kitty-make-frame-suppresses-special-glyphs ()
  "tty は fringe が無いので、`truncate-lines' の行が window の幅ぴったりでも
最終桁が truncation glyph に取られる。placeholder は幅ぴったりに並べるので、
そのままだと画像の右端 1 列が `▸' に置き換わる。frame の `no-special-glyphs'
で glyph を出させない。"
  (let ((params nil))
    (cl-letf (((symbol-function 'selected-frame) (lambda () 'parent-frame))
              ((symbol-function 'make-frame) (lambda (p) (setq params p) 'child-frame))
              ((symbol-function 'frame-root-window) (lambda (_f) 'window))
              ((symbol-function 'set-window-buffer) #'ignore)
              ((symbol-function 'set-window-dedicated-p) #'ignore)
              ((symbol-function 'set-window-parameter) #'ignore))
      (wamei/dired-image-preview-kitty--make-frame 'parent-frame 'buffer '(1 . 2) '(3 . 4))
      (should (eq (alist-get 'no-special-glyphs params) t)))))

(ert-deftest wamei/dired-image-preview-kitty-available-p-does-not-query-the-terminal ()
  "モードの可否判定は端末に訊かない。
`wamei/dired-image-preview-mode' は dired バッファを開くたびに (desktop 復元に
よる起動中も含めて) これを呼ぶ。ここで訊くと応答を取り逃がして、そのバッファの
モードが二度と有効にならない。対応の可否は実際に出すときに訊く。"
  (cl-letf (((symbol-function 'wamei/kitty-graphics-available-p)
             (lambda () (error "ここで端末に訊いてはいけない")))
            ((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    (should (wamei/dired-image-preview-kitty-available-p))))

(ert-deftest wamei/dired-image-preview-kitty-available-p-needs-tty-child-frames ()
  "tty child frame が使えない環境では出せない。"
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
            ((symbol-function 'featurep)
             (lambda (feature) (not (eq feature 'tty-child-frames)))))
    (should-not (wamei/dired-image-preview-kitty-available-p))))

(ert-deftest wamei/dired-image-preview-kitty-available-p-is-nil-on-gui ()
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
    (should-not (wamei/dired-image-preview-kitty-available-p))))

(ert-deftest wamei/dired-image-preview-kitty-show-asks-the-terminal ()
  "実際に出すときに初めて端末に訊く。非対応なら何も送らない。"
  (let ((asked 0) (measured 0))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-available-p)
               (lambda () (setq asked (1+ asked)) nil))
              ((symbol-function 'wamei/dired-image-preview-kitty-hide) #'ignore)
              ((symbol-function 'wamei/kitty-graphics-image-size)
               (lambda (_f) (setq measured (1+ measured)) '(80 . 40))))
      (wamei/dired-image-preview-kitty-show 'target)
      (should (= asked 1))
      ;; 非対応と分かった時点で止まる (画像を測りにも行かない)
      (should (= measured 0)))))

(provide 'dired-image-preview-kitty-test)
;;; dired-image-preview-kitty-test.el ends here
