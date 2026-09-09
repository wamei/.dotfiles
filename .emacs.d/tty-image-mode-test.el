;;; tty-image-mode-test.el --- tests for tty-image-mode -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l tty-image-mode-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "kitty-graphics.el" dir) nil t)
  (load (expand-file-name "tty-image-mode.el" dir) nil t))

;;; 大きさ

(ert-deftest wamei/tty-image-window-cells-uses-body-size ()
  "上限は window の本文の桁数・行数。"
  (cl-letf (((symbol-function 'window-body-width) (lambda (&optional _w) 80))
            ((symbol-function 'window-body-height) (lambda (&optional _w) 24)))
    (should (equal (wamei/tty-image--window-cells 'window) '(80 . 24)))))

(ert-deftest wamei/tty-image-window-cells-is-at-least-one ()
  "0 桁・0 行の window でも 1 を下回らない (0 を渡すと転送が壊れる)。"
  (cl-letf (((symbol-function 'window-body-width) (lambda (&optional _w) 0))
            ((symbol-function 'window-body-height) (lambda (&optional _w) 0)))
    (should (equal (wamei/tty-image--window-cells 'window) '(1 . 1)))))

(ert-deftest wamei/tty-image-target-cells-fits-image-into-window ()
  "画像のピクセル数とセルのピクセル数から、window に収まる (桁 . 行) を出す。
PX は呼び手 (`--render') が測って渡す。ここでは測らない。"
  (cl-letf (((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
            ((symbol-function 'window-body-width) (lambda (&optional _w) 40))
            ((symbol-function 'window-body-height) (lambda (&optional _w) 40)))
    ;; 800/8 = 100 桁、400/16 = 25 行。40 桁に収めるので 0.4 倍 → 40 x 10
    (should (equal (wamei/tty-image--target-cells '(800 . 400) 'window) '(40 . 10)))))

(ert-deftest wamei/tty-image-target-cells-nil-without-window ()
  "window に出ていなければ nil。"
  (should-not (wamei/tty-image--target-cells '(800 . 400) nil)))

(ert-deftest wamei/tty-image-target-cells-nil-when-size-unknown ()
  "PX が nil (大きさを測れない画像) なら nil。"
  (cl-letf (((symbol-function 'window-body-width) (lambda (&optional _w) 40))
            ((symbol-function 'window-body-height) (lambda (&optional _w) 40)))
    (should-not (wamei/tty-image--target-cells nil 'window))))

(ert-deftest wamei/tty-image-image-px-memoizes ()
  "画像の大きさは一度測ったら記憶し、2 回目は測り直さない。"
  (let ((calls 0))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-image-size)
               (lambda (_f) (setq calls (1+ calls)) '(80 . 80))))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/a.png")
        (should (equal (wamei/tty-image--image-px) '(80 . 80)))
        (should (equal (wamei/tty-image--image-px) '(80 . 80)))
        (should (= calls 1))
        (setq buffer-file-name nil)))))

;;; バッファへの被せ方

(ert-deftest wamei/tty-image-show-covers-buffer-without-modifying-it ()
  "バッファのテキストは残したまま display プロパティで placeholder を被せ、
変更フラグを立てない (ファイル訪問バッファを壊さないため)。"
  (cl-letf (((symbol-function 'wamei/kitty-graphics-placeholder-string)
             (lambda (_id _cols _rows) "PLACEHOLDER")))
    (with-temp-buffer
      (insert "\x89PNG\r\n\x1a\n")
      (set-buffer-modified-p nil)
      (let ((text (buffer-string)))
        (wamei/tty-image--show 7 '(4 . 2))
        (should (equal (buffer-string) text))
        (should-not (buffer-modified-p))
        (should (equal (get-text-property (point-min) 'display) "PLACEHOLDER"))
        (should (equal (get-text-property (1- (point-max)) 'display) "PLACEHOLDER"))))))

;;; 再描画

(ert-deftest wamei/tty-image-render-transmits-once-when-size-unchanged ()
  "大きさが変わらなければ 2 回目は転送しない。"
  (let ((puts 0) (deletes 0))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(80 . 80)))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/kitty-graphics-placeholder-string) (lambda (&rest _) "P"))
              ((symbol-function 'wamei/kitty-graphics-put)
               (lambda (&rest _) (setq puts (1+ puts)) 7))
              ((symbol-function 'wamei/kitty-graphics-delete)
               (lambda (id) (when id (setq deletes (1+ deletes)))))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&optional _w) 40))
              ((symbol-function 'window-body-height) (lambda (&optional _w) 40)))
      (with-temp-buffer
        (insert "data")
        (setq buffer-file-name "/tmp/a.png")
        (wamei/tty-image--render)
        (wamei/tty-image--render)
        (should (= puts 1))
        (should (= deletes 0))
        (should (= wamei/tty-image--id 7))
        (setq buffer-file-name nil)))))

(ert-deftest wamei/tty-image-render-retransmits-when-size-changes ()
  "大きさが変わったら古い画像を解放してから送り直す。"
  (let ((puts 0) (deleted nil) (width 40))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(800 . 400)))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/kitty-graphics-placeholder-string) (lambda (&rest _) "P"))
              ((symbol-function 'wamei/kitty-graphics-put)
               (lambda (&rest _) (setq puts (1+ puts)) puts))
              ((symbol-function 'wamei/kitty-graphics-delete)
               (lambda (id) (when id (push id deleted))))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&optional _w) width))
              ((symbol-function 'window-body-height) (lambda (&optional _w) 40)))
      (with-temp-buffer
        (insert "data")
        (setq buffer-file-name "/tmp/a.png")
        (wamei/tty-image--render)
        (setq width 20)
        (wamei/tty-image--render)
        (should (= puts 2))
        (should (equal deleted '(1)))
        (should (= wamei/tty-image--id 2))
        (setq buffer-file-name nil)))))

(ert-deftest wamei/tty-image-render-does-not-show-when-put-fails ()
  "put が失敗 (nil) したら error を投げず、ID は nil のまま、display も被せない。"
  (cl-letf (((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(800 . 400)))
            ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
            ((symbol-function 'wamei/kitty-graphics-put) (lambda (&rest _) nil))
            ((symbol-function 'wamei/kitty-graphics-delete) (lambda (id) (when id (error "呼ばれないはず"))))
            ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
            ((symbol-function 'window-body-width) (lambda (&optional _w) 40))
            ((symbol-function 'window-body-height) (lambda (&optional _w) 40)))
    (with-temp-buffer
      (insert "data")
      (setq buffer-file-name "/tmp/a.png")
      (wamei/tty-image--render)
      (should-not wamei/tty-image--id)
      (should-not (get-text-property (point-min) 'display))
      (setq buffer-file-name nil))))

(ert-deftest wamei/tty-image-render-does-not-retry-put-after-failure-at-same-size ()
  "put が失敗した大きさのままなら、もう一度 render しても put を再試行しない。"
  (let ((puts 0))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(800 . 400)))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/kitty-graphics-put)
               (lambda (&rest _) (setq puts (1+ puts)) nil))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&optional _w) 40))
              ((symbol-function 'window-body-height) (lambda (&optional _w) 40)))
      (with-temp-buffer
        (insert "data")
        (setq buffer-file-name "/tmp/a.png")
        (wamei/tty-image--render)
        (wamei/tty-image--render)
        (should (= puts 1))
        (should-not wamei/tty-image--id)
        (setq buffer-file-name nil)))))

(ert-deftest wamei/tty-image-forget-releases-the-image ()
  "解放すると ID と大きさの記憶を捨てる。"
  (let ((deleted nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-delete)
               (lambda (id) (push id deleted))))
      (with-temp-buffer
        (setq wamei/tty-image--id 9
              wamei/tty-image--cells '(4 . 2))
        (wamei/tty-image--forget)
        (should (equal deleted '(9)))
        (should-not wamei/tty-image--id)
        (should-not wamei/tty-image--cells)))))

(ert-deftest wamei/tty-image-render-measures-image-size-only-once ()
  "同じ大きさで 2 回 render しても、画像のピクセル数は 1 回しか測らない
(`sips' の起動は window-configuration-change-hook から redisplay のたびに
走るので、実際には大きさが変わっていない限り無駄な起動を避ける)。"
  (let ((size-calls 0))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-image-size)
               (lambda (_f) (setq size-calls (1+ size-calls)) '(80 . 80)))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/kitty-graphics-placeholder-string) (lambda (&rest _) "P"))
              ((symbol-function 'wamei/kitty-graphics-put) (lambda (&rest _) 7))
              ((symbol-function 'wamei/kitty-graphics-delete) #'ignore)
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&optional _w) 40))
              ((symbol-function 'window-body-height) (lambda (&optional _w) 40)))
      (with-temp-buffer
        (insert "data")
        (setq buffer-file-name "/tmp/a.png")
        (wamei/tty-image--render)
        (wamei/tty-image--render)
        (should (= size-calls 1))
        (setq buffer-file-name nil)))))

(ert-deftest wamei/tty-image-refresh-remeasures-image-size ()
  "`g' (refresh) はファイルが差し替わったかもしれないので、大きさを測り直す。"
  (let ((size-calls 0))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-image-size)
               (lambda (_f) (setq size-calls (1+ size-calls)) '(80 . 80)))
              ((symbol-function 'wamei/kitty-graphics-cell-size) (lambda () '(8 . 16)))
              ((symbol-function 'wamei/kitty-graphics-placeholder-string) (lambda (&rest _) "P"))
              ((symbol-function 'wamei/kitty-graphics-put) (lambda (&rest _) 7))
              ((symbol-function 'wamei/kitty-graphics-delete) #'ignore)
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-width) (lambda (&optional _w) 40))
              ((symbol-function 'window-body-height) (lambda (&optional _w) 40)))
      (with-temp-buffer
        (insert "data")
        (setq buffer-file-name "/tmp/a.png")
        (wamei/tty-image--render)
        (wamei/tty-image-refresh)
        (should (= size-calls 2))
        (setq buffer-file-name nil)))))

;;; 次/前のファイル

(ert-deftest wamei/tty-image-sibling-moves-within-the-list ()
  (let ((files '("/d/a.png" "/d/b.png" "/d/c.png")))
    (should (equal (wamei/tty-image--sibling "/d/a.png" files 1) "/d/b.png"))
    (should (equal (wamei/tty-image--sibling "/d/c.png" files -1) "/d/b.png"))))

(ert-deftest wamei/tty-image-sibling-stops-at-the-ends ()
  "端では nil を返す (巡回しない)。"
  (let ((files '("/d/a.png" "/d/b.png")))
    (should-not (wamei/tty-image--sibling "/d/b.png" files 1))
    (should-not (wamei/tty-image--sibling "/d/a.png" files -1))))

(ert-deftest wamei/tty-image-sibling-nil-when-file-not-listed ()
  (should-not (wamei/tty-image--sibling "/d/z.png" '("/d/a.png") 1)))

(ert-deftest wamei/tty-image-sibling-nil-for-single-file ()
  (should-not (wamei/tty-image--sibling "/d/a.png" '("/d/a.png") 1)))

(ert-deftest wamei/tty-image-image-files-filters-and-sorts ()
  "ディレクトリの中の画像だけを名前順に返す。"
  (let ((dir (make-temp-file "tty-image-test-" t)))
    (unwind-protect
        (progn
          (dolist (name '("b.png" "a.jpg" "notes.txt" "c.gif"))
            (write-region "" nil (expand-file-name name dir)))
          (should (equal (mapcar #'file-name-nondirectory
                                 (wamei/tty-image--image-files dir))
                         '("a.jpg" "b.png" "c.gif"))))
      (delete-directory dir t))))

;;; モード

(ert-deftest wamei/tty-image-mode-turns-off-line-numbers ()
  "行番号が桁を食うと placeholder の桁数が合わなくなるので必ず切る。
`global-display-line-numbers-mode' が有効だと major mode を変えた直後に
`display-line-numbers-mode' が t になるので、それを打ち消せているかを見る。"
  (cl-letf (((symbol-function 'wamei/tty-image--render) #'ignore))
    (global-display-line-numbers-mode 1)
    (unwind-protect
        (with-temp-buffer
          (wamei/tty-image-mode)
          (should-not display-line-numbers-mode)
          (should truncate-lines)
          (should-not cursor-type))
      (global-display-line-numbers-mode -1))))

(ert-deftest wamei/tty-image-mode-renders-and-hooks-window-changes ()
  "モードに入ると描画し、window の変化で描き直すようにする。"
  (let ((renders 0))
    (cl-letf (((symbol-function 'wamei/tty-image--render)
               (lambda () (setq renders (1+ renders)))))
      (with-temp-buffer
        (wamei/tty-image-mode)
        (should (= renders 1))
        (should (memq #'wamei/tty-image--render
                      (buffer-local-value 'window-configuration-change-hook
                                          (current-buffer))))
        (should (memq #'wamei/tty-image--forget
                      (buffer-local-value 'kill-buffer-hook (current-buffer))))))))

;;; image-mode の乗っ取り

(ert-deftest wamei/tty-image-override-uses-tty-mode-when-available ()
  "端末が対応していて大きさも測れれば自分のモードで開く。"
  (let ((called nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-available-p) (lambda () t))
              ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) '(80 . 40)))
              ((symbol-function 'wamei/tty-image-mode) (lambda () (setq called 'tty)))
              ((symbol-function 'wamei/tty-image--as-text) (lambda () (setq called 'text))))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/a.png")
        (wamei/tty-image--image-mode-override)
        (setq buffer-file-name nil))
      (should (eq called 'tty)))))

(ert-deftest wamei/tty-image-override-falls-back-to-text-when-unavailable ()
  "非対応端末では error を投げずテキスト表示に落ちる
\(tty で debug-on-error t だとデバッガに入って操作不能になるため)。"
  (let ((called nil) (messages nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-available-p) (lambda () nil))
              ((symbol-function 'wamei/tty-image-mode) (lambda () (setq called 'tty)))
              ((symbol-function 'wamei/tty-image--as-text) (lambda () (setq called 'text)))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                            (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (wamei/tty-image--image-mode-override))
      (should (eq called 'text))
      (should messages))))

(ert-deftest wamei/tty-image-override-falls-back-to-text-when-not-an-image ()
  "空ファイルや画像でないファイル (大きさを測れない) もテキスト表示に落ちる。"
  (let ((called nil) (messages nil))
    (cl-letf (((symbol-function 'wamei/kitty-graphics-available-p) (lambda () t))
              ((symbol-function 'wamei/kitty-graphics-image-size) (lambda (_f) nil))
              ((symbol-function 'wamei/tty-image-mode) (lambda () (setq called 'tty)))
              ((symbol-function 'wamei/tty-image--as-text) (lambda () (setq called 'text)))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                            (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/empty.png")
        (wamei/tty-image--image-mode-override)
        (setq buffer-file-name nil))
      (should (eq called 'text))
      (should messages))))

(ert-deftest wamei/tty-image-setup-overrides-image-mode ()
  "setup で image-mode に override の advice が掛かる。"
  (unwind-protect
      (progn
        (wamei/tty-image-setup)
        (should (advice-member-p #'wamei/tty-image--image-mode-override 'image-mode)))
    (advice-remove 'image-mode #'wamei/tty-image--image-mode-override)))

(provide 'tty-image-mode-test)
;;; tty-image-mode-test.el ends here
