;;; eldoc-mouse-test.el --- tests for eldoc-mouse -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l eldoc-mouse-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'eldoc)

;; eldoc-box は読み込まないので、モジュールが参照する変数を special にしておく
;; (テスト側の `let' を動的束縛にするため)。
(defvar eldoc-box--frame nil)

(load (expand-file-name "eldoc-mouse.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;;; ヘルパー

(defvar wamei/eldoc-mouse-test--shown nil
  "スタブの表示関数が受け取った (STRING WINDOW POS) の履歴。新しいものが先頭。")
(defvar wamei/eldoc-mouse-test--hidden 0
  "スタブの非表示関数が呼ばれた回数。")

(defmacro wamei/eldoc-mouse-test--with-buffer (text &rest body)
  "TEXT を入れたバッファを選択 window に表示して BODY を実行する。
eldoc-box の child frame は作らず、表示・非表示は履歴に記録するだけにする。
`eldoc-documentation-functions' は空にしてテストごとに追加する。"
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (set-window-buffer (selected-window) (current-buffer))
     (setq wamei/eldoc-mouse-test--shown nil
           wamei/eldoc-mouse-test--hidden 0)
     (setq-local eldoc-documentation-functions nil)
     (setq-local eldoc-documentation-strategy #'eldoc-documentation-default)
     (cl-letf (((symbol-function 'wamei/eldoc-mouse--show-box)
                (lambda (string window pos)
                  (push (list string window pos) wamei/eldoc-mouse-test--shown)))
               ((symbol-function 'wamei/eldoc-mouse--hide-box)
                (lambda () (cl-incf wamei/eldoc-mouse-test--hidden))))
       (unwind-protect
           (progn (wamei/eldoc-mouse-mode 1) ,@body)
         (wamei/eldoc-mouse-mode -1)))))

(defun wamei/eldoc-mouse-test--move (pos &optional window)
  "POS の上にマウスが来た `mouse-movement' イベントをハンドラに渡す。"
  (wamei/eldoc-mouse--handle-motion
   (list 'mouse-movement
         (list (or window (selected-window)) pos (cons 0 0) 0))))

(defun wamei/eldoc-mouse-test--fire-timer ()
  "保留中の表示タイマーを即時に実行する。タイマーが無ければ nil。"
  (when-let* ((timer wamei/eldoc-mouse--timer))
    (cancel-timer timer)
    (apply (timer--function timer) (timer--args timer))
    t))

;;;; マイナーモード

(ert-deftest wamei/eldoc-mouse-mode-tracks-mouse-only-while-enabled ()
  (with-temp-buffer
    (should-not track-mouse)
    (wamei/eldoc-mouse-mode 1)
    (should (eq track-mouse t))
    (should (local-variable-p 'track-mouse))
    (wamei/eldoc-mouse-mode -1)
    (should-not track-mouse)))

;;;; マウス移動 → タイマー

(ert-deftest wamei/eldoc-mouse-schedules-request-over-symbol ()
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (wamei/eldoc-mouse-test--move 2)
    (should (timerp wamei/eldoc-mouse--timer))))

(ert-deftest wamei/eldoc-mouse-does-not-schedule-over-whitespace ()
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (wamei/eldoc-mouse-test--move 4)
    (should-not wamei/eldoc-mouse--timer)))

;;;; 取得と表示

(ert-deftest wamei/eldoc-mouse-shows-doc-from-sync-function-at-mouse-position ()
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (add-hook 'eldoc-documentation-functions
              (lambda (_cb) (format "doc of %s" (thing-at-point 'symbol)))
              nil t)
    (goto-char 1)
    (wamei/eldoc-mouse-test--move 6)
    (should (wamei/eldoc-mouse-test--fire-timer))
    (should (equal (car wamei/eldoc-mouse-test--shown)
                   (list "doc of bar" (selected-window) 6)))
    ;; point はマウス位置へ動かさない
    (should (= (point) 1))))

(ert-deftest wamei/eldoc-mouse-shows-doc-when-async-callback-arrives ()
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (let (pending)
      (add-hook 'eldoc-documentation-functions
                (lambda (cb) (setq pending cb) t)
                nil t)
      (wamei/eldoc-mouse-test--move 2)
      (wamei/eldoc-mouse-test--fire-timer)
      (should-not wamei/eldoc-mouse-test--shown)
      (funcall pending "async doc" :thing "foo")
      (should (equal (car wamei/eldoc-mouse-test--shown)
                     (list "async doc" (selected-window) 2))))))

(ert-deftest wamei/eldoc-mouse-compose-strategy-waits-for-all-functions ()
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose)
    (let (pending)
      (add-hook 'eldoc-documentation-functions (lambda (_cb) "sync doc") nil t)
      (add-hook 'eldoc-documentation-functions (lambda (cb) (setq pending cb) t) nil t)
      (wamei/eldoc-mouse-test--move 2)
      (wamei/eldoc-mouse-test--fire-timer)
      (should-not wamei/eldoc-mouse-test--shown)
      (funcall pending "async doc")
      (should (equal (car (car wamei/eldoc-mouse-test--shown))
                     "async doc\n\nsync doc")))))

;;;; 表示の維持と取り消し

(ert-deftest wamei/eldoc-mouse-keeps-box-while-moving-inside-symbol ()
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (add-hook 'eldoc-documentation-functions (lambda (_cb) "doc") nil t)
    (wamei/eldoc-mouse-test--move 5)
    (wamei/eldoc-mouse-test--fire-timer)
    (wamei/eldoc-mouse-test--move 7)
    (should (= wamei/eldoc-mouse-test--hidden 0))
    (should-not wamei/eldoc-mouse--timer)))

(ert-deftest wamei/eldoc-mouse-hides-box-when-leaving-symbol ()
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (add-hook 'eldoc-documentation-functions (lambda (_cb) "doc") nil t)
    (wamei/eldoc-mouse-test--move 5)
    (wamei/eldoc-mouse-test--fire-timer)
    (wamei/eldoc-mouse-test--move 4)
    (should (= wamei/eldoc-mouse-test--hidden 1))
    ;; 別のシンボルへ移ったら閉じたうえで新しい予約をする
    (wamei/eldoc-mouse-test--move 1)
    (should (timerp wamei/eldoc-mouse--timer))))

(ert-deftest wamei/eldoc-mouse-does-not-hide-box-it-did-not-show ()
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (wamei/eldoc-mouse-test--move 5)
    (wamei/eldoc-mouse-test--move 4)
    (should (= wamei/eldoc-mouse-test--hidden 0))))

(ert-deftest wamei/eldoc-mouse-ignores-async-response-after-mouse-left ()
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (let (pending)
      (add-hook 'eldoc-documentation-functions (lambda (cb) (setq pending cb) t) nil t)
      (wamei/eldoc-mouse-test--move 2)
      (wamei/eldoc-mouse-test--fire-timer)
      (wamei/eldoc-mouse-test--move 4)
      (funcall pending "late doc")
      (should-not wamei/eldoc-mouse-test--shown))))

(ert-deftest wamei/eldoc-mouse-ignores-motion-inside-eldoc-box-frame ()
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (add-hook 'eldoc-documentation-functions (lambda (_cb) "doc") nil t)
    (wamei/eldoc-mouse-test--move 5)
    (wamei/eldoc-mouse-test--fire-timer)
    ;; child frame の上にマウスがあるときは閉じない
    (let ((eldoc-box--frame (selected-frame)))
      (wamei/eldoc-mouse-test--move 1))
    (should (= wamei/eldoc-mouse-test--hidden 0))
    (should-not wamei/eldoc-mouse--timer)))

(ert-deftest wamei/eldoc-mouse-disabling-mode-hides-box ()
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (add-hook 'eldoc-documentation-functions (lambda (_cb) "doc") nil t)
    (wamei/eldoc-mouse-test--move 5)
    (wamei/eldoc-mouse-test--fire-timer)
    (wamei/eldoc-mouse-mode -1)
    (should (= wamei/eldoc-mouse-test--hidden 1))))

(ert-deftest wamei/eldoc-mouse-disabling-mode-cancels-timer ()
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (wamei/eldoc-mouse-test--move 5)
    (wamei/eldoc-mouse-mode -1)
    (should-not wamei/eldoc-mouse--timer)))

;;;; 位置

(ert-deftest wamei/eldoc-mouse-default-position-is-below-right-of-anchor ()
  (should (equal (wamei/eldoc-mouse-default-position '(10 . 20) 100 50)
                 (cons (+ 10 (frame-char-width)) (+ 20 (frame-char-height))))))

;;;; 他の post-command 処理との共存

(ert-deftest wamei/eldoc-mouse-motion-command-looks-like-ignore-to-hooks ()
  ;; corfu などは `this-command' で継続可否を決める。既定のマウス移動の束縛 (ignore)
  ;; と同じに見せて、マウスを動かしただけで補完が閉じないようにする。
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (let ((this-command 'wamei/eldoc-mouse--handle-motion))
      (wamei/eldoc-mouse-test--move 2)
      (should (eq this-command 'ignore)))))

(ert-deftest wamei/eldoc-mouse-follow-cursor-guard-skips-mouse-motion ()
  (let (called)
    (let ((last-input-event '(mouse-movement (nil 1 (0 . 0) 0))))
      (wamei/eldoc-mouse--unless-mouse-motion (lambda () (setq called t))))
    (should-not called)
    (let ((last-input-event ?a))
      (wamei/eldoc-mouse--unless-mouse-motion (lambda () (setq called t))))
    (should called)))

(ert-deftest wamei/eldoc-mouse-other-command-hides-box ()
  (wamei/eldoc-mouse-test--with-buffer "foo bar"
    (add-hook 'eldoc-documentation-functions (lambda (_cb) "doc") nil t)
    (should (memq #'wamei/eldoc-mouse--pre-command pre-command-hook))
    (wamei/eldoc-mouse-test--move 5)
    (wamei/eldoc-mouse-test--fire-timer)
    (let ((this-command 'wamei/eldoc-mouse--handle-motion))
      (wamei/eldoc-mouse--pre-command))
    (should (= wamei/eldoc-mouse-test--hidden 0))
    (let ((this-command 'self-insert-command))
      (wamei/eldoc-mouse--pre-command))
    (should (= wamei/eldoc-mouse-test--hidden 1))
    (wamei/eldoc-mouse-mode -1)
    (should-not (memq #'wamei/eldoc-mouse--pre-command pre-command-hook))))

;;;; eldoc-box との接続

(defvar eldoc-box--buffer " *eldoc-box-test*")
(defvar eldoc-box-position-function nil)

(defmacro wamei/eldoc-mouse-test--with-fake-eldoc-box (&rest body)
  "eldoc-box の表示関数をバッファ書き込みだけのスタブにして BODY を実行する。
BODY 中は `quits' に `eldoc-box-quit-frame' の呼び出し回数が束縛される。"
  (declare (indent 0))
  `(let ((quits 0))
     (ignore quits)
     (cl-letf (((symbol-function 'eldoc-box--display)
                (lambda (str)
                  (with-current-buffer (get-buffer-create eldoc-box--buffer)
                    (erase-buffer)
                    (insert str))))
               ((symbol-function 'eldoc-box-quit-frame)
                (lambda () (cl-incf quits)))
               ((symbol-function 'eldoc-box--point-position-relative-to-native-frame)
                (lambda (&optional _pos _window) (cons 0 0))))
       (unwind-protect (progn ,@body)
         (when (get-buffer eldoc-box--buffer) (kill-buffer eldoc-box--buffer))))))

(ert-deftest wamei/eldoc-mouse-hide-box-closes-own-box ()
  (wamei/eldoc-mouse-test--with-fake-eldoc-box
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (wamei/eldoc-mouse--show-box "doc" (selected-window) 1)
      (wamei/eldoc-mouse--hide-box)
      (should (= quits 1)))))

(ert-deftest wamei/eldoc-mouse-hide-box-leaves-box-redrawn-by-others ()
  (wamei/eldoc-mouse-test--with-fake-eldoc-box
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (wamei/eldoc-mouse--show-box "doc" (selected-window) 1)
      ;; カーソル位置の eldoc-box が同じバッファを書き換えた
      (with-current-buffer eldoc-box--buffer (erase-buffer) (insert "cursor doc"))
      (wamei/eldoc-mouse--hide-box)
      (should (= quits 0)))))

(ert-deftest wamei/eldoc-mouse-show-box-positions-frame-relative-to-anchor ()
  (wamei/eldoc-mouse-test--with-fake-eldoc-box
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (let* (seen
             (wamei/eldoc-mouse-position-function
              (lambda (anchor width height) (setq seen (list anchor width height)) (cons 1 2))))
        (cl-letf (((symbol-function 'eldoc-box--display)
                   (lambda (_str) (funcall eldoc-box-position-function 30 5))))
          (wamei/eldoc-mouse--show-box "doc" (selected-window) 1))
        (should (equal seen '((0 . 0) 30 5)))))))

(provide 'eldoc-mouse-test)
;;; eldoc-mouse-test.el ends here
