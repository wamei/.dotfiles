;;; dired-image-preview-test.el --- tests for dired-image-preview -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l dired-image-preview-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(package-initialize)
(require 'dired)
(load (expand-file-name "dired-image-preview.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defmacro wamei/dired-image-preview-test--with-dired (dir var &rest body)
  "DIR の dired バッファを VAR に束縛し、選択中の window に表示して BODY を評価する。
後でバッファを kill する。"
  (declare (indent 2))
  `(let ((,var (dired-noselect ,dir)))
     (unwind-protect
         (progn
           (set-window-buffer (selected-window) ,var)
           (with-current-buffer ,var
             ,@body))
       (kill-buffer ,var))))

(defun wamei/dired-image-preview-test--goto-file (name)
  "現在の dired バッファで NAME の行へ移動し、その位置を返す。"
  (goto-char (point-min))
  (should (dired-goto-file (expand-file-name name default-directory)))
  (point))

(defmacro wamei/dired-image-preview-test--with-temp-dir (var &rest body)
  "一時ディレクトリを VAR に束縛して BODY を評価し、後で削除する。
中には画像 (a.png / b.jpg)、ディレクトリ (pics.png はディレクトリ)、テキスト (c.txt) を置く。"
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "dip-" t))))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "a.png" ,var) (insert "png"))
           (with-temp-file (expand-file-name "b.jpg" ,var) (insert "jpg"))
           (with-temp-file (expand-file-name "c.txt" ,var) (insert "txt"))
           (make-directory (expand-file-name "pics.png" ,var))
           (make-symbolic-link "a.png" (expand-file-name "link.png" ,var))
           ,@body)
       (delete-directory ,var t))))

;;; 画像ファイル判定

(ert-deftest wamei/dired-image-preview-image-file-p-accepts-image-extension ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (should (wamei/dired-image-preview--image-file-p (expand-file-name "a.png" dir)))
    (should (wamei/dired-image-preview--image-file-p (expand-file-name "b.jpg" dir)))))

(ert-deftest wamei/dired-image-preview-image-file-p-rejects-non-image ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (should-not (wamei/dired-image-preview--image-file-p (expand-file-name "c.txt" dir)))))

(ert-deftest wamei/dired-image-preview-image-file-p-rejects-directory-named-like-image ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (should-not (wamei/dired-image-preview--image-file-p (expand-file-name "pics.png" dir)))))

(ert-deftest wamei/dired-image-preview-image-file-p-rejects-missing-file ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (should-not (wamei/dired-image-preview--image-file-p (expand-file-name "none.png" dir)))))

(ert-deftest wamei/dired-image-preview-image-file-p-rejects-oversized-file ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (let ((wamei/dired-image-preview-max-file-size 2))
      (should-not (wamei/dired-image-preview--image-file-p (expand-file-name "a.png" dir))))))

;;; symlink

(ert-deftest wamei/dired-image-preview-image-file-p-follows-symlink ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (should (wamei/dired-image-preview--image-file-p (expand-file-name "link.png" dir)))))

;;; 行からターゲットを取る

(ert-deftest wamei/dired-image-preview-target-at-returns-file-and-line-bounds ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir buf
      (let* ((pos (wamei/dired-image-preview-test--goto-file "a.png"))
             (target (wamei/dired-image-preview--target-at (selected-window) pos)))
        (should target)
        (should (eq (wamei/dired-image-preview--target-window target) (selected-window)))
        (should (eq (wamei/dired-image-preview--target-buffer target) buf))
        (should (equal (wamei/dired-image-preview--target-file target)
                       (expand-file-name "a.png" dir)))
        (should (= (wamei/dired-image-preview--target-beg target) (line-beginning-position)))
        (should (= (wamei/dired-image-preview--target-end target) (line-end-position)))
        ;; ポップアップの位置の基準は行頭ではなく、マウス/point のあった文字
        (should (= (wamei/dired-image-preview--target-anchor target) pos))))))

(ert-deftest wamei/dired-image-preview-target-at-is-nil-on-non-image-line ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (let ((pos (wamei/dired-image-preview-test--goto-file "c.txt")))
        (should-not (wamei/dired-image-preview--target-at (selected-window) pos))))))

(ert-deftest wamei/dired-image-preview-target-at-is-nil-on-header-line ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (should-not (wamei/dired-image-preview--target-at (selected-window) (point-min))))))

(ert-deftest wamei/dired-image-preview-target-at-is-nil-in-non-dired-buffer ()
  (with-temp-buffer
    (insert "a.png\n")
    (set-window-buffer (selected-window) (current-buffer))
    (should-not (wamei/dired-image-preview--target-at (selected-window) (point-min)))))

;;; トラッキング

(defvar wamei/dired-image-preview-test--shown nil
  "テスト用の表示関数が受けたターゲットのリスト (新しいものが先)。")
(defvar wamei/dired-image-preview-test--hidden 0
  "テスト用の非表示関数が呼ばれた回数。")

(defmacro wamei/dired-image-preview-test--with-recorder (&rest body)
  "表示/非表示関数を記録用に差し替え、状態を空にして BODY を評価する。
終わりに残ったタイマーと状態を片付ける。"
  (declare (indent 0))
  `(let ((wamei/dired-image-preview-display-function
          (lambda (target) (push target wamei/dired-image-preview-test--shown)))
         (wamei/dired-image-preview-hide-function
          (lambda () (cl-incf wamei/dired-image-preview-test--hidden)))
         (wamei/dired-image-preview-available-predicate #'always)
         (wamei/dired-image-preview-test--shown nil)
         (wamei/dired-image-preview-test--hidden 0))
     (unwind-protect
         (progn ,@body)
       (wamei/dired-image-preview--reset))))

(defun wamei/dired-image-preview-test--fire-timer ()
  "予約中のタイマーを今すぐ発火させる。"
  (let ((timer wamei/dired-image-preview--timer))
    (should timer)
    (apply (timer--function timer) (timer--args timer))))

(ert-deftest wamei/dired-image-preview-track-schedules-show-on-image-line ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (let ((pos (wamei/dired-image-preview-test--goto-file "a.png")))
          (wamei/dired-image-preview--track (selected-window) pos 0.5)
          (should wamei/dired-image-preview--active)
          (should-not wamei/dired-image-preview-test--shown)
          (wamei/dired-image-preview-test--fire-timer)
          (should (= 1 (length wamei/dired-image-preview-test--shown)))
          (should (equal (wamei/dired-image-preview--target-file
                          (car wamei/dired-image-preview-test--shown))
                         (expand-file-name "a.png" dir))))))))

(ert-deftest wamei/dired-image-preview-track-keeps-target-while-on-same-line ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (let ((pos (wamei/dired-image-preview-test--goto-file "a.png")))
          (wamei/dired-image-preview--track (selected-window) pos 0.5)
          (let ((target wamei/dired-image-preview--active)
                (timer wamei/dired-image-preview--timer))
            (wamei/dired-image-preview--track (selected-window) (line-end-position) 0.5)
            (should (eq target wamei/dired-image-preview--active))
            (should (eq timer wamei/dired-image-preview--timer))))))))

(ert-deftest wamei/dired-image-preview-track-hides-when-leaving-to-non-image-line ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview--track
         (selected-window) (wamei/dired-image-preview-test--goto-file "a.png") 0.5)
        (wamei/dired-image-preview-test--fire-timer)
        (wamei/dired-image-preview--track
         (selected-window) (wamei/dired-image-preview-test--goto-file "c.txt") 0.5)
        (should (= 1 wamei/dired-image-preview-test--hidden))
        (should-not wamei/dired-image-preview--active)
        (should-not wamei/dired-image-preview--timer)))))

(ert-deftest wamei/dired-image-preview-track-cancels-pending-timer-without-hiding ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview--track
         (selected-window) (wamei/dired-image-preview-test--goto-file "a.png") 0.5)
        (wamei/dired-image-preview--track
         (selected-window) (wamei/dired-image-preview-test--goto-file "c.txt") 0.5)
        ;; まだ出していないので hide は呼ばない
        (should (= 0 wamei/dired-image-preview-test--hidden))
        (should-not wamei/dired-image-preview--timer)))))

(ert-deftest wamei/dired-image-preview-track-switches-between-image-lines ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview--track
         (selected-window) (wamei/dired-image-preview-test--goto-file "a.png") 0.5)
        (wamei/dired-image-preview-test--fire-timer)
        (wamei/dired-image-preview--track
         (selected-window) (wamei/dired-image-preview-test--goto-file "b.jpg") 0.5)
        (should (= 1 wamei/dired-image-preview-test--hidden))
        (wamei/dired-image-preview-test--fire-timer)
        (should (equal (wamei/dired-image-preview--target-file
                        (car wamei/dired-image-preview-test--shown))
                       (expand-file-name "b.jpg" dir)))))))

(ert-deftest wamei/dired-image-preview-stale-timer-does-not-show ()
  "reset 後に古いタイマーが発火しても表示しない。"
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview--track
         (selected-window) (wamei/dired-image-preview-test--goto-file "a.png") 0.5)
        (let ((timer wamei/dired-image-preview--timer))
          (wamei/dired-image-preview--reset)
          (apply (timer--function timer) (timer--args timer))
          (should-not wamei/dired-image-preview-test--shown))))))

;;; マウスと point のハンドラ

(defun wamei/dired-image-preview-test--motion-event (window pos)
  "WINDOW の POS を指すマウス移動イベントを作る。"
  (list 'mouse-movement (list window pos (cons 0 0) 0)))

(ert-deftest wamei/dired-image-preview-motion-tracks-when-mode-is-on ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview-mode 1)
        (let ((pos (wamei/dired-image-preview-test--goto-file "a.png")))
          (goto-char (point-min))
          (wamei/dired-image-preview--handle-motion
           (wamei/dired-image-preview-test--motion-event (selected-window) pos))
          (should wamei/dired-image-preview--active)
          (should (= (wamei/dired-image-preview--target-beg wamei/dired-image-preview--active)
                     (save-excursion (goto-char pos) (line-beginning-position))))
          (should (eq this-command 'ignore)))))))

(ert-deftest wamei/dired-image-preview-own-frame-window-p ()
  "child frame の window かどうかを判定する。"
  (cl-letf (((symbol-function 'frame-parent) (lambda (&optional _) nil)))
    (should-not (wamei/dired-image-preview--own-frame-window-p (selected-window))))
  (cl-letf (((symbol-function 'frame-parent) (lambda (&optional _) 'parent)))
    (should (wamei/dired-image-preview--own-frame-window-p (selected-window)))))

(ert-deftest wamei/dired-image-preview-motion-over-child-frame-keeps-preview ()
  "マウスがプレビュー自身 (child frame) の上に入っても消さない。
プレビューはポインタのすぐ右下に出るので、少し動かすと child frame に入る。そこで
消すと出す・消すを繰り返して点滅する (eldoc-mouse も同じ guard を持つ)。"
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview-mode 1)
        (let ((pos (wamei/dired-image-preview-test--goto-file "a.png")))
          (wamei/dired-image-preview--track (selected-window) pos 0.5)
          (wamei/dired-image-preview-test--fire-timer)
          (let ((target wamei/dired-image-preview--active))
            (should target)
            ;; ポップアップの window から来た移動イベント (モードは付いていない)
            (with-temp-buffer
              (set-window-buffer (selected-window) (current-buffer))
              (cl-letf (((symbol-function 'wamei/dired-image-preview--own-frame-window-p)
                         (lambda (_) t)))
                (wamei/dired-image-preview--handle-motion
                 (wamei/dired-image-preview-test--motion-event (selected-window) (point-min)))))
            (set-window-buffer (selected-window) buf)
            (should (eq target wamei/dired-image-preview--active))
            (should (= 0 wamei/dired-image-preview-test--hidden))))))))

(ert-deftest wamei/dired-image-preview-motion-ignores-buffer-without-mode ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (let ((pos (wamei/dired-image-preview-test--goto-file "a.png")))
          (wamei/dired-image-preview--handle-motion
           (wamei/dired-image-preview-test--motion-event (selected-window) pos))
          (should-not wamei/dired-image-preview--active))))))

(ert-deftest wamei/dired-image-preview-post-command-tracks-point ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview-mode 1)
        (wamei/dired-image-preview-test--goto-file "a.png")
        (let ((last-input-event ?n))
          (wamei/dired-image-preview--post-command))
        (should wamei/dired-image-preview--active)))))

(ert-deftest wamei/dired-image-preview-post-command-skips-mouse-motion ()
  "マウス移動のコマンドでは point を見ない (マウス側の追跡を壊さない)。"
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview-mode 1)
        (wamei/dired-image-preview-test--goto-file "a.png")
        (let ((last-input-event
               (wamei/dired-image-preview-test--motion-event (selected-window) (point-min))))
          (wamei/dired-image-preview--post-command))
        (should-not wamei/dired-image-preview--active)))))

(ert-deftest wamei/dired-image-preview-post-command-ignores-unselected-window ()
  "point の追跡は選択中の window に表示されているバッファだけ。"
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview-mode 1)
        (wamei/dired-image-preview-test--goto-file "a.png")
        (with-temp-buffer
          (set-window-buffer (selected-window) (current-buffer))
          (with-current-buffer buf
            (let ((last-input-event ?n))
              (wamei/dired-image-preview--post-command))))
        (should-not wamei/dired-image-preview--active)))))

(ert-deftest wamei/dired-image-preview-global-post-command-hides-on-key-in-other-buffer ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview-mode 1)
        (wamei/dired-image-preview--track
         (selected-window) (wamei/dired-image-preview-test--goto-file "a.png") 0.5)
        (wamei/dired-image-preview-test--fire-timer)
        (should (memq #'wamei/dired-image-preview--post-command-global
                      (default-value 'post-command-hook)))
        (with-temp-buffer
          (let ((last-input-event ?n))
            (wamei/dired-image-preview--post-command-global)))
        (should (= 1 wamei/dired-image-preview-test--hidden))
        (should-not wamei/dired-image-preview--active)
        (should-not (memq #'wamei/dired-image-preview--post-command-global
                          (default-value 'post-command-hook)))))))

(ert-deftest wamei/dired-image-preview-global-post-command-keeps-on-mouse-motion-elsewhere ()
  "他のバッファ上でマウスが動いただけでは消さない (sidebar をホバー中に本文側で動くケース)。"
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview-mode 1)
        (wamei/dired-image-preview--track
         (selected-window) (wamei/dired-image-preview-test--goto-file "a.png") 0.5)
        (wamei/dired-image-preview-test--fire-timer)
        (with-temp-buffer
          (let ((last-input-event
                 (wamei/dired-image-preview-test--motion-event (selected-window) 1)))
            (wamei/dired-image-preview--post-command-global)))
        (should (= 0 wamei/dired-image-preview-test--hidden))
        (should wamei/dired-image-preview--active)))))

(ert-deftest wamei/dired-image-preview-global-post-command-hides-when-buffer-gone-from-window ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview-mode 1)
        (wamei/dired-image-preview--track
         (selected-window) (wamei/dired-image-preview-test--goto-file "a.png") 0.5)
        (wamei/dired-image-preview-test--fire-timer)
        (with-temp-buffer
          (set-window-buffer (selected-window) (current-buffer))
          (let ((last-input-event
                 (wamei/dired-image-preview-test--motion-event (selected-window) 1)))
            (wamei/dired-image-preview--post-command-global)))
        (should (= 1 wamei/dired-image-preview-test--hidden))
        (should-not wamei/dired-image-preview--active)))))

;;; モード

(ert-deftest wamei/dired-image-preview-mode-wires-hooks-and-track-mouse ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview-mode 1)
        (should track-mouse)
        (should (local-variable-p 'track-mouse))
        (should (memq #'wamei/dired-image-preview--post-command post-command-hook))
        (should (eq (lookup-key wamei/dired-image-preview-mode-map [mouse-movement])
                    #'wamei/dired-image-preview--handle-motion))
        (wamei/dired-image-preview-mode -1)
        (should-not track-mouse)
        (should-not (local-variable-p 'track-mouse))
        (should-not (memq #'wamei/dired-image-preview--post-command post-command-hook))))))

(ert-deftest wamei/dired-image-preview-mode-off-resets-active-preview ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (wamei/dired-image-preview-test--with-recorder
        (wamei/dired-image-preview-mode 1)
        (wamei/dired-image-preview--track
         (selected-window) (wamei/dired-image-preview-test--goto-file "a.png") 0.5)
        (wamei/dired-image-preview-test--fire-timer)
        (wamei/dired-image-preview-mode -1)
        (should (= 1 wamei/dired-image-preview-test--hidden))
        (should-not wamei/dired-image-preview--active)))))

(ert-deftest wamei/dired-image-preview-mode-stays-off-when-unavailable ()
  "画像を描けない環境 (tty など) では有効にならない。"
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (let ((wamei/dired-image-preview-available-predicate #'ignore))
        (wamei/dired-image-preview-mode 1)
        (should-not wamei/dired-image-preview-mode)
        (should-not track-mouse)))))

(ert-deftest wamei/dired-image-preview-posframe-parameters-refuse-focus ()
  "child frame は表示 (map) されてもキーボードフォーカスを取らない。
posframe は `no-accept-focus' しか付けないので、macOS では frame が map された時点で
フォーカスが移る。corfu / eldoc-box と同じく `no-focus-on-map' も渡す。"
  (should (equal (cdr (assq 'no-focus-on-map wamei/dired-image-preview-posframe-parameters)) t))
  (should (equal (cdr (assq 'no-accept-focus wamei/dired-image-preview-posframe-parameters)) t)))

(ert-deftest wamei/dired-image-preview-posframe-stale-p-detects-missing-parameter ()
  "posframe は引数が変わるまで child frame を作り直さないので、パラメータが
欠けた frame が残ることがある。欠けていれば stale と判定する。"
  (cl-letf (((symbol-function 'frame-live-p) (lambda (_) t)))
    ;; 全部揃っている
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_f key) (cdr (assq key wamei/dired-image-preview-posframe-parameters)))))
      (should-not (wamei/dired-image-preview--posframe-stale-p 'frame)))
    ;; no-focus-on-map が nil
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_f key)
                 (unless (eq key 'no-focus-on-map)
                   (cdr (assq key wamei/dired-image-preview-posframe-parameters))))))
      (should (wamei/dired-image-preview--posframe-stale-p 'frame)))))

(ert-deftest wamei/dired-image-preview-posframe-show-recreates-stale-frame ()
  "パラメータが欠けた child frame は表示前に捨てて、posframe に作り直させる。"
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (let ((pos (wamei/dired-image-preview-test--goto-file "b.jpg"))
            deleted)
        (cl-letf (((symbol-function 'posframe-show) #'ignore)
                  ((symbol-function 'posframe-delete-frame)
                   (lambda (buffer) (push buffer deleted)))
                  ((symbol-function 'face-attribute) (lambda (&rest _) "#525254"))
                  ((symbol-function 'wamei/dired-image-preview--posframe-frame)
                   (lambda () 'stale-frame))
                  ((symbol-function 'wamei/dired-image-preview--posframe-stale-p)
                   (lambda (_) t)))
          (wamei/dired-image-preview--posframe-show
           (wamei/dired-image-preview--target-at (selected-window) pos)))
        (should (equal deleted (list wamei/dired-image-preview--buffer-name)))))))

(ert-deftest wamei/dired-image-preview-posframe-show-keeps-good-frame ()
  "パラメータが揃っている child frame は捨てない (毎回作り直すと重い)。"
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (let ((pos (wamei/dired-image-preview-test--goto-file "b.jpg"))
            deleted)
        (cl-letf (((symbol-function 'posframe-show) #'ignore)
                  ((symbol-function 'posframe-delete-frame)
                   (lambda (buffer) (push buffer deleted)))
                  ((symbol-function 'face-attribute) (lambda (&rest _) "#525254"))
                  ((symbol-function 'wamei/dired-image-preview--posframe-frame)
                   (lambda () 'good-frame))
                  ((symbol-function 'wamei/dired-image-preview--posframe-stale-p)
                   (lambda (_) nil)))
          (wamei/dired-image-preview--posframe-show
           (wamei/dired-image-preview--target-at (selected-window) pos)))
        (should-not deleted)))))

(ert-deftest wamei/dired-image-preview-posframe-show-passes-override-parameters ()
  "`--posframe-show' が `wamei/dired-image-preview-posframe-parameters' を
posframe-show の :override-parameters に渡す。"
  (wamei/dired-image-preview-test--with-temp-dir dir
    (wamei/dired-image-preview-test--with-dired dir _buf
      (let ((pos (wamei/dired-image-preview-test--goto-file "b.jpg"))
            captured)
        (cl-letf (((symbol-function 'posframe-show)
                   (lambda (_buffer &rest args) (setq captured args)))
                  ((symbol-function 'face-attribute) (lambda (&rest _) "#525254")))
          (wamei/dired-image-preview--posframe-show
           (wamei/dired-image-preview--target-at (selected-window) pos)))
        (should (equal (plist-get captured :override-parameters)
                       wamei/dired-image-preview-posframe-parameters))))))

;;; posframe backend

(ert-deftest wamei/dired-image-preview-render-inserts-image ()
  (let ((buf (wamei/dired-image-preview--render '(image :type png :file "/x/a.png"))))
    (unwind-protect
        (with-current-buffer buf
          (should (equal (get-text-property (point-min) 'display)
                         '(image :type png :file "/x/a.png"))))
      (kill-buffer buf))))

;;; サイズと位置

(ert-deftest wamei/dired-image-preview-fit-size-shrinks-keeping-aspect ()
  (should (equal (wamei/dired-image-preview--fit-size '(1600 . 400) '(360 . 280)) '(360 . 90)))
  (should (equal (wamei/dired-image-preview--fit-size '(400 . 400) '(360 . 280)) '(280 . 280)))
  (should (equal (wamei/dired-image-preview--fit-size '(300 . 1200) '(360 . 280)) '(70 . 280))))

(ert-deftest wamei/dired-image-preview-fit-size-does-not-upscale ()
  (should (equal (wamei/dired-image-preview--fit-size '(100 . 50) '(360 . 280)) '(100 . 50))))

(ert-deftest wamei/dired-image-preview-max-image-size-is-absolute-from-parent-frame ()
  "posframe の child frame は最初 32px 程度で、`max-image-size' (frame の 10 倍) が
そこで判定されると 320px を超える画像が読めない。親 frame 基準の絶対値にする。"
  (cl-letf (((symbol-function 'frame-pixel-width) (lambda (&optional _) 1420))
            ((symbol-function 'frame-pixel-height) (lambda (&optional _) 858)))
    (let ((max-image-size 10.0))
      (should (= (wamei/dired-image-preview--max-image-size (selected-frame)) 14200))))
  ;; 既定が絶対値 (整数) ならそのまま
  (let ((max-image-size 4096))
    (should (= (wamei/dired-image-preview--max-image-size (selected-frame)) 4096))))

(ert-deftest wamei/dired-image-preview-pixel-gap-converts-chars-to-pixels ()
  (let ((wamei/dired-image-preview-gap '(2 . 1)))
    (cl-letf (((symbol-function 'frame-char-width) (lambda (&optional _) 10))
              ((symbol-function 'frame-char-height) (lambda (&optional _) 20)))
      (should (equal (wamei/dired-image-preview--pixel-gap (selected-frame)) '(20 . 20))))))

;;; 市松模様の下敷き

(ert-deftest wamei/dired-image-preview-checkerboard-svg-embeds-image-over-pattern ()
  (wamei/dired-image-preview-test--with-temp-dir dir
    (let* ((svg (wamei/dired-image-preview--checkerboard-svg
                 (expand-file-name "a.png" dir) 'png '(120 . 90)))
           (image (car (dom-by-tag svg 'image))))
      (should (equal (dom-attr svg 'width) 120))
      (should (equal (dom-attr svg 'height) 90))
      (should (dom-by-tag svg 'pattern))
      (should image)
      (should (equal (dom-attr image 'width) 120))
      (should (equal (dom-attr image 'height) 90))
      (should (string-prefix-p "data:image/png;base64," (dom-attr image 'xlink:href))))))

(ert-deftest wamei/dired-image-preview-alpha-type-p ()
  (should (wamei/dired-image-preview--alpha-type-p 'png))
  (should (wamei/dired-image-preview--alpha-type-p 'webp))
  (should-not (wamei/dired-image-preview--alpha-type-p 'jpeg)))

(ert-deftest wamei/dired-image-preview-max-pixel-size-scales-frame ()
  (let ((wamei/dired-image-preview-max-size-ratio 0.5))
    (cl-letf (((symbol-function 'frame-inner-width) (lambda (&optional _) 1000))
              ((symbol-function 'frame-inner-height) (lambda (&optional _) 600)))
      (should (equal (wamei/dired-image-preview--max-pixel-size (selected-frame))
                     '(500 . 300))))))

(provide 'dired-image-preview-test)
;;; dired-image-preview-test.el ends here
