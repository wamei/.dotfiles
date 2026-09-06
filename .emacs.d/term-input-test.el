;;; term-input-test.el --- tests for term-input -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l term-input-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "term-input.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; テスト用の vterm スタブ

(defvar wamei/term-input-test--sent nil
  "スタブが受け取った送信内容。(KEY SHIFT META CTRL) か文字列。")

(defvar wamei/term-input-test--delays nil
  "`vterm-send-string' が呼ばれた時点の `vterm-timer-delay'。")

(defvar wamei/term-input-test--buffers nil
  "`vterm-send-string' が呼ばれた時点の `current-buffer' の名前。")

;; vterm 本体の defcustom。term-input.el 側の (defvar vterm-timer-delay) は
;; そのファイル限りの宣言なので、テスト側で束縛するには値付きで special にする。
(defvar vterm-timer-delay 0.1
  "vterm の `accept-process-output' 待ち時間 (テスト用のスタブ定義)。")

(defmacro wamei/term-input-test--with-vterm (&rest body)
  "vterm の送信関数をスタブに差し替えて BODY を実行する。"
  (declare (indent 0))
  `(let ((wamei/term-input-test--sent nil)
         (wamei/term-input-test--delays nil)
         (wamei/term-input-test--buffers nil)
         (vterm-timer-delay 0.1)
         (kill-ring nil)
         (kill-ring-yank-pointer nil)
         (last-command nil)
         (this-command nil))
     (cl-letf (((symbol-function 'vterm-send-key)
                (lambda (key &optional shift meta ctrl _accept)
                  (push (list key shift meta ctrl) wamei/term-input-test--sent)))
               ((symbol-function 'vterm-send-string)
                (lambda (string &optional _paste-p)
                  (push vterm-timer-delay wamei/term-input-test--delays)
                  (push (buffer-name) wamei/term-input-test--buffers)
                  (push string wamei/term-input-test--sent))))
       ,@body)))

(defun wamei/term-input-test--call-kill-line ()
  "コマンドループを模して `wamei/term-input-kill-line' を 1 回呼ぶ。"
  (setq this-command #'wamei/term-input-kill-line)
  (call-interactively #'wamei/term-input-kill-line)
  (setq last-command this-command))

;;; kill-line

(ert-deftest wamei/term-input-kill-line-saves-rest-of-line ()
  "point から行末までを kill-ring に入れ、C-k を端末へ送る。"
  (wamei/term-input-test--with-vterm
    (with-temp-buffer
      (insert "$ echo hello world\n")
      (goto-char (point-min))
      (search-forward "hello")
      (backward-word)
      (wamei/term-input-test--call-kill-line)
      (should (equal (car kill-ring) "hello world"))
      (should (equal wamei/term-input-test--sent '(("k" nil nil t)))))))

(ert-deftest wamei/term-input-kill-line-appends-when-repeated ()
  "連続して呼ぶと Emacs の kill-line と同じく 1 つの kill に連結する。"
  (wamei/term-input-test--with-vterm
    (with-temp-buffer
      (insert "$ foo\n")
      (goto-char (point-min))
      (search-forward "foo")
      (backward-word)
      (wamei/term-input-test--call-kill-line)
      ;; シェル側で行が消えた状態を模す
      (delete-region (point) (line-end-position))
      (insert "bar")
      (backward-word)
      (wamei/term-input-test--call-kill-line)
      (should (equal kill-ring '("foobar"))))))

(ert-deftest wamei/term-input-kill-line-at-eol-leaves-kill-ring ()
  "行末では zsh の kill-line 同様に何も切り取らず、kill-ring も汚さない。"
  (wamei/term-input-test--with-vterm
    (with-temp-buffer
      (insert "$ foo\n")
      (goto-char (point-min))
      (end-of-line)
      (wamei/term-input-test--call-kill-line)
      (should-not kill-ring)
      (should (equal wamei/term-input-test--sent '(("k" nil nil t)))))))

(ert-deftest wamei/term-input-kill-line-ignores-trailing-spaces ()
  "vterm が画面幅まで埋める行末の空白は kill に含めない。"
  (wamei/term-input-test--with-vterm
    (with-temp-buffer
      (insert "$ foo   \n")
      (goto-char (point-min))
      (search-forward "foo")
      (backward-word)
      (wamei/term-input-test--call-kill-line)
      (should (equal (car kill-ring) "foo")))))

;;; マウスホイールの SGR 列

(ert-deftest wamei/term-input-sgr-wheel-up ()
  "上方向ホイールはボタン 64、座標は 1 始まり。"
  (should (equal (wamei/term-input--sgr-mouse 64 0 0) "\e[<64;1;1M")))

(ert-deftest wamei/term-input-sgr-wheel-down-position ()
  "下方向ホイールはボタン 65、列・行をそのまま載せる。"
  (should (equal (wamei/term-input--sgr-mouse 65 9 4) "\e[<65;10;5M")))

(ert-deftest wamei/term-input-wheel-button-from-event ()
  "イベント種別から SGR のボタン番号を求める。double/triple 修飾は無視する。"
  (should (= (wamei/term-input--wheel-button 'wheel-up) 64))
  (should (= (wamei/term-input--wheel-button 'wheel-down) 65))
  (should (= (wamei/term-input--wheel-button 'double-wheel-down) 65))
  (should (= (wamei/term-input--wheel-button 'triple-wheel-up) 64))
  ;; 端末フレームでは mouse-4 / mouse-5 として届く
  (should (= (wamei/term-input--wheel-button 'mouse-4) 64))
  (should (= (wamei/term-input--wheel-button 'mouse-5) 65)))

(ert-deftest wamei/term-input-wheel-button-rejects-other-events ()
  "ホイール以外のイベントは nil。"
  (should-not (wamei/term-input--wheel-button 'mouse-1))
  (should-not (wamei/term-input--wheel-button 'wheel-left)))

;;; ホイール転送コマンド

(defmacro wamei/term-input-test--with-window-buffer (buffer &rest body)
  "選択中の window に BUFFER を出して BODY を実行し、後で元へ戻す。
マウスイベントの posn-window は選択中の window になるので、
「ポインタの下のバッファ」と `current-buffer' を食い違わせるために使う。"
  (declare (indent 1))
  `(let ((wamei/term-input-test--saved (window-buffer (selected-window))))
     (unwind-protect
         (progn (set-window-buffer (selected-window) ,buffer) ,@body)
       (set-window-buffer (selected-window) wamei/term-input-test--saved))))

(defun wamei/term-input-test--wheel-event (type col row)
  "TYPE のホイールイベントを (COL . ROW) の位置で組み立てる。"
  (list type
        (list (selected-window) (point) '(0 . 0) 0 nil (point)
              (cons col row) nil (cons col row) '(1 . 1))))

(ert-deftest wamei/term-input-forward-wheel-sends-sgr ()
  "ホイールイベントをイベント位置の SGR 列として端末へ送る。"
  (wamei/term-input-test--with-vterm
    (with-temp-buffer
      (wamei/term-input-forward-wheel
       (wamei/term-input-test--wheel-event 'wheel-down 3 7))
      (should (equal wamei/term-input-test--sent '("\e[<65;4;8M"))))))

(ert-deftest wamei/term-input-forward-wheel-does-not-wait-for-output ()
  "端末が何も返さなくても待たない。

`vterm-send-string' は末尾で (accept-process-output PROC vterm-timer-delay nil t)
を呼ぶため、既定の 0.1 秒のままだと TUI が末端に達して再描画を返さなくなった
とたんに 1 イベントごとに満額ブロックする。慣性スクロールで数百イベント積まれる
と Emacs 全体が数十秒止まるので、転送中は 0 にして即座に返させる。"
  (wamei/term-input-test--with-vterm
    (with-temp-buffer
      (wamei/term-input-forward-wheel
       (wamei/term-input-test--wheel-event 'wheel-down 3 7))
      (should (equal wamei/term-input-test--delays '(0)))
      ;; 束縛は転送の間だけで、抜けたら元に戻っている
      (should (equal vterm-timer-delay 0.1)))))

(ert-deftest wamei/term-input-forward-wheel-uses-event-window-buffer ()
  "ポインタの下の端末へ送る。window が選択されていなくても効く。

マウスイベントのキー引きはポインタ下のバッファのキーマップで行われるが、
コマンド実行時の `current-buffer' は選択中の window のバッファになる。
`vterm--term' や `vterm--process' はバッファローカルなので、current-buffer の
まま送ると非選択の端末では `vterm-send-string' が黙って何もしない。"
  (wamei/term-input-test--with-vterm
    (let ((term (generate-new-buffer " *term-under-mouse*")))
      (unwind-protect
          (wamei/term-input-test--with-window-buffer term
            ;; current-buffer はポインタ下とは別のバッファ
            (with-temp-buffer
              (wamei/term-input-forward-wheel
               (wamei/term-input-test--wheel-event 'wheel-down 3 7)))
            (should (equal wamei/term-input-test--sent '("\e[<65;4;8M")))
            (should (equal wamei/term-input-test--buffers (list (buffer-name term)))))
        (kill-buffer term)))))

(ert-deftest wamei/term-input-forward-wheel-in-copy-mode-scrolls-emacs ()
  "copy-mode 中は端末へ送らず Emacs の通常スクロールに任せる。"
  (wamei/term-input-test--with-vterm
    (let ((scrolled nil))
      (cl-letf (((symbol-function 'mwheel-scroll)
                 (lambda (event &optional _arg) (setq scrolled event))))
        (let ((term (generate-new-buffer " *term-under-mouse*")))
          (unwind-protect
              (progn
                (with-current-buffer term (setq-local vterm-copy-mode t))
                (wamei/term-input-test--with-window-buffer term
                  (let ((event (wamei/term-input-test--wheel-event 'wheel-up 0 0)))
                    (wamei/term-input-forward-wheel event)
                    (should-not wamei/term-input-test--sent)
                    (should (eq scrolled event)))))
            (kill-buffer term)))))))

;;; minor mode

(ert-deftest wamei/term-input-mouse-mode-binds-wheel-events ()
  "mode を有効にするとホイール系イベントが転送コマンドに束縛される。"
  (with-temp-buffer
    (wamei/term-input-mouse-mode 1)
    (dolist (key '([wheel-up] [wheel-down] [double-wheel-down] [triple-wheel-up]
                   [mouse-4] [mouse-5]))
      (should (eq (key-binding key) #'wamei/term-input-forward-wheel)))
    (should (eq (lookup-key wamei/term-input-mouse-mode-map [remap mwheel-scroll])
                #'wamei/term-input-forward-wheel))))

(ert-deftest wamei/term-input-mouse-mode-off-restores-bindings ()
  "mode を切ると束縛は元に戻る。"
  (with-temp-buffer
    (wamei/term-input-mouse-mode 1)
    (wamei/term-input-mouse-mode -1)
    (should-not (eq (key-binding [wheel-down]) #'wamei/term-input-forward-wheel))))

(provide 'term-input-test)
;;; term-input-test.el ends here
