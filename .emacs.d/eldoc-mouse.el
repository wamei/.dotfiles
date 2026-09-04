;;; eldoc-mouse.el --- マウスの下のシンボルの eldoc を child frame で表示する -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; `wamei/eldoc-mouse-mode' を有効にしたバッファでマウスがシンボルの上に止まると、
;; その位置の `eldoc-documentation-functions' を呼び、結果を eldoc-box の child frame
;; でマウスの下に表示する。カーソル位置の eldoc (eldoc-mode / eldoc-box-hover-at-point-mode)
;; とは独立に動く。
;;
;; eldoc-box 同梱の `eldoc-box-mouse-mode' はバッファの eldoc-mode を切ってしまうので使わない。
;; 表示は `eldoc-display-functions' を通さず、eldoc の内部変数 `eldoc--make-callback' を
;; 差し替えてコールバックを自前で受ける。これで eglot の非同期応答もそのまま扱える。
;;
;;; Code:

(require 'cl-lib)
(require 'eldoc)
(require 'thingatpt)

(declare-function eldoc-box--display "eldoc-box" (str))
(declare-function eldoc-box-quit-frame "eldoc-box" ())
(declare-function eldoc-box--compose-doc "eldoc-box" (doc))
(declare-function eldoc-box--point-position-relative-to-native-frame "eldoc-box"
                  (&optional point window))
(defvar eldoc-box--frame)
(defvar eldoc-box--buffer)
(defvar eldoc-box-position-function)
(defvar eldoc-box-doc-separator)

(defgroup wamei/eldoc-mouse nil
  "マウスの下のシンボルの eldoc を表示する。"
  :group 'eldoc)

(defcustom wamei/eldoc-mouse-delay 0.5
  "マウスがシンボルの上で止まってから表示するまでの秒数。"
  :type 'number)

(defvar wamei/eldoc-mouse-position-function #'wamei/eldoc-mouse-default-position
  "child frame の位置を決める関数。
引数は ANCHOR WIDTH HEIGHT。ANCHOR はマウスの下の文字の位置 (X . Y) で、
native frame からの相対ピクセル (tty では桁・行)。WIDTH と HEIGHT は child frame の
大きさ。返り値は (X . Y)。")

;;;; 状態

(defvar wamei/eldoc-mouse-mode)

(defvar wamei/eldoc-mouse--timer nil
  "表示待ちのタイマー。")

(defvar wamei/eldoc-mouse--active nil
  "いま追っている対象 (WINDOW POS BEG . END)。
マウスがシンボルに入ってから出るまで同じオブジェクトを保ち、非同期応答が
この対象のものかを `eq' で確かめる。追っていなければ nil。")

(defvar wamei/eldoc-mouse--shown nil
  "自分で child frame を表示中なら非 nil。")

(defvar wamei/eldoc-mouse--box-tick nil
  "自分が表示したときの eldoc-box のバッファの `buffer-modified-tick'。
閉じる前にこれと比べ、カーソル位置の eldoc-box が上書きした box は閉じない。")

;;;; ハンドラ

(defun wamei/eldoc-mouse--symbol-bounds (pos)
  "現在のバッファの POS にあるシンボルの範囲 (BEG . END) を返す。無ければ nil。
`bounds-of-thing-at-point' はシンボル直後の空白でも前のシンボルを返すので、
POS の文字自体がシンボル構成文字であることを先に確かめる。"
  (when-let* ((char (char-after pos)))
    (when (memq (char-syntax char) '(?w ?_))
      (save-excursion
        (goto-char pos)
        (bounds-of-thing-at-point 'symbol)))))

(defun wamei/eldoc-mouse--cancel-timer ()
  "表示待ちのタイマーを取り消す。"
  (when wamei/eldoc-mouse--timer
    (cancel-timer wamei/eldoc-mouse--timer)
    (setq wamei/eldoc-mouse--timer nil)))

(defun wamei/eldoc-mouse--in-eldoc-box-p (window)
  "WINDOW が eldoc-box の child frame のものなら非 nil。"
  (and (bound-and-true-p eldoc-box--frame)
       (eq (window-frame window) eldoc-box--frame)))

(defun wamei/eldoc-mouse--still-active-p (window pos)
  "WINDOW の POS が追っている対象のシンボルの中なら非 nil。"
  (pcase wamei/eldoc-mouse--active
    (`(,active-window ,_ ,beg . ,end)
     (and (eq (window-buffer active-window) (window-buffer window))
          (<= beg pos) (< pos end)))))

(defun wamei/eldoc-mouse--reset ()
  "追っている対象を捨て、タイマーと表示を片付ける。"
  (wamei/eldoc-mouse--cancel-timer)
  (when wamei/eldoc-mouse--shown
    (wamei/eldoc-mouse--hide-box)
    (setq wamei/eldoc-mouse--shown nil))
  (setq wamei/eldoc-mouse--active nil))

(defun wamei/eldoc-mouse--handle-motion (event)
  "マウス移動 EVENT を受けて、表示の予約・維持・取り消しを行う。"
  (interactive "e")
  ;; post-command-hook で `this-command' を見る側 (corfu の継続判定など) には、
  ;; 既定のマウス移動の束縛と同じ `ignore' に見せる。
  (setq this-command 'ignore)
  (let* ((posn (event-end event))
         (window (posn-window posn))
         (pos (posn-point posn)))
    (when (and (windowp window)
               (integerp pos)
               (not (wamei/eldoc-mouse--in-eldoc-box-p window))
               (not (wamei/eldoc-mouse--still-active-p window pos)))
      (wamei/eldoc-mouse--reset)
      (with-current-buffer (window-buffer window)
        (when-let* ((bounds (and wamei/eldoc-mouse-mode
                                 (wamei/eldoc-mouse--symbol-bounds pos))))
          (setq wamei/eldoc-mouse--active (cons window (cons pos bounds)))
          (setq wamei/eldoc-mouse--timer
                (run-with-timer wamei/eldoc-mouse-delay nil
                                #'wamei/eldoc-mouse--request
                                wamei/eldoc-mouse--active)))))))

;;;; 他の post-command 処理との共存

(defun wamei/eldoc-mouse--pre-command ()
  "マウス移動以外のコマンドが来たら、予約と表示を片付ける。"
  (unless (eq this-command 'wamei/eldoc-mouse--handle-motion)
    (wamei/eldoc-mouse--reset)))

(defun wamei/eldoc-mouse--unless-mouse-motion (fn &rest args)
  "マウス移動イベント以外のときだけ FN を ARGS で呼ぶ。
eldoc-box-hover-at-point-mode の `eldoc-box--follow-cursor' は post-command-hook で
「入力以外のコマンド」が来るとカーソル位置の box を閉じて 0.5 秒表示を止める。
マウス移動もコマンドとして走るのでそのままだとマウスを動かすたびに box が消える。"
  (unless (mouse-movement-p last-input-event)
    (apply fn args)))

(with-eval-after-load 'eldoc-box
  (advice-add 'eldoc-box--follow-cursor :around #'wamei/eldoc-mouse--unless-mouse-motion))

;;;; 取得

(defun wamei/eldoc-mouse--request (target)
  "TARGET (WINDOW POS BEG . END) のシンボルのドキュメントを取得して表示する。
`eldoc--invoke-strategy' と同じ手順で `eldoc-documentation-strategy' を呼ぶが、
表示先は `eldoc-display-functions' ではなく `wamei/eldoc-mouse--display'。
コールバックの :eager / :patient / :enthusiast の扱いも eldoc に合わせる。"
  (setq wamei/eldoc-mouse--timer nil)
  (pcase-let ((`(,window ,pos . ,_) target))
    (when (window-live-p window)
      (with-current-buffer (window-buffer window)
        (save-excursion
          (goto-char pos)
          (let* ((howmany 0)
                 (want 0)
                 (docs nil)
                 (display
                  (lambda ()
                    (when (eq target wamei/eldoc-mouse--active)
                      (wamei/eldoc-mouse--display
                       (mapcar #'cdr (sort (copy-sequence docs)
                                           (lambda (a b) (< (car a) (car b)))))
                       target))))
                 (register
                  (lambda (index string plist)
                    (when (and string (> (length string) 0))
                      (push (cons index (cons string plist)) docs))))
                 (eldoc--make-callback
                  (lambda (method _origin)
                    (let ((index (prog1 howmany (cl-incf howmany))))
                      (pcase-exhaustive method
                        (:enthusiast
                         (lambda (string &rest plist)
                           (when (cl-loop for (i) in docs never (< i index))
                             (setq docs nil)
                             (funcall register index string plist)
                             (funcall display))
                           t))
                        (:patient
                         (cl-incf want)
                         (lambda (string &rest plist)
                           (funcall register index string plist)
                           (when (zerop (cl-decf want)) (funcall display))
                           t))
                        (:eager
                         (lambda (string &rest plist)
                           (funcall register index string plist)
                           (funcall display)
                           t))))))
                 (res (funcall eldoc-documentation-strategy)))
            (when (stringp res)
              (funcall register 0 res nil)
              (funcall display))))))))

;;;; 表示

(defun wamei/eldoc-mouse--compose (docs)
  "DOCS ((STRING . PLIST) ...) を 1 つの文字列にまとめる。"
  (string-trim
   (string-join (mapcar (lambda (doc)
                          (if (fboundp 'eldoc-box--compose-doc)
                              (eldoc-box--compose-doc doc)
                            (car doc)))
                        docs)
                (if (boundp 'eldoc-box-doc-separator) eldoc-box-doc-separator "\n\n"))))

(defun wamei/eldoc-mouse--display (docs target)
  "DOCS ((STRING . PLIST) ...) を TARGET (WINDOW POS BEG . END) の位置に表示する。"
  (let ((string (wamei/eldoc-mouse--compose docs)))
    (unless (string-empty-p string)
      (wamei/eldoc-mouse--show-box string (car target) (cadr target))
      (setq wamei/eldoc-mouse--shown t))))

(defun wamei/eldoc-mouse-default-position (anchor _width _height)
  "ANCHOR の右下に置く既定の位置関数。"
  (cons (+ (car anchor) (frame-char-width))
        (+ (cdr anchor) (frame-char-height))))

(defun wamei/eldoc-mouse--show-box (string window pos)
  "STRING を eldoc-box の child frame で WINDOW の POS の文字の近くに表示する。"
  (let* ((anchor (eldoc-box--point-position-relative-to-native-frame pos window))
         (eldoc-box-position-function
          (lambda (width height)
            (funcall wamei/eldoc-mouse-position-function anchor width height))))
    (with-current-buffer (window-buffer window)
      (eldoc-box--display string))
    (setq wamei/eldoc-mouse--box-tick (wamei/eldoc-mouse--current-box-tick))))

(defun wamei/eldoc-mouse--current-box-tick ()
  "eldoc-box のドキュメントバッファの `buffer-modified-tick'。バッファが無ければ nil。"
  (when-let* ((buffer (and (boundp 'eldoc-box--buffer) (get-buffer eldoc-box--buffer))))
    (buffer-modified-tick buffer)))

(defun wamei/eldoc-mouse--hide-box ()
  "自分が表示した eldoc-box の child frame を閉じる。
表示後に別の経路 (カーソル位置の eldoc) が中身を書き換えていれば、その box は
自分のものではないので触らない。"
  (when (and (fboundp 'eldoc-box-quit-frame)
             (equal wamei/eldoc-mouse--box-tick (wamei/eldoc-mouse--current-box-tick)))
    (eldoc-box-quit-frame)))

;;;; マイナーモード

(defvar-local wamei/eldoc-mouse--old-track-mouse nil
  "モードを有効にする前の `track-mouse' の値。")

(defvar wamei/eldoc-mouse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-movement] #'wamei/eldoc-mouse--handle-motion)
    map)
  "`wamei/eldoc-mouse-mode' のキーマップ。")

;;;###autoload
(define-minor-mode wamei/eldoc-mouse-mode
  "マウスの下のシンボルの eldoc を child frame で表示する。"
  :lighter nil
  :keymap wamei/eldoc-mouse-mode-map
  (if wamei/eldoc-mouse-mode
      (progn
        (setq wamei/eldoc-mouse--old-track-mouse track-mouse)
        ;; マウス移動イベントは `track-mouse' が非 nil のときだけ届く
        (setq-local track-mouse t)
        (add-hook 'pre-command-hook #'wamei/eldoc-mouse--pre-command nil t))
    (remove-hook 'pre-command-hook #'wamei/eldoc-mouse--pre-command t)
    (wamei/eldoc-mouse--reset)
    (setq track-mouse wamei/eldoc-mouse--old-track-mouse)
    (kill-local-variable 'track-mouse)))

(provide 'eldoc-mouse)
;;; eldoc-mouse.el ends here
