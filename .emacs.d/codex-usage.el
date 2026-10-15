;;; codex-usage.el --- Codex usage in the panel mode line -*- lexical-binding: t; -*-
;;; Commentary:
;; account/rateLimits/read を既存の CLI ログインで取得する。
;; 会話の生成・認証情報の読み出しは行わない。描画は Claude のバーと共通。
;;; Code:
(require 'json)
(require 'codex)
(require 'claude-usage)

(defvar wamei/codex-usage-interval 60 "使用量を更新する秒数。")
(defvar wamei/codex-usage--process nil)
(defvar wamei/codex-usage--timer nil)
(defvar wamei/codex-usage--timeout nil)
(defvar wamei/codex-usage--state nil)
(defvar wamei/codex-usage--updated-at nil)
(defvar wamei/codex-usage--error nil)
(defvar wamei/codex-usage--cache nil)

(defun wamei/codex-usage--entry (window)
  "使用量 WINDOW を共通描画用の plist に変換する。"
  (when-let* ((percent (alist-get 'usedPercent window))
              ((numberp percent)))
    (list :percent (round (max 0 (min 100 percent)))
          :minutes (alist-get 'windowDurationMins window)
          :resets-at (when-let* ((reset (alist-get 'resetsAt window))
                                ((numberp reset)))
                       (seconds-to-time reset)))))

(defun wamei/codex-usage--adopt (result)
  "RESULT の Codex 枠だけを表示に採用する。"
  (when-let* ((limits (or (alist-get 'codex (alist-get 'rateLimitsByLimitId result))
                          (alist-get 'rateLimits result))))
    (setq wamei/codex-usage--state
          (list :primary (wamei/codex-usage--entry (alist-get 'primary limits))
                :secondary (wamei/codex-usage--entry (alist-get 'secondary limits)))
          wamei/codex-usage--updated-at (float-time)
          wamei/codex-usage--error nil
          wamei/codex-usage--cache nil)
    (force-mode-line-update t)))

(defun wamei/codex-usage--label (entry fallback)
  "ENTRY の期間のラベル。期間不明の場合は FALLBACK。"
  (let ((minutes (plist-get entry :minutes)))
    (cond ((equal minutes 10080) "W")
          ((and (numberp minutes) (> minutes 0))
           (if (zerop (% minutes 60))
               (format "%dh" (/ minutes 60))
             (format "%dm" minutes)))
          (t fallback))))

(defun wamei/codex-usage-mode-line ()
  "取得済みの使用率とリセット時刻を描く。通信はしない。"
  (let* ((now (current-time))
         (key (list wamei/codex-usage--state wamei/codex-usage--error
                    (/ (floor (float-time now)) 60)
                    (wamei/claude-usage--images-p)
                    (frame-char-width) (frame-char-height)
                    (face-foreground 'wamei/claude-usage-normal nil t)
                    (face-foreground 'wamei/claude-usage-track nil t)))
         (primary (plist-get wamei/codex-usage--state :primary))
         (secondary (plist-get wamei/codex-usage--state :secondary)))
    (unless (equal key (car wamei/codex-usage--cache))
      (setq wamei/codex-usage--cache
            (cons key
                  (wamei/term-modeline-escape
                   (propertize
                    (concat (wamei/claude-usage--segment
                             (wamei/codex-usage--label primary "5h") primary now)
                            "  "
                            (wamei/claude-usage--segment
                             (wamei/codex-usage--label secondary "W") secondary now)
                            (if wamei/codex-usage--error " · ?" ""))
                    'help-echo (if wamei/codex-usage--error
                                   "使用量を取得できません。表示は最後の取得値です"
                                 "Codex の使用率。M-x wamei/codex-usage-refresh で更新"))))))
    (cdr wamei/codex-usage--cache)))

(defconst wamei/codex-usage--mode-line-format
  '((:eval (wamei/term-modeline-pad-spacer wamei/claude-usage-mode-line-height))
    " " (:eval (wamei/codex-usage-mode-line))
    (:eval (wamei/term-modeline-align (wamei/term-modeline-process-width)))
    mode-line-process))

(defun wamei/codex-usage--setup (buffer)
  "BUFFER のモードラインを Claude と同じ見た目にする。"
  (with-current-buffer buffer
    (when (bound-and-true-p hide-mode-line-mode)
      (hide-mode-line-mode -1))
    (setq-local mode-line-format wamei/codex-usage--mode-line-format)
    (wamei/claude-usage--blend-in)))

(defun wamei/codex-usage--visible-p ()
  "見えているフレームに Codex パネルがあれば非 nil。"
  (seq-some (lambda (frame)
              (and (frame-visible-p frame)
                   (seq-some (lambda (window)
                               (codex--buffer-p (window-buffer window)))
                             (window-list frame 'no-mini))))
            (frame-list)))

(defun wamei/codex-usage--send (process message)
  "PROCESS に JSON の MESSAGE を1行で送る。"
  (process-send-string process (concat (json-encode message) "\n")))

(defun wamei/codex-usage--cancel-timeout ()
  (when (timerp wamei/codex-usage--timeout)
    (cancel-timer wamei/codex-usage--timeout))
  (setq wamei/codex-usage--timeout nil))

(defun wamei/codex-usage--stop ()
  "取得用プロセスだけを終了する。"
  (wamei/codex-usage--cancel-timeout)
  (let ((process wamei/codex-usage--process))
    (setq wamei/codex-usage--process nil)
    (when (process-live-p process)
      (delete-process process))))

(defun wamei/codex-usage--failed ()
  "最後の取得値を残し、失敗を表示する。"
  (setq wamei/codex-usage--error t wamei/codex-usage--cache nil)
  (wamei/codex-usage--stop)
  (force-mode-line-update t))

(defun wamei/codex-usage--request (process)
  (process-put process 'pending t)
  (wamei/codex-usage--send process '((id . 2) (method . "account/rateLimits/read"))))

(defun wamei/codex-usage--message (process message)
  "PROCESS からの MESSAGE を処理する。"
  (let ((id (alist-get 'id message)))
    (cond
     ((and (memq id '(1 2)) (alist-get 'error message))
      (wamei/codex-usage--failed))
     ((eql id 1)
      (process-put process 'initialized t)
      (wamei/codex-usage--send process '((method . "initialized")))
      (wamei/codex-usage--request process))
     ((eql id 2)
      (process-put process 'pending nil)
      (wamei/codex-usage--cancel-timeout)
      (if (alist-get 'result message)
          (wamei/codex-usage--adopt (alist-get 'result message))
        (wamei/codex-usage--failed)))
     ((equal (alist-get 'method message) "account/rateLimits/updated")
      (wamei/codex-usage--adopt (alist-get 'params message))))))

(defun wamei/codex-usage--filter (process output)
  "改行区切り JSON を処理する。途中の行は PROCESS に残す。"
  (when (eq process wamei/codex-usage--process)
    (let* ((text (concat (process-get process 'partial) output))
           (lines (split-string text "\n")))
      (process-put process 'partial (car (last lines)))
      (dolist (line (butlast lines))
        (when (and (process-live-p process) (not (string-empty-p line)))
          (condition-case nil
              (wamei/codex-usage--message
               process (json-parse-string line :object-type 'alist :null-object nil
                                          :false-object nil))
            (error (wamei/codex-usage--failed))))))))

(defun wamei/codex-usage-refresh ()
  "既存の CLI ログインで使用量を非同期取得する。"
  (interactive)
  (unless (and (process-live-p wamei/codex-usage--process)
               (or (process-get wamei/codex-usage--process 'pending)
                   (not (process-get wamei/codex-usage--process 'initialized))))
    (condition-case nil
        (progn
          (setq wamei/codex-usage--timeout
                (run-at-time 30 nil #'wamei/codex-usage--failed))
          (if (process-live-p wamei/codex-usage--process)
              (wamei/codex-usage--request wamei/codex-usage--process)
            (let ((default-directory (expand-file-name "~/")))
              (setq wamei/codex-usage--process
                    (make-process
                     :name "codex-usage" :buffer nil :noquery t
                     :command (list codex-program "app-server" "--listen" "stdio://")
                     :connection-type 'pipe :coding 'utf-8-unix
                     :stderr (get-buffer-create " *codex-usage-stderr*")
                     :filter #'wamei/codex-usage--filter
                     :sentinel (lambda (process _event)
                                 (when (and (eq process wamei/codex-usage--process)
                                            (not (process-live-p process)))
                                   (wamei/codex-usage--failed)))))
              (wamei/codex-usage--send
               wamei/codex-usage--process
               '((id . 1) (method . "initialize")
                 (params (clientInfo (name . "wamei_emacs_usage")
                                     (title . "Emacs Codex usage") (version . "1"))))))))
      (error (wamei/codex-usage--failed)))))

(defun wamei/codex-usage--tick ()
  ;; 応答は tick の少し後に届くので、interval ちょうどで比べると次の tick で
  ;; 毎回「新しい」になり 1 回おきにしか取らない。Claude と同じ幅だけ削る。
  (if (wamei/codex-usage--visible-p)
      (unless (and wamei/codex-usage--updated-at
                   (not wamei/codex-usage--error)
                   (< (- (float-time) wamei/codex-usage--updated-at)
                      (- wamei/codex-usage-interval wamei/claude-usage--fresh-margin)))
        (wamei/codex-usage-refresh))
    (wamei/codex-usage--stop)))

(defvar wamei/codex-usage--was-visible nil
  "前回の window 変化のときにパネルが見えていたか。")

(defun wamei/codex-usage--on-window-change (_frame)
  "`window-state-change-functions' 用。パネルが見えた瞬間に tick する。
Claude と共有のサイドパネルで切り替えると `wamei/codex-panel-show' を通らず、
次のタイマーまで古い値が出たままになる。見えたままの変化では取り直さない。"
  (let ((visible (and (wamei/codex-usage--visible-p) t)))
    (when (and visible (not wamei/codex-usage--was-visible))
      (wamei/codex-usage--tick))
    (setq wamei/codex-usage--was-visible visible)))

(defun wamei/codex-usage--after-display (buffer)
  (wamei/codex-usage--setup buffer)
  (wamei/codex-usage--tick))

(defun wamei/codex-usage--invalidate (&rest _)
  (setq wamei/codex-usage--cache nil)
  (dolist (buffer (codex--find-all-codex-buffers))
    (wamei/codex-usage--setup buffer))
  (force-mode-line-update t))

(defun wamei/codex-usage-enable ()
  "Codex パネルの使用量表示を有効にする。"
  (advice-add 'wamei/codex-panel-show :after #'wamei/codex-usage--after-display)
  (add-hook 'kill-emacs-hook #'wamei/codex-usage--stop)
  (add-hook 'enable-theme-functions #'wamei/codex-usage--invalidate)
  (add-hook 'window-state-change-functions #'wamei/codex-usage--on-window-change)
  (wamei/codex-usage--invalidate)
  (unless (timerp wamei/codex-usage--timer)
    (setq wamei/codex-usage--timer
          (run-with-timer wamei/codex-usage-interval wamei/codex-usage-interval
                          #'wamei/codex-usage--tick))))

(provide 'codex-usage)
;;; codex-usage.el ends here
