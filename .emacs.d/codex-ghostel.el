;;; codex-ghostel.el --- Ghostel backend for Codex -*- lexical-binding: t; -*-
;;; Commentary:
;; codex.el の端末 API を Ghostel に接続する。パッケージ更新では消えない。
;;; Code:
(require 'codex)
(require 'ghostel)

(cl-defmethod codex--term-make ((_backend (eql ghostel)) buffer-name program
                               &optional switches)
  (let ((buffer (get-buffer-create buffer-name)))
    (condition-case err
        (progn
          (ghostel-exec buffer program switches)
          buffer)
      (error (kill-buffer buffer)
             (signal (car err) (cdr err))))))

(cl-defmethod codex--term-send-string ((_backend (eql ghostel)) string)
  (ghostel-send-string (encode-coding-string string 'utf-8-unix)))

(cl-defmethod codex--term-send-action ((_backend (eql ghostel)) action
                                      &optional payload)
  (pcase action
    (:string (codex--term-send-string 'ghostel payload))
    (:return (ghostel-send-key "return"))
    (:escape (ghostel-send-key "escape"))
    (:newline (ghostel-send-key "j" "ctrl"))
    (:tab (ghostel-send-key "tab"))
    (:previous-agent (ghostel-send-key "left" "alt"))
    (:next-agent (ghostel-send-key "right" "alt"))
    (:redraw (ghostel-send-key "l" "ctrl"))
    (_ (error "Unknown Ghostel terminal action: %S" action))))

(cl-defmethod codex--term-submit-command ((_backend (eql ghostel)) command)
  (ghostel-paste-string command)
  (codex--schedule-submit-returns command (current-buffer) (selected-window)))

(cl-defmethod codex--term-kill-process ((_backend (eql ghostel)) buffer)
  (when-let* ((process (get-buffer-process buffer)))
    (delete-process process)))

(cl-defmethod codex--term-read-only-mode ((_backend (eql ghostel)))
  (ghostel-copy-mode))

(cl-defmethod codex--term-interactive-mode ((_backend (eql ghostel)))
  (ghostel-semi-char-mode))

(cl-defmethod codex--term-in-read-only-p ((_backend (eql ghostel)))
  (memq ghostel--input-mode '(copy emacs)))

(cl-defmethod codex--term-configure ((_backend (eql ghostel)))
  ;; Codex はバッファ名でセッションを探すので OSC の自動リネームを止める。
  (setq-local ghostel-buffer-name-function nil)
  (when-let* ((process (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag process nil)))

(cl-defmethod codex--term-customize-faces ((_backend (eql ghostel))) nil)

(cl-defmethod codex--term-get-adjust-process-window-size-fn
  ((_backend (eql ghostel)))
  ;; サイズ同期は Ghostel が担当する。
  nil)

(defmacro wamei/codex-ghostel--define-arrow (name key)
  "KEY (\"up\" など) を端末へ送るコマンド NAME を定義する。"
  `(defun ,name ()
     ,(format "Codex に %s キーを送る。" key)
     (interactive)
     (ghostel-send-key ,key)))

(wamei/codex-ghostel--define-arrow wamei/codex-ghostel-up "up")
(wamei/codex-ghostel--define-arrow wamei/codex-ghostel-down "down")
(wamei/codex-ghostel--define-arrow wamei/codex-ghostel-right "right")
(wamei/codex-ghostel--define-arrow wamei/codex-ghostel-left "left")

(defun wamei/codex-ghostel--when-input (command)
  "入力モードのときだけ COMMAND を返す `menu-item' の :filter。
copy mode では nil を返して、下のキーマップ (Emacs の行移動) に譲る。"
  (and (ghostel--terminal-input-mode-p) command))

(defvar wamei/codex-ghostel-arrow-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,key . ,command)
                   '(("C-p" . wamei/codex-ghostel-up)
                     ("C-n" . wamei/codex-ghostel-down)
                     ("C-f" . wamei/codex-ghostel-right)
                     ("C-b" . wamei/codex-ghostel-left)))
      (define-key map (kbd key)
                  `(menu-item "" ,command :filter wamei/codex-ghostel--when-input)))
    map)
  "Codex の端末で Emacs の移動キーを矢印キーに置き換えるキーマップ。")

(define-minor-mode wamei/codex-ghostel-arrow-keys-mode
  "Codex の端末で C-p/C-n/C-f/C-b を矢印キーとして送る。
Codex の選択式の質問 (request_user_input) は選択肢の移動が矢印キーだけで、
Ctrl+P/N は前後の質問の切り替えに固定されている ([tui.keymap] でも変えられない)。
入力欄では矢印キーと Ctrl+P/N/F/B は同じ動きなので置き換えても困らない。
入力モードの出入りで ghostel が `use-local-map' を差し替えるので、
ローカルマップではなく minor mode のキーマップで持つ。"
  :keymap wamei/codex-ghostel-arrow-keys-mode-map)

(defvar-local wamei/codex-ghostel--copy-notice nil
  "コピー直後に mode-line の右端に表示する文字列。")

(defvar-local wamei/codex-ghostel--copy-notice-timer nil
  "コピー通知を消すタイマー。")

(defun wamei/codex-ghostel--show-copy-notice (count)
  "COUNT 文字のコピー通知を2秒間表示する。"
  (when (timerp wamei/codex-ghostel--copy-notice-timer)
    (cancel-timer wamei/codex-ghostel--copy-notice-timer))
  (setq wamei/codex-ghostel--copy-notice
        (format "copied %d chars to clipboard" count)
        wamei/codex-ghostel--copy-notice-timer
        (run-at-time
         2 nil
         (lambda (buffer)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (setq wamei/codex-ghostel--copy-notice nil
                     wamei/codex-ghostel--copy-notice-timer nil)
               (force-mode-line-update))))
         (current-buffer)))
  (force-mode-line-update))

(defun wamei/codex-ghostel--mouse-copy-and-resume (original &rest args)
  "Codex のマウス選択をコピーし、端末の入力モードへ戻す。"
  (if (not (and (bound-and-true-p wamei/codex-ghostel-arrow-keys-mode)
                (eq ghostel--input-mode 'semi-char)))
      (apply original args)
    (let ((inhibit-message t))
      (prog1 (apply original args)
        (when (use-region-p)
          (kill-new (buffer-substring-no-properties
                     (region-beginning) (region-end)))
          (wamei/codex-ghostel--show-copy-notice
           (- (region-end) (region-beginning)))
          (deactivate-mark))
        (when (memq ghostel--input-mode '(copy emacs))
          (ghostel-semi-char-mode))))))

;; リリース時にコピーする。press はドラッグの追跡も担当するので触らない。
(dolist (command '(ghostel-mouse-release-or-set-point
                   ghostel-mouse-drag-or-set-region))
  (advice-add command :around #'wamei/codex-ghostel--mouse-copy-and-resume))

(cl-defmethod codex--term-post-start ((_backend (eql ghostel)))
  ;; copy mode から戻っても Codex の改行・送信キーを維持する。
  (setq-local ghostel-semi-char-mode-map (current-local-map))
  (wamei/codex-ghostel-arrow-keys-mode 1))

(provide 'codex-ghostel)
;;; codex-ghostel.el ends here
