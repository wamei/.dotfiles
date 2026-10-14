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

(cl-defmethod codex--term-post-start ((_backend (eql ghostel)))
  ;; copy mode から戻っても Codex の改行・送信キーを維持する。
  (setq-local ghostel-semi-char-mode-map (current-local-map)))

(provide 'codex-ghostel)
;;; codex-ghostel.el ends here
