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

(cl-defmethod codex--term-post-start ((_backend (eql ghostel)))
  ;; copy mode から戻っても Codex の改行・送信キーを維持する。
  (setq-local ghostel-semi-char-mode-map (current-local-map))
  (wamei/codex-ghostel-arrow-keys-mode 1))

(provide 'codex-ghostel)
;;; codex-ghostel.el ends here
