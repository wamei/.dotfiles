;;; ai-panel.el --- Claude と Codex の共通セッションパネル -*- lexical-binding: t; -*-
;;; Commentary:
;; バックエンドの起動・入力・usage は従来の処理を使い、表示と巡回を共有する。
;;; Code:
(require 'ai-shortcuts)
(require 'header-tabs)
(require 'claude-grid)
(eval-when-compile (require 'claude-code-ide-mcp nil t))

(defvar claude-code-ide-window-width)
(defvar claude-code-ide-window-side)
(defvar claude-code-ide-use-side-window)
(defvar claude-code-ide-focus-on-open)
(defvar wamei/codex-panel-width)
(defvar wamei/codex-panel-slot)
(defvar wamei/ai-panel--order nil)
(defvar wamei/ai-panel--previous-window nil)
(defvar wamei/ai-panel--previous-buffer nil)
(defvar wamei/ai-panel--grid-timer nil)

(defun wamei/ai-panel--directory (&optional buffer)
  (let* ((buffer (or buffer (current-buffer)))
         (backend (or (wamei/ai--backend buffer)
                      (and (fboundp 'codex--buffer-p) (codex--buffer-p buffer) 'codex)
                      (and (fboundp 'claude-code-ide--buffer-session)
                           (claude-code-ide--buffer-session buffer) 'claude))))
    (or (and backend (wamei/ai--directory buffer backend)) (wamei/ai--root))))

(defun wamei/ai-panel--same-directory-p (a b)
  (and a b (equal (file-name-as-directory (file-truename a))
                  (file-name-as-directory (file-truename b)))))

(defun wamei/ai-panel--buffers (&optional directory)
  "起動中の AI を初回表示順に返す。DIRECTORY があれば同プロジェクトだけ。"
  (let ((active (seq-filter #'wamei/ai--backend (buffer-list))))
    (setq wamei/ai-panel--order
          (seq-filter (lambda (buffer) (memq buffer active)) wamei/ai-panel--order))
    (dolist (buffer active)
      (unless (memq buffer wamei/ai-panel--order)
        (setq wamei/ai-panel--order (append wamei/ai-panel--order (list buffer)))))
    (if directory
        (seq-filter (lambda (buffer)
                      (wamei/ai-panel--same-directory-p
                       directory (wamei/ai--directory buffer (wamei/ai--backend buffer))))
                    wamei/ai-panel--order)
      wamei/ai-panel--order)))

(defun wamei/ai-panel--window (&optional side-only)
  "選択フレームの AI window。SIDE-ONLY なら右パネルのみ。"
  (seq-find (lambda (window)
              (and (or (not side-only) (eq (window-parameter window 'window-side) 'right))
                   (or (and side-only (window-parameter window 'wamei/ai-panel))
                       (wamei/ai--backend (window-buffer window)))))
            (window-list nil 'no-mini)))

(defvar wamei/ai-panel-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'wamei/ai-panel-tab-select)
    map))

(defun wamei/ai-panel--tabs ()
  (mapcar
   (lambda (buffer)
     (let* ((backend (wamei/ai--backend buffer))
            (name (pcase backend
                    ('claude (wamei/claude-panel--tab-name buffer))
                    ('codex (wamei/codex-panel--tab-name buffer))))
            (label (format "%s: %s" (if (eq backend 'claude) "Claude" "Codex") name)))
       (list :label label :current (eq buffer (current-buffer))
             :properties (list 'wamei/ai-buffer buffer 'local-map wamei/ai-panel-tab-map
                               'help-echo label))))
   (wamei/ai-panel--buffers (wamei/ai-panel--directory))))

(define-minor-mode wamei/ai-panel-keys-mode
  "AI の種類をまたいでセッションを巡回する。"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<C-tab>") #'wamei/ai-panel-next)
            (define-key map (kbd "<C-S-tab>") #'wamei/ai-panel-previous)
            (define-key map (kbd "<C-S-iso-lefttab>") #'wamei/ai-panel-previous)
            (define-key map (kbd "s-v") #'wamei/term-input-paste)
            map))

(defun wamei/ai-panel--setup (buffer)
  "BUFFER に共通タブを付ける。入力と usage はバックエンドのものを保つ。"
  (when (buffer-live-p buffer)
    (unless (memq buffer wamei/ai-panel--order)
      (setq wamei/ai-panel--order (append wamei/ai-panel--order (list buffer))))
    (with-current-buffer buffer
      (when (bound-and-true-p wamei/claude-panel-keys-mode)
        (wamei/claude-panel-keys-mode -1))
      (when (bound-and-true-p wamei/codex-panel-keys-mode)
        (wamei/codex-panel-keys-mode -1))
      (wamei/ai-panel-keys-mode 1)
      (setq-local header-line-format '(:eval (wamei/header-tabs-format (wamei/ai-panel--tabs))))
      (add-hook 'kill-buffer-hook #'wamei/ai-panel--before-kill nil t))))

(defun wamei/ai-panel--display-claude (orig buffer)
  "Claude の native 表示を共有パネルの slot と幅で実行する。"
  (let* ((window (wamei/ai-panel--window t))
         (claude-code-ide-window-side 'right)
         (claude-code-ide-use-side-window t)
         (claude-code-ide-window-width
          (if window (window-body-width window) claude-code-ide-window-width)))
    (when-let* ((session (claude-code-ide--buffer-session buffer)))
      (setf (claude-code-ide-mcp-session-window-slot session)
            (if window (window-parameter window 'window-slot) 100)))
    (let ((shown (funcall orig buffer)))
      (when shown (set-window-parameter shown 'wamei/ai-panel t))
      (wamei/ai-panel--setup buffer)
      (wamei/ai-panel--schedule-grid)
      shown)))

(defun wamei/ai-panel--display-codex (orig buffer)
  (let* ((window (wamei/ai-panel--window t))
         (wamei/codex-panel-slot (if window (window-parameter window 'window-slot) 100))
         (wamei/codex-panel-width (if window (window-total-width window) wamei/codex-panel-width))
         (shown (funcall orig buffer)))
    (when shown (set-window-parameter shown 'wamei/ai-panel t))
    (wamei/ai-panel--setup buffer)
    (wamei/ai-panel--schedule-grid)
    shown))

(defun wamei/ai-panel-show (buffer)
  "BUFFER を共通右パネルに表示する。"
  (pcase (wamei/ai--backend buffer)
    ('claude (let ((claude-code-ide-focus-on-open nil))
               (wamei/claude-panel--show buffer)))
    ('codex (wamei/codex-panel-show buffer))))

(defun wamei/ai-panel-tab-select (event)
  (interactive "e")
  (when-let* ((buffer (wamei/header-tabs-event-property event 'wamei/ai-buffer))
              ((buffer-live-p buffer)))
    (with-selected-window (posn-window (event-start event))
      (when-let* ((window (wamei/ai-panel-show buffer))) (select-window window)))))

(defun wamei/ai-panel--cycle (offset)
  (let* ((target (cdr (wamei/ai--target)))
         (directory (if target (wamei/ai-panel--directory target) (wamei/ai--root)))
         (buffers (wamei/ai-panel--buffers directory)))
    (when (> (length buffers) 1)
      (wamei/ai-panel-show
       (nth (mod (+ (or (seq-position buffers target) 0) offset) (length buffers)) buffers)))))

(defun wamei/ai-panel-next (&optional _arg)
  (interactive "P") (wamei/ai-panel--cycle 1))
(defun wamei/ai-panel-previous (&optional _arg)
  (interactive "P") (wamei/ai-panel--cycle -1))

(defun wamei/ai-panel-toggle (&optional arg)
  "共通パネルへ出入りする。未起動なら Claude。ARG 付きなら閉じる。"
  (interactive "P")
  (let ((window (wamei/ai-panel--window)))
    (cond
     (arg (when (and window (window-parent window)) (delete-window window)))
     ((eq window (selected-window))
      (when-let* ((back (or (and (window-live-p wamei/ai-panel--previous-window)
                                (eq (window-buffer wamei/ai-panel--previous-window)
                                    wamei/ai-panel--previous-buffer)
                                wamei/ai-panel--previous-window)
                           (and (buffer-live-p wamei/ai-panel--previous-buffer)
                                (get-buffer-window wamei/ai-panel--previous-buffer))
                           (get-mru-window nil t t t))))
        (unless (eq back window) (select-window back))))
     (t
      (setq wamei/ai-panel--previous-window (selected-window)
            wamei/ai-panel--previous-buffer (current-buffer))
      (if window (select-window window)
        (let ((target (wamei/ai--target)))
          (if (cdr target)
              (when-let* ((shown (wamei/ai-panel-show (cdr target)))) (select-window shown))
            (wamei/claude-toggle))))))))

(defun wamei/ai-panel--hand-over (buffer)
  "終了する BUFFER の右パネルを別の AI に引き継ぐ。"
  (when (buffer-live-p buffer)
    (let ((others (remq buffer (wamei/ai-panel--buffers (wamei/ai-panel--directory buffer)))))
      (dolist (window (get-buffer-window-list buffer nil 'visible))
        (when (and others (eq (window-parameter window 'window-side) 'right))
          (with-selected-window window
            (wamei/ai-panel-show (car others)))))))
  (force-mode-line-update t)
  (wamei/ai-panel--schedule-grid))

(defun wamei/ai-panel--before-kill ()
  (wamei/ai-panel--hand-over (current-buffer)))
(defun wamei/ai-panel--claude-hand-over (_orig session)
  (wamei/ai-panel--hand-over (claude-code-ide-mcp-session-buffer session)))
(defun wamei/ai-panel--exited (buffer &rest _)
  (when (and (buffer-live-p buffer)
             (buffer-local-value 'wamei/ai-panel-keys-mode buffer))
    (wamei/ai-panel--hand-over buffer)))

(defun wamei/ai-panel--grid-setup (buffer)
  (pcase (wamei/ai--backend buffer)
    ('claude (wamei/claude-panel--setup buffer)
             (when (fboundp 'wamei/claude-usage--setup) (wamei/claude-usage--setup buffer)))
    ('codex (wamei/codex-panel--setup buffer)
            (when (fboundp 'wamei/codex-usage--setup) (wamei/codex-usage--setup buffer))))
  (wamei/ai-panel--setup buffer))

(defun wamei/ai-panel-grid-tab (&optional arg)
  "Claude と Codex を AI タブに並べる。ARG 付きなら全プロジェクト。"
  (interactive "P")
  (let* ((directory (and (not arg) (wamei/ai-panel--directory)))
         (buffers (wamei/ai-panel--buffers directory))
         (wamei/claude-grid-tab-name "AI"))
    (if (null buffers) (message "実行中の AI セッションがありません")
      (wamei/claude-grid--select-tab (wamei/claude-grid--tab-name directory))
      (wamei/claude-grid--build buffers #'wamei/ai-panel--grid-setup))))

(defun wamei/ai-panel--rearrange-grid ()
  (setq wamei/ai-panel--grid-timer nil)
  (if (wamei/claude-grid--minibuffer-busy-p)
      (setq wamei/ai-panel--grid-timer (run-at-time 1 nil #'wamei/ai-panel--rearrange-grid))
    (let ((wamei/claude-grid-tab-name "AI"))
      (dolist (frame (frame-list))
        (with-selected-frame frame
          (when-let* ((scope (wamei/claude-grid--tab-scope
                             (alist-get 'name (tab-bar--current-tab-find)))))
            (let* ((current (current-buffer))
                   (buffers (wamei/ai-panel--buffers))
                   (buffers (if (stringp scope)
                                (seq-filter
                                 (lambda (buffer)
                                   (equal scope (wamei/claude-grid--project-name
                                                 (wamei/ai-panel--directory buffer)))) buffers)
                              buffers)))
              (if (null buffers) (wamei/claude-grid--close-tab)
                (let ((windows (wamei/claude-grid--build buffers #'wamei/ai-panel--grid-setup)))
                  (when-let* ((window (seq-find (lambda (window)
                                                 (eq (window-buffer window) current)) windows)))
                    (select-window window)))))))))))

(defun wamei/ai-panel--schedule-grid (&rest _)
  (unless (timerp wamei/ai-panel--grid-timer)
    (setq wamei/ai-panel--grid-timer (run-at-time 0 nil #'wamei/ai-panel--rearrange-grid))))

(defun wamei/ai-panel-restore (spec)
  "会話を ID 指定で復元し、保存時に表示していたパネルだけを開き直す。"
  (wamei/ai-session-restore-panel spec))

(defun wamei/ai-panel-enable ()
  "両バックエンドの表示・タブ・復元を統合する。何度呼んでもよい。"
  (setq wamei/ai-panel-enabled t)
  ;; 旧形式の各 AI の直近1会話の記録は使わず、ai-session の ID 一覧で復元する。
  (advice-remove 'wamei/desktop-side--spec #'wamei/ai-panel--save-spec)
  (advice-add 'wamei/claude-panel--setup :after #'wamei/ai-panel--setup)
  (advice-add 'wamei/codex-panel--setup :after #'wamei/ai-panel--setup)
  (advice-add 'claude-code-ide--display-buffer-in-side-window :around #'wamei/ai-panel--display-claude)
  (advice-add 'wamei/codex-panel-show :around #'wamei/ai-panel--display-codex)
  (advice-add 'wamei/claude-panel--hand-over :around #'wamei/ai-panel--claude-hand-over)
  (add-hook 'ghostel-exit-functions #'wamei/ai-panel--exited)
  (mapc #'wamei/ai-panel--setup (wamei/ai-panel--buffers)))

(provide 'ai-panel)
;;; ai-panel.el ends here
