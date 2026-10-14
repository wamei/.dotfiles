;;; codex-grid.el --- Codex のセッションをグリッドで表示 -*- lexical-binding: t; -*-
;;; Code:
(require 'codex)
(require 'codex-panel)
(require 'claude-grid)

(defvar wamei/codex-grid--timer nil)

(defun wamei/codex-grid--buffers (&optional directory)
  (sort (if directory
            (codex--find-codex-buffers-for-directory directory)
          (codex--find-all-codex-buffers))
        (lambda (a b) (string-lessp (buffer-name a) (buffer-name b)))))

(defun wamei/codex-grid--setup (buffer)
  (wamei/codex-panel--setup buffer)
  (when (fboundp 'wamei/codex-usage--setup)
    (wamei/codex-usage--setup buffer)))

(defun wamei/codex-grid-tab (&optional arg)
  "Codex を専用タブに並べる。ARG 付きなら全プロジェクト。"
  (interactive "P")
  (let* ((directory (and (not arg) (codex--directory)))
         (buffers (wamei/codex-grid--buffers directory))
         (wamei/claude-grid-tab-name "Codex"))
    (if (null buffers)
        (message "実行中の Codex セッションがありません")
      (wamei/claude-grid--select-tab (wamei/claude-grid--tab-name directory))
      (let ((windows (wamei/claude-grid--build buffers #'wamei/codex-grid--setup)))
        (when (< (length windows) (length buffers))
          (message "高さが足りないため %d 個のうち %d 個だけ並べました"
                   (length buffers) (length windows)))))))

(defun wamei/codex-grid--rearrange ()
  (setq wamei/codex-grid--timer nil)
  (if (wamei/claude-grid--minibuffer-busy-p)
      (setq wamei/codex-grid--timer
            (run-at-time 1 nil #'wamei/codex-grid--rearrange))
    (let ((wamei/claude-grid-tab-name "Codex"))
      (dolist (frame (frame-list))
        (with-selected-frame frame
          (when-let* ((scope (wamei/claude-grid--tab-scope
                             (alist-get 'name (tab-bar--current-tab-find)))))
            (let* ((current (current-buffer))
                   (buffers (wamei/codex-grid--buffers))
                   (buffers (if (stringp scope)
                                (seq-filter
                                 (lambda (buffer)
                                   (equal scope (wamei/claude-grid--project-name
                                                 (codex--buffer-directory-for buffer))))
                                 buffers)
                              buffers)))
              (if (null buffers)
                  (wamei/claude-grid--close-tab)
                (let ((windows (wamei/claude-grid--build buffers #'wamei/codex-grid--setup)))
                  (when-let* ((window (seq-find (lambda (window)
                                                 (eq (window-buffer window) current))
                                               windows)))
                    (select-window window)))))))))))

(defun wamei/codex-grid--schedule (&rest _)
  (unless (timerp wamei/codex-grid--timer)
    (setq wamei/codex-grid--timer (run-at-time 0 nil #'wamei/codex-grid--rearrange))))

(defun wamei/codex-grid--exited (buffer &rest _)
  (when (codex--buffer-p buffer) (wamei/codex-grid--schedule)))

(defun wamei/codex-grid-enable ()
  (advice-add 'wamei/codex-panel-show :after #'wamei/codex-grid--schedule)
  (add-hook 'ghostel-exit-functions #'wamei/codex-grid--exited))

(provide 'codex-grid)
;;; codex-grid.el ends here
