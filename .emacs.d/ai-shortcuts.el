;;; ai-shortcuts.el --- AI パネルに応じた共通ショートカット -*- lexical-binding: t; -*-
;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'project)
(defvar wamei/ai-panel-enabled nil)

(defun wamei/ai--backend (buffer)
  "起動中の AI BUFFER の種類。判定のためにパッケージを起動しない。"
  (cond
   ((and (fboundp 'codex--active-buffer-p) (codex--active-buffer-p buffer)) 'codex)
   ((and (fboundp 'claude-code-ide--buffer-session)
         (claude-code-ide--buffer-session buffer)
         (fboundp 'claude-code-ide-mcp--active-sessions)
         (memq buffer (mapcar #'claude-code-ide-mcp-session-buffer
                             (claude-code-ide-mcp--active-sessions))))
    'claude)))

(defun wamei/ai--root ()
  (or (and (fboundp 'wamei/project-tabs-current-root)
           (wamei/project-tabs-current-root))
      (when-let* ((project (project-current))) (project-root project))
      default-directory))

(defun wamei/ai--directory (buffer backend)
  (pcase backend
    ('codex (codex--buffer-directory-for buffer))
    ('claude (wamei/claude-panel--project-dir buffer))))

(defun wamei/ai--target ()
  "選択中、表示中、同プロジェクトの起動中の順で (BACKEND . BUFFER) を選ぶ。
複数候補は buffer-list の最近使った順。未起動なら Claude。"
  (let* ((buffers (buffer-list))
         (current (current-buffer))
         (root (wamei/ai--root))
         (visible (mapcar #'window-buffer (window-list nil 'no-mini)))
         (candidates
          (delq nil (mapcar (lambda (buffer)
                             (when-let* ((backend (wamei/ai--backend buffer)))
                               (cons backend buffer)))
                           buffers))))
    (or (seq-find (lambda (target) (eq (cdr target) current)) candidates)
        (seq-find (lambda (target) (memq (cdr target) visible)) candidates)
        (seq-find
         (lambda (target)
           (when-let* ((directory (wamei/ai--directory (cdr target) (car target))))
             (equal (file-name-as-directory (file-truename directory))
                    (file-name-as-directory (file-truename root)))))
         candidates)
        '(claude))))

(defconst wamei/ai--commands
  '((toggle (claude . wamei/claude-toggle) (codex . wamei/codex-toggle))
    (start (claude . wamei/ai--start-claude) (codex . wamei/ai--start-codex))
    (next (claude . wamei/claude-panel-next) (codex . wamei/codex-panel-next))
    (previous (claude . wamei/claude-panel-previous) (codex . wamei/codex-panel-previous))
    (grid (claude . wamei/claude-grid-tab) (codex . wamei/codex-grid-tab))))

(defun wamei/ai--start-claude ()
  "名前の入力を省き、空入力と同じ自動採番で Claude を新規作成する。"
  (interactive)
  (require 'claude-code-ide)
  (cl-letf (((symbol-function 'claude-code-ide--read-instance-name)
             #'claude-code-ide--auto-instance-name))
    (claude-code-ide)))

(defun wamei/ai--start-codex (&optional arg)
  "既存の会話を再利用せず、空いている既定名で Codex を新規作成する。"
  (interactive "P")
  (require 'codex)
  (let* ((directory (if (equal arg '(16))
                        (read-directory-name "Project directory: ")
                      (codex--directory)))
         (instance "default")
         (number 2))
    (while (get-buffer (codex--buffer-name-for-directory directory instance))
      (setq instance (format "default-%d" number)
            number (1+ number)))
    (codex-start-session :directory directory :instance-name instance)))

(defun wamei/ai--call (action arg)
  "ACTION を選択した AI で実行する。ARG はプレフィックス引数。"
  (let* ((target (wamei/ai--target))
         (shared (and wamei/ai-panel-enabled
                      (alist-get action '((toggle . wamei/ai-panel-toggle)
                                          (next . wamei/ai-panel-next)
                                          (previous . wamei/ai-panel-previous)
                                          (grid . wamei/ai-panel-grid-tab)))))
         (command (or shared (alist-get (car target) (alist-get action wamei/ai--commands))))
         (current-prefix-arg arg))
    ;; 編集バッファから巡回した場合も、表示中のセッションを起点にする。
    ;; 出入りと起動は編集バッファを保ち、フォーカスの戻り先を正しく記録する。
    (if (and (not shared) (memq action '(next previous)) (buffer-live-p (cdr target)))
        (with-current-buffer (cdr target) (call-interactively command))
      (call-interactively command))))

(defun wamei/ai-toggle (&optional arg)
  "起動中の AI パネルに出入りする。未起動なら Claude。"
  (interactive "P") (wamei/ai--call 'toggle arg))
(defun wamei/ai-start (&optional arg)
  "現在の AI の会話を名前入力なしで新規作成する。未起動なら Claude。"
  (interactive "P") (wamei/ai--call 'start arg))
(defun wamei/ai-next (&optional arg)
  "現在の AI の次のセッションを表示する。"
  (interactive "P") (wamei/ai--call 'next arg))
(defun wamei/ai-previous (&optional arg)
  "現在の AI の前のセッションを表示する。"
  (interactive "P") (wamei/ai--call 'previous arg))
(defun wamei/ai-grid-tab (&optional arg)
  "現在の AI のグリッドを表示する。ARG 付きなら全プロジェクト。"
  (interactive "P") (wamei/ai--call 'grid arg))

(provide 'ai-shortcuts)
;;; ai-shortcuts.el ends here
