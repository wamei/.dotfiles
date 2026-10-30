;;; ai-session.el --- 開いていた AI の会話だけを復元する -*- lexical-binding: t; -*-
;;; Code:
(require 'json)
(require 'cl-lib)
(defconst wamei/ai-session--source-directory
  (file-name-directory (file-truename (or load-file-name buffer-file-name))))
(let ((load-path (cons wamei/ai-session--source-directory load-path)))
  (require 'ai-shortcuts))

(defvar wamei/ai-session-saved nil "desktop に保存する、起動中の会話の一覧。")
(defvar wamei/ai-session--restored (make-hash-table :test 'equal))
(defvar wamei/ai-session--names (make-hash-table :test 'equal))
(defvar wamei/ai-session--restoring nil)
(defvar-local wamei/ai-session--id nil)
(defvar ghostel--pid)
(defvar claude-code-ide-cli-extra-flags)
(defvar claude-code-ide-focus-on-open)

(defconst wamei/ai-session--uuid-regexp
  "[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}")

(defun wamei/ai-session--valid-id-p (id)
  "ID が CLI の会話 UUID であれば非 nil。MCP 接続 ID は使わない。"
  (and (stringp id)
       (string-match-p (concat "\\`" wamei/ai-session--uuid-regexp "\\'") id)))

(defun wamei/ai-session--codex-footer-id (buffer)
  "BUFFER のライブ画面末尾にある Codex の status line から ID を読む。
session-id を status line の先頭に設定している。本文中の UUID は採用しない。"
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (forward-line -8)
      (let (id)
        (while (re-search-forward
                (concat "^[ \t]*\\(" wamei/ai-session--uuid-regexp
                        "\\)\\(?:[ \t]*[·•|]\\|[ \t]*$\\)") nil t)
          (setq id (match-string-no-properties 1)))
        id))))

(defun wamei/ai-session--capture-codex-id (&rest _)
  "Ghostel が描画した現在の会話 ID を保存する。フックの環境変数には依存しない。"
  (when (and (fboundp 'codex--buffer-p) (codex--buffer-p (current-buffer)))
    (when-let* ((id (wamei/ai-session--codex-footer-id (current-buffer))))
      (setq-local codex--session-id id wamei/ai-session--id id))))

(defun wamei/ai-session--claude-id (buffer)
  "BUFFER の Ghostel 子 PID に対応する Claude の稼働中メタデータを読む。"
  (with-current-buffer buffer
    (let* ((pid (and (boundp 'ghostel--pid) ghostel--pid))
           (file (and (integerp pid)
                      (expand-file-name (format "sessions/%d.json" pid)
                                        (or (getenv "CLAUDE_CONFIG_DIR") "~/.claude"))))
           (metadata (and file (file-readable-p file)
                          (ignore-errors
                            (with-temp-buffer
                              (insert-file-contents file)
                              (json-parse-buffer :object-type 'alist :null-object nil
                                                 :false-object nil))))))
      (if (and metadata (integerp (alist-get 'pid metadata))
               (= (alist-get 'pid metadata) pid)
               (stringp (alist-get 'cwd metadata))
               (wamei/ai-panel--same-directory-p
                (alist-get 'cwd metadata) (wamei/claude-panel--project-dir buffer)))
          (let ((id (alist-get 'sessionId metadata)))
            (when (wamei/ai-session--valid-id-p id)
              (setq wamei/ai-session--id id)))
        wamei/ai-session--id))))

(defun wamei/ai-session-id (buffer backend)
  "BUFFER の実際の会話 ID。履歴の新しさからの推定はしない。"
  (let ((id (pcase backend
              ('claude (wamei/ai-session--claude-id buffer))
              ('codex
               (or (wamei/ai-session--codex-footer-id buffer)
                   (plist-get (codex-session-identity buffer) :session-id))))))
    (and (wamei/ai-session--valid-id-p id) id)))

(defun wamei/ai-session--entry (buffer)
  (when-let* ((backend (wamei/ai--backend buffer))
              (id (wamei/ai-session-id buffer backend)))
    (with-current-buffer buffer
      (list :backend backend :session-id id :buffer (buffer-name buffer)
            :directory (wamei/ai--directory buffer backend)
            :instance (pcase backend
                        ('codex (codex--buffer-instance-name-for buffer))
                        ('claude (claude-code-ide-mcp-session-instance-name
                                  (claude-code-ide--buffer-session buffer))))
            :title (and (boundp 'ghostel-title) ghostel-title)))))

(defun wamei/ai-session-save ()
  "起動中の会話だけを保存する。パネルを隠した会話も含む。"
  ;; CLI の起動待ち中に自動保存タイマーが動いても、未復元の一覧を失わない。
  (unless wamei/ai-session--restoring
    (setq wamei/ai-session-saved nil)
    (let (missing)
      (dolist (buffer (wamei/ai-panel--buffers))
	(when (process-live-p (get-buffer-process buffer))
          (if-let* ((entry (wamei/ai-session--entry buffer)))
              (push entry wamei/ai-session-saved)
            (push (buffer-name buffer) missing))))
      (setq wamei/ai-session-saved (nreverse wamei/ai-session-saved))
      (when missing
	(message "AI: 会話 ID が未取得のため復元対象に含めません: %s"
		 (mapconcat #'identity (nreverse missing) ", "))))))

(defun wamei/ai-session--suppress-display (orig buffer)
  "復元中は会話を起動するだけにし、保存時に隠れていたパネルを開かない。"
  (unless wamei/ai-session--restoring (funcall orig buffer)))

(defun wamei/ai-session--start (entry)
  "ENTRY の ID を明示して会話を再開する。"
  (let* ((backend (plist-get entry :backend))
         (id (plist-get entry :session-id))
         (directory (plist-get entry :directory))
         (instance (plist-get entry :instance))
         (default-directory directory))
    (unless (and (memq backend '(claude codex))
                 (wamei/ai-session--valid-id-p id)
                 (stringp directory) (not (file-remote-p directory))
                 (file-directory-p directory))
      (error "保存した会話 ID または作業ディレクトリが無効です"))
    (pcase backend
      ('codex
       (require 'codex)
       (let ((buffer (codex--start-session-buffer directory 'ghostel instance nil id nil nil)))
         (with-current-buffer buffer (setq-local codex--session-id id))
         buffer))
      ('claude
       (require 'claude-code-ide)
       (let ((before (mapcar #'claude-code-ide-mcp-session-buffer
                             (claude-code-ide-mcp--active-sessions)))
             (claude-code-ide-focus-on-open nil)
             (claude-code-ide-cli-extra-flags
              (string-join (delq nil (list claude-code-ide-cli-extra-flags
                                           (concat "--resume " (shell-quote-argument id)))) " ")))
         (cl-letf (((symbol-function 'claude-code-ide--get-working-directory) (lambda () directory))
                   ((symbol-function 'claude-code-ide--read-instance-name) (lambda (&rest _) instance)))
		  ;; resume の t は picker を開くため使わず、CLI 引数で ID を指定する。
		  (claude-code-ide--start-session nil nil))
         (or (seq-find (lambda (buffer) (and (buffer-live-p buffer) (not (memq buffer before))))
                       (mapcar #'claude-code-ide-mcp-session-buffer
                               (claude-code-ide-mcp--active-sessions)))
             (error "Claude の復元バッファを取得できません")))))))

(defun wamei/ai-session-restore-all ()
  "保存した会話だけを起動する。繰り返し呼んでも重複して起動しない。"
  (let ((wamei/ai-session--restoring t))
    (dolist (entry wamei/ai-session-saved)
      (let* ((backend (plist-get entry :backend))
             (id (plist-get entry :session-id))
             (key (list backend id (plist-get entry :directory)))
             (cached (gethash key wamei/ai-session--restored :unseen))
             (existing
              (or (and (buffer-live-p cached) (wamei/ai--backend cached)
                       (equal id (wamei/ai-session-id cached backend)) cached)
                  (seq-find (lambda (buffer)
                              (and (eq backend (wamei/ai--backend buffer))
                                   (equal id (wamei/ai-session-id buffer backend))
                                   (wamei/ai-panel--same-directory-p
                                    (plist-get entry :directory)
                                    (wamei/ai--directory buffer backend))))
                            (wamei/ai-panel--buffers)))))
        (when (or existing (eq cached :unseen))
          (condition-case err
              (let ((buffer (or existing (wamei/ai-session--start entry))))
		(with-current-buffer buffer
                  (setq-local wamei/ai-session--id id)
                  (when-let* ((title (plist-get entry :title))) (setq-local ghostel-title title)))
		(unless existing
                  (with-current-buffer buffer (rename-buffer (plist-get entry :buffer) t)))
		(wamei/ai-panel--setup buffer)
		(puthash key buffer wamei/ai-session--restored)
		(puthash (plist-get entry :buffer) buffer wamei/ai-session--names))
            (error
             (puthash key :failed wamei/ai-session--restored)
             (message "AI: %s の会話を復元できません: %s"
                      backend (error-message-string err))))))))
  (wamei/ai-panel--schedule-grid))

(defun wamei/ai-session-restore-panel (spec)
  "会話を復元後、保存時に表示していた会話だけをパネルへ出す。"
  (wamei/ai-session-restore-all)
  (when-let* ((buffer (gethash (plist-get spec :buffer) wamei/ai-session--names))
              ((buffer-live-p buffer))
              (window (wamei/ai-panel-show buffer)))
    (wamei/ai-panel--resize window (plist-get spec :size))
    window))

(defun wamei/ai-session-enable ()
  "会話単位の保存・復元を有効にする。
初回は再起動前にこのファイルを load-file し、このコマンドを実行する。"
  (interactive)
  (when (called-interactively-p 'interactive)
    (let ((load-path (cons wamei/ai-session--source-directory load-path)))
      (load "ai-panel" nil t))
    (wamei/ai-panel-enable))
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-globals-to-save 'wamei/ai-session-saved)
    (add-hook 'desktop-save-hook #'wamei/ai-session-save)
    (add-hook 'desktop-after-read-hook #'wamei/ai-session-restore-all))
  (advice-add 'claude-code-ide--display-buffer-in-side-window :around #'wamei/ai-session--suppress-display)
  (advice-add 'wamei/codex-panel-show :around #'wamei/ai-session--suppress-display)
  (advice-add 'ghostel--redraw-now :after #'wamei/ai-session--capture-codex-id)
  (when (called-interactively-p 'interactive)
    (wamei/ai-session-save)
    (when (and (boundp 'desktop-dirname) desktop-dirname)
      (desktop-save desktop-dirname))
    (message "AI: %d 個の会話 ID を保存しました" (length wamei/ai-session-saved))))

(provide 'ai-session)
;;; ai-session.el ends here
