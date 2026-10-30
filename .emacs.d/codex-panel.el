;;; codex-panel.el --- Codex の右パネル -*- lexical-binding: t; -*-
;;; Commentary:
;; Claude と同じ出入り・プロジェクト単位のタブを Codex に付ける。
;; パッケージ導入: M-x package-vc-install RET https://github.com/benthamite/codex RET
;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'header-tabs)

(defvar ghostel-title)
(declare-function wamei/term-input-paste "term-input")
(declare-function wamei/desktop-side-resize "desktop-side-windows")

(defun wamei/codex-panel--tab-name (buffer)
  "BUFFER の端末タイトルを返す。未設定ならインスタンス名を使う。"
  (or (when (boundp 'ghostel-title)
        (when-let* ((title (buffer-local-value 'ghostel-title buffer))
                    (name (string-trim title))
                    ((not (string-empty-p name))))
          name))
      (codex--buffer-instance-name-for buffer)
      "Codex"))

(defun wamei/codex-panel--title-updated (&rest _)
  "Codex の端末タイトル変更を表示中のセッションタブに反映する。"
  (when (codex--buffer-p (current-buffer))
    (force-mode-line-update t)))

(defvar wamei/codex-panel-width 0.4 "Codex パネルの初期幅 (フレーム幅の割合)。")
(defvar wamei/codex-panel-slot 100 "Codex パネルの side window slot。")
(defvar wamei/codex-panel--order nil "表示順の Codex バッファ。")
(defvar wamei/codex--previous-window nil)
(defvar wamei/codex--previous-buffer nil)

(defun wamei/codex--directory (orig)
  "セッション内はその起点、それ以外はプロジェクトタブの起点を使う。ORIG は元の関数。"
  (if (codex--buffer-p (current-buffer))
      (or (codex--buffer-directory-for (current-buffer)) (funcall orig))
    (or (and (fboundp 'wamei/project-tabs-current-root)
             (wamei/project-tabs-current-root))
        (funcall orig))))

(defun wamei/codex-panel--window ()
  "選択フレームの Codex window を返す。選択中のものを優先する。"
  (seq-find (lambda (window) (codex--buffer-p (window-buffer window)))
            (window-list nil 'no-mini)))

(defun wamei/codex-panel--buffers ()
  "同じプロジェクトのセッションを表示順に返す。"
  (setq wamei/codex-panel--order
        (seq-filter #'buffer-live-p wamei/codex-panel--order))
  (let ((buffers (codex--find-codex-buffers-for-directory (codex--directory))))
    (dolist (buffer buffers)
      (unless (memq buffer wamei/codex-panel--order)
        (setq wamei/codex-panel--order
              (append wamei/codex-panel--order (list buffer)))))
    (seq-filter (lambda (buffer) (memq buffer buffers)) wamei/codex-panel--order)))

(defvar wamei/codex-panel-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'wamei/codex-panel-tab-select)
    map))

(defun wamei/codex-panel--tabs-format ()
  "共通の header-tabs で Codex セッションを描く。"
  (wamei/header-tabs-format
   (mapcar (lambda (buffer)
             (list :label (wamei/codex-panel--tab-name buffer)
                   :current (eq buffer (current-buffer))
                   :properties (list 'wamei/codex-buffer buffer
                                     'local-map wamei/codex-panel-tab-map
                                     'help-echo "mouse-1: 切り替え")))
           (wamei/codex-panel--buffers))))

(defun wamei/codex-panel--setup (buffer)
  "BUFFER にセッションタブと操作キーを付ける。"
  (unless (memq buffer wamei/codex-panel--order)
    (setq wamei/codex-panel--order (append wamei/codex-panel--order (list buffer))))
  (with-current-buffer buffer
    (setq-local header-line-format '(:eval (wamei/codex-panel--tabs-format)))
    (when (bound-and-true-p tab-line-mode)
      (tab-line-mode -1))
    (wamei/codex-panel-keys-mode 1)))

(defun wamei/codex-panel-show (buffer)
  "BUFFER を右の専用 side window に表示し、window を返す。
`display-buffer' を通すので、`display-buffer-overriding-action' で
表示先を差し替えられる (ai-panel.el は本体左端のパネルへ向ける)。"
  (wamei/codex-panel--setup buffer)
  (display-buffer
   buffer `((display-buffer-in-side-window)
            (side . right) (slot . ,wamei/codex-panel-slot)
            (window-width . ,wamei/codex-panel-width)
            (window-parameters . ((no-other-window . t)
                                  (no-delete-other-windows . t))))))

(defun wamei/codex-panel-tab-select (event)
  "EVENT のタブを選ぶ。"
  (interactive "e")
  (when-let* ((buffer (wamei/header-tabs-event-property event 'wamei/codex-buffer))
              ((buffer-live-p buffer)))
    (select-window (wamei/codex-panel-show buffer))))

(defun wamei/codex-panel--cycle (offset)
  "同じプロジェクトのセッションを OFFSET 個巡回する。"
  (let* ((buffers (wamei/codex-panel--buffers))
         (count (length buffers)))
    (when (> count 1)
      (wamei/codex-panel-show
       (nth (mod (+ (or (seq-position buffers (current-buffer)) 0) offset) count)
            buffers)))))

(defun wamei/codex-panel-next ()
  "次の Codex セッションへ移る。"
  (interactive)
  (wamei/codex-panel--cycle 1))

(defun wamei/codex-panel-previous ()
  "前の Codex セッションへ移る。"
  (interactive)
  (wamei/codex-panel--cycle -1))

(define-minor-mode wamei/codex-panel-keys-mode
  "Codex バッファ内で端末パネルの巡回キーを上書きする。"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c x") #'codex-transient)
            (define-key map (kbd "s-v") #'wamei/term-input-paste)
            (define-key map (kbd "<C-tab>") #'wamei/codex-panel-next)
            (define-key map (kbd "<C-S-tab>") #'wamei/codex-panel-previous)
            (define-key map (kbd "<C-S-iso-lefttab>") #'wamei/codex-panel-previous)
            map))

(defun wamei/codex-toggle (&optional arg)
  "Codex に出入りする。ARG があれば閉じる。未起動なら直近の会話を復元する。"
  (interactive "P")
  (unless (require 'codex nil t)
    (user-error "M-x package-vc-install RET https://github.com/benthamite/codex RET で導入してください"))
  (let ((window (wamei/codex-panel--window)))
    (cond
     (arg (when window (delete-window window)))
     ((eq window (selected-window))
      (when-let* ((back (or (and (window-live-p wamei/codex--previous-window)
                                (eq (window-buffer wamei/codex--previous-window)
                                    wamei/codex--previous-buffer)
                                wamei/codex--previous-window)
                           (and (buffer-live-p wamei/codex--previous-buffer)
                                (get-buffer-window wamei/codex--previous-buffer))
                           (and (window-live-p wamei/codex--previous-window)
                                wamei/codex--previous-window)
                           (get-mru-window nil t t t))))
        (select-window back)))
     (t
      (setq wamei/codex--previous-window (selected-window)
            wamei/codex--previous-buffer (current-buffer))
      (let* ((buffers (codex--find-codex-buffers-for-directory (codex--directory)))
             (buffer (or (and window (memq (window-buffer window) buffers)
                              (window-buffer window))
                         (car buffers)
                         (codex--start-subcommand "resume" t))))
        (when (buffer-live-p buffer)
          (select-window (wamei/codex-panel-show buffer))))))))

(defun wamei/codex-panel-restore (spec)
  "保存した SPEC のプロジェクトで会話を続け、パネル幅を戻す。
Claude と同じくプロジェクトの直近の会話を復元する。"
  (require 'codex)
  (let* ((name (plist-get spec :buffer))
         (parsed (codex--parse-buffer-name name))
         (directory (or (plist-get spec :directory) (car parsed)))
         (instance (cadr parsed)))
    (when (and directory (not (file-remote-p directory))
               (file-directory-p directory))
      ;; 復元時には本文が別プロジェクトのバッファのままの場合がある。
      (let ((default-directory directory))
        (cl-letf (((symbol-function 'codex--directory) (lambda () directory)))
          (let* ((existing (get-buffer name))
                 (buffer (if (and existing (codex--buffer-process-live-p existing))
                             existing
                           (codex--start-subcommand "resume" t nil instance)))
                 (window (wamei/codex-panel-show buffer)))
            (wamei/desktop-side-resize window (plist-get spec :size))
            window))))))

(defun wamei/codex-panel-display (buffer)
  "起動・再表示した BUFFER のパネルへフォーカスを移す。"
  (let ((window (wamei/codex-panel-show buffer)))
    (select-window window)
    window))

(defun wamei/codex-panel-enable ()
  "Codex の表示と起点を設定する。繰り返し呼んでもよい。"
  (setq codex-display-window-fn #'wamei/codex-panel-display)
  (advice-add 'ghostel--set-title :after #'wamei/codex-panel--title-updated)
  (advice-add 'codex--directory :around #'wamei/codex--directory))

(provide 'codex-panel)
;;; codex-panel.el ends here
