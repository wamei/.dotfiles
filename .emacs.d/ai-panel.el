;;; ai-panel.el --- Claude と Codex の共通セッションパネル -*- lexical-binding: t; -*-
;;; Commentary:
;; バックエンドの起動・入力・usage は従来の処理を使い、表示と巡回を共有する。
;;
;; 表示先は side window ではなく本体 (`window-main-window') の左端の普通の
;; window。AI とのやりとりが作業の中心なので、sidebar の右・コードの左に置く。
;; 本体の window が 1 つだけなら分割せず、その window を本体いっぱいに使う。
;; 右の side window は端末パネル (term-panel.el) が使う。
;; Claude も Codex も表示は `display-buffer' を通るので、
;; `display-buffer-overriding-action' でこの window へ向ける。
;; 普通の window と同じく C-x 1 や magit の全画面化で消える (C-x C-a で開き直す)。
;; desktop には残さず (desktop-side-windows.el が外す)、ai-session が開き直す。
;;; Code:
(require 'ai-shortcuts)
(require 'header-tabs)
(require 'claude-grid)
(eval-when-compile (require 'claude-code-ide-mcp nil t))

(defvar claude-code-ide-use-side-window)
(defvar claude-code-ide-focus-on-open)
(defvar wamei/ai-panel-width 0.5
  "共通 AI パネルの初期幅 (本体の幅に対する割合)。")
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

(defun wamei/ai-panel--window (&optional panel-only)
  "選択フレームの AI window。PANEL-ONLY なら本体左端のパネルのみ。"
  (seq-find (lambda (window)
              (if panel-only
                  (window-parameter window 'wamei/ai-panel)
                (wamei/ai--backend (window-buffer window))))
            (window-list nil 'no-mini)))

(defun wamei/ai-panel--split ()
  "本体の左端を `wamei/ai-panel-width' の幅で切り出し、パネルにした window を返す。"
  (let* ((main (window-main-window))
         (width (max window-min-width
                     (round (* (window-total-width main) wamei/ai-panel-width))))
         (window (split-window main (- width) 'left)))
    (set-window-parameter window 'wamei/ai-panel t)
    window))

(defun wamei/ai-panel--adopt (window)
  "本体ただ 1 つの WINDOW をパネルにする。閉じるときに戻すバッファを覚える。"
  (set-window-parameter window 'wamei/ai-panel-previous (window-buffer window))
  (set-window-parameter window 'wamei/ai-panel t)
  window)

(defun wamei/ai-panel--release (window)
  "本体いっぱいのパネル WINDOW を普通の window に戻し、パネルを開く前のバッファを出す。
本体ただ 1 つの window は消せないため、閉じる代わりにこうする。"
  (let ((previous (window-parameter window 'wamei/ai-panel-previous))
        (current (window-buffer window)))
    (set-window-dedicated-p window nil)
    (dolist (parameter '(wamei/ai-panel wamei/ai-panel-previous))
      (set-window-parameter window parameter nil))
    (set-window-buffer window (if (and (buffer-live-p previous) (not (eq previous current)))
                                  previous
                                (other-buffer current t)))))

(defun wamei/ai-panel--close (window)
  "AI の WINDOW を閉じる。本体ただ 1 つの window なら普通の window に戻す。"
  (cond
   ((and (window-parameter window 'wamei/ai-panel) (eq window (window-main-window)))
    (wamei/ai-panel--release window))
   ((window-parent window) (delete-window window))))

(defun wamei/ai-panel--display-action (buffer _alist)
  "BUFFER を本体左端のパネルに出す `display-buffer' のアクション。
パネルが無ければ作る。本体の window が 1 つならそれをパネルにする。
dedicated にはしない。パネルで C-x b すればその場で替わり、他の表示に
使われても構わない。パネルから出すバッファだけは
`wamei/ai-panel--display-beside' が別の window に出す。"
  (let ((window (or (wamei/ai-panel--window t)
                    (let ((main (window-main-window)))
                      (if (window-live-p main)
                          (wamei/ai-panel--adopt main)
                        (wamei/ai-panel--split))))))
    (set-window-dedicated-p window nil)
    (set-window-buffer window buffer)
    window))

(defun wamei/ai-panel--code-window (panel)
  "PANEL と同じフレームで、AI から開くバッファの出し先にする window。
PANEL 自身・side window・dedicated な window (sidebar など) を除き、直近に使ったもの。"
  (car (sort (seq-remove (lambda (window)
                           (or (eq window panel)
                               (window-dedicated-p window)
                               (window-parameter window 'window-side)))
                         (window-list (window-frame panel) 'no-mini))
             (lambda (a b) (> (window-use-time a) (window-use-time b))))))

(defun wamei/ai-panel--display-beside (buffer alist)
  "AI パネルから出すバッファを、本体の AI 以外の window に出す `display-buffer' のアクション。
`display-buffer-base-action' に入れる。選択中の window が AI パネルでなければ
何もしない (nil を返して後のアクションに任せる)。
リンクのクリック (find-file-other-window) などで AI 自身を置き換えず、
標準の pop-up-window で window を分割し続けもしない。
出し先が無ければ (AI が本体いっぱい) AI を左右に分ける。"
  (let ((panel (selected-window)))
    (when (window-parameter panel 'wamei/ai-panel)
      (or (display-buffer-reuse-window buffer alist)
          (if-let* ((window (wamei/ai-panel--code-window panel)))
              (window--display-buffer buffer window 'reuse alist)
            (window--display-buffer buffer (split-window panel nil 'right)
                                    'window alist))))))

(defconst wamei/ai-panel--display-override '((wamei/ai-panel--display-action))
  "表示を本体左端のパネルへ向ける `display-buffer-overriding-action'。")

(defun wamei/ai-panel--resize (window size)
  "パネル WINDOW の幅を SIZE (列数) に合わせる。"
  (when (and (window-live-p window) size)
    (let ((delta (- size (window-total-width window))))
      (unless (zerop delta)
        (ignore-errors (window-resize window delta t t))))))

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
  "Claude の native 表示を本体左端のパネルで実行する。
side window を使わない設定にして、パッケージの `display-buffer' を
パネルへ向ける。フォーカスや端末寸法の同期はパッケージ側に任せる。"
  (let* ((claude-code-ide-use-side-window nil)
         (display-buffer-overriding-action wamei/ai-panel--display-override)
         (shown (funcall orig buffer)))
    (wamei/ai-panel--setup buffer)
    (wamei/ai-panel--schedule-grid)
    shown))

(defun wamei/ai-panel--display-codex (orig buffer)
  "Codex の表示を本体左端のパネルへ向ける。"
  (let* ((display-buffer-overriding-action wamei/ai-panel--display-override)
         (shown (funcall orig buffer)))
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
     (arg (when window (wamei/ai-panel--close window)))
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
  "終了する BUFFER のパネルを別の AI に引き継ぐ。
引き継ぐ先が無ければ、切り出したパネルは閉じ、本体いっぱいのパネルは
普通の window に戻す (dedicated ではないので、放っておくと別のバッファを
出したまま残る)。"
  (when (buffer-live-p buffer)
    (let ((others (remq buffer (wamei/ai-panel--buffers (wamei/ai-panel--directory buffer)))))
      (dolist (window (get-buffer-window-list buffer nil 'visible))
        (when (window-parameter window 'wamei/ai-panel)
          (if others
              (with-selected-window window
                (wamei/ai-panel-show (car others)))
            (if (eq window (window-main-window))
                (wamei/ai-panel--release window)
              (delete-window window)))))))
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

;; desktop の保存データから外して開き直せるよう、パネルの印を window 状態に残す。
(add-to-list 'window-persistent-parameters '(wamei/ai-panel . writable))
(defvar wamei/desktop-side-panel-parameters)
(with-eval-after-load 'desktop-side-windows
  (add-to-list 'wamei/desktop-side-panel-parameters 'wamei/ai-panel))

(defun wamei/ai-panel-enable ()
  "両バックエンドの表示・タブ・復元を統合する。何度呼んでもよい。"
  (setq wamei/ai-panel-enabled t)
  ;; AI パネルから出すバッファは本体の別の window へ。指定の無い表示だけが対象。
  (unless (memq #'wamei/ai-panel--display-beside (ensure-list (car display-buffer-base-action)))
    (setq display-buffer-base-action
          (cons (cons #'wamei/ai-panel--display-beside
                      (ensure-list (car display-buffer-base-action)))
                (cdr display-buffer-base-action))))
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
