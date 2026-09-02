;;; init.el --- wamei init file -*- lexical-binding: t; -*-
(keyboard-translate ?\C-h ?\C-?)
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf blackout :ensure t)

    :config
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf s :ensure t)

(provide 'init)

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "ビルトイン変数等の変更"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((user-full-name . "wamei")
            (user-mail-address . "wamei@gmail.com")
            (user-login-name . "wamei")
            (create-lockfiles . nil)
            ;; 左右の side window (treemacs / claude-code-ide) をフレーム全高にし、
            ;; 下部の side window (端末パネル) はその内側に収める。VSCode と同じ配置。
            (window-sides-vertical . t)
            (tab-width . 4)
            (debug-on-error . t)
            (init-file-debug . t)
            (find-file-visit-truename . nil)
            (make-backup-files . nil)
            (auto-save-default . t)
            (auto-save-file-name-transforms . '((".*" "/tmp/" t)))
            (auto-save-timeout . 10)
            (auto-save-interval . 100)
            (frame-resize-pixelwise . t)
            (enable-recursive-minibuffers . t)
            (history-length . 1000)
            (history-delete-duplicates . t)
            (scroll-preserve-screen-position . t)
            (scroll-conservatively . 100)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (ring-bell-function . 'ignore)
            (text-quoting-style . 'straight)
            (truncate-lines . t)
            (truncate-partial-width-windows . t)
            (use-dialog-box . nil)
            (use-file-dialog . nil)
            (menu-bar-mode . nil)
            (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil)
            (xterm-mouse-mode . t)
            (completion-ignore-case . t)
            (read-file-name-completion-ignore-case . t))
  :config
  ;; どちらも関数であって変数ではない。:custom に書くと customize-set-variable で
  ;; 同名の変数が作られるだけで実際には呼ばれない (current-language-environment が
  ;; "English" のままだった)。順序も重要で、set-language-environment は日本語系の
  ;; coding system を優先させるため、その後に utf-8 を最優先へ戻す。
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (defalias 'yes-or-no-p 'y-or-n-p))

(leaf server
  :doc "サーバー"
  :ensure nil
  :after with-editor
  :custom (with-editor-emacsclient-executable . nil)
  :config
  (unless (server-running-p)
    (server-start)))

(leaf mac-settings
  :doc "OS別の設定"
  :if (eq system-type 'darwin)
  :custom
  (option-modifier     . 'meta)
  (ns-option-modifier  . 'meta)
  (mac-option-modifier . 'meta)
  (command-modifier     . 'super)
  (ns-command-modifier  . 'super)
  (mac-command-modifier . 'super))

(leaf tty-clipboard
  :doc "ターミナル(-nw)でのクリップボード連携"
  ;; GUI の Emacs は NSPasteboard を直接扱うのでこの設定は不要。
  ;; TTY では kill/yank が Emacs 内で閉じてしまうため外部コマンドを噛ませる。
  :if (and (not (display-graphic-p))
           (eq system-type 'darwin)
           (executable-find "pbpaste"))
  :preface
  (defvar wamei/tty-clipboard-last nil
    "最後に Emacs 側から送った内容。pbpaste の結果と比較して二重取り込みを防ぐ。")

  (defun wamei/tty-clipboard-cut (text &optional _push)
    "TEXT をシステムのクリップボードへ送る。
~/bin/rpbcopy があればそれを使う (SSH 先では nc でローカルに転送される)。"
    (setq wamei/tty-clipboard-last text)
    (when-let* ((program (or (executable-find "rpbcopy")
                             (executable-find "pbcopy"))))
      (let ((coding-system-for-write 'utf-8))
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max) program)))))

  (defun wamei/tty-clipboard-paste ()
    "システムのクリップボードから取得する。自分が送った内容なら nil を返す。"
    (let* ((coding-system-for-read 'utf-8)
           (text (with-output-to-string
                   (with-current-buffer standard-output
                     (call-process "pbpaste" nil t nil)))))
      (unless (or (string-empty-p text)
                  (equal text wamei/tty-clipboard-last))
        text)))
  :config
  (setq interprogram-cut-function #'wamei/tty-clipboard-cut
        interprogram-paste-function #'wamei/tty-clipboard-paste))

(defvar font-size 180)
(defvar font-family "HackGen Console NF"
  "既定フォント。
Console 版は罫線・ブロック要素・幾何図形 (U+2500-25FF) を半角字形で持つ。
無印の HackGen はこれらが全角字形なので、端末 (vterm) の TUI が 1 セル
前提で描いた図形が 2 倍幅になって崩れる。")

(leaf font
  :doc "フォント"
  :config
  (global-font-lock-mode t)
  ;; set-language-environment "Japanese" は East Asian Ambiguous
  ;; (罫線・ブロック要素・幾何図形・ギリシャ文字など) を幅 2 として数える。
  ;; 一方 Console 版フォントも一般的な端末もこれらを半角で描くため、
  ;; 2 セル確保されると TUI の図形が間延びして崩れる
  ;; (claude / claude-code-ide の起動ロゴなど)。Ambiguous を半角に戻す。
  ;; 全角 (あ) や全角形 (Ａ) は幅 2 のままなので日本語表示は変わらない。
  ;; TTY でも端末側が半角で描くので GUI 限定にはしない。
  (use-default-char-width-table)
  ;; フォント指定は GUI フレームでのみ意味がある。特に set-fontset-font は
  ;; TTY (emacs -nw) で "Can't use fontsets in non-GUI frames" を投げる。
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family font-family :height font-size)
    (set-face-attribute 'variable-pitch nil :family font-family :height font-size)
    (set-face-attribute 'fixed-pitch nil :family font-family :height font-size)
    (set-face-attribute 'mode-line nil :family font-family :height font-size)
    (set-face-attribute 'mode-line-inactive nil :family font-family :height font-size)
    (set-face-attribute 'tooltip nil :family font-family :height font-size)
    (set-fontset-font nil 'japanese-jisx0208
                      (font-spec :family font-family :height font-size))
    ;; 記号ブロックの fallback を行高の合うフォントに固定する。
    ;; HackGen が持たない記号 (claude のスピナー ✢✳✶✻ や ⚙ ⌃ など) は既定だと
    ;; STIX Two Math / Arial Unicode MS に fallback し、ascent/descent が HackGen
    ;; (20px = 16+4) より大きいためその行だけ 23〜28px に伸びる。vterm の TUI は
    ;; 行高固定を前提にしているので、スピナーが回るたびに内容が押し下げられて
    ;; window から溢れ、Emacs が 1 行スクロールして画面全体が上下に揺れる。
    ;; Menlo は 17px にすると 20px = 16+4 で HackGen と一致し、これらの記号を
    ;; 広く持つ。fontset に HackGen → Menlo の順で登録し、HackGen が持つ字形は
    ;; そのまま使う (default fontset の指定は既定フォントより優先されるため、
    ;; HackGen を先頭に明示しないと HackGen の ● や ─ まで置き換わる)。
    (add-to-list 'face-font-rescale-alist '("Menlo" . 0.95))
    (dolist (range '((#x2190 . #x21FF)    ; Arrows
                     (#x2300 . #x23FF)    ; Misc Technical (⌃ ⏎ ...)
                     (#x2600 . #x26FF)    ; Misc Symbols (⚙ ⚠ ...)
                     (#x2700 . #x27BF)    ; Dingbats (✢ ✳ ✶ ✻ ...)
                     (#x2900 . #x2BFF)))  ; Supplemental Arrows / Misc Symbols and Arrows
      (set-fontset-font t range (font-spec :family font-family))
      (set-fontset-font t range (font-spec :family "Menlo") nil 'append))))

(leaf doom-themes
  :doc "テーマ"
  :ensure t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :custom-face
  ;; Emacs の gnus は gnus-group-news-low -> gnus-group-news-low-empty を継承し、
  ;; doom-themes は逆向きに news-low-empty -> news-low を継承させるため循環する。
  ;; gnus のフェイスが実体化する経路 (css-mode -> eww など) で
  ;; "Face inheritance results in inheritance cycle" が起き、major mode が
  ;; fundamental-mode に落ちる。user テーマ側で継承を張り替えて循環を断つ。
  (gnus-group-news-low-empty . '((t (:inherit gnus-group-mail-1 :weight normal))))
  :config
  (load-theme 'doom-molokai t)
  (set-frame-parameter nil 'alpha 80))

(leaf doom-modeline
  :doc "モードライン"
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (inhibit-compacting-font-caches . t)
  (doom-modeline-vcs-max-length . 30)

  ;;:global-minor-mode doom-modeline-mode
  :config
  (column-number-mode 1)
  (defun toggle-show-minor-mode ()
    (interactive)
    (setq doom-modeline-minor-modes (not doom-modeline-minor-modes))))

(leaf minions :ensure t)

(leaf hide-mode-line
  :doc "モードラインを隠す"
  :ensure t
  :leaf-defer nil
  :hook
  ((treemacs-mode-hook vterm-mode-hook) . hide-mode-line-mode)
  ((treemacs-mode-hook dired-mode-hook vterm-mode-hook wamei/term-list-mode-hook)
   . (lambda() (display-line-numbers-mode 0))))

(leaf vterm
  :doc "フレーム下部に固定する端末パネル"
  :ensure t
  :bind (("C-z" . wamei/term-toggle)
         ("C-S-z" . wamei/term-new)
         ;; グローバルに置くことで tab-bar-mode の再有効化に上書きされない
         ("<C-tab>" . wamei/term-next)
         ("<C-S-tab>" . wamei/term-previous)
         ;; 端末によっては Shift-Tab が iso-lefttab として報告される
         ("<C-S-iso-lefttab>" . wamei/term-previous))
  :custom
  ;; C-q / C-z / C-S-z は端末へ送らず Emacs 側で処理する
  ;; (C-c C-x M-x 等は既定で除外済み)。この変数は :set で vterm-mode-map を
  ;; 作り直す defcustom なので、add-to-list ではなく customize 経由で設定する。
  (vterm-keymap-exceptions
   . '("C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-y" "M-y"
       "C-q" "C-z" "C-S-z" "<C-tab>" "<C-S-tab>"))
  (vterm-max-scrollback . 10000)
  :preface
  (defvar wamei/term-height 0.3
    "端末ウィンドウの高さ (フレームに対する割合)。
手動でリサイズすると更新され、次に開くときも同じ割合になる。")

  (defvar wamei/term-list-width 36
    "端末一覧ウィンドウの幅 (文字数)。")

  (defvar wamei/term-list-buffer-name "*terminals*"
    "端末一覧のバッファ名。display-buffer-alist で端末本体と別扱いにするため、
`*term: ' で始まらない名前にする。")

  (defconst wamei/term-glyph-substitutions
    '((?⏺ . ?●)    ; claude の応答・ツール呼び出しの行頭
      (?⏵ . ?▶)    ; claude の "⏵⏵ auto mode on"
      (?⧉ . ?❐))   ; claude の "⧉ In file" (❐ は Menlo が持つ)
    "端末バッファで表示だけ置き換える文字の alist (元の文字 . 表示する文字)。
これらは手元のどのフォントでも行高が既定フォント (20px) に収まらず
(STIX Two Math は descent 9px)、含む行だけ伸びて TUI の画面が上下に揺れる。
バッファの内容は変えず display table で同形の記号を描く。")

  (defun wamei/term--substitute-tall-glyphs ()
    "`wamei/term-glyph-substitutions' を現在のバッファの display table に登録する。
vterm-mode は `buffer-display-table' を自前で用意するので、その表に追記する。
face を付けないので元の文字の色はそのまま引き継がれる。"
    (when (display-graphic-p)
      (let ((table (or buffer-display-table (make-display-table))))
        (pcase-dolist (`(,from . ,to) wamei/term-glyph-substitutions)
          (aset table from (vector (make-glyph-code to))))
        (setq buffer-display-table table))))

  (defvar-local wamei/term--title nil
    "端末が最後に報告したタイトル。
.zshrc の preexec が直前に実行したコマンドをタイトルとして流してくる。")

  (defvar wamei/term--previous-window nil
    "パネルへ移動する直前に選択していた window。")

  (defvar wamei/term--previous-buffer nil
    "パネルへ移動する直前に選択していたバッファ。
window オブジェクトは desktop の自動保存が side window を畳んで
開き直すたびに無効になるため、戻り先はバッファでも覚えておく。")

  (defvar wamei/term--last nil
    "最後に表示した端末バッファ。プロジェクトごとの復帰先として使う。")

  ;;; 高さの記憶

  (defun wamei/term--set-height (window)
    "WINDOW を wamei/term-height の割合にリサイズする。
display-buffer-alist の window-height に関数として渡す。数値を直接書くと
alist 登録時の値で固定されてしまい、リサイズを覚えられない。"
    (let ((delta (- (round (* wamei/term-height (frame-height)))
                    (window-total-height window))))
      (unless (zerop delta)
        (ignore-errors (window-resize window delta nil t)))))

  (defun wamei/term--set-list-width (window)
    "WINDOW を wamei/term-list-width の幅にする。
display-buffer-alist の window-width は bottom の side window では
数値を書いても効かず、変数シンボルは関数扱いで無視される。関数で明示的に
リサイズする必要がある。preserve-size は縮小前の幅で固定してしまうため使わない。"
    (let ((delta (- wamei/term-list-width (window-total-width window))))
      (unless (zerop delta)
        (ignore-errors (window-resize window delta t t)))))

  (defun wamei/term--remember-height ()
    "現在の高さの割合を wamei/term-height に覚える。"
    (when-let* ((window (get-buffer-window (current-buffer))))
      (when (window-parameter window 'window-side)
        (let ((ratio (/ (float (window-total-height window)) (frame-height))))
          (when (< 0.05 ratio 0.95)
            (setq wamei/term-height ratio))))))

  ;;; 端末バッファの管理

  (defun wamei/term--root ()
    "端末を開くディレクトリ。プロジェクト内ならそのルート。"
    (if-let* ((project (project-current nil)))
        (project-root project)
      default-directory))

  (defun wamei/term--project-name ()
    "タブ (プロジェクト) を識別する名前。"
    (file-name-nondirectory (directory-file-name (wamei/term--root))))

  (defun wamei/term--buffer-name (&optional index)
    "INDEX 番目の端末バッファ名。1 は番号なし。"
    (if (and index (> index 1))
        (format "*term: %s %d*" (wamei/term--project-name) index)
      (format "*term: %s*" (wamei/term--project-name))))

  (defun wamei/term--buffer-regexp ()
    "現在のプロジェクトの端末バッファ名にマッチする正規表現。
別プロジェクトの前方一致 (foo と foobar) を拾わないよう末尾まで固定する。"
    (concat "\\`" (regexp-quote (format "*term: %s" (wamei/term--project-name)))
            "\\(?: \\([0-9]+\\)\\)?\\*\\'"))

  (defun wamei/term--buffers ()
    "現在のプロジェクトの端末バッファを番号順に返す。"
    (let ((regexp (wamei/term--buffer-regexp)))
      (sort (seq-filter (lambda (buffer)
                          (string-match-p regexp (buffer-name buffer)))
                        (buffer-list))
            (lambda (a b)
              (< (wamei/term--index a) (wamei/term--index b))))))

  (defun wamei/term--index (buffer)
    "BUFFER の端末番号。番号なしは 1。"
    (if (string-match (wamei/term--buffer-regexp) (buffer-name buffer))
        (string-to-number (or (match-string 1 (buffer-name buffer)) "1"))
      0))

  (defun wamei/term--next-index ()
    "未使用の最小の端末番号。"
    (let ((used (mapcar #'wamei/term--index (wamei/term--buffers)))
          (index 1))
      (while (memq index used) (setq index (1+ index)))
      index))

  (defun wamei/term--window ()
    "端末本体を表示している window。"
    (seq-find (lambda (window)
                (and (eq (window-parameter window 'window-side) 'bottom)
                     (eql (window-parameter window 'window-slot) 0)
                     (string-match-p "\\`\\*term: " (buffer-name (window-buffer window)))))
              (window-list nil 'no-mini)))

  (defun wamei/term--current ()
    "パネルに出すべき端末バッファ。無ければ nil。"
    (let ((buffers (wamei/term--buffers)))
      (or (seq-find (lambda (b) (eq b wamei/term--last)) buffers)
          (car buffers))))

  (defun wamei/term--create (index)
    "INDEX 番目の端末を作る。vterm はバッファへ切り替えるので window 構成は戻す。"
    (let ((default-directory (wamei/term--root)))
      (save-window-excursion
        (vterm (wamei/term--buffer-name index)))))

  ;;; 一覧

  (defvar wamei/term-list-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "RET") #'wamei/term-list-select)
      (define-key map (kbd "d") #'wamei/term-list-kill)
      (define-key map [mouse-1] #'wamei/term-list-select)
      (define-key map (kbd "<C-tab>") #'wamei/term-next)
      (define-key map (kbd "<C-S-tab>") #'wamei/term-previous)
      map)
    "端末一覧のキーマップ。")

  (define-derived-mode wamei/term-list-mode special-mode "Terminals"
    "端末一覧のメジャーモード。")

  (defun wamei/term--list-buffer ()
    "端末一覧のバッファ。無ければ作る。"
    (or (get-buffer wamei/term-list-buffer-name)
        (with-current-buffer (get-buffer-create wamei/term-list-buffer-name)
          (wamei/term-list-mode)
          (setq-local mode-line-format nil)
          (current-buffer))))

  (defun wamei/term--record-title (title)
    "vterm が受け取った TITLE を覚えて一覧に反映する。
vterm--set-title は vterm-buffer-name-string が nil だと何もしないため、
:before advice で横取りする。"
    (setq wamei/term--title title)
    (when (get-buffer-window wamei/term-list-buffer-name)
      (wamei/term--list-refresh)))

  (defun wamei/term--label (buffer)
    "一覧に出す BUFFER の表示名。最後に実行したコマンド、無ければシェル名。"
    (or (buffer-local-value 'wamei/term--title buffer)
        (file-name-nondirectory vterm-shell)))

  (defun wamei/term--list-refresh ()
    "端末一覧を描き直す。"
    (with-current-buffer (wamei/term--list-buffer)
      (let ((inhibit-read-only t)
            (current (wamei/term--current))
            (width (max 8 (- wamei/term-list-width 2))))
        (erase-buffer)
        (dolist (buffer (wamei/term--buffers))
          (let* ((index (wamei/term--index buffer))
                 (label (format "%d: %s" index
                                (truncate-string-to-width
                                 (wamei/term--label buffer) width nil nil t)))
                 (start (point)))
            (insert label "\n")
            (add-text-properties
             start (1- (point))
             (list 'wamei/term-buffer buffer
                   'mouse-face 'highlight
                   'keymap wamei/term-list-mode-map
                   'help-echo "mouse-1: 切り替え / d: 削除"))
            (when (eq buffer current)
              (add-face-text-property start (1- (point)) 'highlight)))))
      (goto-char (point-min))))

  (defun wamei/term--list-update ()
    "端末が 2 つ以上のときだけ一覧を表示する。"
    (let ((buffers (wamei/term--buffers))
          (window (get-buffer-window wamei/term-list-buffer-name)))
      (cond
       ;; パネル自体が閉じている、または端末が 1 つ以下なら一覧は出さない
       ((or (null (wamei/term--window)) (< (length buffers) 2))
        (when (window-live-p window) (delete-window window)))
       (t
        (wamei/term--list-refresh)
        (unless (window-live-p window)
          (display-buffer (wamei/term--list-buffer)))))))

  (defun wamei/term-list-select ()
    "一覧で選んだ端末に切り替える。"
    (interactive)
    (when-let* ((buffer (get-text-property (point) 'wamei/term-buffer)))
      (wamei/term--show buffer)))

  (defun wamei/term-list-kill ()
    "一覧で選んだ端末を削除する。"
    (interactive)
    (when-let* ((buffer (get-text-property (point) 'wamei/term-buffer)))
      ;; vterm はプロセスが生きているため、そのままだと
      ;; process-kill-buffer-query-function が確認を求めて止まる
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer))
      (when (eq buffer wamei/term--last) (setq wamei/term--last nil))
      (if-let* ((next (wamei/term--current)))
          (wamei/term--show next)
        (wamei/term--close))
      (wamei/term--list-update)))

  ;;; パネル操作

  (defun wamei/term--show (buffer &optional no-select)
    "BUFFER をパネルに出す。NO-SELECT が非 nil ならフォーカスは移さない。

既に端末 window があるときは dedicated を一時的に外して差し替える。
dedicated のままだと set-window-buffer が失敗する。"
    (setq wamei/term--last buffer)
    (let ((window (wamei/term--window)))
      (if window
          (progn (set-window-dedicated-p window nil)
                 (set-window-buffer window buffer)
                 (set-window-dedicated-p window t))
        (setq window (display-buffer buffer)))
      (unless no-select
        (when (window-live-p window) (select-window window))))
    (wamei/term--list-update)
    (get-buffer-window buffer))

  (defun wamei/term--close ()
    "パネル (端末と一覧) を閉じる。"
    (when-let* ((window (get-buffer-window wamei/term-list-buffer-name)))
      (delete-window window))
    (when-let* ((window (wamei/term--window)))
      (delete-window window)))

  (defvar wamei/term-cycle-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<C-tab>") #'wamei/term-next)
      (define-key map (kbd "<C-S-tab>") #'wamei/term-previous)
      ;; 端末によっては Shift-Tab が iso-lefttab として報告される
      (define-key map (kbd "<C-S-iso-lefttab>") #'wamei/term-previous)
      map)
    "端末内で tab-bar-mode の C-tab 割り当てを上書きするキーマップ。")

  (defun wamei/term--cycle (offset)
    "現在の端末から OFFSET 個ずれた端末に切り替える。端は巻き戻る。"
    (let* ((buffers (wamei/term--buffers))
           (count (length buffers)))
      (when (> count 1)
        (let* ((current (wamei/term--current))
               (index (or (seq-position buffers current) 0)))
          ;; 切り替えだけを行い、フォーカスは呼び出し元に残す
          (wamei/term--show (nth (mod (+ index offset) count) buffers) t)))))

  (defun wamei/term-next ()
    "次の端末に切り替える。"
    (interactive)
    (wamei/term--cycle 1))

  (defun wamei/term-previous ()
    "前の端末に切り替える。"
    (interactive)
    (wamei/term--cycle -1))

  (defun wamei/term-new ()
    "新しい端末を作ってパネルに出す。"
    (interactive)
    (wamei/term--show (wamei/term--create (wamei/term--next-index))))

  (defun wamei/term--remember-previous ()
    "パネルへ移動する直前の window とバッファを覚える。"
    (setq wamei/term--previous-window (selected-window)
          wamei/term--previous-buffer (current-buffer)))

  (defun wamei/term--back-window ()
    "パネルから戻る先の window。

記録した window が生きていればそれを使う。desktop の自動保存で side window
が畳まれて開き直されると window オブジェクトは死ぬので、そのときは同じ
バッファを表示している window を探す (claude-code-ide や treemacs の
パネルから C-z で入った場合、これが無いと無関係な window に戻ってしまう)。
どちらも無ければ直近の window。パネル自身は no-other-window なので
NO-OTHER 指定で候補から外れる。"
    (or (and (window-live-p wamei/term--previous-window)
             (eq (window-buffer wamei/term--previous-window)
                 wamei/term--previous-buffer)
             wamei/term--previous-window)
        (and (buffer-live-p wamei/term--previous-buffer)
             (get-buffer-window wamei/term--previous-buffer))
        (and (window-live-p wamei/term--previous-window)
             wamei/term--previous-window)
        (get-mru-window nil t t t)))

  (defun wamei/term-toggle (&optional arg)
    "端末パネルへ出入りする。

- 非表示なら開いてフォーカスする
- 表示中でフォーカスが無ければフォーカスを移す
- フォーカス中なら元の window へ戻る (パネルは開いたまま)
- ARG (C-u) 付きならパネルを閉じる

treemacs 側の treemacs-select-when-already-in-treemacs = move-back と
同じ考え方に揃えている。"
    (interactive "P")
    (let ((window (wamei/term--window)))
      (cond
       (arg
        (wamei/term--close))
       ((and window (eq window (selected-window)))
        (let ((back (wamei/term--back-window)))
          (when (and back (not (eq back window)))
            (select-window back))))
       (window
        (wamei/term--remember-previous)
        (select-window window))
       (t
        (wamei/term--remember-previous)
        (wamei/term--show (or (wamei/term--current)
                              (wamei/term--create 1)))))))
  :init
  ;; 端末は下部 side window の slot 0、一覧は同じ side の slot 1 (右隣) へ。
  ;; :config だと vterm がロードされるまで登録されないので :init で行う。
  (add-to-list 'display-buffer-alist
               '("\\`\\*term: "
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . wamei/term--set-height)
                 (dedicated . t)
                 ;; no-other-window: C-x o (other-window) の巡回対象から外す。
                 ;; select-window は影響を受けないので C-z のトグルは通る。
                 ;; no-delete-other-windows: C-x 1 や magit の全画面化
                 ;; (delete-other-windows) で消えないようにする。treemacs と
                 ;; claude-code-ide は同じパラメータをパッケージ側で付けている。
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  ;; window-width は数値か関数のみ有効 (変数シンボルは関数扱いされ無視される)。
  ;; bottom の side window では数値も効かないため関数でリサイズする。
  (add-to-list 'display-buffer-alist
               '("\\`\\*terminals\\*\\'"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 1)
                 (window-width . wamei/term--set-list-width)
                 (dedicated . t)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  :config
  ;; 貼り付けは vterm-yank を使う。yank はバッファに直接挿入するだけで
  ;; 端末プロセスには届かない。コピー (s-c) は通常のリージョン操作で効く。
  (define-key vterm-mode-map (kbd "s-v") #'vterm-yank)
  (define-key vterm-mode-map (kbd "C-y") #'vterm-yank)
  (define-key vterm-mode-map (kbd "M-y") #'vterm-yank-pop)
  ;; 一覧に「最後に実行したコマンド」を出すため、端末が報告するタイトルを拾う
  (advice-add 'vterm--set-title :before #'wamei/term--record-title)

  (add-hook 'vterm-mode-hook #'wamei/term--substitute-tall-glyphs)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (add-hook 'window-configuration-change-hook
                        #'wamei/term--remember-height nil t)
              ;; シェル終了などでバッファが消えたら一覧を追従させる
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (run-at-time 0 nil #'wamei/term--list-update))
                        nil t))))

(leaf claude-code-ide
  :doc "Claude Code の IDE 連携"
  ;; MELPA には無く GitHub 配布のため :ensure t では入らない。導入は
  ;;   M-x package-vc-install RET https://github.com/manzaltu/claude-code-ide.el RET
  ;; 更新は
  ;;   M-x package-vc-upgrade RET claude-code-ide RET
  ;; 依存 (websocket / web-server / transient) は導入時に自動で入る。
  :ensure nil
  :preface
  (defun wamei/claude-code-ide--no-other-window (window)
    "claude のウィンドウを C-x o (other-window) の巡回対象から外す。
パッケージは表示時に display-buffer-alist を let で丸ごと束縛するので、
こちらの display-buffer-alist に window-parameters を書いても効かない。
表示関数の戻り値のウィンドウに直接パラメータを付ける。
なお no-other-window は移動先から外すだけで、select-window は素通りするため
claude-code-ide 側のフォーカス制御 (focus-on-open など) には影響しない。"
    (when (window-live-p window)
      (set-window-parameter window 'no-other-window t))
    window)

  (defvar wamei/claude--previous-window nil
    "claude パネルへ移動する直前に選択していた window。")

  (defvar wamei/claude--previous-buffer nil
    "claude パネルへ移動する直前に選択していたバッファ。
window オブジェクトは desktop の自動保存が side window を畳んで
開き直すたびに無効になるため、戻り先はバッファでも覚えておく。")

  (defun wamei/claude--remember-previous ()
    "claude パネルへ移動する直前の window とバッファを覚える。"
    (setq wamei/claude--previous-window (selected-window)
          wamei/claude--previous-buffer (current-buffer)))

  (defun wamei/claude--back-window ()
    "claude パネルから戻る先の window。判定は `wamei/term--back-window' と同じ。"
    (or (and (window-live-p wamei/claude--previous-window)
             (eq (window-buffer wamei/claude--previous-window)
                 wamei/claude--previous-buffer)
             wamei/claude--previous-window)
        (and (buffer-live-p wamei/claude--previous-buffer)
             (get-buffer-window wamei/claude--previous-buffer))
        (and (window-live-p wamei/claude--previous-window)
             wamei/claude--previous-window)
        (get-mru-window nil t t t)))

  (defun wamei/claude--window ()
    "claude を表示している window。選択中のものがあればそれを優先する。"
    (let ((windows (seq-filter
                    (lambda (window)
                      (claude-code-ide--buffer-session (window-buffer window)))
                    (window-list nil 'no-mini))))
      (or (car (memq (selected-window) windows))
          (car windows))))

  (defun wamei/claude-toggle (&optional arg)
    "claude-code-ide のパネルへ出入りする。

- セッションが無ければ起動する
- 非表示なら表示してフォーカスする
- 表示中でフォーカスが無ければフォーカスを移す
- フォーカス中なら元の window へ戻る (パネルは開いたまま)
- ARG (C-u) 付きならパネルを閉じる

端末パネル (C-z) や treemacs (C-x C-n) と同じ操作感に揃えている。"
    (interactive "P")
    ;; 自前のコマンドなのでパッケージの autoload は効かない
    (require 'claude-code-ide)
    (let ((window (wamei/claude--window)))
      (cond
       (arg
        (when window (claude-code-ide-toggle)))
       ((and window (eq window (selected-window)))
        (let ((back (wamei/claude--back-window)))
          (when (and back (not (eq back window)))
            (select-window back))))
       (window
        (wamei/claude--remember-previous)
        (select-window window))
       (t
        (wamei/claude--remember-previous)
        (if (claude-code-ide-mcp--sessions-for-project
             (claude-code-ide--get-working-directory))
            ;; 停止していないセッションがあるので開き直すだけ
            (claude-code-ide-toggle)
          (claude-code-ide))
        (when-let* ((window (wamei/claude--window)))
          (select-window window))))))

  :bind (("C-c c" . claude-code-ide-menu)
         ("C-x C-a" . wamei/claude-toggle))
  :custom
  ;; 端末バックエンドは導入済みの vterm を使う (既定値だが意図として明示)
  (claude-code-ide-terminal-backend . 'vterm)
  ;; treemacs が左、端末パネルが下なので右に出す
  (claude-code-ide-window-side . 'right)
  :config
  (advice-add 'claude-code-ide--display-buffer-in-side-window
              :filter-return #'wamei/claude-code-ide--no-other-window)
  ;; xref や flymake などの Emacs 側の機能を Claude から使えるようにする
  (claude-code-ide-emacs-tools-setup))

(leaf keybinds
  :doc "キーバインド"
  :bind (("C-q" . nil)
         ("M-b" . 'backward-to-word)
         ("M-f" . 'forward-to-word)
         ("M-h" . 'backward-kill-word)
         ("s-x" . 'kill-region)
         ("s-c" . 'kill-ring-save)
         ("s-v" . 'yank)))

(leaf project
  :doc "プロジェクト操作"
  :ensure nil
  :preface
  (defun wamei/project--tab-root (tab)
    "TAB に紐づけたプロジェクトルート。無ければ nil。"
    (alist-get 'wamei-project tab))

  (defun wamei/project--set-current-tab-root (root)
    "現在のタブに ROOT を紐づける。

tab-bar--tab と tab-bar--current-tab-make はどちらも既知のキー以外を
そのまま引き継ぐので、独自パラメータはタブ切り替えを跨いで残る。

タブは (current-tab (KEY . VALUE) ...) という構造で先頭がシンボルのため、
setf alist-get だと局所変数へ push されるだけで実体に残らない。
保存されているリストへ直接つなぐ必要がある。"
    (when-let* ((tab (tab-bar--current-tab-find)))
      (if-let* ((cell (assq 'wamei-project (cdr tab))))
          (setcdr cell root)
        (setcdr tab (cons (cons 'wamei-project root) (cdr tab))))))

  (defun wamei/project--find-tab-index (root)
    "ROOT に対応するタブの位置 (0 始まり) を返す。無ければ nil。"
    (let ((name (file-name-nondirectory (directory-file-name root))))
      (seq-position
       (funcall tab-bar-tabs-function)
       nil
       (lambda (tab _)
         (or
          ;; 明示的に紐づけたタブ
          (equal (wamei/project--tab-root tab) root)
          ;; 他の経路 (C-x t p など) で作られたタブは名前で拾う。
          ;; タブ名は wamei/tab-bar-tab-name-project がプロジェクト名にしている。
          (equal (alist-get 'name tab) name))))))

  (defun wamei/project-switch-project-in-tab (dir)
    "DIR のプロジェクト用タブへ移動する。無ければ新規タブを作って開く。"
    (interactive (list (project-prompt-project-dir)))
    (let* ((root (expand-file-name (file-name-as-directory dir)))
           (index (wamei/project--find-tab-index root)))
      (if index
          (tab-bar-select-tab (1+ index))
        (tab-new)
        (wamei/project--set-current-tab-root root)
        (project-switch-project root))))
  :bind (("C-x C-f" . project-find-file)
         ("C-x C-p" . project-switch-project)
         ([remap project-switch-project] . wamei/project-switch-project-in-tab)))

(leaf tab-bar
  :doc "プロジェクトごとのタブ"
  :ensure nil
  :bind (("C-q n" . tab-next)
         ("C-q p" . tab-previous)
         ("C-q c" . tab-new)
         ("C-q k" . tab-close)
         ("C-q r" . tab-rename))
  :preface
  (defun wamei/tab-bar-tab-name-project ()
    "プロジェクト名をタブ名にする。プロジェクト外ではバッファ名を使う。

既定の tab-bar-tab-name-current はカレントバッファ名を使うため、treemacs に
フォーカスした状態だと \" *Treemacs-Buffer-Tab ...\" のような内部バッファ名が
そのままタブ名になる。タブ = プロジェクトで運用しているのでプロジェクト名を優先する。

treemacs のバッファは default-directory が ~/ のままで project-current が
効かないため、side window (no-other-window 付き) を選択しているときは
直近の通常 window のバッファを見る。treemacs の内部 API には依存しない。

tab-bar-tabs 内でカレントタブ名の再計算に使われ、タブバーの再描画ごとに
呼ばれる。project-current は 2 回目以降 0.004ms 程度なのでキャッシュは置かない。"
    (let* ((window (if (window-parameter (selected-window) 'no-other-window)
                       (or (get-mru-window nil nil t t) (selected-window))
                     (selected-window)))
           (buffer (if (window-live-p window) (window-buffer window) (current-buffer))))
      (with-current-buffer buffer
        (let ((project (project-current nil)))
          (if project
              (file-name-nondirectory (directory-file-name (project-root project)))
            (buffer-name buffer))))))
  :custom
  (tab-bar-tab-name-function . #'wamei/tab-bar-tab-name-project)
  (tab-bar-tab-hints . t)            ; タブ番号を表示する
  (tab-bar-close-button-show . nil)
  (tab-bar-new-button-show . nil)
  (tab-bar-new-tab-choice . "*scratch*")
  ;; プロジェクトを新しいタブで開くのは組み込みの C-x t p
  ;; (project-other-tab-command)。タブの操作一式は C-x t 配下にある。
  :config
  ;; C-tab は端末の切り替えに使うため tab-bar からは外す。
  ;; tab-bar-mode は有効化のたびに tab-bar-mode-map へ [(control tab)] を
  ;; 定義し直すが、その処理は (unless (global-key-binding [(control tab)]) ...)
  ;; で守られている。グローバル側 (vterm ブロックの :bind) に割り当てておけば
  ;; 再有効化されても上書きされない。ここでは既存分を消すだけでよい。
  ;; タブの切り替えは C-q n / C-q p が残っている。
  (define-key tab-bar-mode-map [(control tab)] nil)
  (define-key tab-bar-mode-map [(control shift tab)] nil)
  (define-key tab-bar-mode-map [(control shift iso-lefttab)] nil)
  :global-minor-mode tab-bar-mode)

(leaf treemacs
  :doc "ファイルツリー"
  :ensure t
  :bind (("C-x C-n" . wamei/treemacs-toggle))
  :preface
  (defun wamei/treemacs-toggle (&optional arg)
    "treemacs を選択する。ARG (C-u) 付きなら閉じる。

フォーカスが既に treemacs にあるときの挙動は
treemacs-select-when-already-in-treemacs (move-back) が決める。
treemacs-quit は bury-buffer を使い side window では挙動が読みにくいので、
window を直接削除する。

なお treemacs-select-window は prefix 引数を workspace 切り替えに使うため、
ここで横取りするとその用途は使えなくなる。必要なら
M-x treemacs-select-window を直接呼ぶ。"
    (interactive "P")
    (if arg
        (when-let* ((window (treemacs-get-local-window)))
          (delete-window window))
      (treemacs-select-window)))

  (defun wamei/treemacs--flatten-dirs-guard (fn dirs)
    "プロジェクトを特定できないときはディレクトリ平坦化をスキップする。

treemacs--flatten-dirs は treemacs--find-project-for-path が nil を返しても
そのまま treemacs-find-file-node に渡すため、treemacs-project->position が
(wrong-type-argument arrayp nil) で落ちる。treemacs-follow-mode の
アイドルタイマーから非同期に呼ばれた際、その時点の workspace に該当
プロジェクトが見つからないと発生する。

平坦化を飛ばしてもツリーは描画される (畳まれず素直な階層で出る) だけなので、
落とすよりこちらを選ぶ。"
    (when (and dirs
               (treemacs--find-project-for-path (cadr (car dirs))))
      (funcall fn dirs)))
  :custom
  ;; C-x o (other-window) の巡回対象から外す
  (treemacs-is-never-other-window . t)
  ;; treemacs 内で再度呼んだら閉じずに元の window へ戻る (既定値だが意図として明示)
  (treemacs-select-when-already-in-treemacs . 'move-back)
  (treemacs-position . 'left)
  (treemacs-width . 35)
  :config
  ;; 外部でのファイル変更に追従する
  (treemacs-filewatch-mode 1)
  ;; 編集中のバッファをツリー上で追う
  (treemacs-follow-mode 1)
  ;; git の状態を色分けする。deferred は python3 を別プロセスで使う非同期版
  (treemacs-git-mode 'deferred)
  (advice-add 'treemacs--flatten-dirs :around #'wamei/treemacs--flatten-dirs-guard))

(leaf treemacs-nerd-icons
  :doc "treemacs のアイコンを nerd-icons に揃える"
  :ensure t
  :after treemacs
  :config
  (treemacs-nerd-icons-config))

(leaf transient
  :doc "magit 等のキー操作メニュー"
  ;; 既定の (display-buffer-in-side-window (side . bottom)) は slot 指定なし
  ;; (= 0) で、端末パネル (bottom / slot 0) と同じ位置になる。
  ;; display-buffer-in-side-window は同じ side・slot の window を dedicated でも
  ;; 再利用するため、メニューが端末パネルの window を奪い、終了時に端末が消える。
  ;; 選択中の window (magit status 等) の直下、主領域内に出すようにする。
  :custom ((transient-display-buffer-action . '(display-buffer-below-selected
                                                (dedicated . t)
                                                (inhibit-same-window . t)))))

(leaf magit
  :doc "git操作"
  :ensure t
  :bind ("C-x g" . magit-status)
  ;; status は side window (treemacs / claude-code-ide / 端末パネル) を残して
  ;; 主領域いっぱいに表示し、q で開く前の window 構成に戻す。
  ;; fullframe 化は delete-other-windows で行われるため、side window 側に
  ;; no-delete-other-windows パラメータが付いていることが前提
  ;; (treemacs と claude-code-ide はパッケージが付け、端末は vterm の
  ;; display-buffer-alist で付けている)。
  :custom ((magit-display-buffer-function . #'magit-display-buffer-fullframe-status-v1)
           (magit-bury-buffer-function . #'magit-restore-window-configuration)))

(leaf treemacs-magit
  :doc "magit の操作後に treemacs の git 表示を更新する"
  :ensure t
  :after treemacs magit)

(leaf treemacs-tab-bar
  :doc "treemacs をタブごとに分ける"
  :ensure t
  :after treemacs
  ;; パッケージのロード時に Tabs スコープが登録されるため require が必要。
  ;; :ensure だけだと未ロードで treemacs-set-scope-type が失敗する。
  :require t
  :config
  ;; treemacs-scope-types は既定で Frames のみ。このパッケージが Tabs を足す
  (treemacs-set-scope-type 'Tabs))

(leaf dired
  :doc "diredの設定"
  :leaf-defer nil
  :bind (("C-x C-j" . dired-toggle-current-or-project-directory)
         (:dired-mode-map
         ("C-c C-s" . dired-toggle-sudo)
         ("RET" . dired-find-file)
         ("a" . dired-find-alternate-file)
         ("^" . dired-up-directory)
         ("C-b" . backward-char)
         ("C-f" . forward-char)))
  :preface
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-isearch-filenames t)
  (setq dired-auto-revert-buffer t)
  (setq dired-listing-switches "--color=auto --group-directories-first -alLv")
  (setq insert-directory-program "/opt/homebrew/bin/gls")
  (put 'dired-find-alternate-file 'disabled nil)

  (defun dired-toggle-current-or-project-directory (n)
    "N が 1 ならカレントファイルの位置、4 (C-u) ならプロジェクトルートを dired で開く。"
    (interactive "p")
    (let ((project (project-current nil)))
      (cond ((= n 1)
             (dired-jump))
            ((= n 4)
             (if project
                 (project-dired)
               (dired-jump)))
            ))))
(leaf dired-toggle-sudo
  :ensure t
  :bind (:dired-mode-map
         ("C-c C-s" . dired-toggle-sudo)))
(leaf dired-rainbow
  :ensure t
  ;; dired-rainbow-define はマクロで autoload されないため、:require t で
  ;; 読み込んでから :config で展開する。
  :require t
  :config
  (dired-rainbow-define dotfiles "#aaaaaa" "\\..*")
  (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*"))
(leaf async
  :ensure t
  :config
  (eval-after-load "dired-aux" '(require 'dired-async)))

(leaf paren
  :doc "対応する括弧を強調して表示する"
  :custom (show-paren-delay . 0)
  :global-minor-mode show-paren-mode)

(leaf autorevert
  :doc "ホットローダーを有効にする"
  :global-minor-mode global-auto-revert-mode)

(leaf delsel
  :doc "選択状態で入力したときに選択範囲を消す"
  :global-minor-mode delete-selection-mode)

(leaf savehist
  :doc "ミニバッファの履歴を保存する"
  :custom (history-length . 10000)
  :global-minor-mode savehist-mode)

(leaf recentf
  :doc "ファイル履歴を保存する"
  :custom
  (recentf-max-menu-items . 100)
  (recentf-max-saved-items . 1000)
  :global-minor-mode recentf-mode)

(leaf desktop
  :doc "セッション (バッファ・ウィンドウ・タブ) を復元する"
  :ensure nil
  :preface
  (defun wamei/desktop--neutralize-side-window (window)
    "WINDOW を side window でなくし、通常のバッファを表示させる。

side window だけで構成されたフレームは window--sides-check を通らず、
復元時に window--sides-check-failed -> split-window が無限再帰して
max-lisp-eval-depth で落ちる。削除できない (唯一の window である) 場合は
属性を外して普通の window に戻すことで、保存される構成を正常にする。"
    (set-window-parameter window 'window-side nil)
    (set-window-parameter window 'window-slot nil)
    (set-window-parameter window 'no-other-window nil)
    (set-window-parameter window 'no-delete-other-windows nil)
    (set-window-dedicated-p window nil)
    (set-window-buffer window (get-scratch-buffer-create)))

  (defconst wamei/desktop--side-order '(left right top bottom)
    "side window を開き直す順序。
window-sides-vertical が t のとき、下部の side window の寸法は左右の
side window の有無で決まる。左右を先に作らないと幅が合わない。")

  (defun wamei/desktop--side-window-spec (window)
    "WINDOW を開き直すのに必要な情報を集める。
戻り値は (buffer side slot no-other-window no-delete-other-windows size)。
SIZE は左右なら幅、上下なら高さ。"
    (let ((side (window-parameter window 'window-side)))
      (list (window-buffer window)
            side
            (or (window-parameter window 'window-slot) 0)
            (window-parameter window 'no-other-window)
            (window-parameter window 'no-delete-other-windows)
            (if (memq side '(left right))
                (window-total-width window)
              (window-total-height window)))))

  (defun wamei/desktop--side-window-spec< (a b)
    "side 順、同じ side なら slot 順に A と B を比較する。
slot 順に戻さないと幅の割り当てが崩れる (端末が slot 0、一覧が slot 1)。"
    (let ((ia (or (seq-position wamei/desktop--side-order (nth 1 a)) 99))
          (ib (or (seq-position wamei/desktop--side-order (nth 1 b)) 99)))
      (if (= ia ib)
          (< (nth 2 a) (nth 2 b))
        (< ia ib))))

  (defun wamei/desktop--restore-side-window (spec)
    "SPEC (`wamei/desktop--side-window-spec' の戻り値) の side window を開き直す。"
    (pcase-let ((`(,buffer ,side ,slot ,no-other ,no-delete ,size) spec))
      (when (buffer-live-p buffer)
        (if (seq-some (lambda (entry) (buffer-match-p (car entry) buffer))
                      display-buffer-alist)
            ;; 端末のように display-buffer-alist に登録済みのものは、そちらの
            ;; 高さ・幅の関数を通さないと寸法が戻らない
            (display-buffer buffer)
          ;; claude-code-ide は表示時に display-buffer-alist を let で束縛するため、
          ;; 外から display-buffer しても side window に戻らず配置が崩れる。
          ;; 記録しておいた side / slot / 寸法で直接開き直す。寸法を渡さないと
          ;; 保存のたびに既定幅へ戻ってしまう。
          (display-buffer buffer
                          `(display-buffer-in-side-window
                            (side . ,side)
                            (slot . ,slot)
                            (dedicated . t)
                            ,(if (memq side '(left right))
                                 (cons 'window-width size)
                               (cons 'window-height size))
                            (window-parameters . ((no-other-window . ,no-other)
                                                  (no-delete-other-windows . ,no-delete)))))))))

  (defun wamei/desktop--side-windows ()
    "現在のタブにある side window のリスト。"
    (seq-filter (lambda (window) (window-parameter window 'window-side))
                (window-list nil 'no-mini)))

  (defun wamei/desktop-save-without-side-windows (fn &rest args)
    "desktop 保存の間だけ side window を畳み、保存後に開き直す。

treemacs や端末のバッファは desktop に復元されないが、window 構成は
frameset として保存される。そのまま保存すると復元時に「存在しない
バッファを指す side window」が残り、side window だけのタブでは
window--sides-check-failed から split-window が無限再帰して落ちる。

desktop-save-hook で閉じるだけだと、アイドル 10 秒ごとの自動保存でも
閉じてしまうため、:around で保存の前後だけ畳んで元に戻す。復元側で
掃除する方式は使えない。クラッシュは desktop 読み込み後ではなく、
タブ切り替え時の window-state-put で起きるため間に合わない。"
    (if (not (bound-and-true-p tab-bar-mode))
        (apply fn args)
      (let ((index (tab-bar--current-tab-index))
            (selected (selected-window))
            ;; side window は畳んで開き直すので window オブジェクトが死ぬ。
            ;; 端末や treemacs、claude にフォーカスがあるまま保存されると
            ;; 戻せずに無関係な window へ飛ぶため、バッファでも覚えておく。
            (selected-buffer (current-buffer))
            (restore nil))
        (unwind-protect
            (progn
              (dotimes (i (length (funcall tab-bar-tabs-function)))
                (tab-bar-select-tab (1+ i))
                (let ((treemacs (and (fboundp 'treemacs-get-local-window)
                                     (treemacs-get-local-window)))
                      (others nil))
                  (dolist (window (wamei/desktop--side-windows))
                    (unless (eq window treemacs)
                      (push (wamei/desktop--side-window-spec window) others)))
                  (when (or treemacs others)
                    (push (list i (and treemacs t) others) restore)
                    (dolist (window (wamei/desktop--side-windows))
                      (if (one-window-p t)
                          (wamei/desktop--neutralize-side-window window)
                        (delete-window window))))))
              (tab-bar-select-tab (1+ index))
              (apply fn args))
          (pcase-dolist (`(,i ,had-treemacs ,buffers) restore)
            (tab-bar-select-tab (1+ i))
            ;; treemacs は自前の表示関数を通す必要があるので専用に開き直す
            (when had-treemacs (ignore-errors (treemacs-select-window)))
            ;; それ以外 (端末・claude-code-ide など) は記録した side / slot に戻す
            (dolist (spec (sort (copy-sequence buffers)
                                #'wamei/desktop--side-window-spec<))
              (ignore-errors (wamei/desktop--restore-side-window spec))))
          (tab-bar-select-tab (1+ index))
          (cond ((window-live-p selected)
                 (select-window selected))
                ((and (buffer-live-p selected-buffer)
                      (get-buffer-window selected-buffer))
                 (select-window (get-buffer-window selected-buffer))))))))

  :custom
  ;; 終了時に確認せず保存する
  (desktop-save . t)
  ;; 異常終了でロックが残っていても読み込む
  (desktop-load-locked-desktop . t)
  ;; 起動時は 10 バッファだけ即時復元し、残りはアイドル時に読む
  (desktop-restore-eager . 10)
  ;; フレーム (サイズ・位置・タブ構成) も復元する。tab-bar のタブは frameset の
  ;; 一部として保存されるため、nil にするとタブが復元されない。
  ;; early-init.el のジオメトリ復元はフレーム生成前に効いてちらつきを防ぐ役割で、
  ;; 最終的な状態はこちらが決める。
  (desktop-restore-frames . t)
  ;; treemacs はツリーを自前で再構築するので復元すると壊れる。
  ;; magit はプロセス状態を持つバッファなので除外する。
  (desktop-modes-not-to-save . '(tags-table-mode
                                 treemacs-mode
                                 magit-status-mode
                                 magit-process-mode
                                 magit-diff-mode
                                 magit-revision-mode))
  :config
  (advice-add 'desktop-save :around #'wamei/desktop-save-without-side-windows)
  :global-minor-mode desktop-save-mode)

(leaf exec-path-from-shell
  :doc "シェルから環境変数を引き継ぐ"
  :ensure t
  :defun (exec-path-from-shell-initialize)
  :custom ((exec-path-from-shell-check-startup-files)
           (exec-path-from-shell-variables . '("PATH")))
  :config
  (exec-path-from-shell-initialize))

(leaf saveplace
  :doc "ファイルを閉じたとき、次に開くときはその場所(point)から開く"
  :custom `((save-place-file . ,(concat user-emacs-directory "places")))
  :global-minor-mode save-place-mode)

(leaf global-hl-line
  :doc "現在行をハイライト"
  :global-minor-mode global-hl-line-mode)

(leaf transient-mark
  :doc "選択範囲に色をつける"
  :global-minor-mode transient-mark-mode)

(leaf window-divider
  :doc "ウィンドウ間の細い区切り線"
  :ensure nil
  ;; ミニバッファとの境界にも引かれる。端末のモードラインを隠したことで
  ;; 下端がミニバッファと地続きに見えるのを防ぐ。
  ;; 色は doom-themes が window-divider :inherit vertical-border を
  ;; 定義しているのでテーマに追随する。
  :preface
  (defvar wamei/window-divider-base-face 'mode-line-inactive
    "divider の色をどの face の背景に合わせるか。

mode-line と mode-line-inactive は背景色が異なるが、divider の色は
フレーム単位でしか持てず window ごとのアクティブ状態を反映できない
(face remapping はバッファローカル、divider はフレームの face で描画される)。
アクティブな window は常に 1 つなので、divider の大半が接するのは
非アクティブなモードラインになる。ズレる箇所が少ない方を既定にする。")

  (defun wamei/window-divider-sync-color (&rest _)
    "divider の色を wamei/window-divider-base-face の背景色に合わせる。

divider は face の foreground で描かれるため、対象 face の background を
foreground として設定する。幅 1 のときに 3 つのうちどの face が使われるかは
実装依存なので全てに同じ色を入れる。enable-theme-functions に載せて
テーマ切替にも追随させる。"
    (let ((color (face-attribute wamei/window-divider-base-face :background nil 'default)))
      (when (and (stringp color) (not (string-prefix-p "unspecified" color)))
        (dolist (face '(window-divider
                        window-divider-first-pixel
                        window-divider-last-pixel))
          (set-face-foreground face color)
          (set-face-background face color)))))
  :custom
  (window-divider-default-places . 'bottom-only)
  (window-divider-default-bottom-width . 1)
  :config
  (wamei/window-divider-sync-color)
  (add-hook 'enable-theme-functions #'wamei/window-divider-sync-color)
  :global-minor-mode window-divider-mode)

(leaf global-display-line-numbers
  :doc "行番号を表示する"
  :global-minor-mode global-display-line-numbers-mode)

(leaf expand-region
  :doc "選択範囲を拡張する"
  :ensure t
  :bind (("C-q C-q" . er/expand-region)
         ("C-q C-z" . er/contract-region)))

(leaf mwim
  :doc "行頭行末移動をいい感じにする"
  :ensure t
  :bind (("C-a" . mwim-beginning-of-line-or-code)
         ("C-e" . mwim-end-of-line-or-code)))

(leaf undo-tree
  :doc "undoを強化する"
  :ensure t
  :blackout t
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo))
  :custom
  (undo-tree-history-directory-alist . '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode . t))

(leaf nerd-icons
  :doc "アイコンを表示する"
  :ensure t
  ;; nerd-icons-font-family は既定の "Symbols Nerd Font Mono" (NFM.ttf) のまま。
  ;; HackGen Console NF はアイコンの advance が 0.527em (半角1セル) しかなく、
  ;; 端末では正しいが GUI では潰れる。NFM は 1.000em で崩れない。
  ;; 大きさはフォントを変えずに scale-factor で詰める。
  ;; TTY は face の :family を無視するので、この設定は端末表示に影響しない。
  :custom (nerd-icons-scale-factor . 0.9)
  :require t)
(leaf nerd-icons-dired
  :ensure t
  :leaf-defer nil
  :hook
  (dired-mode-hook . nerd-icons-dired-mode))
(leaf nerd-icons-completion
  :ensure t
  :leaf-defer nil
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  :global-minor-mode nerd-icons-completion-mode)
(leaf nerd-icons-corfu
  :doc "補完候補にアイコンを表示する"
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(leaf whitespace
  :doc "不要なスペースを可視化する"
  :ensure t
  :custom
  (whitespace-style . '(face tabs spaces newline trailing space-before-tab space-after-tab space-mark tab-mark newline-mark))
  (whitespace-space-regexp . "\\(　+\\)")
  (whitespace-display-mappings . '((space-mark   ?\xA0  [?\xA4]  [?_])
                                   (space-mark   ?\x8A0 [?\x8A4] [?_])
                                   (space-mark   ?\x920 [?\x924] [?_])
                                   (space-mark   ?\xE20 [?\xE24] [?_])
                                   (space-mark   ?\xF20 [?\xF24] [?_])
                                   (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
                                   (space-mark   ?　    [?口]    [?＿])
                                   ;;(newline-mark ?\n    [?\x21B5 ?\n] [?$ ?\n])
                                   ))
  (whitespace-global-modes . '(not dired-mode tar-mode))
  :custom-face
  (whitespace-space . '((t (:foreground "pink4"))))
  (whitespace-tab . '((t (:foreground "gray40" :strike-through t))))
  ;;(whitespace-newline . '((t (:foreground "darkcyan" :height 0.8))))
  :hook
  (after-init-hook . global-whitespace-mode))

(leaf editorconfig
  :doc "editorconfigを適用する"
  :ensure t
  :hook
  (after-init-hook . editorconfig-mode))

(leaf git-gutter
  :doc "フリンジにgitの差分を表示する"
  :ensure t
  :custom
  (git-gutter:modified-sign . " ")
  (git-gutter:added-sign    . " ")
  (git-gutter:deleted-sign  . " ")
  :custom-face
  (git-gutter:modified . '((t (:background "#f1fa8c"))))
  (git-gutter:added    . '((t (:background "#50fa7b"))))
  (git-gutter:deleted  . '((t (:background "#ff79c6"))))
  :hook
  (after-init-hook . global-git-gutter-mode))

(leaf vertico
  :doc "minibuffer補完"
  :ensure t
  :global-minor-mode t
  :custom
  (vertico-count . 20)
  (vertico-cycle . t)
  (vertico-resize . t)
  (enable-recursive-minibuffers . t))

(leaf vertico-mouse
  :doc "補完マウス対応"
  :after vertico
  :global-minor-mode t)

(leaf vertico-posframe
  :doc "ミニバッファを child frame で表示する"
  :ensure t
  :after vertico
  :if (display-graphic-p)
  :custom
  ;; フレーム中央。モードラインやミニバッファの高さに依存しないので
  ;; 端末パネルの有無で位置がずれない。
  (vertico-posframe-poshandler . #'posframe-poshandler-frame-center)
  (vertico-posframe-border-width . 2)
  (vertico-posframe-parameters . '((alpha . 90)))
  :global-minor-mode vertico-posframe-mode)

(leaf marginalia
  :doc "補完に情報付与"
  :ensure t
  :global-minor-mode t)

(leaf corfu
  :doc "inline補完"
  :ensure t
  :custom
  (corfu-cycle . t)
  (corfu-auto . t)
  (corfu-auto-delay . 0)
  (corfu-auto-prefix . 1)
  (corfu-preview-current . nil)
  (corfu-quit-no-match . 'separator)
  (corfu-popupinfo-delay . '(0.5 . 0.2))
  (corfu-popupinfo-max-height . 20)
  :custom-face
  (corfu-current . '((t (:background "#5a5a5c" :extend t))))
  :bind
  ("M-/" . completion-at-point)
  :global-minor-mode global-corfu-mode corfu-popupinfo-mode
  :config
  (add-to-list 'corfu--frame-parameters '(alpha . 90)))

(leaf orderless
  :doc "補完ファジー検索"
  :ensure t
  :after corfu
  :custom ((completion-styles . '(orderless))
           (completion-category-defaults . nil)
           (completion-category-overrides . '((file (styles partial-completion))))))

(leaf locate-db
  :doc "ホーム配下を対象にした locate データベース"
  :ensure nil
  ;; システムの locate DB は locate.updatedb が nobody 権限で走るため、
  ;; $HOME (drwxr-x---) を辿れず配下を索引できない。sudo で有効化しても同じ。
  ;; mdfind (Spotlight) は隠しディレクトリを索引しない。
  ;; そこでユーザー権限で $HOME を走査した専用 DB を持つ。
  :preface
  (defvar wamei/locate-database
    (expand-file-name "locate.db" user-emacs-directory)
    "consult-locate が引くユーザー専用の locate データベース。")

  (defvar wamei/locate-prune-names
    '("node_modules" ".git" ".venv" "target" ".next" "dist")
    "走査から除外するディレクトリ名。")

  (defvar wamei/locate-prune-paths
    '("~/OrbStack" "~/Library/Caches" "~/.Trash")
    "走査から除外するパス。")

  (defun wamei/locate--find-command ()
    "DB を作り直すシェルコマンド文字列を返す。"
    (let ((prunes (string-join
                   (append
                    (mapcar (lambda (path)
                              (format "-path %s" (shell-quote-argument (expand-file-name path))))
                            wamei/locate-prune-paths)
                    (mapcar (lambda (name) (format "-name %s" (shell-quote-argument name)))
                            wamei/locate-prune-names))
                   " -o "))
          (tmp (concat wamei/locate-database ".tmp")))
      (format "find %s \\( %s \\) -prune -o -print 2>/dev/null | sort | %s > %s && mv %s %s"
              (shell-quote-argument (expand-file-name "~"))
              prunes
              (shell-quote-argument "/usr/libexec/locate.mklocatedb")
              (shell-quote-argument tmp)
              (shell-quote-argument tmp)
              (shell-quote-argument wamei/locate-database))))

  (defvar wamei/locate-max-age (* 15 60)
    "DB の許容鮮度 (秒)。これを超えたらアイドル時に作り直す。
Emacs の外で起きた変更 (git checkout や npm install 等) を拾うための保険。")

  (defvar wamei/locate-idle-delay 60
    "この秒数アイドルしたら更新の要否を判定する。")

  (defvar wamei/locate--dirty nil
    "Emacs 側でファイルの作成・削除があったか。")

  (defvar wamei/locate--process nil
    "更新中のプロセス。多重起動を防ぐために保持する。")

  (defun wamei/locate--mark-dirty (&rest _)
    "DB を要更新としてマークする。"
    (setq wamei/locate--dirty t))

  (defun wamei/locate--stale-p ()
    "DB が無い、または wamei/locate-max-age より古いとき non-nil。"
    (let ((attributes (file-attributes wamei/locate-database)))
      (or (null attributes)
          (> (float-time (time-subtract nil (file-attribute-modification-time attributes)))
             wamei/locate-max-age))))

  (defun wamei/locate-update-database (&optional quiet)
    "locate データベースを非同期で作り直す。おおよそ 15 秒かかる。
QUIET が non-nil なら成功時にメッセージを出さない (タイマーからの呼び出し用)。"
    (interactive)
    (unless (process-live-p wamei/locate--process)
      (unless quiet (message "locate DB を更新中..."))
      (setq wamei/locate--dirty nil)
      (setq wamei/locate--process
            (start-process-shell-command
             "wamei-locate-updatedb" " *wamei-locate-updatedb*"
             (wamei/locate--find-command)))
      (set-process-sentinel
       wamei/locate--process
       (lambda (process _event)
         (when (eq (process-status process) 'exit)
           (if (zerop (process-exit-status process))
               (unless quiet (message "locate DB 更新完了 (%s)" wamei/locate-database))
             ;; 失敗したらマークを戻して次のアイドルで再試行する
             (setq wamei/locate--dirty t)
             (message "locate DB の更新に失敗しました (%s)" wamei/locate-database)))))))

  (defun wamei/locate--maybe-update ()
    "必要なら DB を作り直す。アイドルタイマーから呼ばれる。"
    (when (and (not (process-live-p wamei/locate--process))
               (or wamei/locate--dirty (wamei/locate--stale-p)))
      (wamei/locate-update-database t)))
  :config
  ;; ホーム配下は 25 万ディレクトリあり、kqueue (macOS の file-notify backend) は
  ;; ディレクトリごとに fd を要求するうえ再帰監視もできないため、全体監視はしない。
  ;; Emacs 側の作成・削除は即座にマークし、外部の変更はアイドル時の鮮度判定で拾う。
  (add-hook 'before-save-hook
            (lambda ()
              (unless (and buffer-file-name (file-exists-p buffer-file-name))
                (wamei/locate--mark-dirty))))
  (dolist (fn '(delete-file rename-file make-directory delete-directory copy-file))
    (advice-add fn :after #'wamei/locate--mark-dirty))
  ;; init 中にも make-directory 等が呼ばれてフラグが立つため、起動完了時に落とす。
  ;; そうしないと毎回の起動直後に再構築が走る。鮮度判定の方は残るので、
  ;; DB が古ければ結局アイドル時に更新される。
  (add-hook 'emacs-startup-hook (lambda () (setq wamei/locate--dirty nil)))
  (run-with-idle-timer wamei/locate-idle-delay t #'wamei/locate--maybe-update))

(leaf consult
  :ensure t
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :defun consult-line
  :preface
  (defvar wamei/consult-line--overlays nil
    "consult-line 中に元バッファへ張ったハイライト用オーバーレイ。")

  (defun wamei/consult-line--remove-overlays ()
    "ハイライトを消す。"
    (mapc #'delete-overlay wamei/consult-line--overlays)
    (setq wamei/consult-line--overlays nil))

  (defvar wamei/consult-line-highlight-limit 2000
    "1 回のハイライトで張るオーバーレイの上限。")

  (defun wamei/consult-line--highlight (buffer input)
    "BUFFER 全体から INPUT のマッチを探してハイライトする。

入力の解釈は consult--regexp-compiler に任せるので、複数語入力でも
候補の絞り込みと同じ規則になる。"
    (wamei/consult-line--remove-overlays)
    (when (and (buffer-live-p buffer) (> (length input) 0))
      (when-let* ((regexps (car (ignore-errors
                                  (funcall consult--regexp-compiler input 'emacs t)))))
        (with-current-buffer buffer
          (save-excursion
            (save-restriction
              (widen)
              (let ((count 0))
                (dolist (re regexps)
                  (goto-char (point-min))
                  (let ((case-fold-search t))
                    (while (and (< count wamei/consult-line-highlight-limit)
                                (re-search-forward re nil t))
                      (if (= (match-beginning 0) (match-end 0))
                          (unless (eobp) (forward-char 1))
                        (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                          (overlay-put ov 'face 'consult-preview-match)
                          (overlay-put ov 'priority 1)
                          (push ov wamei/consult-line--overlays)
                          (setq count (1+ count)))))))))))))
    wamei/consult-line--overlays)

  (defun wamei/consult-line-highlight-all (fn &rest args)
    "consult-line の実行中、元バッファのマッチを全てハイライトする。

consult のプレビューはプレビュー中の 1 候補しか光らせないため、
isearch の lazy-highlight 相当を補う。"
    (let ((buffer (current-buffer)))
      (unwind-protect
          (minibuffer-with-setup-hook
              (lambda ()
                (add-hook 'post-command-hook
                          (lambda () (wamei/consult-line--highlight buffer (minibuffer-contents-no-properties)))
                          nil t))
            (apply fn args))
        (wamei/consult-line--remove-overlays))))

  (defun c/consult-line (&optional at-point)
    "Consult-line uses things-at-point if set C-u prefix."
    (interactive "P")
    (if at-point
        (consult-line (thing-at-point 'symbol))
      (consult-line)))
  :config
  (advice-add 'consult-line :around #'wamei/consult-line-highlight-all)

  :custom ((consult-async-min-input . 1)  ; 既定 3 だと「日本」のような 2 文字語で検索が走らない
           ;; 既定の "locate --ignore-case" は GNU locate 前提で、macOS の BSD locate は
           ;; 長オプションを受け付けない。-d でユーザー専用 DB を指定する
           ;; (作成は M-x wamei/locate-update-database)。
           ;; sh でラップしているのは、DB が無い/古いときに locate が警告を stderr に
           ;; 出し、consult がその先頭行をエラー表示するため。
           (consult-locate-args
            . '("sh" "-c"
                (format
                 (concat
                  "db=%s\n"
                  "first=$1\n"
                  "shift\n"
                  ;; 2 語目以降は awk で AND 絞り込みする。BSD locate に複数
                  ;; パターンを渡すと OR (和集合) になってしまうため。
                  ;; awk -v は値に改行があると "newline in string" で失敗するため
                  ;; 環境変数で渡して ENVIRON から読む。
                  "PATS=$(printf '%%s\\n' \"$@\")\n"
                  "export PATS\n"
                  "locate -d \"$db\" -i \"$first\" 2>/dev/null | awk '\n"
                  "BEGIN { n = split(ENVIRON[\"PATS\"], p, \"\\n\") }\n"
                  "{ l = tolower($0); ok = 1\n"
                  "  for (i = 1; i <= n; i++)\n"
                  "    if (p[i] != \"\" && index(l, tolower(p[i])) == 0) { ok = 0; break }\n"
                  "  if (ok) print }'\n")
                 (shell-quote-argument wamei/locate-database))
                "--"))
           (xref-show-xrefs-function . #'consult-xref)
           (xref-show-definitions-function . #'consult-xref)
           ;; nil にすると現在行から検索を始めて末尾で折り返す。初期選択が
           ;; point の次の出現地点になる (t だとバッファ先頭から)。
           (consult-line-start-from-top . nil))
  :bind (;; C-c bindings (mode-specific-map)
         ([remap switch-to-buffer] . consult-buffer) ; C-x b
         ([remap list-buffers] . consult-project-buffer) ; C-x C-b
         ([remap project-switch-to-buffer] . consult-project-buffer) ; C-x p b

         ;; M-g bindings (goto-map)
         ([remap goto-line] . consult-goto-line)    ; M-g g
         ([remap imenu] . consult-imenu)            ; M-g i
         ("M-g f" . consult-flymake)

         ;; C-x bindings
         ("C-x C-l" . consult-locate)
         ("C-x C-g" . consult-git-grep)

         ("C-M-s" . c/consult-line)

         (minibuffer-local-map
          :package emacs
          ("C-r" . consult-history))))

(leaf markdown-mode
  :doc "Markdown"
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :custom ((markdown-fontify-code-blocks-natively . t)
           (markdown-header-scaling . t)
           (markdown-display-remote-images . t)))

(leaf treesit
  :doc "tree-sitter"
  :ensure nil
  ;; treesit.el を明示的に読み込む。:ensure nil だけでは require されず、
  ;; :config 内の treesit-ready-p が void になる。
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :require t
  :custom (treesit-font-lock-level . 4)
  :config
  (setq treesit-language-source-alist
        '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))))

  (defun wamei/treesit-install-missing-grammars ()
    "未導入の tree-sitter grammar をまとめて導入する。"
    (interactive)
    (dolist (source treesit-language-source-alist)
      (if (treesit-ready-p (car source) t)
          (message "tree-sitter %s: 導入済み" (car source))
        (message "tree-sitter %s: 導入中..." (car source))
        (treesit-install-language-grammar (car source)))))

  ;; .ts / .tsx は typescript-ts-mode-maybe / tsx-ts-mode-maybe が振り分けるので
  ;; ここでは扱わない。grammar が入っている言語だけ ts 版に寄せる。
  (dolist (entry '((javascript-mode js-ts-mode   javascript)
                   (js-mode         js-ts-mode   javascript)
                   (js-json-mode    json-ts-mode json)
                   (css-mode        css-ts-mode  css)
                   (mhtml-mode      html-ts-mode html)
                   (html-mode       html-ts-mode html)))
    (pcase-let ((`(,from ,to ,lang) entry))
      (when (treesit-ready-p lang t)
        (add-to-list 'major-mode-remap-alist (cons from to))))))

(leaf eglot
  :doc "LSP クライアント (Emacs 組み込み)"
  :ensure nil
  :preface
  (defun wamei/eglot-capf-doc-with-detail (result)
    "eglot の capf に型情報 (LSP の :detail) を混ぜて返すフォールバックを足す。

TypeScript の変数は型が LSP の :detail に入り :documentation は空になる。
eglot は :detail を :company-docsig に、:documentation を :company-doc-buffer に
振り分けるが、corfu-popupinfo は後者しか読まないため型が表示されない。
:company-doc-buffer が両者を連結して返すように差し替える。"
    (when (consp result)
      (let* ((props (nthcdr 3 result))
             (doc-fn (plist-get props :company-doc-buffer))
             (sig-fn (plist-get props :company-docsig)))
        (when (and (functionp doc-fn) (functionp sig-fn))
          (plist-put
           props :company-doc-buffer
           (lambda (proxy)
             (let* ((sig (ignore-errors (funcall sig-fn proxy)))
                    (buf (ignore-errors (funcall doc-fn proxy)))
                    (doc (and (buffer-live-p buf)
                              (with-current-buffer buf (buffer-string))))
                    (sig (and (stringp sig) (not (string-blank-p sig)) sig))
                    (doc (and (stringp doc) (not (string-blank-p doc)) doc)))
               (when (or sig doc)
                 (with-current-buffer (get-buffer-create " *eglot doc+detail*")
                   (erase-buffer)
                   (when sig (insert sig))
                   (when (and sig doc) (insert "\n\n"))
                   (when doc (insert doc))
                   (current-buffer)))))))))
    result)
  :custom
  ;; 最後のバッファを閉じたら言語サーバを落とす
  (eglot-autoshutdown . t)
  ;; イベントログは肥大化して重いので無効化
  (eglot-events-buffer-config . '(:size 0 :format full))
  (eglot-code-action-indications . '(left-fringe))
  :hook ((typescript-ts-mode-hook
          tsx-ts-mode-hook
          js-ts-mode-hook
          js-mode-hook
          json-ts-mode-hook
          css-ts-mode-hook
          css-mode-hook
          html-ts-mode-hook
          mhtml-mode-hook) . eglot-ensure)
  :config
  (advice-add 'eglot-completion-at-point :filter-return
              #'wamei/eglot-capf-doc-with-detail))

(leaf eldoc-box
  :doc "eldoc をカーソル位置に child frame で表示する"
  :ensure t
  :if (display-graphic-p)
  :preface
  (defun wamei/eldoc-box-inhibit-during-completion (fn &rest args)
    "corfu の補完ポップアップ表示中は eldoc-box の自動表示を止める。

どちらも point 位置に child frame を出すため重なって読めなくなる。
eldoc-box--inhibit-childframe は 0.5 秒のアイドルタイマーで勝手に解除される
ため使わず、表示経路そのものを塞ぐ。C-c d (eldoc-box-help-at-point) は
この関数を通らないので手動表示は従来どおり効く。"
    (unless (bound-and-true-p completion-in-region-mode)
      (apply fn args)))

  (defun wamei/eldoc-box-quit-on-completion ()
    "補完が始まったら表示中の eldoc-box を閉じる。"
    (when (and (bound-and-true-p completion-in-region-mode)
               (fboundp 'eldoc-box-quit-frame))
      (eldoc-box-quit-frame)))
  ;; elisp などでは eldoc が常時発火して child frame がちらつくため、
  ;; まずは eglot 管理下のバッファに限定する。
  :hook (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode)
  ;; C-h は keyboard-translate で DEL に潰しているため C-h . は使えない
  :bind ("C-c d" . eldoc-box-help-at-point)
  :config
  (add-to-list 'eldoc-box-frame-parameters '(alpha . 90))
  (advice-add 'eldoc-box--eldoc-display-function :around
              #'wamei/eldoc-box-inhibit-during-completion)
  (add-hook 'completion-in-region-mode-hook #'wamei/eldoc-box-quit-on-completion))
