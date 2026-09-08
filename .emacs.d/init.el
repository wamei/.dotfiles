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
            ;; 左右の side window (sidebar / claude-code-ide) をフレーム全高にし、
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

(leaf minibuffer-ime
  :doc "minibuffer に入ったら OS の IME を英字 (ABC) に切り替える"
  ;; NS ビルドには emacs-mac の `mac-select-input-source' がないので、macism
  ;; (brew install laishulu/homebrew/macism) を非同期に呼んで入力ソースを変える。
  ;; 未導入なら何もしない。minibuffer を抜けても元の IME には戻さない。
  :if (eq system-type 'darwin)
  :preface
  (defvar wamei/minibuffer-ime-ascii-source "com.apple.keylayout.ABC"
    "minibuffer で選ぶ入力ソース ID。シェルで `macism' を実行すると現在値が分かる。")
  (defun wamei/minibuffer-ime-off ()
    "OS の入力ソースを `wamei/minibuffer-ime-ascii-source' に切り替える。"
    (when (executable-find "macism")
      (start-process "macism" nil "macism" wamei/minibuffer-ime-ascii-source)))
  :hook (minibuffer-setup-hook . wamei/minibuffer-ime-off))

(leaf tty-display
  :doc "ターミナル(-nw)での表示記号"
  ;; 端末には fringe がないので、右端を超えた行は display table の truncation
  ;; 文字で示される。既定の `$' は本文と紛れるため `▸' を shadow face で薄く描く。
  ;; 端の 1 桁に収める必要があるので East Asian Width が N の文字を選ぶ (`…' `→' は
  ;; A で日本語端末では 2 桁になりうる)。
  ;; 水平スクロール時は左端にも描かれ、Emacs 31 は鏡像に置き換える。Unicode の
  ;; mirroring 属性で鏡像化する分岐 (`›'→`‹' など) は glyph code の face 番号を
  ;; 実体化済み face ID と取り違えて別の色になる (xdisp.c の IT_TRUNCATION 処理)。
  ;; `special-mirror-table' 経由の分岐は face を正しく解決するので、mirroring 属性を
  ;; 持たない `▸' を選び、鏡像 `◂' をこの表に登録する。
  :if (not (display-graphic-p))
  :config
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?▸ 'shadow))
  (when (boundp 'special-mirror-table)
    (aset special-mirror-table ?▸ ?◂)))

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
  ;; flymake の診断は doom-themes だと細い wave の下線だけで目立たない (wave の太さは
  ;; 変えられない)。doom-molokai が flycheck に付けているのと同じ流儀で背景を薄く
  ;; 色付けする。tty は terminfo (xterm-256color / tmux-256color) に Smulx / Setulc が
  ;; 無く、下線の色も wave も出せず白い直線になるので、下線は残しつつ背景色で種類を
  ;; 示す (256 色の colour52 / colour94 / colour22)。supports で判定しているので、
  ;; 色付き下線を出せる端末なら GUI と同じ見え方になる。
  (flymake-error
   . '((((supports :underline (:style wave)))
        (:underline (:style wave :color "#e74c3c") :background "#3a2523"))
       (t (:underline t :background "#5f0000"))))
  (flymake-warning
   . '((((supports :underline (:style wave)))
        (:underline (:style wave :color "#fd971f") :background "#3e301f"))
       (t (:underline t :background "#875f00"))))
  (flymake-note
   . '((((supports :underline (:style wave)))
        (:underline (:style wave :color "#b6e63e") :background "#2e3623"))
       (t (:underline t :background "#005f00"))))
  :config
  (load-theme 'doom-molokai t)
  (set-frame-parameter nil 'alpha 90))

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
  ((vterm-mode-hook) . hide-mode-line-mode)
  ((dired-mode-hook vterm-mode-hook wamei/term-list-mode-hook)
   . (lambda() (display-line-numbers-mode 0))))

(leaf keybinds
  :doc "キーバインド"
  :bind (("C-q" . nil)
         ("M-b" . 'backward-to-word)
         ("M-f" . 'forward-to-word)
         ("M-h" . 'backward-kill-word)
         ("s-x" . 'kill-region)
         ("s-c" . 'kill-ring-save)
         ("s-v" . 'yank)))

(leaf vterm
  :doc "フレーム下部に固定する端末パネル"
  :ensure t
  :bind (("C-z" . wamei/term-toggle)
         ("C-S-z" . wamei/term-new)
         ("C-q t c" . wamei/term-new)
         ;; グローバルに置くことで tab-bar-mode の再有効化に上書きされない
         ("<C-tab>" . wamei/term-next)
         ("C-q t n" . wamei/term-next)
         ("<C-S-tab>" . wamei/term-previous)
         ("C-q t p" . wamei/term-previous)
         ;; 端末によっては Shift-Tab が iso-lefttab として報告される
         ("<C-S-iso-lefttab>" . wamei/term-previous))
  :custom
  ;; C-q / C-z / C-S-z は端末へ送らず Emacs 側で処理する
  ;; (C-c C-x M-x 等は既定で除外済み)。この変数は :set で vterm-mode-map を
  ;; 作り直す defcustom なので、add-to-list ではなく customize 経由で設定する。
  ;; C-c と C-h は端末へ送りたいが、ここから外すと vterm のキーマップ構築が
  ;; C-c C-y 等の定義で "starts with non-prefix key" と落ちるので、残したまま
  ;; :config で上書きする。
  ;; M-w は除外しないと ESC マップの一括束縛 (vterm--self-insert-meta) に取られ、
  ;; ESC w が端末へ送られて kill-ring-save が呼ばれない。
  (vterm-keymap-exceptions
   . '("C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-y" "M-y" "M-w"
       "C-q" "C-z" "C-S-z" "<C-tab>" "<C-S-tab>"))
  (vterm-max-scrollback . 10000)
  ;; 既定 (80) だと、それより狭い window では libvterm と pty を 80 桁にしたまま
  ;; 表示だけ切り詰めるので、shell の折り返し位置と画面がずれる。パネルは端末が
  ;; 2 つ以上になると右に一覧 (wamei/term-list-width) が出て 80 桁を割りやすく、
  ;; zsh-autocomplete の候補リストが重なって描かれる。window の実幅に追従させる。
  (vterm-min-window-width . 20)
  :preface
  ;; kill-ring 連携とホイール転送。実体は term-input.el (init.el は symlink なので
  ;; 実体の隣から読む)。
  (load (expand-file-name "term-input"
                          (file-name-directory (file-truename user-init-file)))
        nil t)

  ;; 端末パネル (下部 side window) と端末一覧の管理は term-panel.el。
  ;; 端末・一覧ともプロジェクト (タブ) ごとにバッファを分ける。
  (load (expand-file-name "term-panel"
                          (file-name-directory (file-truename user-init-file)))
        nil t)

  ;; libvterm が実装していない faint (SGR 2) を色に置き換える。実体は term-faint.el。
  (load (expand-file-name "term-faint"
                          (file-name-directory (file-truename user-init-file)))
        nil t)

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
  :init
  ;; 端末は下部 side window の slot 0、一覧は同じ side の slot 1 (右隣) へ。
  ;; :config だと vterm がロードされるまで登録されないので :init で行う。
  (wamei/term-panel-setup)
  :config
  ;; vterm は既定色のセルにも `default' face の色を文字列で貼る (vterm--get-color が
  ;; index -1 で default の背景を返す)。背景が明示されると auto-dim-other-buffers の
  ;; window ごとの remapping が透けず、非選択の端末だけ暗くならない。既定背景の
  ;; セルでは nil を返して :background を付けさせない (vterm-module.c は nil なら
  ;; 属性を省く)。反転表示 (:inverse-video) は vterm-color-inverse-video の色が要るので除く。
  ;; 既に描かれた文字は次の再描画まで古い色のまま。
  (defun wamei/vterm-omit-default-background (fn index &rest args)
    "既定の背景色 (INDEX -1、前景でも反転でもない) なら nil、それ以外は FN に委ねる。"
    (if (and (eql index -1)
             (not (memq :foreground args))
             (not (memq :inverse-video args)))
        nil
      (apply fn index args)))
  (advice-add 'vterm--get-color :around #'wamei/vterm-omit-default-background)
  ;; C-c は単発で SIGINT を送る。vterm は C-c C-t (copy-mode) などを定義して
  ;; C-c を prefix にしているため、prefix ごと置き換える。C-c 配下 (C-c C-t /
  ;; C-c C-l / C-c C-n / C-c C-p / C-c C-r とグローバルの C-c 系) は端末バッファ
  ;; 内で使えなくなる。copy-mode は M-x vterm-copy-mode で。
  (define-key vterm-mode-map (kbd "C-c") #'vterm--self-insert)
  ;; C-h は Backspace。init.el 先頭の keyboard-translate で DEL になるが、
  ;; その変換は端末 (kboard) ごとなので、届かない経路があっても効くよう直接束縛する。
  (define-key vterm-mode-map (kbd "C-h") #'vterm-send-backspace)
  ;; C-k はそのまま端末へ送ると zsh の CUTBUFFER にしか残らないので、
  ;; 送る前に point から行末までを kill-ring に入れる (term-input.el)。
  (define-key vterm-mode-map (kbd "C-k") #'wamei/term-input-kill-line)
  ;; 貼り付けは vterm-yank を使う。yank はバッファに直接挿入するだけで
  ;; 端末プロセスには届かない。コピー (M-w / s-c) は通常のリージョン操作で効く。
  (define-key vterm-mode-map (kbd "s-v") #'vterm-yank)
  (define-key vterm-mode-map (kbd "C-y") #'vterm-yank)
  (define-key vterm-mode-map (kbd "M-y") #'vterm-yank-pop)
  ;; 一覧に「最後に実行したコマンド」を出すため、端末が報告するタイトルを拾う
  (advice-add 'vterm--set-title :before #'wamei/term--record-title)
  ;; shell 起動時の stty が作成時の window サイズで pty を上書きするので、最初の
  ;; 出力で一度だけ表示中の window に合わせ直す (term-panel.el)
  (advice-add 'vterm--filter :after #'wamei/term--sync-size-on-first-output)
  ;; libvterm は SGR 2 (faint) を実装しておらず、薄字の指定はセルへ届く前に落ちる。
  ;; Claude Code は入力欄の推奨プロンプトを faint だけで描く (色は付けない) ため、
  ;; そのままだと入力済みの文字と同じ色になる。libvterm へ渡る前に色へ置き換える
  ;; (term-faint.el)。
  (wamei/term-faint-enable)

  (add-hook 'vterm-mode-hook #'wamei/term--substitute-tall-glyphs)
  ;; 高さの記憶、kill 時の後始末、非アクティブ時のカーソル非表示 (term-panel.el)
  (add-hook 'vterm-mode-hook #'wamei/term--setup-buffer))

(leaf claude-code-ide
  :doc "Claude Code の IDE 連携"
  ;; MELPA には無く GitHub 配布のため :ensure t では入らない。導入は
  ;;   M-x package-vc-install RET https://github.com/manzaltu/claude-code-ide.el RET
  ;; 更新は
  ;;   M-x package-vc-upgrade RET claude-code-ide RET
  ;; 依存 (websocket / web-server / transient) は導入時に自動で入る。
  :ensure nil
  :preface
  ;; 複数セッションを 1 つの右パネルに差し替え、上部の tab-line で切り替える。
  ;; 実体は claude-panel.el (init.el は symlink なので実体の隣から読む)。
  (load (expand-file-name "claude-panel"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  ;; パネルの mode-line に使用量 (セッション / 週 / Fable) を出す。
  (load (expand-file-name "claude-usage"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  ;; 動いているセッションを 1 タブに並べて見る (claude-grid.el)。
  ;; claude-panel を require するので読み込みはこの後。
  (load (expand-file-name "claude-grid"
                          (file-name-directory (file-truename user-init-file)))
        nil t)

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
パネルを閉じて開き直すなどで window オブジェクトは無効になりうるため、
戻り先はバッファでも覚えておく。")

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

  (defalias 'wamei/claude--window #'wamei/claude-panel--window
    "claude を表示している window。実体は claude-panel.el。")

  (defun wamei/claude-toggle (&optional arg)
    "claude-code-ide のパネルへ出入りする。

- セッションが無ければ起動する
- 非表示なら表示してフォーカスする
- 表示中でフォーカスが無ければフォーカスを移す
- フォーカス中なら元の window へ戻る (パネルは開いたまま)
- ARG (C-u) 付きならパネルを閉じる

端末パネル (C-z) や sidebar (C-x C-n) と同じ操作感に揃えている。"
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
         ("C-x C-a" . wamei/claude-toggle)
         ("C-q a c" . claude-code-ide)
         ("C-q a n" . wamei/claude-panel-next)
         ("C-q a p" . wamei/claude-panel-previous)
         ("C-q a g" . wamei/claude-grid-tab))
  :custom
  ;; 端末バックエンドは導入済みの vterm を使う (既定値だが意図として明示)
  (claude-code-ide-terminal-backend . 'vterm)
  ;; sidebar が左、端末パネルが下なので右に出す
  (claude-code-ide-window-side . 'right)
  ;; 高さだけのリサイズを Claude に通知しない workaround (upstream #1422 対策) を切る。
  ;; この filter は vterm--set-size で libvterm 側だけ新しい行数にした後 nil を返し、
  ;; pty (Claude) への set-process-window-size を握りつぶす。echo area が複数行に
  ;; 伸びるたびに libvterm と Claude の行数が食い違い、Claude 2.1 系のセル差分描画は
  ;; 自分の画面モデルとの差分しか書かないため、/clear 後などに古い文字がまばらに残る。
  ;; 現行の Claude は alt-screen 上でリサイズ時に ESC[2J 全再描画するので、
  ;; 通知させた方が整合する (再描画の一瞬のちらつきは許容)。
  (claude-code-ide-prevent-reflow-glitch . nil)
  :config
  (advice-add 'claude-code-ide--display-buffer-in-side-window
              :filter-return #'wamei/claude-code-ide--no-other-window)
  ;; Claude は tui fullscreen (alt-screen) で動くため libvterm の scrollback には
  ;; 何も残らず、履歴は Claude 内蔵のスクロールで見るしかない。vterm はホイールを
  ;; pty へ渡さないので、Claude のバッファでだけマウス報告として転送する (term-input.el)。
  (advice-add 'claude-code-ide--configure-vterm-buffer
              :after #'wamei/term-input-mouse-mode)
  ;; Cmd+V は vterm-yank (kill-ring) なのでテキストしか送れない。Claude は C-v を
  ;; 受けると自分でクリップボードの画像を読むので、画像のときだけキーを流す
  ;; (term-input.el)。シェルでは C-v が quoted-insert なので Claude だけに付ける。
  (advice-add 'claude-code-ide--configure-vterm-buffer
              :after #'wamei/term-input-paste-mode)
  ;; セッションを 1 パネル + tab-line にまとめる (claude-panel.el)
  (wamei/claude-panel-enable)
  ;; vterm は hide-mode-line で mode-line を消しているが、Claude のバッファだけは
  ;; 戻して使用量のバーを出す (claude-usage.el)
  (wamei/claude-usage-enable)
  ;; xref や flymake などの Emacs 側の機能を Claude から使えるようにする
  (claude-code-ide-emacs-tools-setup))

(leaf project
  :doc "プロジェクト操作"
  :ensure nil
  :preface
  (defun wamei/project--find-tab-index (root)
    "ROOT に対応するタブの位置 (0 始まり) を返す。無ければ nil。"
    (let ((name (file-name-nondirectory (directory-file-name root))))
      (seq-position
       (funcall tab-bar-tabs-function)
       nil
       (lambda (tab _)
         (or
          ;; プロジェクトを紐づけたタブ (project-tabs.el)
          (equal (wamei/project-tabs-root tab) root)
          ;; 紐づけ前 (タブ名固定より前) のタブは名前で拾う。
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
        (wamei/project-tabs-set-root root)
        (project-switch-project root))))
  :bind (("C-x C-f" . project-find-file)
         ("C-x C-p" . project-switch-project)
         ([remap project-switch-project] . wamei/project-switch-project-in-tab))
  :config
  ;; 未訪問・ignore 済みファイルを project-find-file と consult-project-buffer の
  ;; 候補に足す。init.el は ~/.emacs.d/init.el への symlink なので実体の隣から読む。
  (load (expand-file-name "project-extra-files"
                          (file-name-directory (file-truename user-init-file)))
        nil t))

(leaf tab-bar
  :doc "プロジェクトごとのタブ"
  :ensure nil
  :bind (("C-q n" . tab-next)
         ("C-q p" . tab-previous)
         ("C-q c" . tab-new)
         ("C-q k" . tab-close)
         ("C-q r" . tab-rename))
  :preface
  ;; タブ名の決定と固定は project-tabs.el に分けている。
  ;; init.el は ~/.emacs.d/init.el への symlink なので実体の隣から読む。
  (load (expand-file-name "project-tabs"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
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
  ;; タブにプロジェクトのバッファが初めて出た時点で名前を固定する。
  ;; 固定しないとカレントバッファのプロジェクトが変わるたびにタブ名が動く
  ;; (詳細は project-tabs.el の Commentary)。
  (add-hook 'window-buffer-change-functions #'wamei/project-tabs--pin-name-soon)
  ;; project 系の対話コマンドは、開いているバッファがプロジェクト外 (*scratch*
  ;; など) でも別プロジェクトでも、タブに紐づいたプロジェクトを起点にする
  ;; (詳細は project-tabs.el の Commentary)。
  (wamei/project-tabs-setup)
  :global-minor-mode tab-bar-mode)

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
  ;; status は side window (sidebar / claude-code-ide / 端末パネル) を残して
  ;; 主領域いっぱいに表示し、q で開く前の window 構成に戻す。
  ;; fullframe 化は delete-other-windows で行われるため、side window 側に
  ;; no-delete-other-windows パラメータが付いていることが前提
  ;; (claude-code-ide はパッケージが付け、sidebar と端末は display-buffer-alist で
  ;; 付けている)。
  :custom ((magit-display-buffer-function . #'magit-display-buffer-fullframe-status-v1)
           (magit-bury-buffer-function . #'magit-restore-window-configuration)))

(leaf docker
  :doc "コンテナ / イメージ / compose の操作 (transient)"
  :ensure t
  ;; eldoc-box を C-c h へ移して空けた C-c d に割り当てる。
  :bind ("C-c d" . docker)
  :preface
  (defvar wamei/docker-current-projects nil
    "現在の project に属する compose プロジェクト名のリスト。
コンテナ一覧を開くたびに `wamei/docker--record-current-projects' が更新する。")

  (defun wamei/docker--projects-under (root)
    "ROOT 配下を working_dir に持つ compose プロジェクト名を返す。

compose のプロジェクト名はディレクトリ名からは導けない。compose.yaml の name:
や COMPOSE_PROJECT_NAME で決まり、compose ファイルがサブディレクトリにあることも
ある (例: /path/to/BeecoV2/docker で立てたプロジェクト名が beeco-v2)。
コンテナに付く com.docker.compose.project.working_dir ラベルが ROOT 配下かどうかで
判定する。

docker.el 本体の docker-run-async と違い `process-lines' はシェルを通さないので、
Go テンプレート内の \" をエスケープしなくてよい。docker-command を
bound-and-true-p で読むのは、M-x docker-containers を直接呼ぶと下の advice が
autoload より先に走り、docker-core がまだロードされていないことがあるため。"
    (when root
      (let ((prefix (expand-file-name (file-name-as-directory root))))
        (delete-dups
         (delq nil
               (mapcar
                (lambda (line)
                  (let* ((parts (split-string line "\t"))
                         (name (car parts))
                         (dir (cadr parts)))
                    (and name dir
                         (not (string-empty-p name))
                         (not (string-empty-p dir))
                         (string-prefix-p prefix (file-name-as-directory dir))
                         name)))
                (ignore-errors
                  (process-lines (or (bound-and-true-p docker-command) "docker")
                                 "ps" "--format"
                                 (concat "{{ .Label \"com.docker.compose.project\" }}\t"
                                         "{{ .Label \"com.docker.compose.project.working_dir\" }}")))))))))

  (defun wamei/docker--record-current-projects (&rest _)
    "現在のバッファの project から `wamei/docker-current-projects' を更新する。
一覧のバッファ (*docker-containers*) は一度作られると作り直されず
default-directory が古いままなので、一覧を開く側で拾っておく。"
    (setq wamei/docker-current-projects
          (when-let* ((project (project-current nil default-directory)))
            (wamei/docker--projects-under (project-root project)))))

  (defun wamei/docker--project-first-< (a b)
    "compose プロジェクト名 A を B より前に出すなら非 nil。
現在の project のものを先頭に、compose 以外 (空文字) を末尾に置く。
`docker-container-columns' の :sort は行ではなく列の値だけを受け取る
(docker-utils-columns-list-format が -on で包む)。"
    (let ((a-current (and (member a wamei/docker-current-projects) t))
          (b-current (and (member b wamei/docker-current-projects) t)))
      (cond ((not (eq a-current b-current)) a-current)
            ((string-empty-p a) nil)
            ((string-empty-p b) t)
            (t (string< a b)))))

  (defun wamei/docker--dim-stopped-entry (entry)
    "動いていないコンテナの ENTRY を薄く表示する。

`docker-container-propertize-entry' が Status セルに付けた face
(docker-face-status-down / -other) は残したいので、Status 以外のセルだけ
shadow に塗り替える。tabulated-list は行単位の face を持たないのでセルごとに付ける。
判定は docker.el 自身と同じく Status が \"Up\" で始まるかどうか。"
    (let* ((names (mapcar (lambda (column) (plist-get column :name))
                          docker-container-columns))
           (index (seq-position names "Status"))
           (data (cadr entry))
           (status (and index (substring-no-properties (aref data index)))))
      (when (and status (not (string-prefix-p "Up" status)))
        (dotimes (i (length data))
          (unless (= i index)
            (aset data i (propertize (substring-no-properties (aref data i))
                                     'font-lock-face 'shadow)))))
      entry))
  :init
  ;; 一覧を開く時点のバッファで project を拾う。transient 経由は docker-open-hook、
  ;; M-x docker-containers 直叩きは advice が拾う。
  ;; :config (with-eval-after-load 'docker) に置くと、docker-containers の autoload が
  ;; 読むのは docker-container.el だけで feature docker が provide されないため、
  ;; その経路では結線されない。
  (add-hook 'docker-open-hook #'wamei/docker--record-current-projects)
  (advice-add 'docker-containers :before #'wamei/docker--record-current-projects)
  ;; Up でない行を薄くする。docker-container-propertize-entry は init 時点では
  ;; 未定義だが、advice は後から来る defun を生き延びる。
  (advice-add 'docker-container-propertize-entry :filter-return
              #'wamei/docker--dim-stopped-entry)
  :custom
  ;; docker inspect の JSON を出すモード。既定は json-mode が無ければ js-mode
  ;; だが json-mode は入れておらず、JSON は treesit で見ている。
  ((docker-inspect-view-mode . 'json-ts-mode)
   ;; 対話が要るコマンド (exec / attach / image run) を出す端末。既定の auto は
   ;; eat > ghostel > vterm > shell の順に見つけたものを使うので、後で eat を
   ;; 入れたときに黙って切り替わる。vterm に固定する。
   ;; ここで開く端末のバッファ名は "* docker ... *" で、端末パネルの
   ;; display-buffer-alist ("\\`\\*term: ") には当たらないのでパネルとは独立に出る。
   (docker-terminal-backend . 'vterm)
   ;; 既定から Id と Command を落とし、compose のプロジェクト名とサービス名を出す。
   ;; 操作対象の識別子は docker-container-id-template (.Names) が別に持つので Id 列は
   ;; 無くてよい。Status 列は docker-container-propertize-entry が名前で探すので消せない。
   ;; テンプレート内の \" は、docker-run-async が start-file-process-shell-command で
   ;; --format="..." を渡すため、シェルの二重引用符を閉じないようにエスケープしている。
   ;; :sort に #'foo と書くと '(...) の中では (function foo) というリストのまま残り、
   ;; docker.el 側の -on が funcall して落ちるので裸のシンボルを渡す。
   (docker-container-columns
    . '((:name "Project" :width 16 :template "{{ json (.Label \\\"com.docker.compose.project\\\") }}"
                :sort wamei/docker--project-first-< :format nil)
        (:name "Service" :width 22 :template "{{ json (.Label \\\"com.docker.compose.service\\\") }}"
                :sort nil :format nil)
        (:name "Names"   :width 33 :template "{{ json .Names }}"  :sort nil :format nil)
        (:name "Status"  :width 27 :template "{{ json .Status }}" :sort nil :format nil)
        (:name "Image"   :width 30 :template "{{ json .Image }}"  :sort nil :format nil)
        (:name "Ports"   :width 24 :template "{{ json .Ports }}"  :sort nil :format nil)))
   ;; 現在の project → 他の compose プロジェクト (名前順) → compose 以外 の順に並ぶ。
   (docker-container-default-sort-key . '("Project" . nil))))

(leaf claude-cli
  :doc "claude -p でコミットメッセージ生成と単発 prompt"
  :ensure nil
  :preface
  ;; 実体は claude-cli.el。init.el は ~/.emacs.d/init.el への symlink なので
  ;; 実体の隣から読む (desktop-side-windows と同じ)。
  (load (expand-file-name "claude-cli"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  :config
  ;; git-commit-mode-map は magit 同梱の git-commit が定義するので、その後に束縛する。
  ;; wamei/claude-haiku / -sonnet / -opus は M-x から使う想定でキーは割り当てない。
  (with-eval-after-load 'git-commit
    (define-key git-commit-mode-map (kbd "C-c C-m") #'wamei/claude-commit-message)))

(leaf claude-complete
  :doc "claude -p によるゴーストテキスト補完"
  :ensure nil
  :after claude-cli
  :preface
  ;; 実体は claude-complete.el。claude-cli と同じく init.el の実体の隣から読む。
  (load (expand-file-name "claude-complete"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  ;; prog-mode 全体で有効化する。eglot が無いバッファでも動き、eglot 管理下なら
  ;; 補完候補の識別子名がプロンプトに加わる。
  :hook (prog-mode-hook . wamei/claude-complete-mode)
  :config
  ;; 既定の haiku は 2.3-3.2 秒だが、禁止しているコードフェンスを付けたり行頭の
  ;; インデントを落としたりする。sonnet は +0.5 秒程度で指示に従う (2026-09-03 実測、
  ;; opus は +1.2-1.5 秒)。
  (setq wamei/claude-complete-model "sonnet")
  ;; 自動のゴーストテキストは copilot に任せ、claude は C-c C-. の手動要求だけにする。
  ;; claude -p は 1 回 3 秒前後かかり、Copilot の 0.3-1 秒に比べて体感が重かった。
  (setq wamei/claude-complete-auto nil))

(leaf copilot
  :doc "GitHub Copilot のゴーストテキスト補完"
  :ensure t
  ;; 初回のみ M-x copilot-install-server (npm 経由、node 22+) と
  ;; M-x copilot-login (ブラウザでデバイスコード入力) が必要。
  :hook (prog-mode-hook . copilot-mode)
  :custom
  (copilot-idle-delay . 0.3)
  ;; モードごとの indent offset 変数が見つからないときの警告を止める。
  (copilot-indent-offset-warning-disable . t)
  :config
  ;; キーは既定の copilot-completion-map をそのまま使う: TAB で確定、C-TAB で単語ごと、
  ;; M-n / M-p で候補切替。このマップは補完表示中だけ効くので、非表示時の TAB は
  ;; 従来どおりインデントや corfu に落ちる。
  ;; corfu のポップアップ中はゴーストテキストを出さない (二重表示と TAB の取り合いを避ける)。
  (add-to-list 'copilot-disable-predicates
               (lambda () (bound-and-true-p completion-in-region-mode))))

(leaf dired
  :doc "diredの設定"
  :leaf-defer nil
  :bind (("C-x C-j" . dired-toggle-current-or-project-directory)
         (:dired-mode-map
         ("C-c C-s" . dired-toggle-sudo)
         ("C-c o" . dired-do-open)
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
  ;; ファイルを掴んで別の dired バッファへ落とせるようにする (down-mouse-1)。
  ;; 動かさずに離したときは mouse-1 が押し戻されるので通常のクリックと両立する。
  ;; macOS では drop は常に action `private' で届き、修飾キー (Shift / Control / Meta)
  ;; は受け手に伝わらない。private は `wamei/dired-tree-drop-action' (既定 `move')
  ;; に読み替えるので、既定は Finder と同じ「移動」になる。Finder からの drop も
  ;; 同じく移動になる。copy したいときは `wamei/dired-tree-drop-action' を `copy'
  ;; にするか、dired の `C' (dired-do-copy) を使う。
  (setq dired-mouse-drag-files t)
  ;; auto-revert の "Reverting buffer..." などのメッセージを出さない。
  (setq auto-revert-verbose nil)

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
            )))

  (defun wamei/dired-context-menu-extras (menu click)
    "右クリックメニューに dired のファイル操作を足す。`context-menu-functions' 用。
dired 組み込みの `dired-context-menu' (Find / Open / Open With) に続けて、
コピー・改名・削除・新規作成、ディレクトリ行なら展開/折りたたみを出す。"
    (when (and (derived-mode-p 'dired-mode)
               (mouse-posn-property (event-start click) 'dired-filename))
      ;; 右クリックした行に point を移す。メニューの各コマンドは point の
      ;; ファイル (またはマーク) に効くので、save-excursion で戻さない。
      (mouse-set-point click)
      (let ((file (dired-get-filename nil t)))
        (define-key menu [wamei-dired-separator] menu-bar-separator)
        (when (and file (file-directory-p file) (fboundp 'dired-subtree-toggle))
          (define-key menu [wamei-dired-toggle]
                      '(menu-item "Expand / Collapse" dired-subtree-toggle)))
        ;; dired-copy-filename-as-kill は引数 0 で絶対パスをコピーする
        (define-key menu [wamei-dired-copy-path]
                    '(menu-item "Copy Path" (lambda () (interactive) (dired-copy-filename-as-kill 0))))
        (define-key menu [wamei-dired-copy] '(menu-item "Copy…" dired-do-copy))
        (define-key menu [wamei-dired-rename] '(menu-item "Rename…" dired-do-rename))
        (define-key menu [wamei-dired-delete] '(menu-item "Delete…" dired-do-delete))
        (define-key menu [wamei-dired-new-file] '(menu-item "New File…" dired-create-empty-file))
        (define-key menu [wamei-dired-new-dir] '(menu-item "New Directory…" dired-create-directory))))
    menu)
  :hook
  ;; 外部でのファイル変更に追従する (file-notify 経由)。
  (dired-mode-hook . auto-revert-mode)
  :config
  ;; 右クリックメニュー。dired-mode では dired-context-menu が組み込みで足される。
  (context-menu-mode 1)
  (add-hook 'context-menu-functions #'wamei/dired-context-menu-extras))

(leaf dired-subtree
  :doc "dired でディレクトリをその場で展開する"
  :ensure t
  :after dired
  :bind (:dired-mode-map
         ("TAB" . dired-subtree-toggle)
         ("<backtab>" . dired-subtree-cycle))
  :custom
  ;; 背景色で深さを表すのはやめ、line-prefix のインデントだけにする
  (dired-subtree-use-backgrounds . nil)
  :config
  ;; nerd-icons-dired は dired-after-readin-hook でしか付け直さないので、
  ;; 展開した行にもアイコンを付ける。revert 時の復元も dired-subtree-insert を
  ;; 通るのでここ一箇所で足りる。
  (with-eval-after-load 'nerd-icons-dired
    (add-hook 'dired-subtree-after-insert-hook #'nerd-icons-dired--refresh)))

(leaf dired-tree
  :doc "dired-subtree の展開記憶、展開ディレクトリの監視、D&D の落下先"
  :ensure nil
  ;; :after は付けない。dired-tree.el が dired-subtree を require するので
  ;; :preface の load 時点で両方読み込まれる。:after を付けると :hook の登録が
  ;; eval-after-load に包まれて、読む順が変わったときに黙って効かなくなる。
  :preface
  ;; 実体は dired-tree.el。init.el は symlink なので実体の隣から読む。
  (load (expand-file-name "dired-tree"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  :hook
  (dired-mode-hook . wamei/dired-tree-mode))

(leaf dired-git-status
  :doc "dired のファイル名を git の状態で色分けする (treemacs-git-mode の代替)"
  :ensure nil
  :preface
  (load (expand-file-name "dired-git-status"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  :hook
  (dired-mode-hook . wamei/dired-git-status-mode))

(leaf project-sidebar
  :doc "dired ベースのプロジェクトサイドバー (treemacs の代替)"
  :ensure nil
  :bind (("C-x C-n" . wamei/project-sidebar-toggle))
  :preface
  (load (expand-file-name "project-sidebar"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  ;; minor-mode 関数は引数なしで呼ぶと「有効化」なので、hook に
  ;; hide-mode-line-mode を直接置くと sidebar mode を切ったときにも ON になる。
  ;; sidebar mode の状態に合わせる関数を挟む。
  (defun wamei/project-sidebar--sync-hide-mode-line ()
    "`hide-mode-line-mode' を `wamei/project-sidebar-mode' の on/off に合わせる。"
    (hide-mode-line-mode (if wamei/project-sidebar-mode 1 -1)))
  :init
  (wamei/project-sidebar-setup)
  :hook
  (wamei/project-sidebar-mode-hook . wamei/project-sidebar--sync-hide-mode-line)
  :config
  (wamei/project-sidebar-follow-mode 1))

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
  ;; side window の扱いは desktop-side-windows.el に分けている。
  ;; 保存時は live window に触らず、frameset のデータから side window を外して
  ;; side / slot / 寸法だけ記録し、読み込み後にタブごとに開き直す。
  ;; init.el は ~/.emacs.d/init.el への symlink なので実体の隣から読む。
  (load (expand-file-name "desktop-side-windows"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  ;; 端末 (vterm) の数・作業ディレクトリ・タイトル・直前の出力は term-restore.el
  ;; が desktop のグローバル変数として保存し、読み込み後に端末を作り直す。
  (load (expand-file-name "term-restore"
                          (file-name-directory (file-truename user-init-file)))
        nil t)

  (defvar wamei/desktop-claude-restore-command #'claude-code-ide-continue
    "desktop 読み込み後に claude パネルを開き直すコマンド。nil なら開き直さない。
プロセスは残らないので、既定ではそのディレクトリの直近の会話を続ける (-c)。")

  (defun wamei/desktop--restore-sidebar (spec)
    "sidebar を開き直し、幅を SPEC の :size に合わせる。
desktop-side-windows が SPEC の :directory を default-directory に束縛して呼ぶので、
そのディレクトリのプロジェクトの sidebar が出る。"
    (wamei/desktop-side-resize (wamei/project-sidebar-show default-directory)
                               (plist-get spec :size)))

  (defun wamei/desktop--restore-term (spec)
    "端末パネルを開き直す。
端末バッファは term-restore (desktop-after-read-hook の先頭) が記録どおりに
作り直しているので、パネルに出ていたもの (SPEC の :buffer) をそのまま出す。
記録が無い (初回や旧形式の desktop) ときは同プロジェクトの端末か新しい端末を使う。
高さは display-buffer-alist の wamei/term--set-height が wamei/term-height
(desktop に保存) から決める。一覧 (*terminals: <project>*) は端末が 2 つ以上のときだけ
自動で出るので復元しない。幅は同様に wamei/term-list-width (desktop に保存) から
wamei/term--set-list-width が決める。"
    (wamei/term--show (or (get-buffer (plist-get spec :buffer))
                          (wamei/term--current)
                          (wamei/term--create 1))
                      t))

  (defun wamei/desktop--restore-claude (spec)
    "claude パネルを `wamei/desktop-claude-restore-command' で開き直し、幅を SPEC に合わせる。
プロジェクトは default-directory から決まる (claude-code-ide--get-working-directory)。
desktop-side-windows が SPEC の :directory (保存時の claude バッファの作業
ディレクトリ) を束縛して呼ぶので、選択 window に前のタブのバッファが残っていても
別タブと同じプロジェクトのセッションにはならない。"
    (when wamei/desktop-claude-restore-command
      (require 'claude-code-ide)
      (funcall wamei/desktop-claude-restore-command)
      (wamei/desktop-side-resize (wamei/claude--window) (plist-get spec :size))))

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
  ;; magit はプロセス状態を持つバッファなので除外する。
  (desktop-modes-not-to-save . '(tags-table-mode
                                 magit-status-mode
                                 magit-process-mode
                                 magit-diff-mode
                                 magit-revision-mode))
  :config
  ;; ターミナル (emacs -nw) と Emacs.app ではウィンドウ構成やフォント周りが
  ;; 違い、同じ desktop を共有すると互いのセッションを上書きしてしまう。
  ;; GUI は既定の .emacs.desktop のまま、ターミナルだけ .emacs.desktop-nw に分ける。
  ;; ロックも分けて、両方を同時に起こしても互いのロックを掴まないようにする。
  ;; leaf の :custom は値に if 式を置けないので、ここで setq する。
  ;; 読み込みは after-init-hook の desktop-read で行われるので、この時点の設定で間に合う。
  (unless (display-graphic-p)
    (setq desktop-base-file-name ".emacs.desktop-nw"
          desktop-base-lock-name ".emacs.desktop-nw.lock"))
  ;; 端末パネルの高さの割合と一覧の幅も次回に引き継ぐ
  (add-to-list 'desktop-globals-to-save 'wamei/term-height)
  (add-to-list 'desktop-globals-to-save 'wamei/term-list-width)
  ;; バッファ名で開き直し方を選ぶ。sidebar は " *sidebar: " で始まる。
  (setq wamei/desktop-side-restorers
        `(("\\` \\*sidebar: " . wamei/desktop--restore-sidebar)
          ("\\`\\*term: " . wamei/desktop--restore-term)
          (,wamei/term-list-buffer-regexp . ignore)
          ("\\`\\*claude-code\\[" . wamei/desktop--restore-claude)))
  ;; sidebar は dired バッファなので desktop が普通の dired として保存してしまう。
  ;; 除外して restorer に任せる。
  (setq desktop-buffers-not-to-save
        (if desktop-buffers-not-to-save
            (concat "\\` \\*sidebar: \\|" desktop-buffers-not-to-save)
          "\\` \\*sidebar: "))
  (wamei/desktop-side-setup)
  (wamei/term-restore-setup)
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

(leaf auto-dim-other-buffers
  :doc "選択中以外の window の背景を少し暗くして、どこにいるか分かるようにする"
  ;; face remapping はバッファ単位だが、このパッケージは :filtered (:window ...) 付きの
  ;; remapping で window ごとに色を分け、選択の切り替えに追随する。
  :ensure t
  :custom-face
  ;; 既定の "#122" は青緑がかって doom-molokai に合わないので、default (#1c1e1f) を
  ;; 落とした色にする。hide は org-hide 用で前景も背景に合わせる。
  (auto-dim-other-buffers . '((t (:background "#121314"))))
  (auto-dim-other-buffers-hide . '((t (:foreground "#121314" :background "#121314"))))
  :custom
  ;; ミニバッファに入っても直前の window を暗くしない (戻る場所が分かるように)
  (auto-dim-other-buffers-dim-on-switch-to-minibuffer . nil)
  :global-minor-mode auto-dim-other-buffers-mode)

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

(leaf *popup-appearance
  :doc "corfu / eldoc-box / vertico-posframe の child frame の背景色と枠線を揃える"
  ;; 各パッケージの本体・枠線 face をここで定義する 2 つの face に継承させる。
  ;; 色を変えるときはこの 2 つだけ触る。枠幅は各 leaf で 1 に揃えている。
  :preface
  (defface wamei/popup-body '((t (:inherit tooltip)))
    "child frame ポップアップ本体の face。背景色の共通元。")
  (defface wamei/popup-border '((t (:background "#525254")))
    "child frame ポップアップ枠線の face。GUI の 1px の帯はこの :background で塗られる。")
  (defface wamei/popup-border-line '((t nil))
    "tty の枠線 (罫線文字) に載せる face。
`wamei/popup-border' の :background を起動時に :foreground へ写す。背景まで付けると
罫線のセルがベタ塗りになるので別 face にしている。")
  :custom-face
  (corfu-default . '((t (:inherit wamei/popup-body))))
  (corfu-border . '((t (:inherit wamei/popup-border))))
  (eldoc-box-body . '((t (:inherit wamei/popup-body))))
  (eldoc-box-border . '((t (:inherit wamei/popup-border))))
  (vertico-posframe . '((t (:inherit wamei/popup-body))))
  (vertico-posframe-border . '((t (:inherit wamei/popup-border))))
  :config
  ;; tty の child frame の枠 (undecorated nil) は display table の box-* スロットの
  ;; 文字で、フレームの外側 1 文字に描かれる。既定の +-| を罫線にし、glyph に face を
  ;; 載せて色を付ける (枠の色は face では変えられず、glyph の face だけが効く)。
  ;; 罫線は East Asian Width が A だが Warp は半角で描く。
  (unless (display-graphic-p)
    (set-face-foreground 'wamei/popup-border-line
                         (face-attribute 'wamei/popup-border :background nil t))
    (unless standard-display-table
      (setq standard-display-table (make-display-table)))
    (dolist (slot '((box-vertical . ?│) (box-horizontal . ?─)
                    (box-down-right . ?┌) (box-down-left . ?┐)
                    (box-up-right . ?└) (box-up-left . ?┘)))
      (set-display-table-slot standard-display-table (car slot)
                              (make-glyph-code (cdr slot) 'wamei/popup-border-line)))
    ;; ウィンドウの縦の分割線も同じ罫線にする。doom-themes は `vertical-border' の
    ;; 前景と背景を同じ色にしているので tty ではベタ塗りの 1 桁になる。背景を外して
    ;; GUI と同じ「細い線」に見せる (色はテーマの前景のまま)。
    (set-face-attribute 'vertical-border nil :background 'unspecified)
    (set-display-table-slot standard-display-table 'vertical-border
                            (make-glyph-code ?│))))

(leaf tooltip
  :doc "help-echo (flymake の診断メッセージなど) のツールチップ"
  :ensure nil
  :preface
  (defun wamei/tty-tip-shift-for-border (pos)
    "tty-tip の位置 POS (X . Y) を枠の分だけ右下へ 1 文字ずらす。
枠 (undecorated nil) はフレームの外側に描かれるので、そのままだとマウス位置の
文字を枠の角が隠す。端末の右端・下端からはみ出さないように収める。"
    (cons (min (1+ (car pos))
               (max 0 (- (display-pixel-width) (frame-width tty-tip--frame) 1)))
          (min (1+ (cdr pos))
               (max 0 (- (display-pixel-height) (frame-height tty-tip--frame) 1)))))

  (defun wamei/frame-unset-internal-border-color (frame)
    "通常フレーム FRAME の内側の枠 (internal border) の色を無色に戻す。
GUI の tooltip の枠色は `internal-border' face のグローバル値で与えるが、そのままだと
通常フレームの内側の枠 (macOS では幅 2px) にも色が付くので、フレームごとに打ち消す。
child frame (corfu / eldoc-box / posframe) は自分でこの face を設定するので触らない。"
    (unless (frame-parameter frame 'parent-frame)
      (set-face-background 'internal-border 'unspecified frame)))
  :custom
  ;; macOS のネイティブ tooltip は `tooltip' face を無視して小さなシステムフォントで
  ;; 描く。Emacs 自前の tip frame にすると face の色とフォントが効き、背景が
  ;; 他のポップアップ (wamei/popup-body は tooltip を継承) と揃う。
  (use-system-tooltips . nil)
  ;; 枠は他のポップアップと同じ 1px、透過も同じ 90。外側の枠 (border-width) は
  ;; macOS では描かれない。
  (tooltip-frame-parameters . '((name . "tooltip")
                                (internal-border-width . 1)
                                (border-width . 0)
                                (alpha . 90)
                                (no-special-glyphs . t)))
  :config
  ;; tip frame は `frame-list' に現れず after-make-frame-functions も走らないので、
  ;; 枠色は face のグローバル値でしか渡せない。
  (when (display-graphic-p)
    (set-face-background 'internal-border
                         (face-attribute 'wamei/popup-border :background nil t))
    (mapc #'wamei/frame-unset-internal-border-color (frame-list))
    (add-hook 'after-make-frame-functions #'wamei/frame-unset-internal-border-color))
  ;; tty では tty-tip (Emacs 31) が help-echo を child frame で出す。他のポップアップと
  ;; 同じ罫線の枠を付け、枠の分だけマウス位置からずらす。マウスは xterm-mouse-mode。
  (unless (display-graphic-p)
    (require 'tty-tip)
    (setq tty-tip-frame-parameters
          (cons '(undecorated . nil)
                (assq-delete-all 'undecorated (copy-alist tty-tip-frame-parameters))))
    (advice-add 'tty-tip--compute-position :filter-return
                #'wamei/tty-tip-shift-for-border)
    (tty-tip-mode 1)))

(leaf vertico-posframe
  :doc "ミニバッファを child frame で表示する"
  :ensure t
  :after vertico
  ;; Emacs 31 は tty でも child frame を作れる。posframe 側が `posframe-workable-p' で
  ;; 同じ判定をし、tty では枠を文字で描く (undecorated nil)。
  :if (or (display-graphic-p) (featurep 'tty-child-frames))
  :custom
  ;; フレーム中央。モードラインやミニバッファの高さに依存しないので
  ;; 端末パネルの有無で位置がずれない。
  (vertico-posframe-poshandler . #'posframe-poshandler-frame-center)
  ;; corfu / eldoc-box と同じ幅 (*popup-appearance 参照)
  (vertico-posframe-border-width . 1)
  (vertico-posframe-parameters . '((alpha . 90)))
  :global-minor-mode vertico-posframe-mode)

(leaf marginalia
  :doc "補完に情報付与"
  :ensure t
  :global-minor-mode t)

(leaf corfu
  :doc "inline補完"
  :ensure t
  :preface
  (defvar wamei/corfu--tty-shift nil
    "非 nil なら `wamei/corfu-tty-make-frame-args' が位置を補正する。
候補ポップアップ (`corfu--popup-show') の間だけ t にし、同じ `corfu--make-frame' を
通る corfu-popupinfo には掛けない (そちらは `wamei/corfu-popupinfo-tty-areas' で補正)。")

  (defun wamei/corfu-tty-popup-show (fn &rest args)
    "`corfu--popup-show' の間だけ `wamei/corfu--tty-shift' を立てる。"
    (let ((wamei/corfu--tty-shift t))
      (apply fn args)))

  (defun wamei/corfu-tty-make-frame-args (args)
    "tty では枠がフレームの外側 1 文字に描かれるので、その分ポップアップを内側へずらす。
ARGS は `corfu--make-frame' の (FRAME X Y WIDTH HEIGHT)。corfu は tty の枠幅を 0 として
位置を計算するため、そのままだと上枠がカーソル行を塗りつぶし、左端では左枠が切れる。
カーソル行より上に出るときは下枠がカーソル行に掛かるので逆向きにずらす。"
    (if (not wamei/corfu--tty-shift)
        args
      (pcase-let* ((`(,frame ,x ,y ,width ,height) args)
                   (point-y (+ (cadr (window-inside-pixel-edges))
                               (or (cdr (posn-x-y (posn-at-point))) 0))))
        (list frame
              (max 1 (min (1+ x) (- (frame-width) width 1)))
              (if (> y point-y) (1+ y) (1- y))
              width height))))

  (defun wamei/corfu-popupinfo-tty-areas (areas)
    "corfu-popupinfo の候補領域 AREAS (左 右 縦) を tty の枠の分だけずらす。
corfu-popupinfo は tty の枠幅を 0 として候補ポップアップに密着させるため、そのままだと
枠が候補の文字に重なる。左右は 1 桁外へ、縦は候補の下なら 1 行下、上なら 1 行上へ
ずらし、候補ポップアップと枠線を共有する位置に置く。"
    (pcase-let* ((`(,al ,ar ,av) areas)
                 (`(,_ ,cfy ,_ ,_) (corfu-popupinfo--frame-geometry corfu--frame)))
      (list (cons (1- (car al)) (cdr al))
            (list (1+ (car ar)) (nth 1 ar)
                  (min (nth 2 ar) (- (frame-width) (car ar) 2)) (nth 3 ar) 'right)
            (list (car av) (+ (nth 1 av) (if (> (nth 1 av) cfy) 1 -1))
                  (nth 2 av) (nth 3 av) 'vertical))))
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
  (add-to-list 'corfu--frame-parameters '(alpha . 90))
  ;; tty では undecorated nil にすると枠が文字で描かれる (posframe と同じ見た目)。
  (unless (display-graphic-p)
    (setq corfu--frame-parameters
          (cons '(undecorated . nil)
                (assq-delete-all 'undecorated (copy-alist corfu--frame-parameters))))
    (advice-add 'corfu--popup-show :around #'wamei/corfu-tty-popup-show)
    (advice-add 'corfu--make-frame :filter-args #'wamei/corfu-tty-make-frame-args)
    (with-eval-after-load 'corfu-popupinfo
      (advice-add 'corfu-popupinfo--possible-areas :filter-return
                  #'wamei/corfu-popupinfo-tty-areas))))

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

  :custom ((consult-async-min-input . 1)
           ;; C-x p b: バッファ → recentf → 未訪問のプロジェクトファイル →
           ;; ignore 済みファイル → 既知ルート。既定の "Project File" は recentf 由来で
           ;; 未訪問のものが出ないため、project-extra-files.el のソースで補う (project leaf)。
           (consult-project-buffer-sources
            . '(consult-source-project-buffer
                consult-source-project-recent-file
                wamei/consult-source-project-files
                wamei/consult-source-project-ignored-files
                consult-source-project-root))  ; 既定 3 だと「日本」のような 2 文字語で検索が走らない
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

(leaf embark
  :doc "ミニバッファ候補へのアクション。consult-git-grep 等の候補を
embark-export で grep-mode バッファに書き出せる"
  :ensure t
  :bind (("C-." . embark-act)
         (minibuffer-local-map
          :package emacs
          ("C-c C-e" . embark-export)     ; 候補全件を種類に応じたバッファ (grep-mode / dired 等) へ
          ("C-c C-c" . embark-collect)))) ; 候補文字列をそのまま一覧バッファへ

(leaf embark-consult
  :doc "embark と consult の橋渡し。export 先を grep-mode にする等。
export したバッファは Emacs 31 標準の grep-edit-mode (e で編集開始、
C-c C-c で元ファイルへ書き戻し) で編集できるので wgrep は入れない"
  :ensure t
  :after (embark consult)
  :require t ; autoload 連携がないので :after だけでは読み込まれない
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(leaf markdown-mode
  :doc "Markdown"
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :custom ((markdown-fontify-code-blocks-natively . t)
           (markdown-header-scaling . t)
           (markdown-display-remote-images . t)))

(leaf dotenv-mode
  :doc ".env 系ファイル (KEY=VALUE / ${VAR} 展開 / export / コメントを着色)"
  :ensure t
  ;; パッケージ標準の autoload は .env.* しか拾わないため、
  ;; 素の .env と foo.env 形式も対象にする
  :mode ("\\.env\\'" "\\.env\\.[^/]*\\'"))

(leaf sql
  :doc "SQL バッファと SQLi (組み込み)。psql / mysql を comint で動かして結果を見る。

接続先は Emacs 用に書き直さず、クライアントが元々読む設定ファイルだけを情報源にする
(sql-connections.el が列挙する):
  ~/.my.cnf          の [client<名前>] → C-u M-x sql-connect の mysql:<名前>
  ~/.pg_service.conf の [<名前>]       → 同じく postgres:<名前>
ホスト・ユーザ・パスワードはそちらに書く (mysql / psql / sqls / 他のツールと共有できる)。
Emacs は名前だけを渡すので、認証情報はプロセスの引数に出ない。設定ファイルの編集は
接続先を選ぶ直前に自動で読み直す (Emacs で保存した場合は sqls にも即座に反映する)。
カレント project の .wrangler にある wrangler dev のローカル D1 も
sqlite:<repo>/<ファイル名の先頭 8 桁> として並ぶ。実体は SQLite ファイルなので
wrangler dev を動かしたまま読めるが、miniflare が握っているので読む用途だけにする
(書き込みとスキーマ変更は wrangler d1 execute / migrations 経由)。
SQL バッファからの送信は C-c C-c (段落) / C-c C-r (リージョン) / C-c C-b (バッファ)。
補完は sqls (eglot ブロック) が同じ設定ファイルを見る。"
  :ensure nil
  :preface
  (defun wamei/sql-interactive-setup ()
    "SQLi バッファの表示設定。"
    ;; 結果の 1 行はウィンドウ幅を超えるのが普通なので、折り返さず横スクロールで読む
    (setq-local truncate-lines t))

  (defun wamei/sql-connections-reload (&rest _)
    "接続先を client 設定ファイルから読み直す。advice 用に引数を捨てる。"
    (sql-connections-refresh))

  (defun wamei/sql-eglot-servers ()
    "sql-mode のバッファを管理している eglot サーバの一覧。"
    (when (fboundp 'eglot-current-server)
      (delete-dups
       (delq nil (mapcar (lambda (buffer)
                           (with-current-buffer buffer
                             (and (derived-mode-p 'sql-mode) (eglot-current-server))))
                         (buffer-list))))))

  (defun wamei/sql-connections-reload-on-save ()
    "client 設定ファイルを保存したら接続先を読み直し、sqls にも新しい設定を送る。
sqls は workspace/didChangeConfiguration で接続一覧を作り直すので、再起動しなくてよい。"
    (when (and buffer-file-name
               (member (file-truename buffer-file-name)
                       (mapcar #'file-truename
                               (seq-filter #'file-exists-p
                                           (list sql-connections-my-cnf
                                                 sql-connections-pg-service-file)))))
      (sql-connections-refresh)
      (dolist (server (wamei/sql-eglot-servers))
        (eglot-signal-didChangeConfiguration server))
      (message "sql-connections: %s"
               (mapconcat #'symbol-name (mapcar #'car sql-connection-alist) " "))))
  :custom
  ;; 接続先を指定せずに SQLi を起動したときの既定 (sql-connect は接続定義側の指定を使う)
  (sql-product . 'postgres)
  ;; 入力履歴を Emacs のセッション間で残す (SQLi の終了時に書き出される)
  (sql-input-ring-file-name . "~/.emacs.d/sql-history")
  ;; mysql は出力先が tty でないと罫線なしの TSV を吐く。comint は tty ではないので
  ;; -t (--table) を明示する。-A は起動時のテーブル名読み込み (補完用) を止める指定で、
  ;; 補完は sqls に任せるため不要、大きい DB では接続が目に見えて遅くなる。
  ;; psql 側は sql-postgres-options の既定 ("-P" "pager=off") で足りる。
  (sql-mysql-options . '("-t" "-A"))
  :hook (sql-interactive-mode-hook . wamei/sql-interactive-setup)
  :config
  ;; 接続先の列挙は sql-connections.el (init.el は symlink なので実体の隣から読む)
  (load (expand-file-name "sql-connections"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  (sql-connections-refresh)
  ;; 設定ファイルを編集した後に M-x sql-connections-refresh を打たなくて済むようにする。
  ;; 接続先を選ぶ直前に読み直せば、Emacs の外で編集した場合も拾える。
  (advice-add 'sql-read-connection :before #'wamei/sql-connections-reload)
  (advice-add 'sql-connect :before #'wamei/sql-connections-reload)
  ;; Emacs で保存したときは走っている sqls にも知らせる (補完がすぐ追従する)
  (add-hook 'after-save-hook #'wamei/sql-connections-reload-on-save))

(leaf prisma-ts-mode
  :doc "Prisma スキーマ (tree-sitter)"
  :ensure t
  ;; .prisma → prisma-ts-mode の紐付けはパッケージの autoload が行うので :mode は不要。
  ;; grammar は treesit ブロックの treesit-language-source-alist に登録してあり、
  ;; wamei/treesit-install-missing-grammars で導入する。
  :preface
  (defun wamei/prisma-format-on-save ()
    "eglot 管理下なら保存前に言語サーバでフォーマットする。
サーバが落ちていても保存自体は止めない。"
    (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
      (condition-case err
          (eglot-format-buffer)
        (error (message "prisma format: %s" (error-message-string err))))))
  (defun wamei/prisma-setup ()
    "prisma-ts-mode のバッファ設定。保存時フォーマットと言語サーバ向け設定。"
    ;; prisma format CLI はプロジェクトごとの導入とエンジン起動が必要で重いため、
    ;; 言語サーバの textDocument/formatting を使う。
    (add-hook 'before-save-hook #'wamei/prisma-format-on-save nil t)
    ;; eglot は textDocument/formatting の tabSize に tab-width をそのまま渡す。
    ;; Prisma の慣習 (prisma format) は 2 スペースなので、モードのインデント幅に揃える。
    (setq-local tab-width prisma-ts-mode-indent-level))
  :hook (prisma-ts-mode-hook . wamei/prisma-setup)
  :config
  ;; tree-sitter-prisma の現行 grammar に ";" トークンが無く、パッケージの
  ;; delimiter 規則 ["," ";" ":"] がコンパイルできずに機能ごと無効化される
  ;; (Warning treesit-font-lock-rules-mismatch)。";" を除いた規則に差し替える。
  (setq prisma-ts-mode--font-lock-settings
        (append (seq-remove (lambda (setting) (eq (nth 2 setting) 'delimiter))
                            prisma-ts-mode--font-lock-settings)
                (treesit-font-lock-rules
                 :language 'prisma
                 :feature 'delimiter
                 '(["," ":"] @font-lock-delimiter-face)))))

(leaf treesit
  :doc "tree-sitter"
  :ensure nil
  ;; treesit.el を明示的に読み込む。:ensure nil だけでは require されず、
  ;; :config 内の treesit-ready-p が void になる。
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :require t
  :custom (treesit-font-lock-level . 4)
  :config
  ;; dockerfile-ts-mode / yaml-ts-mode は自分が動作確認した commit の recipe を
  ;; treesit-language-source-alist に append するが、それはモードを読み込んだ後の話で、
  ;; 起動直後の wamei/treesit-install-missing-grammars からは見えない。ここに書いておけば
  ;; grammar 導入だけ先にできる (assoc は先頭が勝つのでこちらの指定が使われる)。
  (setq treesit-language-source-alist
        '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          (prisma     . ("https://github.com/victorhqc/tree-sitter-prisma"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (yaml       . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))))

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
        (add-to-list 'major-mode-remap-alist (cons from to)))))

  ;; markdown-mode は ```typescript のようなフェンスを ts 系モードで色付けする際、そのモードが
  ;; major-mode-remap-alist か auto-mode-alist に直接載っていることを条件にする。.ts / .tsx は
  ;; typescript-ts-mode-maybe 経由なのでどちらにも載らず、eglot のホバー (TS の型表示) だけ
  ;; 色が付かなかった。typescript-mode / tsx-mode は未導入なので remap としては何も起こさない。
  (when (treesit-ready-p 'typescript t)
    (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode)))
  (when (treesit-ready-p 'tsx t)
    (add-to-list 'major-mode-remap-alist '(tsx-mode . tsx-ts-mode))))

(leaf json-ts-mode
  :doc "JSON と JSONC (コメント付き JSON)"
  ;; .json は組み込みの auto-mode-alist (js-json-mode) と treesit ブロックの remap で
  ;; json-ts-mode になる。tree-sitter-json は comment ノードを持つのでコメントの
  ;; ハイライトも効く。
  ;; JSONC は json-ts-mode の派生モードにする。編集機能は同じだが、eglot が
  ;; vscode-json-language-server に伝える languageId を "jsonc" にできる ("json" だと
  ;; コメントが構文エラーとして報告される。eglot ブロックの eglot-server-programs 参照)。
  ;; 派生モードなので json-ts-mode-hook (eglot / apheleia) はそのまま走る。
  :when (and (fboundp 'treesit-ready-p) (treesit-ready-p 'json t))
  :init
  (define-derived-mode wamei/jsonc-ts-mode json-ts-mode "JSONC"
    "コメント付き JSON (JSONC) のメジャーモード。
`json-ts-mode' と同じ編集機能で、言語サーバには jsonc として伝える。")
  ;; tsconfig / jsconfig と VS Code の設定は仕様上コメントを許す JSONC
  (dolist (pattern '("\\.jsonc\\'"
                     "\\(?:tsconfig\\|jsconfig\\)[^/]*\\.json\\'"
                     "/\\.vscode/[^/]+\\.json\\'"))
    (add-to-list 'auto-mode-alist (cons pattern #'wamei/jsonc-ts-mode))))

;; Emacs 31 標準の auto-mode-alist は .js / .jsm / .jsx しか javascript-mode に振らず、
;; .mjs / .cjs は fundamental-mode になる。javascript-mode に登録しておけば上の
;; major-mode-remap-alist 経由で js-ts-mode に寄る。
(add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . javascript-mode))

(leaf yaml-ts-mode
  :doc "YAML と docker-compose"
  :ensure nil
  ;; .yml / .yaml → yaml-ts-mode の紐付けは Emacs 同梱の autoload (yaml-ts-mode-maybe) が
  ;; 持っているので :mode は不要。grammar は treesit ブロックで導入する。
  ;; docker-compose は yaml-ts-mode の派生モードにする。編集機能は同じだが、
  ;; モードラインで compose のバッファだと分かり、compose 固有の設定を足す場所ができる。
  ;; 言語サーバに送る languageId は eglot 側のエントリで "yaml" に固定する
  ;; (派生モードのままだと eglot がモード名から作ってしまう。eglot ブロック参照)。
  ;; compose の schema は SchemaStore のカタログがファイル名で当てるので設定は要らない。
  :when (and (fboundp 'treesit-ready-p) (treesit-ready-p 'yaml t))
  :preface
  (defun wamei/yaml-ts-mode-disable-flymake-unless-yamllint ()
    "yamllint が PATH に無ければ `yaml-ts-mode-flymake' を backend から外す。

`yaml-ts-mode' は yamllint の有無を見ずに backend を登録するが、
`yaml-ts-mode-flymake' は yamllint が見つからないと即 error を投げる。
eglot が flymake-mode を有効にすると必ずそこを踏み、`debug-on-error' が t
(この init.el の設定) だと `condition-case-unless-debug' をすり抜けて
デバッガが開く。desktop の復元中に起きると復元がそこで止まる。
yamllint は mise で入れてある (~/.config/mise/config.toml) ので通常は残るが、
未導入のマシンでも黙って eglot の診断だけになるようにしておく。"
    (unless (executable-find "yamllint")
      (remove-hook 'flymake-diagnostic-functions #'yaml-ts-mode-flymake t)))
  :hook (yaml-ts-mode-hook . wamei/yaml-ts-mode-disable-flymake-unless-yamllint)
  :init
  (define-derived-mode wamei/docker-compose-ts-mode yaml-ts-mode "Compose"
    "docker-compose ファイルのメジャーモード。
`yaml-ts-mode' と同じ編集機能で、言語サーバには yaml として伝える。")
  ;; docker compose が既定で読むファイル名 (compose.yaml / docker-compose.yml と、
  ;; docker-compose.override.yml のような中置き付き)。同梱の "\\.ya?ml\\'" より
  ;; 前に積まれるので、compose だけこちらに振り分かる。
  (add-to-list 'auto-mode-alist
               '("/\\(?:docker-\\)?compose\\(?:\\.[^/]*\\)?\\.ya?ml\\'"
                 . wamei/docker-compose-ts-mode)))

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

  (defun wamei/eglot-code-action-params-quickfix-only (args)
    "eglot の自動問い合わせ (:triggerKind 2) のコードアクションを quickfix に絞る。

`eglot-code-action-suggestion' はカーソル移動ごとに種類を絞らず textDocument/codeAction
を送るが、サーバによっては statement 上のほぼ全ての位置で refactor (Extract to
function / Move to a new file など) を返すため、ヒントが常時出てしまう
(typescript-language-server がそうだった。今の tsc --lsp は codeActionKinds に
quickfix と source.* しか出さない)。
ここで :only を付け、直せる問題があるときだけヒントが出るようにする。
`eglot--code-action-params' の :filter-args advice。手動の `eglot-code-actions'
(triggerKind 無し) には影響しない。"
    (if (and (eq (plist-get args :triggerKind) 2)
             (not (plist-member args :only)))
        (append args '(:only "quickfix"))
      args))

  (defun wamei/eglot-code-action-hint-p (doc)
    "DOC (STRING . PLIST) が eglot のコードアクションヒントなら non-nil。
eldoc が各 doc に付ける :origin (生成元の documentation function) で判別する。"
    (eq (plist-get (cdr doc) :origin) 'eglot-code-action-suggestion))

  (defvar wamei/eglot-code-action-hint--last nil
    "`wamei/eglot-code-action-hint-display' が最後に echo area に出したヒント。
自分が出したものだけを消すために覚えておく。")

  (defun wamei/eglot-code-action-hint-display (docs interactive)
    "DOCS のうち eglot のコードアクションヒントだけを echo area に出す。

`eldoc-display-functions' の一員。eglot バッファでは eldoc-box-hover-at-point-mode が
`eldoc-display-in-echo-area' を外して child frame に出すので、ヒントだけをここで
echo area に戻す。echo area を触ってよいかの判定は `eldoc-display-in-echo-area' と同じ。
ヒントが無くなったときは、自分が出したヒントが残っている場合だけ消す。"
    (when (or interactive
              (and (eldoc-display-message-no-interference-p)
                   (not this-command)
                   (eldoc--message-command-p last-command)))
      (let ((hint (car (cl-find-if #'wamei/eglot-code-action-hint-p docs))))
        (cond (hint
               (setq wamei/eglot-code-action-hint--last (eldoc--message hint)))
              ((and wamei/eglot-code-action-hint--last
                    (equal eldoc-last-message wamei/eglot-code-action-hint--last))
               (setq wamei/eglot-code-action-hint--last nil)
               (eldoc--message nil))))))

  (defun wamei/eglot-code-action-hint-strip-args (args)
    "ARGS (DOCS . REST) の DOCS からコードアクションヒントを除く。
child frame (eldoc-box / eldoc-mouse) の表示関数への :filter-args advice。
ヒントは echo area に出すので、child frame には重複させない。"
    (cons (cl-remove-if #'wamei/eglot-code-action-hint-p (car args)) (cdr args)))

  (defun wamei/eglot-server-available-p ()
    "現在のバッファを担当する言語サーバが起動できそうなら非 nil。
`eglot-server-programs' から実行ファイルを引いて `executable-find' で探す。
REMOTE 引数を渡すので、TRAMP 越しのバッファではリモート側の PATH を見る
\(/docker:… でコンテナ内のファイルを開いたとき、mac 側の道具を誤って
見つけない)。host + port の接続は実行ファイルではないので判定せず許可する。"
    ;; `eglot--guess-contact' は該当エントリが無いと contact に nil を返し、
    ;; `eglot-alternatives' のエントリは候補が全滅すると error を投げる。
    ;; どちらも「起動できない」なので nil に畳む。
    (ignore-errors
      (let ((contact (nth 3 (eglot--guess-contact))))
        (when contact
          (let ((program (and (stringp (car contact))
                              ;; ("host" 1234) は TCP 接続で実行ファイルではない
                              (or (null (cdr contact)) (stringp (cadr contact)))
                              (car contact))))
            (if program
                (and (executable-find program t) t)
              t))))))

  (defun wamei/eglot-ensure-if-available ()
    "言語サーバの実行ファイルが見つかるときだけ eglot を起動する。
言語サーバはプロジェクトの依存ではなく mise で入れる道具なので
\(~/.config/mise/config.toml)、入っていない環境ではエラーにせず黙って諦める。
これが無いと、TRAMP でコンテナ内のファイルを開いたときに eglot が
リモートで存在しないコマンドを起動しようとする。リモートではコマンドが
シェル越しに起動する (`eglot--cmd') ので `make-process' 自体は成功し、
死んだプロセスに initialize を送って \"Output file descriptor … is closed\"
になる (`debug-on-error' が t だとデバッガが開く)。"
    (when (wamei/eglot-server-available-p)
      (eglot-ensure)))

  (defun wamei/sqls-switch-connection ()
    "sqls が持っている接続 (.dir-locals.el の :sqls :connections) を選び直す。
sqls は同時に 1 接続しか見ないので、複数 DB を行き来するときに使う。"
    (interactive)
    (let* ((server (or (eglot-current-server) (user-error "言語サーバに接続していない")))
           (lines (split-string
                   (string-trim (eglot-execute server '(:command "showConnections" :arguments [])))
                   "\n" t))
           (choice (completing-read "sqls connection: " lines nil t)))
      ;; 各行は "<番号> <driver>  <dataSourceName>"。switchConnections は番号の文字列を取る。
      (eglot-execute server `(:command "switchConnections"
                              :arguments [,(car (split-string choice))]))
      (message "sqls: %s" choice)))
  :custom
  ;; 最後のバッファを閉じたら言語サーバを落とす
  (eglot-autoshutdown . t)
  ;; イベントログは肥大化して重いので無効化
  (eglot-events-buffer-config . '(:size 0 :format full))
  ;; コードアクションは eldoc の文字ヒントだけにし、echo area に出す
  ;; (wamei/eglot-code-action-hint-display)。left-fringe の雷マークは、どこでも
  ;; refactor アクションを返すサーバ (tsserver 系) では常時点灯になるので使わない。
  (eglot-code-action-indications . '(eldoc-hint))
  ;; eglot-ensure ではなく wamei/eglot-ensure-if-available を通す (:preface 参照)。
  ;; サーバが PATH に無い環境では黙って諦める。
  :hook ((typescript-ts-mode-hook
          tsx-ts-mode-hook
          js-ts-mode-hook
          js-mode-hook
          json-ts-mode-hook
          css-ts-mode-hook
          css-mode-hook
          html-ts-mode-hook
          mhtml-mode-hook
          prisma-ts-mode-hook
          dockerfile-ts-mode-hook
          sql-mode-hook
          ;; 派生の wamei/docker-compose-ts-mode でもこのフックは走る
          yaml-ts-mode-hook) . wamei/eglot-ensure-if-available)
  :config
  ;; コードアクションヒント: quickfix に絞り、echo area にだけ出す (:preface の各関数参照)。
  ;; eldoc-display-functions は eldoc-box が有効化時に global 値を複製して buffer-local
  ;; にするので、eglot-managed-mode-hook より前 (= eglot 読み込み時) に global へ足す。
  ;; advice-add は対象が未定義でも登録でき、定義時に適用される。
  (advice-add 'eglot--code-action-params :filter-args
              #'wamei/eglot-code-action-params-quickfix-only)
  (add-hook 'eldoc-display-functions #'wamei/eglot-code-action-hint-display)
  (advice-add 'eldoc-box--eldoc-display-function :filter-args
              #'wamei/eglot-code-action-hint-strip-args)
  (advice-add 'wamei/eldoc-mouse--display :filter-args
              #'wamei/eglot-code-action-hint-strip-args)
  ;; TypeScript / JavaScript は typescript-language-server ではなく tsc 本体を使う。
  ;; TypeScript 7 の tsc は native バイナリ (typescript-go) で、--lsp を付けると
  ;; 言語サーバになる。サーバがバイナリそのものなので、プロジェクトに typescript が
  ;; 入っていないディレクトリでも動く (typescript-language-server は node_modules の
  ;; typescript を探して見つからないと initialize で失敗する)。診断は
  ;; publishDiagnostics ではなく pull (textDocument/diagnostic) で返るが、eglot は
  ;; :diagnosticProvider があればそちらを使う。languageId は組み込みエントリと同じに
  ;; 揃える (tsx は "typescriptreact"、js は "javascript")。
  (add-to-list 'eglot-server-programs
               '(((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript"))
                 . ("tsc" "--lsp" "-stdio")))
  ;; eglot 組み込みに Prisma のエントリは無い。@prisma/language-server は
  ;; プロジェクトの依存に入らないのが普通なので mise で入れ (~/.config/mise/config.toml)、
  ;; PATH から解決する (mise が差し込む bin を exec-path-from-shell で引き継ぐ)。
  (add-to-list 'eglot-server-programs
               '(prisma-ts-mode . ("prisma-language-server" "--stdio")))
  ;; sqls (Go 製の SQL 言語サーバ、mise で導入) はテーブル / カラム名の補完とホバーを返す。
  ;; 接続情報は ~/.my.cnf と ~/.pg_service.conf から wamei/eglot-workspace-configuration
  ;; 経由で渡す。sqls が見るのは常に 1 接続なので、切り替えは
  ;; M-x wamei/sqls-switch-connection。プロジェクト固有にしたければ .dir-locals.el で
  ;; eglot-workspace-configuration を上書きできる (eglot は workspace configuration を
  ;; 一時バッファで評価する際に hack-dir-local-variables-non-file-buffer を呼ぶため)。
  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls")))
  ;; JSONC (wamei/jsonc-ts-mode、json-ts-mode ブロック) は languageId を "jsonc" で伝える。
  ;; json-ts-mode の派生なので組み込みの json エントリにも当たるが、そちらだと "json"
  ;; になり vscode-json-language-server がコメントをエラーにする。eglot は
  ;; 派生関係で先に一致したモードの languageId を使うので、派生モードを先頭に置いた
  ;; エントリを前に積み、json / jsonc を 1 つの server で受ける。
  (add-to-list 'eglot-server-programs
               '(((wamei/jsonc-ts-mode :language-id "jsonc") js-json-mode json-ts-mode)
                 . (wamei/eglot-json-server "vscode-json-language-server" "--stdio")))
  ;; docker-compose (wamei/docker-compose-ts-mode、yaml-ts-mode ブロック) は yaml-ts-mode の
  ;; 派生なので組み込みの yaml エントリでもサーバは起動するが、そのままだと eglot が
  ;; モード名から languageId を作り "wamei/docker-compose" を送る。yaml-language-server は
  ;; schema をファイルの URI で当てるので実害は出なかったが (手元で確認)、正しい languageId
  ;; を送るために派生モードを先頭に置いたエントリを前に積む。
  ;; compose の schema は yaml-language-server が既定で参照する SchemaStore のカタログが
  ;; compose*.y*ml / docker-compose*.y*ml に compose-spec を当てるので、yaml.schemas は不要。
  (add-to-list 'eglot-server-programs
               '(((wamei/docker-compose-ts-mode :language-id "yaml") yaml-ts-mode)
                 . ("yaml-language-server" "--stdio")))
  ;; vscode-json-language-server の code action "Sort JSON" は command "json.sort" を
  ;; 返すが、server は workspace/executeCommand を実装していない (VS Code の拡張が
  ;; クライアント側で独自リクエスト json/sort を送り、返ってきた TextEdit を当てる)。
  ;; 既定の eglot-execute だと "Unhandled method workspace/executeCommand" になるので、
  ;; server を専用クラスにして json.sort だけ同じ手順で処理する。
  (defclass wamei/eglot-json-server (eglot-lsp-server) ()
    :documentation "vscode-json-language-server。Sort JSON をクライアント側で実行する。")

  (cl-defmethod eglot-execute ((server wamei/eglot-json-server) action)
    "ACTION が json.sort なら json/sort を送って結果の編集を当てる。他は既定の処理。"
    (let* ((command (plist-get action :command))
           (name (if (stringp command) command (plist-get command :command))))
      (if (equal name "json.sort")
          (eglot--apply-text-edits
           (eglot--request server :json/sort
                           `(:uri ,(plist-get (eglot--TextDocumentIdentifier) :uri)
                             ;; 整形幅はバッファの TAB と同じ json-ts-mode の幅に合わせる
                             :options (:tabSize ,(if (boundp 'json-ts-mode-indent-offset)
                                                     json-ts-mode-indent-offset
                                                   tab-width)
                                       :insertSpaces ,(if indent-tabs-mode :json-false t)))))
        (cl-call-next-method))))
  ;; @prisma/language-server は起動直後に workspace/configuration (section "prisma")
  ;; を要求し、null が返ると settings.enableDiagnostics の参照でクラッシュする
  ;; (31.12.2 で確認)。eglot はこの値を一時バッファ (major-mode 変数だけ設定、
  ;; hook は走らない) で評価するため setq-local では届かず、server を見て
  ;; 返す関数にする。.dir-locals.el の指定があればそちらが優先される。
  (defun wamei/eglot-workspace-configuration (server)
    "SERVER の管理するモードに応じた workspace configuration を返す。"
    (let ((modes (eglot--major-modes server)))
      (cond
       ((memq 'prisma-ts-mode modes)
        '(:prisma (:enableDiagnostics t)))
       ;; sqls は my.cnf / pg_service.conf を自分では読めないので、sql-connections.el が
       ;; 展開した接続情報を渡す (sql ブロック参照)。
       ((and (memq 'sql-mode modes) (fboundp 'sql-connections-sqls))
        (let ((sqls (sql-connections-sqls)))
          (and sqls (list :sqls sqls)))))))
  (setq-default eglot-workspace-configuration #'wamei/eglot-workspace-configuration)
  (advice-add 'eglot-completion-at-point :filter-return
              #'wamei/eglot-capf-doc-with-detail))

(leaf apheleia
  :doc "保存時フォーマット。biome か prettier の設定があるプロジェクトだけ有効にする"
  :ensure t
  :preface
  ;; 設定ファイルの探索と apheleia への登録は project-formatter.el (init.el は symlink
  ;; なので実体の隣から読む)。
  (load (expand-file-name "project-formatter"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  :custom
  ;; TRAMP のバッファではリモート側でフォーマッタを走らせる (デフォルトは cancel で
  ;; 何もしない)。リモート実行は apheleia の制約で同期になる。
  (apheleia-remote-algorithm . 'remote)
  ;; biome / prettier が対応するモードだけ。global-mode は使わず他言語の挙動は変えない。
  :hook ((typescript-ts-mode-hook
          tsx-ts-mode-hook
          js-ts-mode-hook
          js-mode-hook
          json-ts-mode-hook
          css-ts-mode-hook
          css-mode-hook
          ;; yaml は prettier だけが対象。biome は .yml / .yaml を扱えず標準出力が空になり、
          ;; apheleia は空の出力ではバッファを触らないので biome のプロジェクトでは何もしない。
          yaml-ts-mode-hook) . wamei/project-formatter-maybe-enable)
        ;; 編集時のインデントをフォーマッタの実測値に合わせる。editorconfig が変数を
        ;; 適用する直前に props を書き換えるので .editorconfig よりフォーマッタが優先。
        (editorconfig-hack-properties-functions
         . wamei/project-formatter-hack-editorconfig-properties)
  :config
  (wamei/project-formatter-setup))

(leaf eldoc-box
  :doc "eldoc をカーソル位置に child frame で表示する"
  :ensure t
  ;; Emacs 31 は tty でも child frame を作れる (`tty-child-frames' は端末初期化時に
  ;; provide されるので init.el の時点で判定できる)。
  :if (or (display-graphic-p) (featurep 'tty-child-frames))
  :preface
  (defvar wamei/eldoc-box-at-point-gap '(2 . 1)
    "eldoc-box の child frame をカーソルから離す距離 (桁 . 行)。")

  (defun wamei/eldoc-box-position-near (anchor width height)
    "ANCHOR の文字から `wamei/eldoc-box-at-point-gap' だけ離れた child frame の位置を返す。
ANCHOR は native frame 相対の (X . Y)、WIDTH と HEIGHT は child frame のピクセルサイズ
(tty では桁・行)。下に収まらなければ上、どちらも無理なら下端に寄せる。
カーソル位置 (`wamei/eldoc-box-at-point-position') とマウス位置 (eldoc-mouse) の両方で使う。"
    (let* ((gap-x (* (frame-char-width) (car wamei/eldoc-box-at-point-gap)))
           (gap-y (* (frame-char-height) (cdr wamei/eldoc-box-at-point-gap)))
           (below (+ (cdr anchor) (frame-char-height) gap-y))
           (above (- (cdr anchor) gap-y height)))
      (cons (max 0 (min (+ (car anchor) gap-x) (- (frame-inner-width) width)))
            (cond ((<= (+ below height) (frame-inner-height)) below)
                  ((>= above 0) above)
                  (t (max 0 (- (frame-inner-height) height)))))))

  (defun wamei/eldoc-box-at-point-position (width height)
    "カーソルから `wamei/eldoc-box-at-point-gap' だけ離れた child frame の位置を返す。
WIDTH と HEIGHT は child frame のピクセルサイズ (tty では桁・行)。
本家の `eldoc-box--default-at-point-position-function' はカーソルの直下・同じ桁に
出すので近すぎて読みにくい。"
    (wamei/eldoc-box-position-near
     (eldoc-box--point-position-relative-to-native-frame) width height))

  ;; マウスの下のシンボルの eldoc をマウス位置の child frame で表示する (eldoc-mouse.el)。
  ;; eldoc-box 同梱の eldoc-box-mouse-mode はバッファの eldoc-mode を切るので使わない。
  ;; :hook で参照するので eldoc-box の読み込みを待たずここで load する。
  (load (expand-file-name "eldoc-mouse"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  (setq wamei/eldoc-mouse-position-function #'wamei/eldoc-box-position-near)

  (defun wamei/eldoc-box-tty-update-childframe-geometry (frame window)
    "tty 版 `eldoc-box--update-childframe-geometry'。FRAME は child frame、WINDOW はその窓。
本家は child frame を親フレームより 32px 小さく clip するが、tty ではピクセル単位が
1 文字なので 32 行/桁も削られ、小さい端末では高さが負になって 1 行に潰れる。
余白を上下左右 1 文字にして親フレーム内に収める。
tty の枠 (undecorated nil) はフレームの外側 1 文字に描かれるので、位置関数には
枠込みの大きさを渡し、返った位置から枠の分だけ内側にフレームを置く。"
    (let* ((parent (frame-parent frame))
           (border (if (frame-parameter frame 'undecorated) 0 1))
           (max-width (- (frame-width parent) 2 (* 2 border)))
           (max-height (- (frame-height parent) 2 (* 2 border)))
           (size (window-text-pixel-size window nil nil max-width max-height t))
           (width (min (1+ (car size)) max-width))
           (height (min (cdr size) max-height))
           (pos (funcall eldoc-box-position-function
                         (+ width (* 2 border)) (+ height (* 2 border)))))
      (set-frame-size frame width height t)
      (set-frame-position frame (+ (car pos) border) (+ (cdr pos) border))))

  (defun wamei/eldoc-box-inhibit-during-completion (fn &rest args)
    "補完ポップアップやゴーストテキストの表示中は eldoc-box の自動表示を止める。

どちらも point 位置に child frame を出すため重なって読めなくなる。
eldoc-box--inhibit-childframe は 0.5 秒のアイドルタイマーで勝手に解除される
ため使わず、表示経路そのものを塞ぐ。C-c h (eldoc-box-help-at-point) は
この関数を通らないので手動表示は従来どおり効く。
claude-complete と copilot のゴーストテキストも point の直後に描かれるため、同じ理由で塞ぐ。"
    (unless (or (bound-and-true-p completion-in-region-mode)
                (and (fboundp 'wamei/claude-complete--visible-p)
                     (wamei/claude-complete--visible-p))
                (and (fboundp 'copilot--overlay-visible)
                     (copilot--overlay-visible)))
      (apply fn args)))

  (defun wamei/eldoc-box-quit-on-completion ()
    "補完が始まったら表示中の eldoc-box を閉じる。"
    (when (and (bound-and-true-p completion-in-region-mode)
               (fboundp 'eldoc-box-quit-frame))
      (eldoc-box-quit-frame)))
  ;; elisp などでは eldoc が常時発火して child frame がちらつくため、
  ;; まずは eglot 管理下のバッファに限定する。マウスホバーも同じ範囲。
  :hook ((eglot-managed-mode-hook . eldoc-box-hover-at-point-mode)
         (eglot-managed-mode-hook . wamei/eldoc-mouse-mode))
  ;; C-h は keyboard-translate で DEL に潰しているため C-h . は使えない
  :bind ("C-c h" . eldoc-box-help-at-point)
  :config
  (add-to-list 'eldoc-box-frame-parameters '(alpha . 90))
  ;; defvar なので :custom ではなく setq。C-c d と hover-at-point-mode の両方が読む。
  (setq eldoc-box-at-point-position-function #'wamei/eldoc-box-at-point-position)
  (unless (display-graphic-p)
    ;; 枠を文字で描く (corfu / posframe と同じ見た目)
    (setq eldoc-box-frame-parameters
          (cons '(undecorated . nil)
                (assq-delete-all 'undecorated (copy-alist eldoc-box-frame-parameters))))
    (advice-add 'eldoc-box--update-childframe-geometry :override
                #'wamei/eldoc-box-tty-update-childframe-geometry))
  (advice-add 'eldoc-box--eldoc-display-function :around
              #'wamei/eldoc-box-inhibit-during-completion)
  (add-hook 'completion-in-region-mode-hook #'wamei/eldoc-box-quit-on-completion))
