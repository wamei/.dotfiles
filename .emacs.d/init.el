;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
(setq straight-repository-branch "develop")
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
     (bootstrap-version 5))
 (unless (file-exists-p bootstrap-file)
   (with-current-buffer
       (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
     (goto-char (point-max))
     (eval-print-last-sexp)))
 (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package docker
  :bind ("C-c d" . docker))
(use-package dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-mode)))
(use-package docker-compose-mode)

(use-package el-x)
(use-package s)

(setq native-comp-async-report-warnings-errors 'nil)

;; Avoid to write `package-selected-packages` in init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; WSL判定関数
(defvar is-wsl (let ((name (s-chomp (shell-command-to-string "uname -a"))))
  (and (s-starts-with? "Linux" name)
       (s-matches? "microsoft" name))))

;; load environment value
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; 単語操作周りを変更する
(global-set-key (kbd "M-b") 'backward-to-word)
(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-h") nil)
(define-key key-translation-map [?\C-h] [?\C-?])

;; キーバインドを開ける
(global-set-key (kbd "C-q") nil)
(global-set-key (kbd "C-z") nil)

;; 対応する括弧を強調して表示する
(show-paren-mode t)
(setq show-paren-delay 0)

;; 文字コードを指定する
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; ミニバッファの履歴を保存する
(savehist-mode t)
(setq history-length 10000)

;; ホットローダーを有効にする
(global-auto-revert-mode 1)

;; ファイル履歴を保存する
(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 1000)

;; タブをスペースに
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; フレームのリサイズ
(setq frame-resize-pixelwise t)

;; symbolic link のファイル名のまま開く
(setq find-file-visit-truename nil)

;; バックアップファイルを作らない
(setq make-backup-files nil)
;; オートセーブファイルを作る
(setq auto-save-default t)
(setq auto-save-file-name-transforms '((".*" "/tmp/" t)))
(setq auto-save-timeout 10)
(setq auto-save-interval 100)

;; 折り返ししない
(setq truncate-lines t)
(setq truncate-partial-width-windows t)

;; ファイルを閉じたとき、次に開くときはその場所(point)から開く
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory "places"))

;; history保存
(savehist-mode 1)
(push 'kill-ring savehist-additional-variables)

;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; ベル音
(setq ring-bell-function 'ignore)

;; メニューバー消す
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; フレームサイズ・透明度を設定する
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-frame-parameter frame 'alpha 87)))
(set-frame-parameter nil 'alpha 87)

;; 現在行をハイライト
(global-hl-line-mode)

;; 選択範囲に色をつける
(transient-mark-mode t)

;; 行番号を表示する
(global-display-line-numbers-mode)

;; モードラインの更新時間を設定する
(setq display-time-interval 1)

;; フォント関係
(defvar font-size 140)
(defvar font-family "HackGen")
(set-face-attribute 'default nil :family font-family :height font-size)
(set-face-attribute 'variable-pitch nil :family font-family :height font-size)
(set-face-attribute 'fixed-pitch nil :family font-family :height font-size)
(set-face-attribute 'mode-line nil :family font-family :height font-size)
(set-face-attribute 'mode-line-inactive nil :family font-family :height font-size)
(set-face-attribute 'tooltip nil :family font-family :height font-size)

(global-font-lock-mode t)

;; trampモードの設定
(use-package tramp
  :straight nil
  :config
  (setq shell-file-name "/bin/bash")
  (setq explicit-shell-file-name "/bin/bash")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  )

;; 選択範囲をisearch
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;; 選択範囲を拡張する
(use-package expand-region
  :bind (("C-q C-q" . er/expand-region)
         ("C-q C-z" . er/contract-region)))

;; 行頭行末移動をいい感じにする
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-line-or-code)
         ("C-e" . mwim-end-of-line-or-code)))

;; undoを強化する
(use-package undo-tree
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :hook
  (after-init . global-undo-tree-mode))

;; キャメルケース変換系
(use-package string-inflection
  :bind (("C-q C-i C-a" . string-inflection-all-cycle)
         ("C-q C-i C-l" . string-inflection-underscore)
         ("C-q C-i C-c" . string-inflection-lower-camelcase)
         ("C-q C-i C-p" . string-inflection-camelcase)
         ("C-q C-i C-u" . string-inflection-upcase)
         ("C-q C-i C-k" . string-inflection-kebab-case)))

;; アイコンを表示する
(use-package all-the-icons)
(use-package all-the-icons-dired
  :custom
  (all-the-icons-dired-monochrome nil)
  :hook
  (dired-mode . all-the-icons-dired-mode))
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; 起動画面をダッシュボードにする
(use-package dashboard
  :custom
  (dashboard-items '((recents  . 20)
                     (bookmarks . 10)
                     ;;(projects . 10)
                     ;;(agenda . 5)
                     ;;(registers . 5)
                     ))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-startup-banner 'logo)
  :config
  (set-face-attribute 'dashboard-heading nil :foreground "orange")
  (dashboard-setup-startup-hook))

;; モードラインを変更する
(use-package doom-modeline
  :custom
  (inhibit-compacting-font-caches t)
  (doom-modeline-vcs-max-length 30)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (column-number-mode 1)
  (defun toggle-show-minor-mode ()
    (interactive)
    (setq doom-modeline-minor-modes (not doom-modeline-minor-modes))))

(use-package minions)

;; テーマを変更する
(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-custom-auto-reload t)
  (modus-themes-disable-other-themes t)
  :config
  (modus-themes-load-theme 'modus-vivendi-tinted))

;; モードラインを隠す
(use-package hide-mode-line
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode)
  ((neotree-mode imenu-list-minor-mode minimap-mode treemacs-mode term-mode) . (lambda() (display-line-numbers-mode 0))))

;; 不要なスペースを可視化する
(use-package whitespace
  :config
  (setq whitespace-style '(face tabs spaces newline trailing space-before-tab space-after-tab space-mark tab-mark newline-mark))
  (setq whitespace-space-regexp "\\(　+\\)")
  (setq whitespace-display-mappings
        '((space-mark   ?\xA0  [?\xA4]  [?_])
          (space-mark   ?\x8A0 [?\x8A4] [?_])
          (space-mark   ?\x920 [?\x924] [?_])
          (space-mark   ?\xE20 [?\xE24] [?_])
          (space-mark   ?\xF20 [?\xF24] [?_])
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
          (space-mark   ?　    [?口]    [?＿])
          ;;(newline-mark ?\n    [?\x21B5 ?\n] [?$ ?\n])
          ))
  (setq whitespace-global-modes '(not dired-mode tar-mode))
  :custom-face
  (whitespace-space ((t (:foreground "pink4"))))
  (whitespace-tab ((t (:foreground "gray40" :strike-through t))))
  ;;(whitespace-newline ((t (:foreground "darkcyan" :height 0.8))))
  :hook
  (after-init . global-whitespace-mode))

;; grepで編集
(use-package wgrep
  :custom
  (wgrep-change-readonly-file t)
  (wgrep-enable-key "e"))

;; minibuffer補完
(use-package vertico
  :straight (vertico :type git
                     :host github
                     :repo "minad/vertico"
                     :files (:defaults "extensions/*"))
  :custom
  (vertico-count 20)
  (vertico-cycle t)
  (vertico-resize t)
  (enable-recursive-minibuffers t)
  :config
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (vertico-mode))

(use-package vertico-mouse
  :straight nil
  :after vertico
  :config
  (vertico-mouse-mode))

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-from-kill-ring)     ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s l" . consult-locate)
         ("M-s G" . consult-grep)
         ("M-s g" . consult-git-grep)
         ("M-s a" . consult-ripgrep)
         ("M-s s" . consult-line)
         ("M-s S" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  )

(use-package affe
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-."))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; フリンジにgitの差分を表示する
(use-package git-gutter
  :custom
  (git-gutter:modified-sign " ")
  (git-gutter:added-sign    " ")
  (git-gutter:deleted-sign  " ")
  :custom-face
  (git-gutter:modified ((t (:background "#f1fa8c"))))
  (git-gutter:added    ((t (:background "#50fa7b"))))
  (git-gutter:deleted  ((t (:background "#ff79c6"))))
  :hook
  (after-init . global-git-gutter-mode))

;; editorconfigを適用する
(use-package editorconfig
  :hook
  (after-init . editorconfig-mode))

;; 復数カーソルを扱う
(use-package phi-search)
(use-package multiple-cursors
  :bind (("C-q C-l" . mc/edit-lines)
         ("C-q C-a" . mc/mark-all-like-this)
         ("C-q C-d" . mc/mark-all-like-this-in-defun)
         ("C-q C-n" . mc/mark-more-like-this-extended)
         ("C-q C-p" . my-mc-put-cursor)
         :map mc/mark-more-like-this-extended-keymap
         ("C-p" . mc/mmlte--up)
         ("C-n" . mc/mmlte--down)
         ("C-b" . mc/mmlte--left)
         ("C-f" . mc/mmlte--right)
         :map mc/keymap
         ("C-s" . phi-search)
         ("C-r" . phi-replace))
  :config
  (defadvice mc--in-defun (around mc--in-defun-ad activate)
    (setq ad-return-value t))
  (defun my-mc-toggle-cursor-at-point ()
    (let ((f t)
          (pos (point)))
      (dolist (o (overlays-at pos))
        (when (mc/fake-cursor-p o)
          (mc/remove-fake-cursor o)
          (setq f nil)))
      (when f
        (mc/create-fake-cursor-at-point)
        )
      )
    )
  (defun my-mc-put-cursor ()
    (interactive)
    (mc/remove-fake-cursors)
    (let ((c)
          (fp (point)))
      (catch 'end-flag
        (while t
          (message "put-cursors: [spc]: put cursor, [C-g]: cancel, [RET]: done")
          (condition-case err
              (setq c (read-key-sequence nil))
            (error
             (throw 'end-flag t)))
          (cond ((equal c (kbd "SPC"))
                 (my-mc-toggle-cursor-at-point))
                ((equal c (kbd "C-SPC"))
                 (my-mc-toggle-cursor-at-point))
                ((equal c (kbd "p"))
                 (previous-line))
                ((equal c (kbd "n"))
                 (next-line))
                ((equal c (kbd "f"))
                 (forward-char))
                ((equal c (kbd "b"))
                 (backward-char))
                ((equal c (kbd "e"))
                 (mwim-end-of-code-or-line))
                ((equal c (kbd "a"))
                 (mwim-beginning-of-code-or-line))
                ((equal c (kbd "C-p"))
                 (previous-line))
                ((equal c (kbd "C-n"))
                 (next-line))
                ((equal c (kbd "C-f"))
                 (forward-char))
                ((equal c (kbd "C-b"))
                 (backward-char))
                ((equal c (kbd "C-e"))
                 (mwim-end-of-code-or-line))
                ((equal c (kbd "C-a"))
                 (mwim-beginning-of-code-or-line))
                ((equal c (kbd "C-m"))
                 (mc/pop-state-from-overlay (mc/last-fake-cursor-before (point-max)))
                 (mc/maybe-multiple-cursors-mode)
                 (throw 'end-flag t))
                ((equal c (kbd "C-g"))
                 (mc/remove-fake-cursors)
                 (goto-char fp)
                 (message "Quit")
                 (throw 'end-flag t)))))))
   )

;; ファイルツリーを表示する
(use-package neotree
  :after
  projectile
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :custom
  (neo-theme 'icons)
  (neo-smart-open t)
  (neo-show-hidden-files t)
  (neo-window-width 30)
  (projectile-switch-project-action 'neotree-projectile-action)
  (neo-display-action '((lambda(buffer _alist)
                          (let ((window-pos (if (eq neo-window-position 'left) 'left 'right)))
                            (display-buffer-in-side-window buffer `((side . ,window-pos)
                                                                    (window-parameters . ((no-other-window . t)
                                                                                          (no-delete-other-windows . t)))))))))
  :preface
  (defun wamei/neotree-show ()
    (interactive)
    (if (eq major-mode 'neotree-mode)
        (neotree-hide)
      (neotree-show)))
  (defun wamei/neotree-mode-hook ()
    (setq cursor-type nil))
  :bind
  ("M-s t" . wamei/neotree-show)
  :hook
  (neotree-mode . wamei/neotree-mode-hook))

;; lsを置き換える
(use-package ls-lisp
  :straight nil
  :custom
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-ignore-case t)
  (ls-lisp-dirs-first t)
  (dired-listing-switches "-alLv")
  :config
  (when (> emacs-major-version 25.1) (setq ls-lisp-UCA-like-collation nil)))

(use-package dired
  :straight nil
  :bind (("C-x C-j" . dired-toggle-current-or-project-directory)
         :map dired-mode-map
         ("C-c C-s" . dired-toggle-sudo)
         ("RET" . dired-find-file)
         ("a" . dired-find-alternate-file)
         ("^" . dired-up-directory)
         ("C-b" . backward-char)
         ("C-f" . forward-char))
  :custom
  ;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
  (dired-dwim-target t)
  ;; ディレクトリを再帰的にコピーする
  (dired-recursive-copies 'always)
  ;; diredバッファでC-sした時にファイル名だけにマッチするように
  (dired-isearch-filenames t)
  ;; auto revert
  (dired-auto-revert-buffer t)
  :config
  ;; dired-find-alternate-file の有効化
  (put 'dired-find-alternate-file 'disabled nil)

  (defun dired-toggle-current-or-project-directory (n)
    (interactive "p")
    (let ((dir (projectile-project-p)))
      (cond ((= n 1)
             (dired-jump))
            ((= n 4)
             (if dir
                 (projectile-dired)
               (dired-jump)))
            ))))
(use-package dired-toggle-sudo
  :after dired
  :bind (:map dired-mode-map
         ("C-c C-s" . dired-toggle-sudo)))
(use-package dired-rainbow
  :after dired
  :config
  (dired-rainbow-define dotfiles "#aaaaaa" "\\..*")
  (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*"))
(use-package async
  :config
  (eval-after-load "dired-aux" '(require 'dired-async))
  )

(use-package popwin
  :config
  (push '(term-mode :position :bottom :height 30 :stick t) popwin:special-display-config)
  (popwin-mode 1))

;; 復数のターミナルを使えるようにする
(use-package wamei-multi-term
  :no-require
  :straight nil
  :after multi-term
  :bind
  ("C-z" . wamei/multi-term-pop)
  :preface
  (defvar wamei/multi-term-buffer-name "TABMINAL")
  (defun wamei/get-multi-term-name (tab index)
    (format "%s<%s-%s>" wamei/multi-term-buffer-name tab index))
  (defun wamei/get-multi-term-buffer-name (tab index)
    (format "*%s*" (wamei/get-multi-term-name tab index)))
  (defun wamei/make-term (term-name shell-name)
    (let ((buffer
           (if multi-term-program-switches
               (make-term term-name shell-name nil multi-term-program-switches)
             (make-term term-name shell-name))))
      (with-current-buffer buffer
        (multi-term-internal))
      buffer))
  (defun wamei/get-multi-term-buffer (tab index)
    (let* ((term-name (wamei/get-multi-term-name tab index))
           (buffer-name (wamei/get-multi-term-buffer-name tab index))
           (buffer (get-buffer buffer-name))
           (shell-name (or multi-term-program
                           (getenv "SHELL")
                           (getenv "ESHELL")
                           "/bin/sh")))
      (if buffer
          (if (eq (buffer-local-value 'major-mode buffer) 'term-mode)
              buffer
            (let ((kill-buffer-query-functions nil))
              (kill-buffer buffer))
            (wamei/make-term term-name shell-name))
        (wamei/make-term term-name shell-name))))
  (defun wamei/s-match-multi-term-buffer-name (buffer-name &optional tab)
    (s-matches?
     (format "\\*%s<%s-[0-9]+>\\*" wamei/multi-term-buffer-name (or tab (+ 1 (tab-bar--current-tab-index))))
     buffer-name))
  (defun wamei/get-multi-term-buffer-list (&optional tab)
    (seq-filter
      '(lambda (buffer)
         (wamei/s-match-multi-term-buffer-name (buffer-name buffer) tab))
      (buffer-list)))
  (defun wamei/get-multi-term-buffer-index (buffer)
    (cadr
     (mapcar
      'string-to-number
      (s-split "-" (s-chop-prefix (format "*%s<" wamei/multi-term-buffer-name) (s-chop-suffix ">*" (buffer-name buffer)))))))
  (defun wamei/get-multi-term-indicies ()
    (mapcar
     'wamei/get-multi-term-buffer-index
     (wamei/get-multi-term-buffer-list)))
  (defun wamei/get-multi-term-max-index ()
    (let ((indicies (wamei/get-multi-term-indicies)))
      (if indicies (apply 'max indicies) 0)))
  (defun wamei/get-multi-term-min-index ()
    (let ((indicies (wamei/get-multi-term-indicies)))
      (if indicies (apply 'min indicies) 0)))
  (defun wamei/get-multi-term-current-index ()
    (let ((indicies (wamei/get-multi-term-indicies)))
      (if indicies (car indicies) 0)))
  (defun wamei/get-multi-term-window ()
    (let ((windows
           (seq-filter
            '(lambda (window)
               (wamei/s-match-multi-term-buffer-name (buffer-name (window-buffer window))))
            (window-list))))
      (car windows)))
  (defun wamei/multi-term-create ()
    (interactive)
    (let* ((buffer (wamei/get-multi-term-buffer (+ 1 (tab-bar--current-tab-index)) (+ 1 (wamei/get-multi-term-max-index)))))
      (wamei/multi-term-pop buffer)))
  (defun wamei/multi-term-next ()
    (interactive)
    (let* ((indicies (wamei/get-multi-term-indicies))
           (max-index (if indicies (apply 'max indicies) 1))
           (min-index (if indicies (apply 'min indicies) 1))
           (current-index (if indicies (car indicies) 1))
           (target-index
            (if (= max-index current-index)
                min-index
              (car (sort (seq-filter #'(lambda (index) (> index current-index)) (wamei/get-multi-term-indicies)) '<))))
           (buffer (wamei/get-multi-term-buffer (+ 1 (tab-bar--current-tab-index)) target-index)))
      (wamei/multi-term-pop buffer)))
  (defun wamei/multi-term-prev ()
    (interactive)
    (let* ((indicies (wamei/get-multi-term-indicies))
           (max-index (if indicies (apply 'max indicies) 1))
           (min-index (if indicies (apply 'min indicies) 1))
           (current-index (if indicies (car indicies) 1))
           (target-index
            (if (= min-index current-index)
                max-index
              (car (sort (seq-filter #'(lambda (index) (< index current-index)) (wamei/get-multi-term-indicies)) '>))))
           (buffer (wamei/get-multi-term-buffer (+ 1 (tab-bar--current-tab-index)) target-index)))
      (wamei/multi-term-pop buffer)))
  (defun wamei/multi-term-pop (&optional _buffer)
    (interactive)
    (let* ((buffer (or _buffer (wamei/get-multi-term-buffer (+ 1 (tab-bar--current-tab-index)) (max 1 (wamei/get-multi-term-current-index)))))
          (window (wamei/get-multi-term-window)))
      (if (not window)
          (progn
            (pop-to-buffer buffer)
            (switch-to-buffer buffer))
        (if (eq window (selected-window))
            (if (eq buffer (current-buffer))
                (delete-window window)
              (switch-to-buffer buffer))
          (select-window window)
          (switch-to-buffer buffer)))))
  )
(use-package multi-term
  :preface
  (defun term-send-forward-char ()
    (interactive)
    (term-send-raw-string "\C-f"))
  (defun term-send-backward-char ()
    (interactive)
    (term-send-raw-string "\C-b"))
  (defun term-send-previous-line ()
    (interactive)
    (term-send-raw-string "\C-p"))
  (defun term-send-next-line ()
    (interactive)
    (term-send-raw-string "\C-n"))
  (defun term-send-tab ()
    (interactive)
    (term-send-raw-string "\C-i"))
  :custom
  (multi-term-dedicated-window-height 50)
  (multi-term-dedicated-max-window-height 100)
  (multi-term-scroll-show-maximum-output t)
  :config
  (setenv "TERMINFO" "~/.terminfo")
  (defadvice term-send-return (after check-cd-action activate)
    (let* ((cmd (term-get-old-input-default))
           (match (string-match-p "$ cd" cmd)))
      (when match
        (let ((dir (substring cmd (+ 4 (string-match "$ cd" cmd)))))
          (if (equal (length dir) 0)
              (setq dir "~")
            (setq dir (substring dir 1)))
          (cd dir)))))
  :hook
  (term-mode . (lambda ()
                 (define-key term-raw-map (kbd "C-q") nil)
                 (define-key term-raw-map (kbd "C-q C-q") 'er/expand-region)
                 (define-key term-raw-map (kbd "C-q C-z") 'er/contract-region)
                 (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
                 (define-key term-raw-map (kbd "C-f") 'term-send-forward-char)
                 (define-key term-raw-map (kbd "C-b") 'term-send-backward-char)
                 (define-key term-raw-map (kbd "C-p") 'term-send-previous-line)
                 (define-key term-raw-map (kbd "C-n") 'term-send-next-line)
                 (define-key term-raw-map (kbd "C-y") 'term-paste)
                 (define-key term-raw-map (kbd "H-v") 'term-paste)
                 (define-key term-raw-map (kbd "s-v") 'term-paste)
                 (define-key term-raw-map (kbd "M-d") 'term-send-forward-kill-word)
                 (define-key term-raw-map (kbd "M-<backspace>") 'term-send-backward-kill-word)
                 (define-key term-raw-map (kbd "M-DEL") 'term-send-backward-kill-word)
                 (define-key term-raw-map (kbd "M-h") 'term-send-backward-kill-word)
                 (define-key term-raw-map (kbd "C-z") 'wamei/multi-term-pop)
                 (define-key term-raw-map (kbd "C-c c") 'wamei/multi-term-create)
                 (define-key term-raw-map (kbd "C-c C-n") 'wamei/multi-term-next)
                 (define-key term-raw-map (kbd "C-c C-p") 'wamei/multi-term-prev)
                 (define-key term-raw-map (kbd "TAB") 'term-send-tab)
                 (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
                 (define-key term-raw-map (kbd "C-c C-k") 'term-char-mode)
                 )))

;; タブの設定
(use-package tab-bar-mode
  :no-require
  :straight nil
  :init
  (defun wamei/tab-bar-new-tab-to:before (&optional tab-number)
    (let ((last-index (length (tab-bar-tabs)))
          (current-index (or tab-number (+ 1 (tab-bar--current-tab-index)))))
      (when (not (= last-index current-index))
        (mapcar
         #'(lambda (index)
             (mapcar #'(lambda (buffer)
                        (with-current-buffer buffer (rename-buffer (s-replace (format "<%s" index) (format "<%s" (+ index 1)) (buffer-name)))))
                     (wamei/get-multi-term-buffer-list index)))
         (reverse (number-sequence (+ 1 current-index) last-index))))))
  (advice-add #'tab-bar-new-tab-to :before #'wamei/tab-bar-new-tab-to:before)
  (defun wamei/tab-bar-close-tab:before (&optional tab-number to-number)
    (let ((last-index (length (tab-bar-tabs)))
          (current-index (or tab-number (+ 1 (tab-bar--current-tab-index)))))
      (mapcar #'(lambda (buffer)
                  (let ((kill-buffer-query-functions nil))
                    (kill-buffer buffer)))
              (wamei/get-multi-term-buffer-list current-index))
      (when (not (= last-index current-index))
        (mapcar
         #'(lambda (index)
             (mapcar #'(lambda (buffer)
                        (with-current-buffer buffer (rename-buffer (s-replace (format "<%s" index) (format "<%s" (- index 1)) (buffer-name)))))
                     (wamei/get-multi-term-buffer-list index)))
         (number-sequence (+ 1 current-index) last-index)))))
  (advice-add #'tab-bar-close-tab :before #'wamei/tab-bar-close-tab:before)
  (defun wamei/tab-bar-move-tab-to:before (to-number &optional from-number)
    (let* ((tabs (funcall tab-bar-tabs-function))
           (from-number (or from-number (1+ (tab-bar--current-tab-index tabs))))
           (from-tab (nth (1- from-number) tabs))
           (to-number (if to-number (prefix-numeric-value to-number) 1))
           (to-number (if (< to-number 0) (+ (length tabs) (1+ to-number))
                        to-number))
           (to-index (max 0 (min (1- to-number) (1- (length tabs))))))
      (mapcar #'(lambda (buffer)
                  (with-current-buffer buffer (rename-buffer (s-replace (format "<%s" to-number) (format "<%s" 0) (buffer-name)))))
              (wamei/get-multi-term-buffer-list to-number))
      (mapcar #'(lambda (buffer)
                  (with-current-buffer buffer (rename-buffer (s-replace (format "<%s" from-number) (format "<%s" to-number) (buffer-name)))))
              (wamei/get-multi-term-buffer-list from-number))
      (mapcar #'(lambda (buffer)
                  (with-current-buffer buffer (rename-buffer (s-replace (format "<%s" 0) (format "<%s" from-number) (buffer-name)))))
              (wamei/get-multi-term-buffer-list 0))))
  (advice-add #'tab-bar-move-tab-to :before #'wamei/tab-bar-move-tab-to:before)
  :bind (("C-q n" . tab-bar-switch-to-next-tab)
         ("C-q p" . tab-bar-switch-to-prev-tab)
         ("C-q c" . tab-bar-new-tab)
         ("C-q k" . tab-bar-close-tab)
         ("C-q ," . tab-bar-rename-tab)
         ("C-q s" . tab-bar-move-tab)
         )
  :custom-face
  (tab-bar-tab-inactive ((t (:foreground "#aaaaaa"))))
  :custom
  (tab-bar-new-tab-choice "*dashboard*")
  (tab-bar-tab-name-function
   #'(lambda ()
      (let ((project-name (with-current-buffer (window-buffer (minibuffer-selected-window)) (projectile-project-name))))
        (if (s-equals? project-name "-")
            (tab-bar-tab-name-current)
          project-name))))
  :hook
  (after-init . tab-bar-mode))

;; magitの設定
(use-package magit
  :after (git-gutter)
  :bind (("C-x g" . magit-status)
         :map magit-status-mode-map
         ("q" . my/magit-quit-session))
  :preface
  (defun my/magit-quit-session ()
    (interactive)
    (kill-buffer)
    (jump-to-register (intern (concat "magit-fullscreen-" (number-to-string (tab-bar--current-tab-index)))))
    (git-gutter:update-all-windows))
  (defun my/magit-status (orig-fun &rest args)
    (window-configuration-to-register (intern (concat "magit-fullscreen-" (number-to-string (tab-bar--current-tab-index)))))
    (apply orig-fun args)
    (delete-other-windows))
  :custom
  (magit-diff-auto-show nil)
  :config
  (advice-add 'magit-status :around 'my/magit-status))

;; カラーコードに色を付ける
(use-package rainbow-mode
  :config
  (use-package ov)
  (defun wamei/rainbow-colorize-match:override (color &optional match)
    (let ((match (or match 0)))
      (ov-clear (match-beginning match) (match-end match) 'ovrainbow t)
      (ov
       (match-beginning match) (match-end match)
       'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                 "white" "black"))
               (:background ,color))
       'ovrainbow t
       'priority 5000)))
  (advice-add 'rainbow-colorize-match :override 'wamei/rainbow-colorize-match:override)
  (defun wamei/rainbow-turn-off:after (&rest r)
    (ov-clear (point-min) (point-max) 'ovrainbow t))
  (advice-add 'rainbow-turn-off :after 'wamei/rainbow-turn-off:after)
  :hook
  (after-change-major-mode . rainbow-mode))

(use-package flycheck
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; OS別の設定
(use-package mac-settings
  :no-require t
  :straight nil
  :if (eq system-type 'darwin)
  :config
  ;; optionをmetaキーに
  (setq option-modifier (quote meta))
  (setq ns-option-modifier (quote meta))
  (setq mac-option-modifier (quote meta))

  ;; Emacsにフォーカスされたとき英数にする
  (add-hook 'focus-in-hook (lambda()
                             (let ((inhibit-message t))
                               (shell-command "osascript -e 'tell application \"System Events\" to key code 102'")))))

(use-package wsl-settings
  :no-require t
  :straight nil
  :if is-wsl
  :config
  ;; (custom-set-variables '(tramp-chunksize 1024))
  ;; (setq-default find-file-visit-truename t)
  (use-package browse-url
    :config
    (setq browse-url-browser-function 'browse-url-generic)
    (setq browse-url-generic-program "wslstart")))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)
    (setq with-editor-emacsclient-executable nil)
    ))

;;
;; 補完設定
;;----------------------------------------------------------------------------------------------------
(use-package corfu
  :straight (corfu :type git
                   :host github
                   :repo "minad/corfu"
                   :branch "async"
                   :files (:defaults "extensions/*"))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-preview-current nil)
  (corfu-quit-no-match 'separator)
  :bind
  ("M-/" . completion-at-point)
  :init
  (global-corfu-mode))

(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :config
  (corfu-popupinfo-mode +1))

(use-package orderless
  :after corfu
  :custom ((completion-styles '(orderless))
           (completion-category-defaults nil)
           (completion-category-overrides '((file (styles partial-completion)))))
  :hook (corfu-mode . (lambda () (setq-local orderless-matching-styles '(orderless-flex)))))

(use-package prescient
  :config
  (prescient-persist-mode +1))

(use-package corfu-prescient
  :after corfu
  :init
  (with-eval-after-load 'orderless
    (setq corfu-prescient-enable-filtering nil
          corfu-prescient-override-sorting t))
  (corfu-prescient-mode +1))

(use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;
;; 言語設定
;;----------------------------------------------------------------------------------------------------
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :bind
  ("C-M-/" . copilot-complete)
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion)
        ("TAB" . copilot-accept-completion))
  :hook
  (prog-mode . copilot-mode))

(use-package prisma-mode
  :straight (prisma-mode :type git :host github :repo "pimeys/emacs-prisma-mode")
  :mode (("\\.prisma\\'" . prisma-mode)))

(setq js-indent-level 2)
(use-package typescript-mode)
(use-package add-node-modules-path
  :hook
  (js-mode . add-node-modules-path)
  (js2-mode . add-node-modules-path)
  (web-mode . add-node-modules-path)
  (typescript-mode . add-node-modules-path))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)))

(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :custom
  (web-mode-script-padding 0)
  (web-mode-style-padding 0)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-auto-indentation nil)
  (web-mode-engines-alist
   '(("php"    . "\\.phtml\\'")
     ("blade"  . "\\.blade\\.")))
  :hook
  (web-mode . (lambda ()
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
                  (lsp))
                (when (string-equal "vue" (file-name-extension buffer-file-name))
                  (lsp)))))

(use-package projectile-rails
  :bind (:map projectile-rails-mode-map
         ("C-c r" . projectile-rails-command-map))
  :hook
  (after-init . projectile-rails-global-mode))
(use-package rbenv
  :custom
  (rbenv-installation-dir "~/.rbenv")
  (rbenv-show-active-ruby-in-modeline nil)
  :hook
  (after-init . global-rbenv-mode))

(use-package powershell
  :mode (("\\.ps[dm]?1\\'" . powershell-mode)))

(use-package php-mode
  :mode (("\\.php\\'" . php-mode)))

;;
;; LSP設定
;;----------------------------------------------------------------------------------------------------
(use-package markdown-mode)
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((ruby-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (json-mode . lsp-deferred)
         (powershell-mode . lsp-deferred)
         (php-mode . lsp-deferred)
         (prisma-mode . lsp-deferred))
  :custom
  (lsp-enable-snippet nil)
  (gc-cons-threshold 12800000)
  (read-process-output-max (* 1024 1024))
  :config
  (use-package lsp-ui
    :custom
    (lsp-ui-doc-header t)
    (lsp-ui-doc-max-width 300)
    (lsp-ui-doc-max-height 80)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit nil)
    (lsp-ui-peek-list-width 150)
    :bind (:map lsp-mode-map
           ("M-s r" . lsp-ui-peek-find-references)
           ("M-s d" . lsp-ui-peek-find-definitions)
           ("M-s i" . lsp-ui-peek-find-implementation)
           ("C-q r" . lsp-rename)
           ("C-q a" . lsp-execute-code-action)
           ("C-q i" . lsp-ui-imenu)
           ("C-q d" . wamei/toggle-lsp-ui-doc)
           ("C-q f" . lsp-format-buffer))
    :preface
    (defun wamei/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
         (lsp-ui-doc-mode 1)))
    :hook
    (lsp-mode . lsp-ui-mode)
    (lsp-mode . lsp-ui-doc-mode)))
(use-package dap-mode)
