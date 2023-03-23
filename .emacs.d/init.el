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

(use-package tramp
  :straight nil
  :config
  (setq shell-file-name "/bin/bash")
  (setq explicit-shell-file-name "/bin/bash")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  )

(use-package expand-region
  :bind (("C-q C-q" . er/expand-region)
         ("C-q C-z" . er/contract-region)))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-line-or-code)
         ("C-e" . mwim-end-of-line-or-code)))

(use-package undo-tree
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :hook
  (after-init . global-undo-tree-mode))

(use-package visual-regexp-steroids
  :bind (("C-r" . vr/replace)
         ("C-q C-r" . vr/query-replace)
         ("C-q C-m" . vr/mc-mark)))

(use-package string-inflection
  :bind (("C-q C-i C-a" . string-inflection-all-cycle)
         ("C-q C-i C-l" . string-inflection-underscore)
         ("C-q C-i C-c" . string-inflection-lower-camelcase)
         ("C-q C-i C-p" . string-inflection-camelcase)
         ("C-q C-i C-u" . string-inflection-upcase)
         ("C-q C-i C-k" . string-inflection-kebab-case)))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :custom
  (all-the-icons-dired-monochrome nil)
  :hook
  (dired-mode . all-the-icons-dired-mode))

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

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-custom-auto-reload t)
  (modus-themes-disable-other-themes t)
  :config
  (modus-themes-load-theme 'modus-vivendi-tinted))

(use-package hide-mode-line
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode lsp-ui-imenu-mode) . hide-mode-line-mode)
  ((neotree-mode imenu-list-minor-mode minimap-mode lsp-ui-imenu-mode treemacs-mode term-mode) . (lambda() (display-line-numbers-mode 0))))

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

;; (use-package eldoc
;;   :config
;;   (defun ad:eldoc-message (f &optional string)
;;     (unless (active-minibuffer-window)
;;       (funcall f string)))
;;   (advice-add 'eldoc-message :around #'ad:eldoc-message))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x p" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("M-s a" . wamei/counsel-grep)
         ("M-s s" . swiper)
         ("M-s l" . counsel-locate))
  :preface
  (defun wamei/counsel-grep()
    (interactive)
    (cond ((not (s-equals? "" (shell-command-to-string "which rg")))
           (counsel-rg))
          ((not (s-equals? "" (shell-command-to-string "which ag")))
           (counsel-ag))
          (t
           (counsel-grep))))
  :custom
  (ivy-wrap t)
  (ivy-virtual-abbreviate 'full)
  (ivy-use-virtual-buffers t)
  (ivy-height 20)
  (ivy-count-format "(%d/%d) ")
  (ivy-more-chars-alist '((t . 1)))
  (ivy-use-selectable-prompt t)
  (counsel-yank-pop-separator "\n   -------\n")
  (enable-recursive-minibuffers t)
  :config
  (setq counsel-rg-base-command (cons "rg" (cons "--hidden" (nthcdr 1 counsel-rg-base-command))))
  (minibuffer-depth-indicate-mode t)
  (push '(counsel-locate . nil) ivy-sort-functions-alist)
  (advice-add
   'counsel--yank-pop-format-function
   :override
   (lambda (cand-pairs)
     (ivy--format-function-generic
      (lambda (str)
        (ivy--add-face
         (concat
          "➡"
          (s-chop-prefix
           "  "
           (mapconcat
            (lambda (s)
              (concat "   " s))
            (split-string
             (counsel--yank-pop-truncate str) "\n" t)
            "\n")))
         'ivy-current-match))
      (lambda (str)
        (mapconcat
         (lambda (s)
           (concat "   " s))
         (split-string
          (counsel--yank-pop-truncate str) "\n" t)
         "\n"))
      cand-pairs
      counsel-yank-pop-separator)))
  (use-package ivy-prescient
    :config
    (prescient-persist-mode 1)
    (ivy-prescient-mode 1)
    (setf (alist-get 'counsel-rg ivy-re-builders-alist) #'ivy--regex-plus))
  :hook
  (after-init . ivy-mode))

(use-package projectile
  :custom
  (projectile-completion-system 'ivy)
  :config
  (defun do-not-use-file-truename-in-projectile-project-root (old-fn &rest args)
    (dflet ((file-truename (d) d))
      (apply old-fn args)))
  (advice-add 'projectile-project-root :around 'do-not-use-file-truename-in-projectile-project-root))

(use-package wgrep
  :custom
  (wgrep-change-readonly-file t)
  (wgrep-enable-key "e"))

(use-package counsel-projectile
  :bind (("M-s g" . wamei/counsel-projectile-grep)
         ("M-s f" . counsel-projectile-find-file)
         ("M-s p" . counsel-projectile-switch-project)))
  :preface
  (defun wamei/counsel-projectile-grep()
    (interactive)
    (cond ((not (s-equals? "" (shell-command-to-string "which rg")))
           (counsel-projectile-rg))
          ((not (s-equals? "" (shell-command-to-string "which ag")))
           (counsel-projectile-ag))
          (t
           (counsel-projectile-grep))))
(use-package all-the-icons-ivy-rich
  :custom
  (inhibit-compacting-font-caches t)
  :preface
  (defun wamei/ivy-format-function (cands)
    (ivy--format-function-generic
     (lambda (str)
       (ivy--add-face (concat "➡" str "\n") 'ivy-current-match))
     (lambda (str)
       (concat "  " str "\n"))
     cands
     ""))
  :config
  (setq all-the-icons-ivy-rich-display-transformers-list
   (append all-the-icons-ivy-rich-display-transformers-list
           '(counsel-locate
             (:columns
              ((all-the-icons-ivy-rich-file-icon)
               (ivy-rich-candidate (:width 0.8))
               (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
              :delimiter "\t"))))
  (all-the-icons-ivy-rich-mode)
  (use-package ivy-rich
    :config
    (defvar ivy-rich--ivy-switch-buffer-cache
      (make-hash-table :test 'equal))

    (define-advice ivy-rich--ivy-switch-buffer-transformer
        (:around (old-fn x) cache)
      (let ((ret (gethash x ivy-rich--ivy-switch-buffer-cache)))
        (unless ret
          (setq ret (funcall old-fn x))
          (puthash x ret ivy-rich--ivy-switch-buffer-cache))
        ret))

    (define-advice +ivy/switch-buffer
        (:before (&rest _) ivy-rich-reset-cache)
      (clrhash ivy-rich--ivy-switch-buffer-cache))
    (ivy-rich-mode)
    (setcdr (assq t ivy-format-functions-alist) #'wamei/ivy-format-function)))

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

(use-package editorconfig
  :hook
  (after-init . editorconfig-mode))

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
      (mc/create-fake-cursor-at-point)
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

(use-package ls-lisp
  :straight nil
  :custom
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-ignore-case t)
  (ls-lisp-dirs-first t)
  (dired-listing-switches "-alv")
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

(use-package multi-term
  :bind (("C-z" . multi-term-pop))
  :preface
  (defun multi-term-pop ()
    (interactive)
    (multi-term-dedicated-toggle)
    (when (multi-term-dedicated-exist-p)
      (multi-term-dedicated-select)))
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
                 (define-key term-raw-map (kbd "C-z") 'multi-term-pop)
                 (define-key term-raw-map (kbd "TAB") 'term-send-tab)
                 (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
                 (define-key term-raw-map (kbd "C-c C-k") 'term-char-mode)
                 )))

(use-package tab-bar-mode
  :no-require
  :straight nil
  :bind (("C-q n" . tab-bar-switch-to-next-tab)
         ("C-q p" . tab-bar-switch-to-prev-tab)
         ("C-<tab>" . tab-bar-switch-to-next-tab)
         ("C-S-<iso-lefttab>" . tab-bar-switch-to-prev-tab)
         ("C-q c" . tab-bar-new-tab)
         ("C-q k" . tab-bar-close-tab)
         ("C-q ," . tab-bar-rename-tab)
         ("C-q s" . tab-bar-move-tab)
         )
  :custom
  (tab-bar-new-tab-choice "*dashboard*")
  :hook
  (after-init . tab-bar-mode))

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

(use-package popwin
  :config
  (push '("\\*screen terminal<.*?>\\*" :regexp t :position bottom :height 0.5 :stick t) popwin:special-display-config)
  (popwin-mode 1))

(use-package flycheck
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

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
;; 言語設定
;;----------------------------------------------------------------------------------------------------
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  )

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
  (ruby-insert-encoding-magic-comment nil)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection '("solargraph" "stdio"))
    :major-modes '(ruby-mode enh-ruby-mode)
    :multi-root t
    :library-folders-fn (lambda (_workspace) lsp-solargraph-library-directories)
    :server-id 'ruby-ls-remote
    :remote? t
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         (lsp-configuration-section "solargraph"))))
    ))
  (use-package lsp-ui
    :custom
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-max-width 300)
    (lsp-ui-doc-max-height 50)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit nil)
    :bind (:map lsp-mode-map
           ("M-s r" . lsp-ui-peek-find-references)
           ("M-s d" . lsp-ui-peek-find-definitions)
           ("M-s i" . lsp-ui-peek-find-implementation)
           ("C-q r" . lsp-rename)
           ("C-q a" . lsp-execute-code-action)
           ("C-q i" . lsp-ui-imenu)
           ("C-q d" . wamei/toggle-lsp-ui-doc))
    :preface
    (defun wamei/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
         (lsp-ui-doc-mode 1)))
    :config
    (defun lsp-ui-peek--peek-display (src1 src2)
      (-let* ((win-width (frame-width))
              (lsp-ui-peek-list-width (/ (frame-width) 2))
              (string (-some--> (-zip-fill "" src1 src2)
                        (--map (lsp-ui-peek--adjust win-width it) it)
                        (-map-indexed 'lsp-ui-peek--make-line it)
                        (-concat it (lsp-ui-peek--make-footer))))
              )
        (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
        (posframe-show lsp-ui-peek--buffer
                       :string (mapconcat 'identity string "")
                       :min-width (frame-width)
                       :poshandler #'posframe-poshandler-frame-center)))

    (defun lsp-ui-peek--peek-destroy ()
      (when (bufferp lsp-ui-peek--buffer)
        (posframe-delete lsp-ui-peek--buffer))
      (setq lsp-ui-peek--buffer nil
            lsp-ui-peek--last-xref nil)
      (set-window-start (get-buffer-window) lsp-ui-peek--win-start))

    (advice-add #'lsp-ui-peek--peek-new :override #'lsp-ui-peek--peek-display)
    (advice-add #'lsp-ui-peek--peek-hide :override #'lsp-ui-peek--peek-destroy)
    :hook
    (lsp-mode . lsp-ui-mode)))
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)
(use-package dap-mode)
