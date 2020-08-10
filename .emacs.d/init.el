;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;
;; package.el
;;----------------------------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(setq use-package-enable-imenu-support t)
(require 'use-package)

;; Avoid to write `package-selected-packages` in init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package s :ensure t)
(defvar is-wsl (let ((name (s-chomp (shell-command-to-string "uname -a"))))
  (and (s-starts-with? "Linux" name)
       (s-matches? "microsoft" name))))

;; load environment value
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package el-x :ensure t)

;;
;; 基本設定
;;----------------------------------------------------------------------------------------------------

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

;; symbolic link のファイル名のまま開く
(setq find-file-visit-truename nil)

;; バックアップファイルを作らない
(setq make-backup-files nil)
;; オートセーブファイルを作らない
(setq auto-save-default nil)

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

;; 透明度を設定する
(set-frame-parameter nil 'alpha 85)

;; 選択範囲に色をつける
(transient-mark-mode t)

;; フレームサイズ
(setq default-frame-alist
  '(
    (width . 240)
    (height . 70)
    ))

;; 行番号を表示する
(global-display-line-numbers-mode)

;; モードラインの更新時間を設定する
(setq display-time-interval 1)

;; フォント関係
(defvar font-size 100)
(defvar font-family "HackGen")
(set-face-attribute 'default nil :family font-family :height font-size)
(set-face-attribute 'variable-pitch nil :family font-family :height font-size)
(set-face-attribute 'fixed-pitch nil :family font-family :height font-size)
(set-face-attribute 'mode-line nil :family font-family :height font-size)
(set-face-attribute 'mode-line-inactive nil :family font-family :height font-size)
(set-face-attribute 'tooltip nil :family font-family :height 90)

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

;;
;; パッケージ設定
;;----------------------------------------------------------------------------------------------------


;; (defhydra hydra-error (global-map "M-g")
;;   "goto-error"
;;   ("n" flycheck-next-error "next")
;;   ("p" flycheck-previous-error "previous")
;;   ("l" flycheck-list-errors "list")
;;   ("q" nil "quit")
;;   )
;; (defhydra hydra-git-gutter (global-map "M-g")
;;   "git-gutter"
;;   ("j" git-gutter:next-hunk "next hunk")
;;   ("C-n" git-gutter:next-hunk)
;;   ("k" git-gutter:previous-hunk "previous hunk")
;;   ("C-p" git-gutter:previous-hunk)
;;   ("d" git-gutter:popup-hunk "show diff")
;;   ("r" git-gutter:revert-hunk "revert hunk")
;;   ("q" nil "quit")
;;   )

;; (defhydra hydra-resize (global-map "C-x")
;;   "resize"
;;   ("{" shrink-window-horizontally "shrink-horizontally")
;;   ("}" enlarge-window-horizontally "enlarge-horizontally")
;;   ("^" enlarge-window "enlarge")
;;   ("q" nil "quit")
;;   )

;; (defhydra hydra-rotate (global-map "C-c l")
;;   "rotate"
;;   ("l" rotate-layout "layout")
;;   ("w" rotate-window "window")
;;   ("h" rotate:even-horizontal "horizontal")
;;   ("v" rotate:even-vertical "vertical")
;;   ("q" nil "quit")
;;   )

(use-package tramp
  :config
  (setq shell-file-name "/bin/sh")
  (setq explicit-shell-file-name "/bin/sh")
  ;; (setq tramp-copy-size-limit nil)
  ;; (setq tramp-shell-prompt-pattern "^.*[#$%>] *")
  ;;(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;;(add-to-list 'tramp-remote-process-environment "HISTFILE=/dev/null")
  ;;(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  ;; (unless (eq system-type 'cygwin)
  ;;   (setenv "TMPDIR" "/tmp"))
  ;; (customize-set-variable
  ;;  'tramp-password-prompt-regexp
  ;;   (concat
  ;;    "^.*"
  ;;    (regexp-opt
  ;;     '("passphrase" "Passphrase"
  ;;       "password" "Password"
  ;;       "Your OTP")
  ;;     t)
  ;;    ".*:\0? *"))
  )

(use-package expand-region
  :ensure t
  :bind (("C-q C-q" . er/expand-region)
         ("C-q C-z" . er/contract-region)))

(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-line-or-code)
         ("C-e" . mwim-end-of-line-or-code)))

(use-package undo-tree
  :ensure t
  :bind (("C--" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         ("M--" . undo-tree-redo))
  :hook
  (after-init . global-undo-tree-mode))

(use-package visual-regexp-steroids
  :ensure t
  :bind (("C-r" . vr/replace)
         ("C-q C-r" . vr/query-replace)
         ("C-q C-m" . vr/mc-mark)))

(use-package string-inflection
  :ensure t
  :bind (("C-q C-i C-a" . string-inflection-all-cycle)
         ("C-q C-i C-l" . string-inflection-underscore)
         ("C-q C-i C-c" . string-inflection-lower-camelcase)
         ("C-q C-i C-p" . string-inflection-camelcase)
         ("C-q C-i C-u" . string-inflection-upcase)
         ("C-q C-i C-k" . string-inflection-kebab-case)))

(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-items '((recents  . 20)
                     (bookmarks . 10)
                     (projects . 10)
                     (agenda . 5)
                     (registers . 5)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  :config
  (set-face-attribute 'dashboard-heading nil :foreground "orange")
  (dashboard-setup-startup-hook))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-molokai t)
  (doom-themes-visual-bell-config)
  ;;(doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (doom-modeline-def-modeline 'main
                              '(bar matches buffer-info remote-host buffer-position parrot selection-info)
                              '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))

(use-package hide-mode-line
  :ensure t
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
          (newline-mark ?\n    [?\x21B5 ?\n] [?$ ?\n])
          ))
  (setq whitespace-global-modes '(not dired-mode tar-mode))
  :custom-face
  (whitespace-space ((t (:foreground "pink4"))))
  (whitespace-tab ((t (:foreground "gray40" :strike-through t))))
  (whitespace-newline ((t (:foreground "darkcyan" :height 0.8))))
  :hook
  (after-init . global-whitespace-mode))

(use-package eldoc
  :ensure t
  :config
  (defun ad:eldoc-message (f &optional string)
    (unless (active-minibuffer-window)
      (funcall f string)))
  (advice-add 'eldoc-message :around #'ad:eldoc-message))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x p" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("M-s a" . counsel-ag)
         ("M-s s" . swiper)
         ("M-s r" . counsel-recentf)
         ("M-s l" . counsel-locate))
  :custom
  (ivy-wrap t)
  (ivy-virtual-abbreviate 'full)
  (ivy-use-virtual-buffers t)
  (ivy-height 20)
  (ivy-count-format "(%d/%d) ")
  (ivy-more-chars-alist '((t . 1)))
  (counsel-yank-pop-separator "\n-------\n")
  (counsel-ag-base-command "ag -u --vimgrep %s")
  :config
  (use-package ivy-prescient
    :ensure t
    :config
    (ivy-prescient-mode 1))
  :hook
  (after-init . ivy-mode))
(use-package counsel-projectile
  :ensure t
  :bind (("M-s g" . counsel-projectile-ag)
         ("M-s f" . counsel-projectile-find-file)
         ("M-s p" . counsel-projectile-switch-project)))
(use-package all-the-icons-ivy-rich
  :ensure t
  :custom
  (inhibit-compacting-font-caches t)
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
    :ensure t
    :config
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    (ivy-rich-mode)))

(use-package projectile
  :ensure t
  :config
  (defun do-not-use-file-truename-in-projectile-project-root (old-fn &rest args)
    (dflet ((file-truename (d) d))
      (apply old-fn args)))
  (advice-add 'projectile-project-root :around 'do-not-use-file-truename-in-projectile-project-root))

(use-package git-gutter
  :ensure t
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
  :ensure t
  :hook
  (after-init . editorconfig-mode))

(use-package company-box
  :ensure t
  :hook
  (company-mode . company-box-mode))
(use-package company
  :ensure t
  :bind (:map company-mode-map
         ("M-/" . company-complete)
         ;;("<tab>" . company-indent-or-complete-common)
         :map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-h" . nil)
         ("<tab>" . company-complete-common-or-cycle)
         ("M-d" . company-show-doc-buffer))
  :custom
  ;;(company-transformers (company-sort-by-statistics company-sort-by-backend-importance))
  (company-minimum-prefix-length 1)
  (company-tooltip-minimum-width 40)
  (company-tooltip-align-annotations t)
  (company-dabbrev-downcase nil)
  (company-idle-delay 0)
  (company-selection-wrap-around t)
  :config
  (use-package company-prescient
    :ensure t
    :config
    (company-prescient-mode 1))
  :hook
  (after-init . global-company-mode))

(use-package phi-search
  :ensure t)

(use-package multiple-cursors
  :ensure t
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
  :ensure t
  :after
  projectile
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-smart-open t)
  (neo-autorefresh t)
  (neo-show-hidden-files t)
  (neo-window-width 30)
  (neo-hide-cursor t)
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
  :bind
  ("<f9>" . wamei/neotree-show))

(use-package ls-lisp
  :custom
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-ignore-case t)
  (ls-lisp-dirs-first t)
  (dired-listing-switches "-alG")
  :config
  (when (> emacs-major-version 25.1) (setq ls-lisp-UCA-like-collation nil)))

(use-package dired
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
  :ensure t
  :after dired
  :bind (:map dired-mode-map
         ("C-c C-s" . dired-toggle-sudo)))
(use-package dired-rainbow
  :ensure t
  :after dired
  :config
  (dired-rainbow-define dotfiles "#aaaaaa" "\\..*")
  (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*"))
(use-package async
  :ensure t
  :config
  (eval-after-load "dired-aux" '(require 'dired-async))
  )

(use-package multi-term
  :ensure t
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
                 (define-key term-raw-map (kbd "C-z") 'emt-pop-multi-term)
                 (define-key term-raw-map (kbd "TAB") 'term-send-tab)
                 (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
                 (define-key term-raw-map (kbd "C-c C-k") 'term-char-mode)
                 )))

(use-package elscreen
  :ensure t
  :bind (("C-q n" . elscreen-next)
         ("C-q p" . elscreen-previous)
         ("C-<tab>" . elscreen-next)
         ("C-S-<iso-lefttab>" . elscreen-previous)
         ("C-q c" . elscreen-create)
         ("C-q k" . elscreen-kill)
         ("C-q ," . elscreen-screen-nickname)
         ("C-q s" . elscreen-swap)
         ("C-q 0" . (lambda() (interactive) (elscreen-goto 0)))
         ("C-q 1" . (lambda() (interactive) (elscreen-goto 1)))
         ("C-q 2" . (lambda() (interactive) (elscreen-goto 2)))
         ("C-q 3" . (lambda() (interactive) (elscreen-goto 3)))
         ("C-q 4" . (lambda() (interactive) (elscreen-goto 4)))
         ("C-q 5" . (lambda() (interactive) (elscreen-goto 5)))
         ("C-q 6" . (lambda() (interactive) (elscreen-goto 6)))
         ("C-q 7" . (lambda() (interactive) (elscreen-goto 7)))
         ("C-q 8" . (lambda() (interactive) (elscreen-goto 8)))
         ("C-q 9" . (lambda() (interactive) (elscreen-goto 9))))
  :custom
  (elscreen-prefix-key (kbd "C-q C-w"))
  (elscreen-default-buffer-name "*dashboard*")
  :config
  (elscreen-start))
(use-package elscreen-tab
  :ensure t
  :after elscreen
  :config
  (defun elscreen-tab--update-buffer:after ()
    (with-current-buffer (elscreen-tab--dedicated-tab-buffer-name)
      (setq cursor-type nil)))
  (advice-add 'elscreen-tab--update-buffer :after 'elscreen-tab--update-buffer:after)
  (defun elscreen-tab--elscreen-goto:around (func &rest r)
    (when (s-equals? (buffer-name (current-buffer)) elscreen-tab--dedicated-tab-buffer-name)
      (other-window 1))
    (apply func r))
  (advice-add 'elscreen-goto :around 'elscreen-tab--elscreen-goto:around)
  (defun elscreen-tab--create-tab-unit:override (screen-id)
    (let* ((nickname-or-buf-names (assoc-default screen-id (elscreen-get-screen-to-name-alist)))
           (nickname-or-1st-buffer
            (elscreen-tab--avoid-undesirable-name (split-string nickname-or-buf-names ":")))
           (tab-name
            (elscreen-truncate-screen-name nickname-or-1st-buffer (elscreen-tab-width) t))
           (tab-id (number-to-string screen-id))
           tab-title
           tab-unit)
      (setq tab-title (format "[%s] %s" tab-id tab-name))
      (setq tab-title (if (eq (elscreen-get-current-screen) screen-id)
                          (propertize tab-title 'face 'elscreen-tab-current-screen-face)
                        (propertize tab-title 'face 'elscreen-tab-other-screen-face)))
      (setq tab-unit (elscreen-tab--propertize-click-to-jump tab-title screen-id))
      tab-unit))
  (advice-add 'elscreen-tab--create-tab-unit :override 'elscreen-tab--create-tab-unit:override)
  (setq elscreen-tab--tab-unit-separator
    #s(hash-table size 4
                  test eq
                  data (right "\n" top " " left "\n" bottom " ")))
  (elscreen-tab-set-position 'top)
  (elscreen-tab-mode))
(use-package elscreen-multi-term
  :ensure t
  :after elscreen
  :bind (("C-z" . emt-pop-multi-term)))
(use-package elscreen-separate-buffer-list
  :ensure t
  :after elscreen
  :config
  (elscreen-separate-buffer-list-mode))

(use-package magit
  :ensure t
  :after (git-gutter elscreen)
  :bind (("C-x g" . magit-status)
         :map magit-status-mode-map
         ("q" . my/magit-quit-session))
  :preface
  (defun my/magit-quit-session ()
    (interactive)
    (kill-buffer)
    (jump-to-register (intern (concat "magit-fullscreen-" (number-to-string (elscreen-get-current-screen)))))
    (git-gutter:update-all-windows))
  :custom
  ;; (magit-last-seen-setup-instructions "1.4.0")
  ;; (magit-diff-refine-hunk 'all)
  (magit-diff-auto-show nil)
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register (intern (concat "magit-fullscreen-" (number-to-string (elscreen-get-current-screen)))))
    ad-do-it
    (delete-other-windows))
  :hook
  (magit-mode . (lambda () (company-mode -1))))

(use-package rainbow-mode
  :ensure t
  :hook
  (after-change-major-mode . rainbow-mode))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
(use-package dockerfile-mode
  :ensure t
  :mode (("Dockerfile\\'" . dockerfile-mode)))
(use-package docker-compose-mode
  :ensure t)
(use-package docker-tramp
  :ensure t
  :config
  (require 'docker-tramp-compat)
  (set-variable 'docker-tramp-use-names t))

(use-package popwin
  :ensure t
  :config
  (push '("\\*screen terminal<.*?>\\*" :regexp t :position bottom :height 0.5 :stick t) popwin:special-display-config)
  (popwin-mode 1))

;;
;; mac用設定
;;----------------------------------------------------------------------------------------------------
(when (eq system-type 'darwin)
  ;; optionをmetaキーに
  (setq option-modifier (quote meta))
  (setq ns-option-modifier (quote meta))
  (setq mac-option-modifier (quote meta))
  )

;;
;; WSL用設定
;;----------------------------------------------------------------------------------------------------
(when is-wsl
  (use-package browse-url
    :config
    (setq browse-url-browser-function 'browse-url-generic)
    (setq browse-url-generic-program "wslstart"))

  (use-package mozc
    :if (not (equal window-system nil))
    :ensure t
    :bind (("M-`" . toggle-input-method))
    :custom
    (mozc-leim-title "かな")
    (default-input-method "japanese-mozc"))
  (use-package mozc-cand-posframe
    :if (not (equal window-system nil))
    :ensure t
    :after mozc
    :custom
    ;; (custom-set-variables '(tramp-chunksize 1024))
    ;; (setq-default find-file-visit-truename t)
    (mozc-candidate-style 'posframe)))

;;
;; サーバー起動
;;----------------------------------------------------------------------------------------------------
(use-package server
  :config
  (unless (server-running-p)
    (server-start)
    (require 'elscreen-server)
    (setq with-editor-emacsclient-executable nil)
    ))

;;
;; 言語設定
;;----------------------------------------------------------------------------------------------------
(use-package flycheck-posframe
  :ensure t
  :if (not (equal window-system nil))
  :after flycheck
  :custom-face
  (flycheck-posframe-border-face ((t (:foreground "#444444"))))
  :custom
  (flycheck-posframe-border-width 1)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (ruby-mode . lsp-deferred)
  :config
  (use-package lsp-ui
    :ensure t
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 150)
    (lsp-ui-doc-max-height 50)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit t)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable t)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    ;;(lsp-ui-sideline-show-diagnostics nil)
    ;;(lsp-ui-sideline-show-code-actions nil)
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
    :preface
    (defun ladicle/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
         (lsp-ui-doc-mode 1)))
    :bind (("<f10>"   . lsp-ui-imenu)
           :map lsp-mode-map
           ("C-c C-r" . lsp-ui-peek-find-references)
           ("C-c C-j" . lsp-ui-peek-find-definitions)
           ("C-c i"   . lsp-ui-peek-find-implementation)
           ("C-c m"   . lsp-ui-imenu)
           ("C-c s"   . lsp-ui-sideline-mode)
           ("C-c d"   . ladicle/toggle-lsp-ui-doc))
    :hook
    (lsp-mode . lsp-ui-mode)))
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))
(use-package lsp-docker
  :ensure t)

(use-package rbenv
  :ensure t
  :custom
  (rbenv-installation-dir "~/.rbenv")
  (rbenv-show-active-ruby-in-modeline nil)
  :hook
  (after-init . global-rbenv-mode))
