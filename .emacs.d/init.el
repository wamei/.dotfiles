;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;----------------------------------------------------------------------------------------------------
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp")

(when (< emacs-major-version 24.3) (require 'cl-lib))

(add-to-load-path "elpa")

;; load environment value
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;; package.el
(when (require 'package nil t)
  ;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  ;; (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  )

(require 'misc)
(require 'expand-region)
(require 'visual-regexp-steroids)
(require 'hydra)

;;
;; キーバインド
;;----------------------------------------------------------------------------------------------------
(global-set-key (kbd "C-a")     'mwim-beginning-of-line-or-code)
(global-set-key (kbd "C-e")     'mwim-end-of-line-or-code)
(global-set-key (kbd "C-h")     nil)
(global-set-key (kbd "C-r")     'vr/replace)
(global-set-key (kbd "C--")     'undo-tree-undo)
(define-key key-translation-map [?\C-h] [?\C-?])

(global-set-key (kbd "M-b")     'backward-word)
(global-set-key (kbd "M-f")     'forward-to-word)
(global-set-key (kbd "M-h")     'backward-kill-word)
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "M--")     'undo-tree-redo)

(global-set-key (kbd "M-s s")   'helm-swoop)
(global-set-key (kbd "M-s g")   'helm-git-grep)
(global-set-key (kbd "M-s a")   'ag)
(global-set-key (kbd "M-s o")   'projectile-multi-occur)
(global-set-key (kbd "M-s f")   'helm-projectile-find-file)
(global-set-key (kbd "M-s p")   'helm-projectile-switch-project)
(global-set-key (kbd "M-s b")   'helm-bookmarks)

(defhydra hydra-error (global-map "M-g")
  "goto-error"
  ("n" flycheck-next-error "next")
  ("p" flycheck-previous-error "previous")
  ("l" flycheck-list-errors "list")
  ("q" nil "quit")
  )
(defhydra hydra-git-gutter (global-map "M-g")
  "git-gutter"
  ("j" git-gutter:next-hunk "next hunk")
  ("C-n" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk "previous hunk")
  ("C-p" git-gutter:previous-hunk)
  ("d" git-gutter:popup-hunk "show diff")
  ("r" git-gutter:revert-hunk "revert hunk")
  ("q" nil "quit")
  )

(global-set-key (kbd "C-c C-c") 'quickrun)
(global-set-key (kbd "C-c C-u") 'pop-tag-mark)

(global-set-key (kbd "C-q") nil)
(global-set-key (kbd "C-q C-q") 'er/expand-region)
(global-set-key (kbd "C-q C-l") 'mc/edit-lines)
(global-set-key (kbd "C-q C-r") 'vr/mc-mark)
(global-set-key (kbd "C-q C-a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-q C-d") 'mc/mark-all-like-this-in-defun)
(global-set-key (kbd "C-q C-n") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-q C-p") 'my-mc-put-cursor)

(global-set-key (kbd "C-x g")   'magit-status)
(global-set-key (kbd "C-x p")   'helm-resume)
(global-set-key (kbd "C-x t")   'emt-pop-multi-term)

(defhydra hydra-rotate (global-map "C-c l")
  "rotate"
  ("l" rotate-layout "layout")
  ("w" rotate-window "window")
  ("h" rotate:even-horizontal "horizontal")
  ("v" rotate:even-vertical "vertical")
  ("q" nil "quit")
  )

(global-set-key (kbd "C-x n n") 'linum-mode)
(global-set-key (kbd "C-x n r") 'narrow-to-region)

(global-set-key (kbd "C-x C-b")   'helm-filelist++)

(global-set-key (kbd "C-M-r")   'vr/query-replace)
(global-set-key (kbd "C-M-s")   'vr/isearch-forward)
(global-set-key (kbd "C-M-h")   'c-hungry-backspace)

(global-set-key [wheel-up]   '(lambda () (interactive) (scroll-down 2)))
(global-set-key [wheel-down] '(lambda () (interactive) (scroll-up   2)))
(global-set-key [mouse-4]    '(lambda () (interactive) (scroll-down 2)))
(global-set-key [mouse-5]    '(lambda () (interactive) (scroll-up   2)))
(global-set-key [mouse-8]    '(lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-9]    '(lambda () (interactive) (scroll-up   1)))
(global-set-key [mouse-20]   '(lambda () (interactive) (scroll-down (/ (window-height) 2))))
(global-set-key [mouse-21]   '(lambda () (interactive) (scroll-up   (/ (window-height) 2))))

(define-key isearch-mode-map (kbd "M-s") 'helm-swoop-from-isearch)

(define-minor-mode overriding-key-map-mode
  "キーマップ上書き用マイナーモード"
  t
  ""
  `(
    (,(kbd "C-z") . emt-pop-multi-term)
    (,(kbd "C-x C-j") . dired-toggle-current-or-project-directory)
    (,(kbd "C-q n")   . elscreen-next)
    (,(kbd "C-q p")   . elscreen-previous)
    (,(kbd "C-<tab>") . elscreen-next)
    (,(kbd "C-q c")   . elscreen-create)
    (,(kbd "C-q k")   . elscreen-kill)
    (,(kbd "C-q ,")   . elscreen-screen-nickname)
    (,(kbd "C-q s")   . elscreen-swap)
    )
  )

;;
;; モード設定
;;----------------------------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.\\(tex\\|ltx\\|cls\\|sty\||clo\\|bbl\\)\\'" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.\\(html\\|xhtml\\|shtml\\|tpl\\|hbs\\)\\'" . web-mode))

;; org table mode
(require 'org)
(require 'org-table)
(add-hook 'lisp-interaction-mode-hook 'turn-on-orgtbl)
(add-hook 'text-mode-hook 'turn-on-orgtbl)
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (if (equal major-mode 'fundamental-mode)
                (orgtbl-mode))))

;;
;; windows用設定
;;----------------------------------------------------------------------------------------------------
(when (eq system-type 'cygwin)
  ;; altをmetaキーに
  (setq w32-alt-is-meta t)
  ;; 漢字/変換キー入力時のエラーメッセージ抑止
  (global-set-key (kbd "<A-kanji>") 'ignore)
  (global-set-key (kbd "<M-kanji>") 'ignore)
  (global-set-key (kbd "<kanji>") 'ignore)

  (require 'shell)
  (setq explicit-shell-file-name "bash.exe")
  (setq shell-command-switch "-c")

  ;; (M-! and M-| and compile.el)
  (setq shell-file-name "bash.exe")
  (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)

  (require 'server)
  (defun server-ensure-safe-dir (dir) "Noop" t)
  (setq server-socket-dir "~/.emacs.d")
  )

;;
;; mac用設定
;;----------------------------------------------------------------------------------------------------
(when (eq system-type 'darwin)
  ;; optionをmetaキーに
  (setq option-modifier (quote meta))
  (setq ns-option-modifier (quote meta))
  (setq mac-option-modifier (quote meta))

  (require 'shell)
  (setq explicit-shell-file-name "bash")
  (setq shell-file-name "bash")
  )

;;
;; ウィンドウ設定
;;----------------------------------------------------------------------------------------------------

;; 透過度調整関数
(defvar frame-alpha 85)
(defun set-frame-alpha(n)
  (interactive "p")
  (when (>= n 100) (setq n 100))
  (when (<= n 0) (setq n 0))
  (setq frame-alpha n)
  (set-frame-parameter nil 'alpha frame-alpha))

(if window-system
    (progn
      (set-frame-alpha frame-alpha)
      (setq ns-pop-up-frames nil)
      (scroll-bar-mode 0)
      ))
(when tool-bar-mode (tool-bar-mode 0))
(when menu-bar-mode (menu-bar-mode 0))
(setq frame-title-format
      '("emacs " emacs-version))

;;
;; テーマの読み込み
;;----------------------------------------------------------------------------------------------------
;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)
(make-face 'mode-line-delim-face-1)
(make-face 'mode-line-git-face)
(make-face 'mode-line-name-face)
(make-face 'mode-line-projectile-face)
(make-face 'mode-line-time-face)
(let ((class '((class color) (min-colors 89))))
(custom-set-faces
   ;; Ensure sufficient contrast on 256-color xterms.
   `(default ((((class color) (min-colors 4096))
       (:background "#1e1e1e" :foreground "#e1e1e0"))
       (,class
       (:background "#1e1e1e" :foreground "#e1e1e0"))))
   `(cursor ((,class (:background "#a4a4a4"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#2e3748"))))
   `(highlight ((,class (:background "#035f56"))))
   `(region ((,class (:background "#98514B" :foreground "#878787"))))
   `(isearch ((,class (:background "#fcffad" :foreground "#000000"))))
   `(lazy-highlight ((,class (:background "#ff4242"))))
   `(trailing-whitespace ((,class (:background "#ff4242"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "#0B2087" :foreground "#eeeeec"))))
   `(mode-line-inactive ((,class (:background "#878787" :foreground "#eeeeec"))))
   `(header-line ((,class (:background "#2e2e2e" :foreground "#eeeeec"))))
   `(mode-line-name ((,class (:foreground "#ed74cd"))))
   `(mode-line-filename-face ((,class (:inherit mode-line-face :foreground "#eab700" :weight bold))))
   `(mode-line-read-only-face ((,class (:inherit mode-line-face :foreground "#4271ae"))))
   `(mode-line-modified-face ((,class (:inherit mode-line-face :foreground "#c82829" :background "#ffffff"))))
   `(mode-line-folder-face ((,class (:inherit mode-line-face :weight extra-light :height 0.8 :foreground "#e5e5e5"))))
   `(mode-line-position-face ((,class (:inherit mode-line-face))))
   `(mode-line-mode-face ((,class (:inherit mode-line-face :foreground "#eeeeec"))))
   `(mode-line-minor-mode-face ((,class (:inherit mode-line-face :weight extra-light :height 0.8 :foreground "#cccccc"))))
   `(mode-line-process-face ((,class (:inherit mode-line-face :foreground "green"))))
   `(mode-line-80col-face ((,class (:inherit mode-line-position-face :foreground "black" :background "#eab700"))))
   `(mode-line-delim-face-1 ((,class (:inherit mode-line-face :foreground "white"))))
   `(mode-line-git-face ((,class (:inherit mode-line-face :foreground "green"))))
   `(mode-line-name-face ((,class (:inherit mode-line-face :foreground "#ed74cd"))))
   `(mode-line-projectile-face ((,class (:inherit mode-line-face :foreground "green"))))
   `(mode-line-time-face ((,class (:inherit mode-line-face :foreground "#ed74cd"))))
   `(linum ((,class (:height 0.8 :background "#2e2e2e"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground "#729fcf" :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#23d7d7"))))
   `(font-lock-comment-face ((,class (:foreground "#74af68"))))
   `(font-lock-constant-face ((,class (:foreground "#729fcf"))))
   `(font-lock-function-name-face ((,class (:foreground "#00ede1" :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground "#ffad29" :weight bold))))
   `(font-lock-string-face ((,class (:foreground "#e67128"))))
   `(font-lock-type-face ((,class (:foreground "#34cae2"))))
   `(font-lock-variable-name-face ((,class (:foreground "#dbdb95"))))
   `(font-lock-warning-face ((,class (:foreground "#ff4242" :weight bold))))
   ;; Buttons and links
   `(button ((,class (:underline t))))
   `(link ((,class (:foreground "#59e9ff" :underline t))))
   `(link-visited ((,class (:foreground "#ed74cd" :underline t))))
   ;; Helm faces
   `(helm-selection ((,class (:background "#035f56"))))
   `(helm-ff-symlink ((,class (:foreground "#ed74cd"))))
   `(helm-ff-file ((,class (:foreground "#eeeeee"))))
   `(helm-ff-directory ((,class (:foreground "#23d7d7"))))
   `(helm-buffer-directory ((,class (:foreground "#eeeeec" :background "#4271ae"))))
   ;; Elscreen faces
   `(elscreen-tab-background-face ((,class (:background "#2e2e2e"))))
   `(elscreen-tab-current-screen-face ((,class (:foreground "#eab700" :background "#1e1e1e"))))
   `(elscreen-tab-other-screen-face ((,class (:foreground "#888888" :background "#2e2e2e"))))
   ;; Diff
   `(diff-file-header ((,class (:foreground "#eeeeee" :background "#222222"))))
   `(diff-header ((,class (:foreground "#eeeeee" :background "#555555"))))
   `(diff-added ((,class (:foreground "#33aa33" :background nil))))
   `(diff-refine-added ((,class (:foreground "#00f900" :background nil))))
   `(diff-removed ((,class (:foreground "#aa2600" :background nil))))
   `(diff-refine-removed ((,class (:foreground "#ff2600" :background nil))))
   `(diff-context ((,class (:foreground "#eeeeee" :background nil))))
   `(magit-item-highlight ((,class (:background "#000000"))))
   `(magit-branch ((,class (:foreground "#34cae2" :background nil))))
   `(magit-log-sha1 ((,class (:foreground "#ff4242" :background nil))))
   `(magit-hash ((,class (:foreground "#dbdb95" :background nil))))
   ;; Git-gutter
   `(git-gutter:modified ((,class (:foreground "#ed74cd" :background "#2e2e2e"))))
   `(git-gutter:added ((,class (:foreground "#00f900" :background "#2e2e2e"))))
   `(git-gutter:deleted ((,class (:foreground "#aa2600" :background "#2e2e2e"))))
   `(git-gutter-fr:modified ((,class (:foreground "#ed74cd" :background "#2e2e2e"))))
   `(git-gutter-fr:added ((,class (:foreground "#00f900" :background "#2e2e2e"))))
   `(git-gutter-fr:deleted ((,class (:foreground "#aa2600" :background "#2e2e2e"))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:foreground "#ff4242" :weight bold))))
   `(gnus-group-news-1-low ((,class (:foreground "#ff4242"))))
   `(gnus-group-news-2 ((,class (:foreground "#00ede1" :weight bold))))
   `(gnus-group-news-2-low ((,class (:foreground "#00ede1"))))
   `(gnus-group-news-3 ((,class (:foreground "#23d7d7" :weight bold))))
   `(gnus-group-news-3-low ((,class (:foreground "#23d7d7"))))
   `(gnus-group-news-4 ((,class (:foreground "#74af68" :weight bold))))
   `(gnus-group-news-4-low ((,class (:foreground "#74af68"))))
   `(gnus-group-news-5 ((,class (:foreground "#dbdb95" :weight bold))))
   `(gnus-group-news-5-low ((,class (:foreground "#dbdb95"))))
   `(gnus-group-news-low ((,class (:foreground "#008b8b"))))
   `(gnus-group-mail-1 ((,class (:foreground "#ff4242" :weight bold))))
   `(gnus-group-mail-1-low ((,class (:foreground "#ff4242"))))
   `(gnus-group-mail-2 ((,class (:foreground "#00ede1" :weight bold))))
   `(gnus-group-mail-2-low ((,class (:foreground "#00ede1"))))
   `(gnus-group-mail-3 ((,class (:foreground "#23d7d7"  :weight bold))))
   `(gnus-group-mail-3-low ((,class (:foreground "#23d7d7"))))
   `(gnus-group-mail-low ((,class (:foreground "#008b8b"))))
   `(gnus-header-content ((,class (:weight normal :foreground "#ffad29"))))
   `(gnus-header-from ((,class (:foreground "#e67128" :weight bold))))
   `(gnus-header-subject ((,class (:foreground "#dbdb95"))))
   `(gnus-header-name ((,class (:foreground "#00ede1"))))
   `(gnus-header-newsgroups ((,class (:foreground "#e67128"))))
   ;; Message faces
   `(message-header-name ((,class (:foreground "#ffad29" :weight bold))))
   `(message-header-cc ((,class (:foreground "#e67128"))))
   `(message-header-other ((,class (:foreground "#e67128"))))
   `(message-header-subject ((,class (:foreground "#dbdb95"))))
   `(message-header-to ((,class (:foreground "#00ede1"))))
   `(message-cited-text ((,class (:foreground "#74af68"))))
   `(message-separator ((,class (:foreground "#23d7d7"))))
   ;; web-mode
   `(web-mode-doctype-face ((,class (:foreground "#82AE46"))))
   `(web-mode-html-tag-face ((,class (:foreground "#E6B422" :weight bold))))
   `(web-mode-html-attr-name-face ((,class (:foreground "#C97586"))))
   `(web-mode-html-attr-value-face ((,class (:foreground "#82AE46"))))
   `(web-mode-comment-face ((,class (:foreground "#D9333F"))))
   `(web-mode-server-comment-face ((,class (:foreground "#D9333F"))))
   `(web-mode-css-rule-face ((,class (:foreground "#A0D8EF"))))
   `(web-mode-css-pseudo-class-face ((,class (:foreground "#FF7F00"))))
   `(web-mode-css-at-rule-face ((,class (:foreground "#FF7F00"))))
   ;; term color
   `(term-color-black   ((,class (:foreground "#000000" :background "#000000"))))
   `(term-color-red     ((,class (:foreground "#ff2600" :background "#ff2600"))))
   `(term-color-green   ((,class (:foreground "#00f900" :background "#00f900"))))
   `(term-color-yellow  ((,class (:foreground "#fefb00" :background "#fefb00"))))
   `(term-color-blue    ((,class (:foreground "#598eff" :background "#598eff"))))
   `(term-color-magenta ((,class (:foreground "#ff40ff" :background "#ff40ff"))))
   `(term-color-cyan    ((,class (:foreground "#00fcff" :background "#00fcff"))))
   `(term-color-white   ((,class (:foreground "#eeeeee" :background "#eeeeee"))))
   `(term-default-fg-color ((,class (:inherit term-color-white))))
   `(term-default-bg-color ((,class (:inherit term-color-black))))
   ))

;; ansi-term colors
(setq ansi-term-color-vector
  [term term-color-black term-color-red term-color-green term-color-yellow
    term-color-blue term-color-magenta term-color-cyan term-color-white])

(global-font-lock-mode t)
;;(global-hl-line-mode)

;; 対応するカッコを強調
(show-paren-mode t)

;; なんか色々色づけ
(when (require 'whitespace nil t)
  (setq whitespace-style '(face tabs spaces newline trailing space-before-tab space-after-tab space-mark tab-mark newline-mark))
  (set-face-attribute 'whitespace-space nil
                      :foreground "pink4"
                      :background 'unspecified)
  (set-face-attribute 'whitespace-tab nil
                      :foreground "gray40"
                      :background 'unspecified
                      :strike-through t)
  (set-face-attribute 'whitespace-newline nil
                      :foreground "darkcyan"
                      :height 0.8)
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
  (global-whitespace-mode 1))
(require 'generic-x)

;;
;; モードライン設定
;;----------------------------------------------------------------------------------------------------

(defvar mode-line-shorten-p nil)
(defvar mode-line-shorten-length 20)
;; 時刻の表示( 曜日 月 日 時間:分 )
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq display-time-string-forms
      '((propertize (format " %s/%s(%s)%s:%s"
                month day dayname
                24-hours minutes
                ) 'face 'mode-line-time-face)))
(display-time-mode t)

;; mode-line-setup
(setq-default
 mode-line-position
 '(
   " "
   ;; Position, including warning for 80 columns
   (:propertize "%5l" face mode-line-position-face)
   (:propertize "/" face mode-line-delim-face-1)
   (:eval
    (number-to-string (count-lines (point-min) (point-max))))
   " "
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   " "
   ))
;; form
(setq-default
 mode-line-format
 '(
   "%e"
   mode-line-mule-info
   ;; emacsclient [default -- keep?]
   mode-line-client
   mode-line-remote
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize "RO" 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize "**" 'face 'mode-line-modified-face))
          (t "--")))
   " "
   mode-line-position
   ;; directory and buffer/file name
   (:eval (cond
           ((eq major-mode 'dired-mode)
            (concat (when (s-starts-with? "/sudo:" default-directory) (propertize "/sudo:" 'face 'mode-line-git-face))
                    (propertize (shorten-directory (s-chomp (shell-command-to-string "cd ../;pwd")) _mode-line-shorten-length) 'face 'mode-line-folder-face)
                    (propertize (buffer-name) 'face 'mode-line-filename-face)))
           ((eq buffer-file-name nil)
            (propertize (buffer-name) 'face 'mode-line-filename-face))
           (t
            (concat
             (propertize (shorten-directory default-directory _mode-line-shorten-length) 'face 'mode-line-folder-face)
             (propertize (file-name-nondirectory (buffer-file-name)) 'face 'mode-line-filename-face)))
           ))
   ;; narrow [default -- keep?]
   "%n"
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (:eval (propertize (format " [%s]" (projectile-project-name)) 'face 'mode-line-projectile-face))
   (:propertize (vc-mode vc-mode) face mode-line-git-face)
   " %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%]"
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   " "
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(defvar _mode-line-shorten-length mode-line-shorten-length)
(defun toggle-shorten-file-name ()
  (interactive)
  (if mode-line-shorten-p
      (progn
        (setq _mode-line-shorten-length 200)
        (setq mode-line-shorten-p nil)
        (message "Show full file path"))
    (setq _mode-line-shorten-length mode-line-shorten-length)
    (setq mode-line-shorten-p t)
    (message "Show shorten file path")))
(when (not mode-line-shorten-p) (setq _mode-line-shorten-length 200))

;;
;; フォント関係
;;----------------------------------------------------------------------------------------------------
(defvar font-size 140)
(set-face-attribute 'default nil :family "Migu 1M" :height font-size)
(set-face-attribute 'variable-pitch nil :family "Migu 1M" :height font-size)
(set-face-attribute 'fixed-pitch nil :family "Migu 1M" :height font-size)
(set-face-attribute 'tooltip nil :family "Migu 1M" :height font-size)

;;
;; その他設定
;;----------------------------------------------------------------------------------------------------
;; 文字コード
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; GCのしきい値変更
(setq gc-cons-threshold 40960000)

;; window内のカーソル行番号
(defun current-line ()
  "Return the vertical position of point…"
  (+ (count-lines (window-start) (point))
     (if (= (current-column) 0) 1 0)
     -1))

;; 引数取得用関数
(defun region-or-read-string (prompt &optional initial history default inherit)
  "regionが指定されているときはその文字列を取得し、それ以外では`read-string'を呼ぶ。"
  (if (not (region-active-p))
      (read-string prompt initial history default inherit)
    (prog1
        (buffer-substring-no-properties (region-beginning) (region-end))
      (deactivate-mark)
      (message ""))))

;; imenu自動再読み込み
(setq imenu-auto-rescan t)
(setq imenu-after-jump-hook (lambda () (recenter 10)))

;; クリップボード共有
(defun copy-from-tmux ()
  (let ((text (shell-command-to-string "[ ! -z \"$TMUX\" ] && tmux show-buffer")))
    (if (string= text "")
        (car kill-ring)
      text)))
(defun paste-to-tmux (text &optional push)
  (paste-to-remote text push)
  (shell-command-to-string (concat "[ ! -z \"$TMUX\" ] && tmux set-buffer '" (replace-regexp-in-string ";\\'" "\\\\;" (replace-regexp-in-string "'" "'\\\\''" text)) "'")))
(defun paste-to-remote (text &optional push)
  (shell-command-to-string (concat "which rpbcopy 1>/dev/null 2>&1 && echo -n '" (replace-regexp-in-string "'" "'\\\\''" text) "' | rpbcopy")))
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (paste-to-tmux text push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(if window-system
    (setq x-select-enable-clipboard t)
  (cond ((eq system-type 'darwin)
         (setq interprogram-cut-function 'paste-to-osx)
         (setq interprogram-paste-function 'copy-from-osx))
        (t
         (setq interprogram-cut-function 'paste-to-tmux)
         (setq interprogram-paste-function 'copy-from-tmux))))

;; ロックファイルを作らない
(setq create-lockfiles nil)

;; pcompleteにgit追加
(require 'pcmpl-git)

;; ispellの代わりにaspellを使う
(setq ispell-program-name "aspell")

;; re-builderの設定をstringに
(require 're-builder)
(setq reb-re-syntax 'string)

;; ホットローダー
(global-auto-revert-mode 1)

;; ファイル履歴
(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 1000)

;; バックアップを残さない
(setq make-backup-files nil)
(setq auto-save-default nil)

;; ファイルを閉じたとき、次に開くときはその場所(point)から開く
(if (and (>= emacs-major-version 24) (>= emacs-minor-version 5))
    ;; For GNU Emacs 24.5 and older versions.
    (progn (require 'saveplace) (setq-default save-place t))
  ;; For GNU Emacs 25.1 and newer versions.
  (save-place-mode 1))
(setq save-place-file (concat user-emacs-directory "places"))
;; backupファイル保存先
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backups/"))
            backup-directory-alist))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))
(setq auto-save-list-file-prefix "~/.emacs.d/backups/")

;; history保存
(savehist-mode 1)
(push 'kill-ring savehist-additional-variables)

;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; ベル音
(setq ring-bell-function 'ignore)

;; スクロール
;;(setq scroll-step 1)
(setq scroll-conservatively 2)

;; 列番号
(column-number-mode t)

;; 折り返ししない
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; インデント
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(c-set-offset 'case-label '+)

;; Narrowing
(put 'narrow-to-region 'disabled nil)

;; 行番号表示
;;(global-linum-mode)
(if window-system
    (setq linum-format "%5d")
  (setq linum-format "%5d "))

;; スタートページ非表示
(setq inhibit-startup-message t)

;; 新しいel,elcを読み込み
(setq load-prefer-newer t)

;; switch-toでvisibleなbufferには遷移しない
(setq switch-to-visible-buffer nil)

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

;; バッファ名変更
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; *scratch*の初期メッセージを消す
(setq initial-scratch-message "")

;; *scratch*を消さない
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))

;; スクラッチの保存と復元
(defun save-scratch-data ()
  (interactive)
  (let ((str (progn
               (set-buffer (get-buffer "*scratch*"))
               (buffer-substring-no-properties
                (point-min) (point-max))))
        (file "~/.scratch"))
    (if (get-file-buffer (expand-file-name file))
        (setq buf (get-file-buffer (expand-file-name file)))
      (setq buf (find-file-noselect file)))
    (set-buffer buf)
    (erase-buffer)
    (insert str)
    (save-buffer)
    (kill-buffer buf)))

(defadvice save-buffers-kill-emacs
  (before save-scratch-buffer activate)
  (save-scratch-data))

(defun read-scratch-data ()
  (let ((file "~/.scratch"))
    (when (file-exists-p file)
      (set-buffer (get-buffer "*scratch*"))
      (erase-buffer)
      (insert-file-contents file))
    ))

(read-scratch-data)

;; tmp buffer作成
(defun generate-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "*scratch* "))
  (org-mode)
  (whitespace-mode))

;; 設定ファイル再読み込み
(defun reload-init-file (arg)
  "reload ~/.emacs.d/init.el"
  (interactive "p")
  (case arg
    (4 (let ((input (read-file-name "Load init file: " "~/.emacs.d/init.el" "~/.emacs.d/init.el")))
         (load-file input))
       )
    (t (load-file "~/.emacs.d/init.el")))
  )

;; git関係便利関数
(defun git-project-p ()
  (string=
   (s-chomp
    (shell-command-to-string "git rev-parse --is-inside-work-tree"))
   "true"))

(defun git-root-directory ()
  (cond ((git-project-p)
         (s-chomp
          (shell-command-to-string "git rev-parse --show-toplevel")))
        (t
         "")))

;; Tramp設定
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
;; (unless (eq system-type 'cygwin)
;;   (setenv "TMPDIR" "/tmp"))

;; (defadvice tramp-handle-vc-registered (around tramp-handle-vc-registered-around activate)
;;   (let ((vc-handled-backends '(SVN Git))) ad-do-it))

;; 非同期shell command
(defadvice erase-buffer (around erase-buffer-noop)
  "make erase-buffer do nothing")

(defadvice shell-command (around shell-command-unique-buffer activate compile)
  (if (or current-prefix-arg
          (not (string-match "[ \t]*&[ \t]*\\'" command)) ;; background
          (bufferp output-buffer)
          (stringp output-buffer))
      ad-do-it

    ;; else we need to set up buffer
    (let* ((command-buffer-name
            (format "*Async Shell Command: %s*"
                    (substring command 0 (match-beginning 0))))
           (command-buffer (get-buffer command-buffer-name)))

      (when command-buffer
        ;; if the buffer exists, reuse it, or rename it if it's still in use
        (cond ((get-buffer-process command-buffer)
               (set-buffer command-buffer)
               (rename-uniquely))
              ('t
               (kill-buffer command-buffer))))
      (setq output-buffer command-buffer-name)

      ;; insert command at top of buffer
      (switch-to-buffer-other-window output-buffer)

      ;; temporarily blow away erase-buffer while doing it, to avoid
      ;; erasing the above
      (ad-activate-regexp "erase-buffer-noop")
      ad-do-it
      (view-mode)
      (ad-deactivate-regexp "erase-buffer-noop"))))

;;
;; view-mode
;;----------------------------------------------------------------------------------------------------
(setq view-read-only t)
;; 書き込み不能なファイルはview-modeで開くように
(defadvice find-file
  (around find-file-switch-to-view-file (file &optional wild) activate)
  (if (and (not (file-writable-p file))
           (not (file-directory-p file)))
      (view-file file)
    ad-do-it))
;; 書き込み不能な場合はview-modeを抜けないように
(defvar view-mode-force-exit nil)
(defmacro do-not-exit-view-mode-unless-writable-advice (f)
  `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
     (if (and (buffer-file-name)
              (not view-mode-force-exit)
              (not (file-writable-p (buffer-file-name))))
         (message "File is unwritable, so stay in view-mode.")
       ad-do-it)))

(do-not-exit-view-mode-unless-writable-advice view-mode-exit)
(do-not-exit-view-mode-unless-writable-advice view-mode-disable)

;;
;; パッケージ関係
;;----------------------------------------------------------------------------------------------------

;;
;; dired.el
;;----------------------------------------------------------------------------------------------------
(when (require 'dired nil t)
  (require 'dired-subtree)
  (require 'dired-rainbow)
  ;; dired-find-alternate-file の有効化
  (put 'dired-find-alternate-file 'disabled nil)
  ;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
  (setq dired-dwim-target t)
  ;; ディレクトリを再帰的にコピーする
  (setq dired-recursive-copies 'always)
  ;; diredバッファでC-sした時にファイル名だけにマッチするように
  (setq dired-isearch-filenames t)
  ;; auto revert
  (setq dired-auto-revert-buffer t)
  ;; lsの設定
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t)

  (define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
  (define-key dired-mode-map (kbd "RET")     'dired-find-file)
  (define-key dired-mode-map [mouse-1]       'dired-find-file)
  (define-key dired-mode-map [mouse-2]       'dired-find-file)
  (define-key dired-mode-map (kbd "a")       'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")       'dired-up-directory)
  (define-key dired-mode-map (kbd "C-<tab>") '(lambda () (interactive) (dired-subtree-up) (dired-subtree-toggle)))
  (define-key dired-mode-map (kbd "C-i")   'dired-subtree-toggle)
  (define-key dired-mode-map (kbd "M-n")     'dired-subtree-next-sibling)
  (define-key dired-mode-map (kbd "M-p")     'dired-subtree-previous-sibling)
  (define-key dired-mode-map (kbd "C-M-n")   'dired-subtree-down)
  (define-key dired-mode-map (kbd "C-M-p")   'dired-subtree-up)
  (define-key dired-mode-map (kbd "C-b")     'backward-char)
  (define-key dired-mode-map (kbd "C-f")     'forward-char)
  (define-key dired-mode-map (kbd "e")       'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "M-s f")   nil)
  (define-key dired-mode-map (kbd "M-s a")   'ag)

  (add-hook 'dired-mode-hook '(lambda () (company-mode -1) (auto-revert-mode t)))

  ;; (require 'dired-toggle)
  (defun dired-toggle-current-or-project-directory (n)
    (interactive "p")
    (let ((dir (projectile-project-p)))
      (cond ((= n 1)
             (dired-jump))
            ((= n 4)
             (if dir
                 (projectile-dired)
               (dired-jump)))
            ;; (t
            ;;  (if dir
            ;;      (dired-toggle dir)
            ;;    (dired-toggle)))
            )))

  (setq dired-subtree-use-backgrounds nil)
  (dired-rainbow-define dotfiles "#aaaaaa" "\\..*")
  (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")
  )

;;
;; split-root.el
;;----------------------------------------------------------------------------------------------------
(when (require 'split-root nil t)
  (defun display-buffer-function--split-root (buf &optional size horflag top-left)
    (let* ((window (split-root-window size horflag top-left)))
      (set-window-buffer window buf)
      window))
  (setq helm-display-function 'display-buffer-function--split-root)
  (setq helm-swoop-split-window-function 'display-buffer-function--split-root))

;;
;; popwin.el
;;----------------------------------------------------------------------------------------------------
(when (require 'popwin nil t)
  (defun popwin:close-popup-window-if-necessary:around (orign)
    (let ((last-command nil))
    (funcall orign)))
  (advice-add 'popwin:close-popup-window-if-necessary :around 'popwin:close-popup-window-if-necessary:around)
  (setq display-buffer-function 'popwin:display-buffer)
  (push '(occur-mode :position bottom :height 0.5) popwin:special-display-config)
  (push '(ag-mode :position bottom :height 0.5) popwin:special-display-config)
  (push '("\\*screen terminal<.*?>\\*" :regexp t :position bottom :height 0.5 :stick t) popwin:special-display-config)
  (push `("\\*FileTree\\* .*" :regexp t :position left :width 40 :stick t) popwin:special-display-config)
  )

;;
;; ag.el
;;----------------------------------------------------------------------------------------------------
(when (require 'ag nil t)
  (custom-set-variables
   '(ag-highlight-search t)  ; 検索結果の中の検索語をハイライトする
   '(ag-reuse-window 'nil)   ; 現在のウィンドウを検索結果表示に使う
   '(ag-reuse-buffers 't)) ; 検索用バッファを使いまわす
  )

;;
;; wgrep.el
;;----------------------------------------------------------------------------------------------------
(when (require 'wgrep nil t)
  (setq wgrep-auto-save-buffer t)
  (autoload 'wgrep-setup "wgrep")
  (add-hook 'grep-mode-hook 'wgrep-setup)
  (define-key grep-mode-map (kbd "e") 'wgrep-change-to-wgrep-mode)
  )
(when (require 'wgrep-ag nil t)
  (setq wgrep-auto-save-buffer t)
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  (define-key ag-mode-map (kbd "e") 'wgrep-change-to-wgrep-mode)
  )

;;
;; elscreen.el
;;----------------------------------------------------------------------------------------------------
(when (require 'elscreen nil t)
  (setq elscreen-prefix-key (kbd "C-q C-w"))
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (setq elscreen-display-screen-number nil)
  (setq elscreen-display-tab 24)
  (elscreen-start)
  (require 'elscreen-multi-term)
  (require 'elscreen-separate-buffer-list)
  (elscreen-separate-buffer-list-mode)
  )

;;
;; rotate.el
;;----------------------------------------------------------------------------------------------------
(when (require 'rotate nil t)
  (defun rotate-cursor:after-rotate-window ()
    (other-window 1))
  (advice-add 'rotate-window :after 'rotate-cursor:after-rotate-window))

;;
;; multiple-cursor.el
;;----------------------------------------------------------------------------------------------------
(when (require 'multiple-cursors nil t)
  (define-key mc/mark-more-like-this-extended-keymap (kbd "C-p") 'mc/mmlte--up)
  (define-key mc/mark-more-like-this-extended-keymap (kbd "C-n") 'mc/mmlte--down)
  (define-key mc/mark-more-like-this-extended-keymap (kbd "C-b") 'mc/mmlte--left)
  (define-key mc/mark-more-like-this-extended-keymap (kbd "C-f") 'mc/mmlte--right)
  ;;(require 'phi-search-migemo)
  (define-key mc/keymap (kbd "C-s") 'phi-search)
  (define-key mc/keymap (kbd "C-r") 'phi-replace)
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

;; projectile.el
;;----------------------------------------------------------------------------------------------------
(when (require 'projectile nil t)
  (projectile-global-mode)
  (require 'helm-projectile)
  (helm-projectile-on)

  (setq projectile-mode-line "")
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action 'projectile-dired))

;; magit.el
;;----------------------------------------------------------------------------------------------------
(when (require 'magit nil t)
  ;; (setq magit-last-seen-setup-instructions "1.4.0")
  ;; (setq magit-diff-refine-hunk 'all)
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register (intern (concat "magit-fullscreen-" (number-to-string (elscreen-get-current-screen)))))
    ad-do-it
    (delete-other-windows))

  (defun my/magit-quit-session ()
    (interactive)
    (kill-buffer)
    (jump-to-register (intern (concat "magit-fullscreen-" (number-to-string (elscreen-get-current-screen)))))
    (git-gutter:update-all-windows))

  (define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)

  (add-hook 'magit-mode-hook '(lambda () (company-mode -1)))
  )

;;
;; multi-term.el
;;----------------------------------------------------------------------------------------------------
(when (require 'multi-term nil t)
  (setenv "TERMINFO" "~/.terminfo")
  (setq multi-term-program "zsh")
  (if (eq system-type 'cygwin)
      (setq multi-term-program "bash.exe"))
  (add-to-list 'term-unbind-key-list '"M-x")
  (add-to-list 'term-unbind-key-list '"C-z")
  (setq multi-term-scroll-show-maximum-output t)

  (defadvice term-send-return (after check-cd-action activate)
    (let* ((cmd (term-get-old-input-default))
           (match (string-match-p "$ cd" cmd)))
      (when match
        (let ((dir (substring cmd (+ 4 (string-match "$ cd" cmd)))))
          (if (equal (length dir) 0)
              (setq dir "~")
            (setq dir (substring dir 1)))
          (cd dir)))))

  (defun term-send-current-directory(&optional dir)
    (interactive)
    (let* ((mt-buffer-name (emt-get-or-create-multi-term-buffer)))
      (unless dir (setq dir (s-chomp (shell-command-to-string "pwd"))))
      (comint-send-string (get-buffer-process mt-buffer-name) (format "cd %s\n" dir))
      (with-current-buffer mt-buffer-name
        (cd dir))))

  (defun term-send-cd ()
    (interactive)
    (call-interactively 'cd)
    (term-send-current-directory))

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
  (add-hook 'term-mode-hook
            '(lambda ()
               (company-mode -1)
               (define-key term-raw-map (kbd "C-q") nil)
               (define-key term-raw-map (kbd "C-q C-q") 'er/expand-region)
               (define-key term-raw-map (kbd "C-q C-z") 'er/contract-region)
               (define-key term-raw-map (kbd "C-c C-d") 'term-send-cd)
               (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
               (define-key term-raw-map (kbd "C-f") 'term-send-forward-char)
               (define-key term-raw-map (kbd "C-b") 'term-send-backward-char)
               (define-key term-raw-map (kbd "C-p") 'term-send-previous-line)
               (define-key term-raw-map (kbd "C-n") 'term-send-next-line)
               (define-key term-raw-map (kbd "C-y") 'term-paste)
               (define-key term-raw-map (kbd "H-v") 'term-paste)
               (define-key term-raw-map (kbd "M-d") 'term-send-forward-kill-word)
               (define-key term-raw-map (kbd "M-<backspace>") 'term-send-backward-kill-word)
               (define-key term-raw-map (kbd "M-DEL") 'term-send-backward-kill-word)
               (define-key term-raw-map (kbd "M-h") 'term-send-backward-kill-word)
               (define-key term-raw-map (kbd "C-z") 'emt-pop-multi-term)
               (define-key term-raw-map (kbd "TAB") 'term-send-tab)
               (define-key term-raw-map (kbd "M-p") 'previous-line)
               (define-key term-raw-map (kbd "M-n") 'next-line)
               (define-key term-raw-map (kbd "M-f") 'forward-char)
               (define-key term-raw-map (kbd "M-b") 'backward-char)
               (define-key term-raw-map (kbd "<up>")   'scroll-down-line)
               (define-key term-raw-map (kbd "<down>") 'scroll-up-line)
               (define-key term-raw-map (kbd "<right>")'scroll-left)
               (define-key term-raw-map (kbd "<left>") 'scroll-right)
               ))
  )

;;
;; undo-tree.el
;;----------------------------------------------------------------------------------------------------
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  )

;;
;; company.el
;;----------------------------------------------------------------------------------------------------
(when (require 'company nil t)
  (require 'company-statistics)
  (company-statistics-mode)

  (global-company-mode 1)

  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)

  (setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-minimum-width 40)
  (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)

  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  ;; 色の設定
  (set-face-attribute 'company-tooltip nil
                      :foreground "black"
                      :background "lightgray")
  (set-face-attribute 'company-preview-common nil
                      :foreground "dark gray"
                      :background "black"
                      :underline t)
  (set-face-attribute 'company-tooltip-selection nil
                      :background "steelblue"
                      :foreground "white")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black"
                      :underline t)
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white"
                      :background "steelblue"
                      :underline t)
  (set-face-attribute 'company-tooltip-annotation nil
                      :foreground "red")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "dark gray")
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "black")
  )

;;
;; helm.el
;;----------------------------------------------------------------------------------------------------
(when (require 'helm-config nil t)
  (require 'helm-descbinds)
  (require 'helm-gtags)

  (helm-mode 1)
  ;; gtags
  (add-hook 'c-mode-hook (lambda () (helm-gtags-mode)))
  (add-hook 'php-mode-hook (lambda () (helm-gtags-mode)))
  ;; customize
  (setq helm-c-gtags-path-style 'relative)
  (setq helm-c-gtags-ignore-case t)
  ;; key bindings
  (add-hook 'helm-gtags-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-j") 'helm-gtags-dwim)
               (local-set-key (kbd "C-c C-u") 'helm-gtags-previous-history)
               (local-set-key (kbd "C-c C-p") 'helm-gtags-previous-history)
               (local-set-key (kbd "C-c C-n") 'helm-gtags-next-history)
               (local-set-key (kbd "C-c C-s") 'helm-gtags-show-stack)
               ))

  ;; buffer名の表示幅
  (setq helm-buffer-max-length 40)

  ;; kill-ring
  (setq kill-ring-max 1000)

  ;; 自動補完をやめる
  (setq helm-ff-auto-update-initial-value nil)

  ;; tabで補完
  (define-key helm-read-file-map (kbd "C-i") 'helm-execute-persistent-action)
  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  ;; descbindsを置き換え
  (helm-descbinds-mode t)

  ;; bookmarkの場所を表示
  (setq helm-bookmark-show-location t)

  (defclass helm-source-normal-buffers (helm-source-sync helm-type-buffer)
    ((buffer-list
      :initarg :buffer-list
      :initform #'helm-buffer-list
      :custom function
      :documentation
      "  A function with no arguments to create dired buffer list.")
     (init :initform 'helm-buffers-list--init)
     (candidates :initform (lambda ()
                             (cl-remove-if
                              (lambda (elm)
                                (with-current-buffer elm
                                  (equal major-mode 'dired-mode)))
                              helm-buffers-list-cache)))
     (matchplugin :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (resume :initform (lambda ()
                         (run-with-idle-timer
                          0.1 nil (lambda ()
                                    (with-helm-buffer
                                      (helm-force-update))))))
     (keymap :initform helm-buffer-map)
     (migemo :initform 'nomultimatch)
     (volatile :initform t)
     (help-message :initform 'helm-buffer-help-message)
     (persistent-help
      :initform
      "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))

  (defclass helm-source-dired-buffers (helm-source-sync helm-type-buffer)
    ((buffer-list
      :initarg :buffer-list
      :initform #'helm-buffer-list
      :custom function
      :documentation
      "  A function with no arguments to create dired buffer list.")
     (init :initform 'helm-buffers-list--init)
     (candidates :initform (lambda ()
                             (cl-remove-if-not
                              (lambda (elm)
                                (with-current-buffer elm
                                  (equal major-mode 'dired-mode)))
                              helm-buffers-list-cache)))
     (matchplugin :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (resume :initform (lambda ()
                         (run-with-idle-timer
                          0.1 nil (lambda ()
                                    (with-helm-buffer
                                      (helm-force-update))))))
     (keymap :initform helm-buffer-map)
     (migemo :initform 'nomultimatch)
     (volatile :initform t)
     (help-message :initform 'helm-buffer-help-message)
     (persistent-help
      :initform
      "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))

  (setq helm-locate-command
        (concat "locate_case=$(echo '%s' | sed 's/-//'); cat /tmp/all.filelist |"
        ;;(concat "locate_case=$(echo '%s' | sed 's/-//'); locate '' |"
                "perl -ne \"$(echo '%s' |"
                (if (eq system-type 'darwin)
                    "sed -E -e 's/[\\\\ ] /__SpAcE__/g' "
                  "sed -r -e 's/[\\\\ ] /__SpAcE__/g' ")
                "-e 's/^ +//' "
                "-e 's/ +$//' "
                "-e 's_/_\\\\&_g' "
                "-e 's_ +_/'$locate_case' \\&\\& m/_g' "
                "-e 's_.*_$| = 1; print if (m/&/'$locate_case')_' "
                "-e 's_m/!_!m/_g' "
                "-e 's/__SpAcE__/ /g')\" 2> /dev/null |"
                "head -n " (number-to-string helm-candidate-number-limit)))
  (defun helm-filelist-real-to-display (candidate)
    (let ((directory-abbrev-alist `((,(concat "\\`" (getenv "HOME")) . "~"))))
      (abbreviate-file-name candidate)))
  (setq helm-source-locate (cons '(real-to-display . helm-filelist-real-to-display) helm-source-locate))

  (defvar helm-source-normal-buffers-list nil)
  (defvar helm-source-dired-buffers-list nil)
  (defun helm-filelist++ ()
    (interactive)
    (unless helm-source-normal-buffers-list
      (setq helm-source-normal-buffers-list
            (helm-make-source "Buffers" 'helm-source-normal-buffers)))
    (unless helm-source-dired-buffers-list
      (setq helm-source-dired-buffers-list
            (helm-make-source "Dired Buffers" 'helm-source-dired-buffers)))
    (let ((helm-ff-transformer-show-only-basename nil)
          (recentf-list (mapcar (lambda (path)
                                   (replace-regexp-in-string (expand-file-name (getenv "HOME")) "~" path))
                                 recentf-list)))
      (helm :sources `(helm-source-normal-buffers-list
                       helm-source-dired-buffers-list
                       helm-source-recentf
                       helm-source-buffer-not-found
                       helm-source-locate)
            :buffer "*helm filelist++*"
            :truncate-lines t)))
  )

;;
;; yasnippet.el
;;----------------------------------------------------------------------------------------------------
(when (require 'yasnippet nil t)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets" ;; 作成するスニペットはここに入る
          ))
  (yas-global-mode 1)
  ;; 単語展開キー
  (custom-set-variables '(yas-trigger-key "TAB"))

  ;; other windowでnew snippet
  (defun yas-new-snippet-other-window (&optional no-template)
    "Pops a new buffer for writing a snippet.

Expands a snippet-writing snippet, unless the optional prefix arg
NO-TEMPLATE is non-nil."
    (interactive "P")
    (let ((guessed-directories (yas--guess-snippet-directories)))

      (switch-to-buffer-other-window "*new snippet*")
      (erase-buffer)
      (kill-all-local-variables)
      (snippet-mode)
      (yas-minor-mode 1)
      (set (make-local-variable 'yas--guessed-modes) (mapcar #'(lambda (d)
                                                                 (yas--table-mode (car d)))
                                                             guessed-directories))
      (if (and (not no-template) yas-new-snippet-default)
          (yas-expand-snippet yas-new-snippet-default))))

  ;; 既存スニペットを挿入する
  (define-key yas-minor-mode-map (kbd "C-c s i") 'yas-insert-snippet)
  ;; 新規スニペットを作成するバッファを用意する
  (define-key yas-minor-mode-map (kbd "C-c s n") 'yas-new-snippet-other-window)
  ;; 既存スニペットを閲覧・編集する
  (define-key yas-minor-mode-map (kbd "C-c s v") 'yas-visit-snippet-file)

  (defun my-yas/prompt (prompt choices &optional display-fn)
    (let* ((names (loop for choice in choices
                        collect (or (and display-fn (funcall display-fn choice))
                                    choice)))
           (selected (helm-other-buffer
                      `(((name . ,(format "%s" prompt))
                         (candidates . names)
                         (action . (("Insert snippet" . (lambda (arg) arg))))))
                      "*helm yas/prompt*")))
      (if selected
          (let ((n (position selected names :test 'equal)))
            (nth n choices))
        (signal 'quit "user quit!"))))
  (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))

  (custom-set-variables '(yas-new-snippet-default "\
# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# --
$0"))
  )

;;
;; ssh-agent.el
;;----------------------------------------------------------------------------------------------------
(require 'ssh-agent)

;;
;; flycheck.el
;;----------------------------------------------------------------------------------------------------
(add-hook 'after-init-hook #'global-flycheck-mode)
(custom-set-variables '(flycheck-pos-tip-timeout 3600))
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;;
;; go-mode.el
;;----------------------------------------------------------------------------------------------------
(when (require 'go-mode nil t)
  (defun go-mode-init ()
    (set (make-variable-buffer-local 'view-mode-jump-to-definition)
         (lambda ()
           (interactive)
           (godef-jump (point))
           ))
    )
  (add-hook 'go-mode-hook 'go-mode-init)
  )

;;
;; typescript.el
;;----------------------------------------------------------------------------------------------------
(when (require 'typescript-mode nil t)
  (require 'tide)
  (add-hook 'typescript-mode-hook
            (lambda ()
              (tide-setup)
              (flycheck-mode t)
              ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode t)
              (company-mode-on)
              (add-to-list 'company-backends '(company-tide :with company-yasnippet))
              ))

  (define-key typescript-mode-map (kbd "C-c C-j") 'tide-jump-to-definition)
  (define-key typescript-mode-map (kbd "C-c C-u") 'tide-jump-back)
  (define-key typescript-mode-map (kbd "C-c r s") 'tide-restart-server)
  (define-key typescript-mode-map (kbd "C-c r n") 'tide-rename-symbol)
  (define-key typescript-mode-map (kbd "C-c r l") 'tide-references)
  )

;;
;; js2-mode
;;----------------------------------------------------------------------------------------------------
(when (require 'js2 nil t)
  (setq js2-cleanup-whitespace nil
        js2-mirror-mode nil
        js2-bounce-indent-flag nil)

  (defun indent-and-back-to-indentation ()
    (interactive)
    (indent-for-tab-command)
    (let ((point-of-indentation
           (save-excursion
             (back-to-indentation)
             (point))))
      (skip-chars-forward "\s " point-of-indentation)))
  (define-key js2-mode-map "\C-i" 'indent-and-back-to-indentation)
  )

;;
;; markdown-mode.el
;;----------------------------------------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)

;;
;; php-mode.el
;;----------------------------------------------------------------------------------------------------
(when (require 'php-mode)
  (setq php-mode-force-pear t)
  (defun my-php-mode-hook ()
    (require 'company-php)
    (add-to-list 'company-backends '(company-ac-php-backend company-dabbrev-code company-gtags company-keywords :with company-yasnippet))
    )
  (add-hook 'php-mode-hook 'my-php-mode-hook)
  )

;;
;; web-mode.el
;;----------------------------------------------------------------------------------------------------
(when (require 'web-mode)
  (defun my-web-mode-hook ()
    (require 'company-web-html)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset    4)
    (setq web-mode-code-indent-offset   4)
    (setq web-mode-style-padding  2)
    (setq web-mode-script-padding 2)
    (setq web-mode-block-padding  0)
    (setq web-mode-enable-auto-pairing nil)
    (setq web-mode-enable-css-colorization t)
    (add-to-list 'company-backends '(company-web-html company-dabbrev-code company-gtags company-keywords :with company-yasnippet))
    )
  (add-hook 'web-mode-hook 'my-web-mode-hook)
)

;;
;; edit-indirect.el
;;----------------------------------------------------------------------------------------------------
(when (require 'edit-indirect nil t)
  (define-key web-mode-map (kbd "C-c C-c") 'edit-indirect-dwim)
  (defvar edit-indirect-guess-mode-alist
    '(("<script.*>"  "</script.*>" js2-mode)
      ("<style.*>"  "</style.*>" css-mode)))
  (defun edit-indirect-guess-mode-at-point ()
    (cl-loop with s
             with e
             with region-start
             with region-end
             with pt = (point)
             for (start end mode) in
             edit-indirect-guess-mode-alist
             when
             (save-excursion
               (setq s (re-search-backward start nil t)
                     region-start (match-end 0))
               (setq e (re-search-forward end nil t)
                     region-end (match-beginning 0))
               (and s e (< s pt e)))
             return (list mode region-start region-end)))
  (defun edit-indirect-dwim (s e)
    (interactive "r")
    (let (it)
      (cond ((region-active-p)
             (edit-indirect-region s e t))
            ((setq it (edit-indirect-guess-mode-at-point))
             (edit-indirect-region (nth 1 it) (nth 2 it) t)
             (funcall (car it)))
            (t
                        (user-error "No region")))))
  )

;;
;; git-gutter.el
;;----------------------------------------------------------------------------------------------------
(when (require 'git-gutter nil t)
  (when window-system
    (require 'git-gutter-fringe))
  (global-git-gutter-mode t)
  (add-to-list 'git-gutter:update-commands 'my/magit-quit-session)
  (custom-set-variables
   '(git-gutter:update-interval 1))
  (custom-set-variables
   '(git-gutter:lighter ""))
  ;; (defun git-gutter:my-original-file-content (file)
  ;;   (let ((file (replace-regexp-in-string (concat (git-root-directory) "/") "" (file-truename file))))
  ;;     (with-temp-buffer
  ;;       (when (zerop (process-file "git" nil t nil "show" (concat "HEAD:" file)))
  ;;         (buffer-substring-no-properties (point-min) (point-max))))))
  ;; (advice-add 'git-gutter:original-file-content :override 'git-gutter:my-original-file-content)
  )

;;
;; hl-line+.el
;;----------------------------------------------------------------------------------------------------
(when (require 'hl-line+ nil t)
  (toggle-hl-line-when-idle)
  (setq hl-line-idle-interval 3)
  (set-face-background 'hl-line "#035f56")
  (defun toggle-hl-line-mode ()
    (interactive)
    (if global-hl-line-mode
        (global-hl-line-mode -1)
      (global-hl-line-mode 1))
    )
  )

;;
;; rainbow.el
;;----------------------------------------------------------------------------------------------------
(when (require 'rainbow-mode nil t)
  (add-hook 'js-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'scss-mode-hook 'rainbow-mode)
  (add-hook 'php-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'go-mode-hook 'rainbow-mode)
  (add-hook 'coffee-mode-hook 'rainbow-mode)
  )

;;
;; マイナーモードの省略
;;----------------------------------------------------------------------------------------------------
(defun shorten-minor-mode-name (mode-sym short-name &optional face)
  "minor-modeの名前を短くする。"
  (let ((cell (assq mode-sym minor-mode-alist)))
    (when (consp cell)
      (if face
          (setq short-name (propertize short-name 'face face))
        (setq short-name (concat short-name)))
      (setcar (cdr cell) short-name))
    ))

(shorten-minor-mode-name 'helm-gtags-mode " GT")
(shorten-minor-mode-name 'abbrev-mode " Ab")
(shorten-minor-mode-name 'yas-minor-mode " Ys")
(shorten-minor-mode-name 'orglink-mode " OL")
(shorten-minor-mode-name 'orgtbl-mode " OT")
(shorten-minor-mode-name 'undo-tree-mode "")
(shorten-minor-mode-name 'rainbow-mode "")
(shorten-minor-mode-name 'helm-mode "")
(shorten-minor-mode-name 'whitespace-mode "")
(shorten-minor-mode-name 'global-whitespace-mode "")

(setq eldoc-minor-mode-string " El")

;;
;; bracketed paste
;;----------------------------------------------------------------------------------------------------
(require 'bracketed-paste)
(bracketed-paste-enable)

;;
;; サーバー起動
;;----------------------------------------------------------------------------------------------------
(unless (server-running-p)
  (server-start))

;;
;; multi-tramp-settings
;;----------------------------------------------------------------------------------------------------
(when (require 'multi-tramp-settings nil t))
