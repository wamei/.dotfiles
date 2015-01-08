;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;----------------------------------------------------------------------------------------------------

;; load-pathの追加関数
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
(cond ((eq system-type 'darwin)
       (require 'exec-path-from-shell)
       (let ((envs '("PATH" "VIRTUAL_ENV" "GOROOT" "GOPATH")))
         (exec-path-from-shell-copy-envs envs)))
       ;;(exec-pathth-from-shell-initialize)
       ((eq system-type 'windows-nt)
        ;; ------------------------------------------------------------------------
        ;; @ setup-cygwin
        (setq cygwin-mount-cygwin-bin-directory
              (concat (getenv "CYGWIN_DIR") "\\bin"))
        (require 'setup-cygwin)
        (file-name-shadow-mode -1)

        ;; ------------------------------------------------------------------------
        ;; @ shell
        (require 'shell)
        (setq explicit-shell-file-name "bash.exe")
        (setq shell-command-switch "-c")
        (setq shell-file-name "bash.exe")

        ;; (M-! and M-| and compile.el)
        (modify-coding-system-alist 'process ".*sh\\.exe" 'cp932)

        (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

        (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")

        (autoload 'ansi-color-for-comint-mode-on "ansi-color"
          "Set `ansi-color-for-comint-mode' to t." t)

        (setq shell-mode-hook
              (function
               (lambda ()
                 (set-buffer-process-coding-system 'sjis-dos 'sjis-unix)
                 (set-buffer-file-coding-system    'sjis-unix)
                 )))

        ;; ------------------------------------------------------------------------
        ;; @ w32-symlinks

        (custom-set-variables '(w32-symlinks-handle-shortcuts t))
        (require 'w32-symlinks)

        (defadvice insert-file-contents-literally
          (before insert-file-contents-literally-before activate)
          (set-buffer-multibyte nil))

        (defadvice minibuffer-complete (before expand-symlinks activate)
          (let ((file (expand-file-name
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))))
            (when (file-symlink-p file)
              (delete-region (line-beginning-position) (line-end-position))
              (insert (w32-symlinks-parse-symlink file)))))
        )
       )

;;; package.el
(when (require 'package nil t)
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  )

(require 'misc)
(require 'expand-region)
(require 'visual-regexp-steroids)

;;
;; キーバインド
;;----------------------------------------------------------------------------------------------------
(global-set-key (kbd "C-h")     nil)
(global-set-key (kbd "C-r")     'vr/replace)
(global-set-key (kbd "C-v")     'scroll-up-with-cursor)
(global-set-key (kbd "C--")     'undo-tree-undo)
(define-key key-translation-map [?\C-h] [?\C-?])

(global-set-key (kbd "M-b")     'backward-word)
(global-set-key (kbd "M-f")     'forward-to-word)
;;(global-set-key (kbd "M-g")     'goto-line)
(global-set-key (kbd "M-h")     'backward-kill-word)
(global-set-key (kbd "M-n")     'forward-list)
(global-set-key (kbd "M-p")     'backward-list)
(global-set-key (kbd "M-v")     'scroll-down-with-cursor)
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "M-;")     'comment-or-uncomment-region)
(global-set-key (kbd "M--")     'undo-tree-redo)

(global-set-key (kbd "M-s s")   'helm-swoop)
(global-set-key (kbd "M-s g")   'helm-git-grep)
(global-set-key (kbd "M-s f")   'helm-cmd-t)
(global-set-key (kbd "M-s a")   'ag)
(global-set-key (kbd "M-s o")   'occur)

(global-set-key (kbd "C-c C-c") 'quickrun)
(global-set-key (kbd "C-c C-u") 'pop-tag-mark)

(global-set-key (kbd "C-q") nil)
(global-set-key (kbd "C-q C-q") 'er/expand-region)
(global-set-key (kbd "C-q C-z") 'er/contract-region)
(global-set-key (kbd "C-q C-l") 'mc/edit-lines)
(global-set-key (kbd "C-q C-r") 'vr/mc-mark)
(global-set-key (kbd "C-q C-i") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-q C-a") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-q C-d") 'mc/mark-all-like-this-in-defun)
(global-set-key (kbd "C-q C-s") 'mc/mark-all-symbols-like-this)
(global-set-key (kbd "C-q C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-q C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-q C-e") 'mc/mark-all-symbols-like-this-in-defun)
(global-set-key (kbd "C-q C-m") 'my-mc-put-cursor)

(global-set-key (kbd "C-x b")   'helm-bookmarks)
(global-set-key (kbd "C-x g")   'magit-status)
(global-set-key (kbd "C-x n")   'linum-mode)
(global-set-key (kbd "C-x p")   'helm-resume)
(global-set-key (kbd "C-x t")   'twittering-update-status-interactive)

(global-set-key (kbd "C-x e") nil)
(global-set-key (kbd "C-x e r")   'resize)
(global-set-key (kbd "C-x e a")   'set-frame-alpha-interactive)

(global-set-key (kbd "C-x m") nil)
(global-set-key (kbd "C-x m a")   'org-agenda)
(global-set-key (kbd "C-x m o")   'org-capture)
(global-set-key (kbd "C-x m m")   'org-capture-memo)
(global-set-key (kbd "C-x m c")   'org-capture-code-reading)

(global-set-key (kbd "C-x C-b") 'helm-filelist++)
(global-set-key (kbd "C-x C-i") 'direx:jump-to-git-project-directory)
(global-set-key (kbd "C-x C-j") 'dired-jump-other-window)

(global-set-key (kbd "C-M-r")   'vr/query-replace)
(global-set-key (kbd "C-M-s")   'vr/isearch-forward)

(global-set-key (kbd "C-S-h")   'kill-whole-line)

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
    (,(kbd "C-z") . switch-to-multi-term)
    (,(kbd "C-M-b") . windmove-left)
    (,(kbd "C-M-f") . windmove-right)
    (,(kbd "C-M-n") . windmove-down)
    (,(kbd "C-M-p") . windmove-up)
    (,(kbd "C-q C-w C-f") . elscreen-next)
    (,(kbd "C-q C-w C-b") . elscreen-previous)
    (,(kbd "C-q C-w n") . elscreen-next)
    (,(kbd "C-q C-w p") . elscreen-previous)
    (,(kbd "C-q C-w c") . elscreen-create)
    (,(kbd "C-q C-w k") . elscreen-kill)
    (,(kbd "C-q C-w A") . elscreen-screen-nickname)
    (,(kbd "C-q C-w C-s") . elscreen-swap)
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
(add-to-list 'auto-mode-alist '("\\.\\(html\\|xhtml\\|shtml\\|tpl\\|hbs\\)\\'" . web-mode))

(require 'org)
(add-hook 'lisp-interaction-mode-hook 'orgtbl-mode)
(add-hook 'text-mode-hook 'orgtbl-mode)
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (if (equal major-mode 'fundamental-mode)
                (orgtbl-mode))))

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
(defun set-frame-alpha-interactive ()
  "Control frame alpha."
  (interactive)
  (let (c)
    (catch 'end-flag
      (while t
        (message "Frame alpha [%d]" frame-alpha)
        (condition-case err
            (setq c (read-key-sequence nil))
          (error
           (throw 'end-flag t)))
        (cond ((equal c "p")
               (set-frame-alpha (- frame-alpha 1)))
              ((equal c "n")
               (set-frame-alpha (+ frame-alpha 1)))
              ((equal c "d")
               (set-frame-alpha 85))
              ((equal c "1")
               (set-frame-alpha 10))
              ((equal c "2")
               (set-frame-alpha 20))
              ((equal c "3")
               (set-frame-alpha 30))
              ((equal c "4")
               (set-frame-alpha 40))
              ((equal c "5")
               (set-frame-alpha 50))
              ((equal c "6")
               (set-frame-alpha 60))
              ((equal c "7")
               (set-frame-alpha 70))
              ((equal c "8")
               (set-frame-alpha 80))
              ((equal c "9")
               (set-frame-alpha 90))
              ((equal c "0")
               (set-frame-alpha 100))
              ((equal c "\C-g")
               (message "Quit")(throw 'end-flag t)))))))

;;リサイズ用関数
(defun resize ()
  "Control frame and window size."
  (interactive)
  (let (c)
    (catch 'end-flag
      (while t
        (let ((window-obj (selected-window))
              (dx (if (= (nth 0 (window-edges)) 0) 1
                    -1))
              (dy (if (= (nth 1 (window-edges)) 0) 1
                    -1))
              )
        (message "Window[%dx%d]"
                 (window-width)
                 (window-height))
        (condition-case err
            (setq c (read-key-sequence nil))
          (error
           (throw 'end-flag t)))
        (cond ((equal c "f")
               (enlarge-window-horizontally dx))
              ((equal c "b")
               (shrink-window-horizontally dx))
              ((equal c "p")
               (shrink-window dy))
              ((equal c "n")
               (enlarge-window dy))
              ((equal c "\C-p")
               (windmove-up))
              ((equal c "\C-n")
               (windmove-down))
              ((equal c "\C-f")
               (windmove-right))
              ((equal c "\C-b")
               (windmove-left))
              ((equal c "\C-x0")
               (delete-window))
              ((equal c "\C-x1")
               (delete-other-window))
              ((equal c "\C-x2")
               (split-window-vertically))
              ((equal c "\C-x3")
               (split-window-horizontally))
              ((equal c "\C-x\C-b")
               (helm-mini))
              ((equal c "\C-g")
               (message "Quit")(throw 'end-flag t))))))))


(if window-system
    (progn
      (set-frame-alpha frame-alpha)
      (setq ns-pop-up-frames nil)
      (scroll-bar-mode 0)
      ;;(global-linum-mode t)
      (when (require 'yascroll nil t)
        (global-yascroll-bar-mode 1)
        )
      ))
(when tool-bar-mode (tool-bar-mode 0))
(when menu-bar-mode (menu-bar-mode 0))

;; サーバー起動
(server-start)

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
   `(mode-line-position-face ((,class (:inherit mode-line-face :family "Menlo"))))
   `(mode-line-mode-face ((,class (:inherit mode-line-face :foreground "#eeeeec"))))
   `(mode-line-minor-mode-face ((,class (:inherit mode-line-face :weight extra-light :height 0.8 :foreground "#cccccc"))))
   `(mode-line-process-face ((,class (:inherit mode-line-face :foreground "green"))))
   `(mode-line-80col-face ((,class (:inherit mode-line-position-face :foreground "black" :background "#eab700"))))
   `(mode-line-delim-face-1 ((,class (:inherit mode-line-face :foreground "white"))))
   `(mode-line-git-face ((,class (:inherit mode-line-face :foreground "green"))))
   `(mode-line-name-face ((,class (:inherit mode-line-face :foreground "#ed74cd"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground "#729fcf" :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#23d7d7"))))
   `(font-lock-comment-face ((,class (:foreground "#74af68"))))
   `(font-lock-constant-face ((,class (:foreground "#008b8b"))))
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
   `(elscreen-tab-current-screen-face ((,class (:foreground "#eeeeee" :background "#878787"))))
   `(elscreen-tab-other-screen-face ((,class (:foreground "#888888" :background "#2e2e2e"))))
   ;; Diff
   `(diff-file-header ((,class (:foreground "#eeeeee" :background "#222222"))))
   `(diff-header ((,class (:foreground "#eeeeee" :background "#555555"))))
   `(diff-added ((,class (:foreground "#00f900" :background nil))))
   `(diff-removed ((,class (:foreground "#ff2600" :background nil))))
   `(diff-context ((,class (:foreground "#eeeeee" :background nil))))
   `(magit-item-highlight ((,class (:background "#000000"))))
   `(magit-branch ((,class (:foreground "#34cae2" :background nil))))
   `(magit-log-sha1 ((,class (:foreground "#ff4242" :background nil))))
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

;; 全角色付け
(when (and (>= emacs-major-version 23)
           (require 'whitespace nil t))
  (setq whitespace-style
        '(face
          tabs spaces newline trailing space-before-tab space-after-tab
          space-mark tab-mark newline-mark))
  (let ((dark (eq 'dark (frame-parameter nil 'background-mode))))
    (set-face-attribute 'whitespace-space nil
                        :foreground (if dark "pink4" "azure3")
                        :background 'unspecified)
    (set-face-attribute 'whitespace-tab nil
                        :foreground (if dark "gray40" "gray80")
                        :background 'unspecified
                        :strike-through t)
    (set-face-attribute 'whitespace-newline nil
                        :foreground (if dark "darkcyan" "darkseagreen")
                        :height 0.8))
  (setq whitespace-space-regexp "\\(　+\\)")
  (setq whitespace-display-mappings
        '((space-mark   ?\xA0  [?\xA4]  [?_]) ; hard space - currency
          (space-mark   ?\x8A0 [?\x8A4] [?_]) ; hard space - currency
          (space-mark   ?\x920 [?\x924] [?_]) ; hard space - currency
          (space-mark   ?\xE20 [?\xE24] [?_]) ; hard space - currency
          (space-mark   ?\xF20 [?\xF24] [?_]) ; hard space - currency
          (space-mark   ?　    [?口]    [?＿]) ; full-width space - square
          (newline-mark ?\n    [?\x21B5 ?\n] [?$ ?\n])   ; eol - right quote mark
          ))
  (setq whitespace-global-modes '(not dired-mode tar-mode))
  (global-whitespace-mode 1))
(show-paren-mode t)

;; なんか色々色付け
(require 'generic-x)

;;
;; モードライン設定
;;----------------------------------------------------------------------------------------------------

;; 時刻の表示( 曜日 月 日 時間:分 )
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq display-time-string-forms
      '((format "%s/%s(%s)%s:%s"
                month day dayname
                24-hours minutes
                )))
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
           ((eq buffer-file-name nil)
            (propertize (buffer-name) 'face 'mode-line-filename-face))
           (t
            (concat
             (propertize (shorten-directory default-directory 20) 'face 'mode-line-folder-face)
             (propertize (buffer-name) 'face 'mode-line-filename-face)))
           ))
   ;; narrow [default -- keep?]
   "%n"
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
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


;;
;; フォント関係
;;----------------------------------------------------------------------------------------------------
(set-language-environment       "Japanese")
(prefer-coding-system           'utf-8-unix)
(setq                           default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system  'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(set-clipboard-coding-system    'utf-8)

;; Fontを指定
(set-face-attribute 'default nil :family "menlo" :height 120)
(if (display-graphic-p)
    (progn
      (if (eq system-type 'darwin)
          (progn
            (set-fontset-font
             (frame-parameter nil 'font)
             'japanese-jisx0208
             '("Hiragino Maru Gothic Pro" . "iso10646-1"))
            (set-fontset-font
             (frame-parameter nil 'font)
             'japanese-jisx0212
             '("Hiragino Maru Gothic Pro" . "iso10646-1"))
            (set-fontset-font
             (frame-parameter nil 'font)
             'mule-unicode-0100-24ff
             '("menlo" . "iso10646-1"))
            (setq face-font-rescale-alist
                  '(("^-apple-hiragino.*" . 1.1)
                    (".*courier-bold-.*-mac-roman" . 1.0)
                    (".*menlo cy-bold-.*-mac-cyrillic" . 0.9)
                    (".*menlo-bold-.*-mac-roman" . 0.9)
                    ("-cdac$" . 1.3)))))
      (if (eq system-type 'windows-nt)
          (progn
            (set-face-attribute 'default nil :family "MeiryoKe_Console" :height 100)
            (setq line-spacing 0)
            ))
      ))

;;
;; その他設定
;;----------------------------------------------------------------------------------------------------
;; window内のカーソル行番号
(defun current-line ()
  "Return the vertical position of point…"
  (+ (count-lines (window-start) (point))
     (if (= (current-column) 0) 1 0)
     -1))

;; 1画面スクロール
(defun scroll-up-with-cursor()
  "End of buffer までカーソル移動する scroll-up。"
  (interactive)
  (if(= (window-end) (point-max))
      (move-to-window-line (window-end))
    (scroll-up (window-body-height))))

(defun scroll-down-with-cursor()
  "Beginning of buffer までカーソル移動する scroll-down。"
  (interactive)
  (if(= (window-start) 1)
      (move-to-window-line 0)
    (scroll-down (window-body-height))))

;; 引数取得用関数
(defun region-or-read-string (prompt &optional initial history default inherit)
  "regionが指定されているときはその文字列を取得し、それ以外では`read-string'を呼ぶ。"
  (if (not (region-active-p))
      (read-string prompt initial history default inherit)
    (prog1
        (buffer-substring-no-properties (region-beginning) (region-end))
      (deactivate-mark)
      (message ""))))

;; OS Xの辞書を引く
(defun look-up-in-OS-X-dictionary (str)
  (interactive (list
                (region-or-read-string "Look up in OS X dictionary: ")))
  (let ((url (concat "dict://" str)))
    (browse-url url)))

;; imenu自動再読み込み
(setq imenu-auto-rescan t)
(setq imenu-after-jump-hook (lambda () (recenter 10)))

;; クリップボード共有
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(if window-system
    (setq x-select-enable-clipboard t)
  (if (eq system-type 'darwin)
      (progn
        (setq interprogram-cut-function 'paste-to-osx)
        (setq interprogram-paste-function 'copy-from-osx)
        )
    )
  )

;; ロックファイルを作らない
(setq create-lockfiles nil)

;; pcompleteにgit追加
(require 'pcmpl-git)

;; ispellの代わりにaspellを使う
(setq ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; re-builderの設定をstringに
(require 're-builder)
(setq reb-re-syntax 'string)

;; ホットローダー
(global-auto-revert-mode 1)

;; ファイル履歴
(recentf-mode 1)
(setq recentf-max-menu-items 20)
(setq recentf-max-saved-items 100)

;; バックアップを残さない
;; (setq make-backup-files nil)
;; (setq auto-save-default nil)

;; ファイルを閉じたとき、次に開くときはその場所(point)から開く
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
;; backupファイル保存先
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backups/"))
            backup-directory-alist))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))
(setq auto-save-list-file-prefix "~/.emacs.d/backups/")

;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(custom-set-variables '(read-file-name-completion-ignore-case t))

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
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(c-set-offset 'case-label '+)

;; Narrowing
(put 'narrow-to-region 'disabled nil)

;; 行番号表示
;;(global-linum-mode)
(setq linum-format "%4d")

;; スタートページ非表示
(setq inhibit-startup-message t)

;; 新しいel,elcを読み込み
(setq load-prefer-newer t)

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

;; ファイルを管理者権限で開く
(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

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

;; (add-hook 'after-save-hook
;;           ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
;;           (lambda ()
;;             (unless (member (get-buffer "*scratch*") (buffer-list))
;;               (my-make-scratch 1))))

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

;; 一つ前のバッファへトグル
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; git関係便利関数
(defun chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))

(defun git-project-p ()
  (string=
   (chomp
    (shell-command-to-string "git rev-parse --is-inside-work-tree"))
   "true"))

(defun git-root-directory ()
  (cond ((git-project-p)
         (chomp
          (shell-command-to-string "git rev-parse --show-toplevel")))
        (t
         "")))

;; grep,ag
(defun my-ag (arg &optional topdir)
  (interactive "sgrep find: ")
  (let ((command))
    (if topdir
        (cd topdir))
    (setq command "ag --nocolor --nogroup --ignore-case --line-numbers ")
    (setq command (concat command arg))
    (grep-find command)))
(defun popup-my-ag-buffer ()
  (interactive)
  (let ((buffer (get-buffer "*grep*")))
    (cond ((and current-prefix-arg buffer)
           (pop-to-buffer buffer))
          (t
           (popwin:close-popup-window)
           (call-interactively 'my-ag)))
    )
  )

;; Tramp設定
(setenv "TMPDIR" "/tmp")

(defadvice tramp-handle-vc-registered (around tramp-handle-vc-registered-around activate)
  (let ((vc-handled-backends '(SVN Git))) ad-do-it))

;;
;; gtags
;;----------------------------------------------------------------------------------------------------
(defun gtags-get-rootpath ()
  (let (path buffer)
    (save-excursion
      (setq buffer (generate-new-buffer (generate-new-buffer-name "*rootdir*")))
      (set-buffer buffer)
      (setq n (process-file "global" nil t nil "-pr"))
      (if (= n 0)
          (setq path (file-name-as-directory (buffer-substring (point-min)(1- (point-max))))))
      (kill-buffer buffer))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p default-directory))
        (with-parsed-tramp-file-name default-directory tr
          (when path
            (concat (substring default-directory 0 (* -1 (length tr-localname)))
                    path)))
      path)))
(defun update-gtags (&optional prefix)
  (interactive "P")
  (let ((rootdir (gtags-get-rootpath))
        (args (if prefix "-v" "-iv")))
    (when rootdir
      (let* ((default-directory rootdir)
             (buffer (get-buffer-create "*update GTAGS*")))
        (save-excursion
          (set-buffer buffer)
          (erase-buffer)
          (let ((result (process-file "gtags" nil buffer nil args)))
            (if (= 0 result)
                (message "GTAGS successfully updated.")
              (message "update GTAGS error with exit status %d" result))))))))
;;(add-hook 'after-save-hook 'update-gtags)

;;
;; view-mode
;;----------------------------------------------------------------------------------------------------
(setq view-read-only t)
(defvar view-mode-jump-to-definition
  (lambda ()
    (interactive)
    (tss-jump-to-definition)))
(defun jump-to-definition ()
  (interactive)
  (call-interactively view-mode-jump-to-definition)
  (read-only-mode)
  )
(defvar pager-keybind
      `( ;; vi-like
        ("h" . backward-word)
        ("l" . forward-to-word)
        ("j" . (lambda (arg) (interactive "p") (scroll-up     arg)))
        ("k" . (lambda (arg) (interactive "p") (scroll-down   arg)))
        ("n" . next-line)
        ("p" . previous-line)
        ("b" . backward-char)
        ("f" . forward-char)
        ("u" . scroll-down)
        ("d" . scroll-up)
        (" " . scroll-up)
        ("o" . jump-to-definition)
        ("i" . pop-tag-mark)
        ))
(defun define-many-keys (keymap key-table &optional includes)
  (let (key cmd)
    (dolist (key-cmd key-table)
      (setq key (car key-cmd)
            cmd (cdr key-cmd))
      (if (or (not includes) (member key includes))
        (define-key keymap key cmd))))
  keymap)

(defun view-mode-hook0 ()
  (define-many-keys view-mode-map pager-keybind)
  (define-key view-mode-map " " 'scroll-up))
(add-hook 'view-mode-hook 'view-mode-hook0)

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
;; split-root.el
;;----------------------------------------------------------------------------------------------------
(require 'split-root)
(defvar split-root-window-height nil)
(defun display-buffer-function--split-root (buf &optional ignore)
  (let ((window (split-root-window split-root-window-height)))
    (set-window-buffer window buf)
    window))
(setq helm-display-function 'display-buffer-function--split-root)

;;
;; popwin.el
;;----------------------------------------------------------------------------------------------------
(when (require 'popwin nil t)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config '(
                                        (direx:direx-mode :position left :width 40 :dedicated t)
                                        (occur-mode :position bottom :height 0.5)
                                        (ag-mode :position bottom :height 0.5)
                                        ))
  )

;;
;; direx.el
;;----------------------------------------------------------------------------------------------------
(when (require 'direx nil t)
  (defun direx:jump-to-git-project-directory ()
    (interactive)
    (let* ((git-root-dir))
      (setq git-root-dir (git-root-directory))
      (unless (string= git-root-dir "")
        (direx:find-directory-noselect git-root-dir))
      (direx:jump-to-directory-other-window)))
  (setq direx:leaf-icon "  ")
  (setq direx:open-icon "▾ ")
  (setq direx:closed-icon "▸ ")
  (add-hook 'direx:direx-mode-hook '(lambda () (hl-line-mode) ))
  (define-key direx:direx-mode-map (kbd "n") 'direx:next-sibling-item)
  (define-key direx:direx-mode-map (kbd "p") 'direx:previous-sibling-item)
  (define-key direx:direx-mode-map (kbd "u") 'direx:up-item)
  (define-key direx:direx-mode-map (kbd "d") 'direx:down-item)
  (define-key direx:direx-mode-map (kbd "q") 'delete-window)
  )

;;
;; dired.el
;;----------------------------------------------------------------------------------------------------
(when (require 'dired nil t)
  ;; dired-find-alternate-file の有効化
  (put 'dired-find-alternate-file 'disabled nil)
  ;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
  (setq dired-dwim-target t)
  ;; ディレクトリを再帰的にコピーする
  (setq dired-recursive-copies 'always)
  ;; diredバッファでC-sした時にファイル名だけにマッチするように
  (setq dired-isearch-filenames t)
  ;; ファイルなら別バッファで、ディレクトリなら同じバッファで開く
  (defun dired-open-in-accordance-with-situation ()
    (interactive)
    (cond ((string-match "\\(\\.\\.\\)"
                         (format "%s" (thing-at-point 'filename)))
           (dired-find-alternate-file))
          ((file-directory-p (dired-get-filename))
           (dired-find-alternate-file))
          (t
           (dired-find-file))))
  ;; RET 標準の dired-find-file では dired バッファが複数作られるので
  ;; dired-find-alternate-file を代わりに使う
  ;; また左右キーでディレクトリの昇り降り
  (define-key dired-mode-map (kbd "RET") 'dired-open-in-accordance-with-situation)
  (define-key dired-mode-map (kbd "a")   'dired-find-file)
  (define-key dired-mode-map (kbd "^")   (lambda () (interactive) (find-alternate-file "..")))
  (define-key dired-mode-map (kbd "C-b") (lambda () (interactive) (find-alternate-file "..")))
  (define-key dired-mode-map (kbd "C-f") 'dired-open-in-accordance-with-situation)
  (define-key dired-mode-map (kbd "e")   'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "M-s f")     nil)
  ;; lsの設定
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t)
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
  (require 'elscreen-separate-buffer-list)
  (elscreen-start)
  )

;;
;; multiple-cursor.el
;;----------------------------------------------------------------------------------------------------
(when (require 'multiple-cursors nil t)
  (define-key mc/mark-more-like-this-extended-keymap (kbd "C-p") 'mc/mmlte--up)
  (define-key mc/mark-more-like-this-extended-keymap (kbd "C-n") 'mc/mmlte--down)
  (define-key mc/mark-more-like-this-extended-keymap (kbd "C-b") 'mc/mmlte--left)
  (define-key mc/mark-more-like-this-extended-keymap (kbd "C-f") 'mc/mmlte--right)
  (require 'phi-search-migemo)
  (define-key mc/keymap (kbd "C-s") 'phi-search-migemo)
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
                  ((equal c (kbd "C-p"))
                   (previous-line))
                  ((equal c (kbd "C-n"))
                   (next-line))
                  ((equal c (kbd "C-f"))
                   (forward-char))
                  ((equal c (kbd "C-b"))
                   (backward-char))
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

;; magit.el
;;----------------------------------------------------------------------------------------------------
(when (require 'magit nil t)
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun my/magit-quit-session ()
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)
    (if git-gutter+-mode
        (if (and (git-gutter+-file-buffer-p)
                 (git-gutter+-in-git-repository-p (buffer-file-name)))
            (git-gutter+-refresh)
          )))

  (define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)

  (defadvice git-commit-commit (after move-to-magit-buffer activate)
    (delete-window))
  )

;;
;; multi-term.el
;;----------------------------------------------------------------------------------------------------
(when (require 'multi-term nil t)
  (setenv "TERMINFO" "~/.terminfo")
  (setq multi-term-program "zsh")
  (if (eq system-type 'windows-nt)
      (setq multi-term-program "bash.exe"))
  (add-to-list 'term-unbind-key-list '"M-x")
  (add-to-list 'term-unbind-key-list '"C-t")
  (defun init-multi-term (number)
    (let ((mt-buffer-name "*terminal<1>*"))
      (multi-term)
      (with-current-buffer mt-buffer-name
        (rename-buffer (format "*screen terminal<%d>*" number))))
    )
  (defun switch-to-multi-term ()
    (interactive)
    (let* ((screen-number (elscreen-get-current-screen))
           (buffer (get-buffer (format "*screen terminal<%d>*" screen-number))))
      (cond ((equal buffer (current-buffer))
             (switch-to-buffer (other-buffer (current-buffer) 1)))
            (buffer
             (switch-to-buffer buffer))
            (t
             (init-multi-term screen-number))))
    )
  (defadvice elscreen-kill (before elscreen-kill-with-terminal activate)
    (let* ((screen-number (elscreen-get-current-screen))
           (buffer (get-buffer (format "*screen terminal<%d>*" screen-number))))
      (cond (buffer
             (kill-buffer buffer))))
    )
  (defadvice term-send-return (after check-cd-action activate)
    (let* ((cmd (term-get-old-input-default))
           (match (string-match-p "$ cd" cmd)))
      (when match
        (let ((dir (substring cmd (+ 4 (string-match "$ cd" cmd)))))
          (if (equal (length dir) 0)
              (setq dir "~")
            (setq dir (substring dir 1)))
          (cd dir)))
      ))
  (defun term-send-current-directory(&optional dir)
    (interactive)
    (let* ((screen-number (elscreen-get-current-screen))
           (mt-buffer-name (buffer-name (get-buffer (format "*screen terminal<%d>*" screen-number)))))
      (unless dir (setq dir (chomp (shell-command-to-string "pwd"))))
      (comint-send-string (get-buffer-process mt-buffer-name) (format "cd %s\n" dir)))
    )
  (defun term-send-cd ()
    (interactive)
    (call-interactively 'cd)
    (term-send-current-directory)
    )
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
               (define-key term-raw-map (kbd "C-f") 'term-send-forward-char)
               (define-key term-raw-map (kbd "C-b") 'term-send-backward-char)
               (define-key term-raw-map (kbd "C-p") 'term-send-previous-line)
               (define-key term-raw-map (kbd "C-n") 'term-send-next-line)
               (define-key term-raw-map (kbd "C-y") 'term-paste)
               (define-key term-raw-map (kbd "M-d") 'term-send-forward-kill-word)
               (define-key term-raw-map (kbd "M-<backspace>") 'term-send-backward-kill-word)
               (define-key term-raw-map (kbd "M-DEL") 'term-send-backward-kill-word)
               (define-key term-raw-map (kbd "M-h") 'term-send-backward-kill-word)
               (define-key term-raw-map (kbd "C-t") 'switch-to-multi-term)
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
  )

;;
;; undo-tree.el
;;----------------------------------------------------------------------------------------------------
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  )

;;
;; auto-complete.el
;;----------------------------------------------------------------------------------------------------
(when (require 'auto-complete nil t)
  (require 'auto-complete-config)

  ;; キーバインド
  (setq ac-use-menu-map t)
  (define-key ac-menu-map (kbd "C-n")   'ac-next)
  (define-key ac-menu-map (kbd "C-p")   'ac-previous)
  (define-key ac-menu-map (kbd "TAB")   'ac-next)
  (define-key ac-menu-map (kbd "S-TAB") 'ac-previous)
  (define-key ac-mode-map (kbd "M-/")   'auto-complete)
  (ac-set-trigger-key "TAB")
  ;; 自動的に補完開始
  (setq ac-auto-start t)
  ;; 補完メニューを自動表示
  (setq ac-auto-show-menu t)
  ;; 最適なカラム計算をオフ
  ;;(setq popup-use-optimized-column-computation nil)
  ;; ツールチップの表示なし
  (setq ac-use-quick-help nil)
  ;; do i what mean
  (setq ac-dwim t)
  ;; 大文字小文字を区別しない
  (setq ac-ignore-case t)
  ;; 補完候補を自動展開しない
  (setq ac-expand-on-auto-complete nil)
  ;; font-lock
  (setq ac-disable-faces '(font-lock-comment-face font-lock-doc-face))
  ;; 起動モード
  (global-auto-complete-mode t)
  (add-to-list 'ac-modes 'web-mode)
  (add-to-list 'ac-modes 'js2-mode)
  (add-to-list 'ac-modes 'text-mode)
  (add-to-list 'ac-modes 'org-mode)
  (add-to-list 'ac-modes 'fundamental-mode)
  (add-to-list 'ac-modes 'markdown-mode)
  ;; 辞書ファイル
  (add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs.d/ac-dict/"))
  (setq ac-comphist-file (expand-file-name "~/.emacs.d/ac-comphist.dat"))
  ;; ユーザ辞書
  (defvar ac-user-dict-dir (expand-file-name "~/.emacs.d/ac-user-dict/"))

  ;; 情報源
  (setq-default ac-sources
    '(ac-source-filename
      ac-source-yasnippet
      ac-source-abbrev
      ac-source-dictionary
      ac-source-words-in-same-mode-buffers))

  ;; js-mode
  (defun ac-js-mode-setup ()
    (setq-local ac-dictionary-files '("~/.emacs.d/ac-dict/javascript-mode" "~/.emacs.d/ac-dict/javascript-mode")))
  (add-hook 'js-mode-hook 'ac-js-mode-setup)
  (add-hook 'js2-mode-hook 'ac-js-mode-setup)

  ;; lisp-mode
  (defun ac-lisp-mode-setup ()
    (setq-local ac-sources (append '(ac-source-symbols) ac-sources)))
  (add-hook 'emacs-lisp-mode-hook 'ac-lisp-mode-setup)

  ;; css-mode
  (defun ac-css-mode-setup ()
    (setq-local ac-sources (append '(ac-source-css-property) ac-sources)))
  (add-hook 'css-mode-hook 'ac-css-mode-setup)

  ;; (when (require 'auto-complete-latex nil t)
  ;;    (setq ac-l-dict-directory "~/.emacs.d/elisp/auto-complete/ac-l-dict/")
  ;;    (add-to-list 'ac-modes 'latex-mode)
  ;;    (add-hook 'LaTeX-mode-hook 'ac-l-setup))
  )

;;
;; helm.el
;;----------------------------------------------------------------------------------------------------
(when (require 'helm-config nil t)
  (require 'helm-descbinds)
  ;;(require 'helm-migemo)
  ;;(setq helm-use-migemo t)
  (require 'helm-C-x-b)
  (require 'helm-filelist)
  (require 'helm-gtags)

  (helm-mode 1)
  (setq helm-samewindow nil)
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
  ;; helmで置き換えない
  ;; (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))
  ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
  ;; (add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
  ;; (add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil))
  ;; (add-to-list 'helm-completing-read-handlers-alist '(ag . nil))

  ;; buffer名の表示幅
  (setq helm-buffer-max-length 40)

  ;; kill-ring
  (setq kill-ring-max 50)

  ;; 遅延を短く
  (setq helm-idle-delay 0.1)
  (setq helm-input-idle-delay 0.1)
  (setq helm-candidate-number-limit 200)

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

  ;; sortを無効化
  (defadvice helm-buffers-sort-transformer (around ignore activate)
  (setq ad-return-value (ad-get-arg 0)))

  ;; helm buffer list改
  (defvar normal-buffer-list '())
  (defvar dired-buffer-list '())
  (defvar tmp-buffer-list '())
  (defclass helm-source-normal-buffers (helm-source-sync helm-type-buffer)
    ((init :initform (lambda ()
                       ;; Issue #51 Create the list before `helm-buffer' creation.
                       (setq helm-buffers-list-cache (helm-buffer-list))
                       (let ((result (cl-loop for b in helm-buffers-list-cache
                                              maximize (length b) into len-buf
                                              maximize (length (with-current-buffer b
                                                                 (symbol-name major-mode)))
                                              into len-mode
                                              finally return (cons len-buf len-mode))))
                         (unless helm-buffer-max-length
                           (setq helm-buffer-max-length (car result)))
                         (unless helm-buffer-max-len-mode
                           ;; If a new buffer is longer that this value
                           ;; this value will be updated
                           (setq helm-buffer-max-len-mode (cdr result))))
                       (cl-loop for i in helm-buffers-list-cache
                                with normal-local
                                with dired-local
                                with tmp-local
                                if (= 0 (or (string-match-p "\\*.+\\*" i) -1)) collect i into tmp-local
                                else if (with-current-buffer (get-buffer i) (eq major-mode 'dired-mode)) collect i into dired-local
                                else collect i into normal-local end
                                finally
                                (setq normal-buffer-list normal-local)
                                (setq dired-buffer-list dired-local)
                                (setq tmp-buffer-list tmp-local))
                       ))
     (candidates :initform normal-buffer-list)
     (matchplugin :initform nil)
     (match :initform 'helm-buffers-list--match-fn)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (keymap :initform helm-buffer-map)
     (volatile :initform t)
     (mode-line :initform helm-buffer-mode-line-string)
     (persistent-help
      :initform
      "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))
  (defvar helm-source-normal-buffers-list (helm-make-source "Buffers" 'helm-source-normal-buffers))

  (defclass helm-source-dired-buffers (helm-source-sync helm-type-buffer)
    ((candidates :initform dired-buffer-list)
     (matchplugin :initform nil)
     (match :initform 'helm-buffers-list--match-fn)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (keymap :initform helm-buffer-map)
     (volatile :initform t)
     (mode-line :initform helm-buffer-mode-line-string)
     (persistent-help
      :initform
      "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))
  (defvar helm-source-dired-buffers-list (helm-make-source "Dired Buffers" 'helm-source-dired-buffers))

  (defclass helm-source-tmp-buffers (helm-source-sync helm-type-buffer)
    ((candidates :initform tmp-buffer-list)
     (matchplugin :initform nil)
     (match :initform 'helm-buffers-list--match-fn)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (keymap :initform helm-buffer-map)
     (volatile :initform t)
     (mode-line :initform helm-buffer-mode-line-string)
     (persistent-help
      :initform
      "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))
  (defvar helm-source-tmp-buffers-list (helm-make-source "* Buffers" 'helm-source-tmp-buffers))

  (defclass helm-source-recentf+ (helm-source-sync)
    ((init :initform (lambda ()
                       (require 'recentf)
                       (recentf-mode 1)))
     (candidates :initform (lambda () recentf-list))
     (pattern-transformer :initform 'helm-recentf-pattern-transformer)
     (match-part :initform (lambda (candidate)
                             (if (or helm-ff-transformer-show-only-basename
                                     helm-recentf--basename-flag)
                                 (helm-basename candidate) candidate)))
     (filter-one-by-one :initform (lambda (c)
                                    (let ((directory-abbrev-alist `((,(concat "\\`" (getenv "HOME")) . "~"))))
                                      (if helm-ff-transformer-show-only-basename
                                          (cons (helm-basename c) c)
                                        (abbreviate-file-name c)))))
     (keymap :initform helm-generic-files-map)
     (help-message :initform helm-generic-file-help-message)
     (mode-line :initform helm-generic-file-mode-line-string)
     (action :initform (helm-actions-from-type-file))))
  (defvar helm-source-recentf+-list (helm-make-source "Recentf" 'helm-source-recentf+))

  (defun helm-filelist++ ()
    (interactive)
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm-other-buffer
       `(helm-source-normal-buffers-list
         helm-source-dired-buffers-list
         helm-source-tmp-buffers-list
         helm-source-recentf+-list
         ,(helm-source-filelist))
       "*helm filelist++*")))
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

  ;; 既存スニペットを挿入する
  (define-key yas-minor-mode-map (kbd "C-c s i") 'yas-insert-snippet)
  ;; 新規スニペットを作成するバッファを用意する
  (define-key yas-minor-mode-map (kbd "C-c s n") 'yas-new-snippet)
  ;; 既存スニペットを閲覧・編集する
  (define-key yas-minor-mode-map (kbd "C-c s v") 'helm-yas-visit-snippet-file)

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
;; Org-mode
;;----------------------------------------------------------------------------------------------------
(when (require 'org-install)
  ;; 見出しの余分な*を消す
  (setq org-hide-leading-stars t)
  ;; 画面端で改行する
  (setq org-startup-truncated nil)
  ;; 見出しを畳んで表示する
  (setq org-startup-folded nil)
  ;; returnでリンクを飛ぶ
  (setq org-return-follows-link t)
  (setq org-directory "~/org/")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-capture-templates
      '(
        ("m" "Memo" entry (file+headline nil "Memos") "** %?\n   %T")
        ("M" "Memo(with file link)" entry (file+headline nil "Memos") "** %?\n   %a\n   %T")
        ))
  ;; agendaを使う
  (setq org-agenda-files (list org-directory))

  ;; org-capture-memo
  (defun org-capture-memo (n)
    (interactive "p")
    (case n
      (4 (org-capture nil "M"))
      (t (org-capture nil "m"))))

  ;; code-reading
  (defvar org-code-reading-software-name nil)
  ;; ~/memo/code-reading.org に記録する
  (defvar org-code-reading-file "code-reading.org")
  (defun org-code-reading-read-software-name ()
    (set (make-local-variable 'org-code-reading-software-name)
         (read-string "Code Reading Software: " 
                      (or org-code-reading-software-name
                          (file-name-nondirectory
                           (buffer-file-name))))))

  (defun org-code-reading-get-prefix (lang)
    (let ((sname (org-code-reading-read-software-name))
          (fname (file-name-nondirectory
                           (buffer-file-name))))
    (concat "[" lang "]"
            "[" sname "]"
            (if (not (string= sname fname)) (concat "[" fname "]")))))
  (defun org-capture-code-reading ()
    (interactive)
    (let* ((prefix (org-code-reading-get-prefix (substring (symbol-name major-mode) 0 -5)))
           (org-capture-templates
            '(("c" "Code Reading" entry (file+headline (concat org-directory org-code-reading-file) "Code Readings") "** %(identity prefix) %?\n   %a\n   %T")
              )))
      (org-capture nil "c")))
  )

;;
;; flycheck.el
;;----------------------------------------------------------------------------------------------------
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;;
;; go-mode.el
;;----------------------------------------------------------------------------------------------------
(when (require 'go-mode nil t)
  (require 'go-autocomplete)
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
(when (require 'typescript nil t)
  (require 'tss)
  ;; (tss-config-default)から抜粋したtss設定
  (loop for mode in tss-enable-modes
        for hook = (intern-soft (concat (symbol-name mode) "-hook"))
        do (add-to-list 'ac-modes mode)
        if (and hook
                (symbolp hook))
        do (add-hook hook 'tss-setup-current-buffer t))
  (add-hook 'kill-buffer-hook 'tss--delete-process t)
  (defvar tss-defun-beginning-regexp "{")
  (defvar tss-defun-end-regexp "}")
  (defun tss-beginning-of-defun (&optional arg)
    (interactive "p")
    (end-of-line 1)
    (let ((pos (point))
          (bgn)
          (end))
      (re-search-backward tss-defun-end-regexp nil t)
      (if (= pos (point))
          (setq end 0)
        (setq end (1- (point))))
      (goto-char pos)
      (re-search-backward tss-defun-beginning-regexp nil t)
      (setq bgn (1- (point)))
      (while (> end bgn)
        (goto-char end)
        (re-search-backward tss-defun-end-regexp nil t)
        (if (= end (point))
            (setq end 0)
          (setq end (1- (point))))
        (goto-char bgn)
        (re-search-backward tss-defun-beginning-regexp nil t)
        (setq bgn (1- (point))))
      )
    (beginning-of-line)
    (back-to-indentation)
    nil)
  (defun tss-end-of-defun (&optional arg)
    (interactive "p")
    (tss-beginning-of-defun 1)
    (re-search-forward tss-defun-beginning-regexp nil t)
    (backward-char)
    (forward-list)
    (backward-char)
    nil)
  (defun typescript-mode-init ()
    (set (make-local-variable 'compile-command)
         (format "tsc -sourcemap %s"
                 (file-name-nondirectory (buffer-file-name))
                 )
         )
    (setq-local beginning-of-defun-function 'tss-beginning-of-defun)
    (setq-local end-of-defun-function 'tss-end-of-defun)
    (setq-local open-paren-in-column-0-is-defun-start nil)
    )
  (add-hook 'typescript-mode-hook 'typescript-mode-init)
  (define-key typescript-mode-map (kbd "C-c C-j") 'tss-jump-to-definition)
  (define-key typescript-mode-map (kbd "C-c r l") 'tss-reload-current-project)
  (define-key typescript-mode-map (kbd "C-c r s") 'tss-restart-current-buffer)
  )

;;
;; coffee-mode.el
;;----------------------------------------------------------------------------------------------------
(when (require 'coffee nil t))

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
;; AUCTeX
;;----------------------------------------------------------------------------------------------------
(setq TeX-default-mode 'japanese-latex-mode)
(setq japanese-LaTeX-default-style "jsarticle")
(setq japanese-LaTeX-command-default "pdfpLaTex")
(setq-default indent-tabs-mode nil) ; タブでインデント
(setq-default TeX-newline-function 'newline-and-indent)
(setq preview-image-type 'dvipng)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)

;; reftexの設定
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
; RefTeXで使用するbibファイルの位置を指定する
(setq reftex-default-bibliography '("~/.emacs.d/tex/references.bib"))

(setq TeX-engine-alist '((ptex "pTeX" "eptex" "platex" "eptex")
                         (uptex "upTeX" "euptex" "uplatex" "euptex")))
(setq TeX-engine 'ptex)
(setq TeX-view-program-list '(("open dvi" "/usr/bin/open -a Preview.app %s.pdf")
                              ("open pdf" "/usr/bin/open -a Preview.app %o")))
(setq TeX-view-program-selection '((output-pdf "open pdf")
                                   (output-dvi "open dvi")))
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook
          (function (lambda ()
                      (add-to-list 'TeX-command-list
                                   '("pdfpLaTeX" "platex %S %(mode) %t && pbibtex %s  && dvipdfmx %d"
                                     TeX-run-TeX nil (latex-mode) :help "Run pLaTeX and dvipdfmx"))
                      ;; (add-to-list 'TeX-command-list
                      ;;              '("pdfpLaTeX2" "platex %S %(mode) %t && dvips -Ppdf -z -f %d | convbkmk -g > %f && ps2pdf %f"
                      ;;                TeX-run-TeX nil (latex-mode) :help "Run pLaTeX, dvips, and ps2pdf"))
                      (add-to-list 'TeX-command-list
                                   '("Latexmk" "latexmk %t"
                                     TeX-run-TeX nil (latex-mode) :help "Run Latexmk"))
                      (add-to-list 'TeX-command-list
                                   '("pBibTeX" "pbibtex %s"
                                     TeX-run-BibTeX nil t :help "Run pBibTeX"))
                      (add-to-list 'TeX-command-list
                                   '("jBibTeX" "pbibtex %s"
                                     TeX-run-BibTeX nil t :help "Run pBibTeX"))
                      (add-to-list 'TeX-command-list
                                   '("open" "open %s.pdf"
                                     TeX-run-discard-or-function t t :help "open pdf file")))))

;;
;; php-mode.el
;;----------------------------------------------------------------------------------------------------
(require 'php-mode)
(setq php-mode-force-pear t)

;;
;; web-mode.el
;;----------------------------------------------------------------------------------------------------
(when (require 'web-mode)
  (defun my-web-mode-hook ()
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset    4)
    (setq web-mode-code-indent-offset   4)
    (setq web-mode-style-padding  2)
    (setq web-mode-script-padding 2)
    (setq web-mode-block-padding  0)
    (setq web-mode-enable-auto-pairing nil)
    (setq web-mode-enable-css-colorization t)
    (setq web-mode-ac-sources-alist
          '(("php" . (ac-source-filename
                      ac-source-yasnippet
                      ac-source-abbrev
                      ac-source-dictionary
                      ac-source-words-in-same-mode-buffers))
            ("html" . (ac-source-filename
                      ac-source-yasnippet
                      ac-source-abbrev
                      ac-source-dictionary
                      ac-source-words-in-same-mode-buffers))
            ("css" . (ac-source-css-property
                      ac-source-filename
                      ac-source-yasnippet
                      ac-source-abbrev
                      ac-source-dictionary
                      ac-source-words-in-same-mode-buffers))))
    (add-hook 'web-mode-before-auto-complete-hooks
              '(lambda ()
                 (let ((web-mode-cur-language
                        (web-mode-language-at-pos)))
                   (ac-clear-dictionary-cache)
                   (if (string= web-mode-cur-language "php")
                       (progn
                         (yas-activate-extra-mode 'php-mode)
                         (setq-local ac-dictionary-files '("~/.emacs.d/ac-dict/php-mode")))
                     (yas-deactivate-extra-mode 'php-mode))
                   (if (string= web-mode-cur-language "html")
                       (progn
                         (yas-activate-extra-mode 'html-mode)
                         (setq-local ac-dictionary-files '("~/.emacs.d/ac-dict/html-mode")))
                     (yas-deactivate-extra-mode 'html-mode))
                   (if (string= web-mode-cur-language "javascript")
                       (progn
                         (yas-activate-extra-mode 'js2-mode)
                         (setq-local ac-dictionary-files '("~/.emacs.d/ac-dict/js2-mode" "~/.emacs.d/ac-dict/javascript-mode")))
                     (yas-deactivate-extra-mode 'js2-mode))
                   (if (string= web-mode-cur-language "css")
                       (progn
                         (setq-local ac-dictionary-files '("~/.emacs.d/ac-dict/css-mode")))
                     )
                 )))
    )
  (add-hook 'web-mode-hook  'my-web-mode-hook)
)

;;
;; twittering-mode.el
;;----------------------------------------------------------------------------------------------------
(when (require 'twittering-mode nil t)
  (setq twittering-use-master-password t)
  (setq twittering-private-info-file "~/.emacs.d/twittering-mode.gpg")

  (setq twittering-icon-mode t)
  (setq twittering-convert-fix-size 30)
  (setq twittering-timer-interval 120)
  (setq twittering-status-format "%RT{RT by %S@%s\n}%i %S@%s %p: %@\n%T\n------------------------------------------------------------")
  (defadvice twittering-update-status-from-pop-up-buffer (around split-root activate)
    ""
    (let ((display-buffer-function 'display-buffer-function--split-root)
          (split-root-window-height 26))
      ad-do-it))
  )
;;
;; git-gutter.el
;;----------------------------------------------------------------------------------------------------
(when (require 'git-gutter+ nil t)
  (when window-system
    (require 'git-gutter-fringe+))
  (global-git-gutter+-mode t)
  (define-key git-gutter+-mode-map (kbd "C-c n") 'git-gutter+-next-hunk)
  (define-key git-gutter+-mode-map (kbd "C-c p") 'git-gutter+-previous-hunk)
  (define-key git-gutter+-mode-map (kbd "C-c d") 'git-gutter+-popup-hunk)
  (defadvice git-gutter+-process-diff (before git-gutter+-process-diff-advice activate)
    (ad-set-arg 0 (file-truename (ad-get-arg 0))))
  )

;;
;; eww.el
;;----------------------------------------------------------------------------------------------------
(when (require 'eww nil t)
  (defun eww-update-header-line-format:override(&rest _))
  (advice-add 'eww-update-header-line-format :override 'eww-update-header-line-format:override)
  (defun eww-render:tab-update (&rest _) (elscreen-tab-update t))
  (advice-add 'eww-render :after 'eww-render:tab-update)
  (defvar eww-data)
  (defun eww-current-url ()
    "バージョン間の非互換を吸収する。"
    (if (boundp 'eww-current-url)
        eww-current-url                   ;emacs24.4
      (plist-get eww-data :url)))         ;emacs25
  (defun eww-current-title ()
    "バージョン間の非互換を吸収する。"
    (if (boundp 'eww-current-title)
        eww-current-title                   ;emacs24.4
      (plist-get eww-data :title)))

  ;; [2014-11-17 Mon]背景・文字色を無効化する
  (defvar eww-disable-colorize t)
  (defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
    (unless eww-disable-colorize
      (funcall orig start end fg)))
  (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
  (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
  (defun eww-disable-color ()
    "ewwで文字色を反映させない"
    (interactive)
    (setq-local eww-disable-colorize t)
    (eww-reload))
  (defun eww-enable-color ()
    "ewwで文字色を反映させる"
    (interactive)
    (setq-local eww-disable-colorize nil)
    (eww-reload))

  ;; 画像を表示させない
  (defun eww-disable-images ()
    "ewwで画像表示させない"
    (interactive)
    (setq-local shr-put-image-function 'shr-put-image-alt)
    (eww-reload))
  (defun eww-enable-images ()
    "ewwで画像表示させる"
    (interactive)
    (setq-local shr-put-image-function 'shr-put-image)
    (eww-reload))
  (defun shr-put-image-alt (spec alt &optional flags)
    (insert alt))
  (defun eww-mode-hook--disable-image ()
    (setq-local shr-put-image-function 'shr-put-image-alt))
  (add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)

  ;; ewwのhelm history
  (require 'helm)
  (require 'cl-lib)

  (defun helm-eww-history-candidates ()
    (cl-loop with hash = (make-hash-table :test 'equal)
             for b in (buffer-list)
             when (eq (buffer-local-value 'major-mode b) 'eww-mode)
             append (with-current-buffer b
                      (clrhash hash)
                      (puthash (eww-current-url) t hash)
                      (cons
                       (cons (format "%s (%s) <%s>" (eww-current-title) (eww-current-url) b) b)
                       (cl-loop for pl in eww-history
                                unless (gethash (plist-get pl :url) hash)
                                collect
                                (prog1 (cons (format "%s (%s) <%s>" (plist-get pl :title) (plist-get pl :url) b)
                                             (cons b pl))
                                  (puthash (plist-get pl :url) t hash)))))))
  (defun helm-eww-history-browse (buf-hist)
    (if (bufferp buf-hist)
        (switch-to-buffer buf-hist)
      (switch-to-buffer (car buf-hist))
      (eww-save-history)
      (eww-restore-history (cdr buf-hist))))
  (defvar helm-source-eww-history
    '((name . "eww history")
      (candidates . helm-eww-history-candidates)
      (migemo)
      (action . helm-eww-history-browse)))
  (defvaralias 'anything-c-source-eww-history 'helm-source-eww-history)
  (defun helm-eww-history ()
    (interactive)
    (helm :sources 'helm-source-eww-history
          :buffer "*helm eww*"))

  (define-key eww-mode-map (kbd "H") 'helm-eww-history)

  ;; weblio検索
  (defun eww-set-start-at (url-regexp search-regexp)
    "URL-REGEXPにマッチするURLにて、SEARCH-REGEXPの行から表示させる"
    (when (string-match url-regexp (eww-current-url))
      (goto-char (point-min))
      (when (re-search-forward search-regexp nil t)
        (recenter 0))))

  (defun eww-render--after (&rest _)
    (eww-set-start-at "www.weblio.jp" "^ *Weblio 辞書")
    (eww-set-start-at "ejje.weblio.jp" "^ *Weblio 辞書")
    )
  (advice-add 'eww-render :after 'eww-render--after)

  ;; weblio
  (defun weblio (str)
    (interactive (list
                  (region-or-read-string "Weblio: ")))
    (eww-browse-url (format "http://www.weblio.jp/content/%s"
                            (upcase (url-hexify-string str)))))

  ;; weblio-english
  (defun weblio-english (str)
    (interactive (list
                  (region-or-read-string "Weblio[English]: ")))
    (eww-browse-url (format "http://ejje.weblio.jp/content/%s"
                            (upcase (url-hexify-string str)))))

  ;; wikipedia
  (defun wikipedia (str)
    (interactive (list
                  (region-or-read-string "Wikipedia: ")))
    (eww-browse-url (format "http://ja.wikipedia.org/wiki/%s"
                            (upcase (url-hexify-string str)))))
  )

;;
;; マイナーモードの省略
;;----------------------------------------------------------------------------------------------------
(setcar (cdr (assq 'helm-gtags-mode minor-mode-alist)) " GT")
(setcar (cdr (assq 'abbrev-mode minor-mode-alist)) " Ab")
(setcar (cdr (assq 'flymake-mode minor-mode-alist)) " FM")
(setcar (cdr (assq 'yas-minor-mode minor-mode-alist)) " YS")
(setcar (cdr (assq 'undo-tree-mode minor-mode-alist)) "")
(setcar (cdr (assq 'rainbow-mode minor-mode-alist)) "")
(setcar (cdr (assq 'git-gutter+-mode minor-mode-alist)) "")
(setcar (cdr (assq 'helm-mode minor-mode-alist)) "")
(setcar (cdr (assq 'whitespace-mode minor-mode-alist)) "")
(setcar (cdr (assq 'global-whitespace-mode minor-mode-alist)) "")
(setcar (cdr (assq 'magit-auto-revert-mode minor-mode-alist)) "")

;;
;; bracketed paste
;;----------------------------------------------------------------------------------------------------
(require 'bracketed-paste)
(bracketed-paste-enable)

;;
;; migemo.el
;;----------------------------------------------------------------------------------------------------
(when (require 'migemo)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (cond ((eq system-type 'windows-nt)
         (setq migemo-command (concat (getenv "INST_DIR")
                                      "\\app\\cmigemo\\cmigemo.exe"))
         (setq migemo-dictionary (concat (getenv "INST_DIR")
                                         "\\app\\cmigemo\\dict\\utf-8\\migemo-dict"))
         ))
  (load-library "migemo")
  (migemo-init)
  )

