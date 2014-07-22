;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ------------------------------------------------------------------------
;; @ load-path

(require 'cl)

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

;;; package.el
(when (require 'package nil t)
  ;; パッケージリポジトリにMarmaladeを追加
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  )

;; load environment value
(require 'exec-path-from-shell)
(let ((envs '("PATH" "VIRTUAL_ENV" "GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))
;;(exec-pathth-from-shell-initialize)

;;
;; キーバインド
;;______________________________________________________________________
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "C-r")     'replace-string)
(global-set-key (kbd "C-t")     'switch-to-multi-term)
(global-set-key (kbd "C-z")     'switch-to-previous-buffer)
(global-set-key (kbd "C--")     'undo-tree-undo)

(global-set-key (kbd "M-g")     'goto-line)
(global-set-key (kbd "M-h")     'backward-kill-word)
(global-set-key (kbd "M-n")     (kbd "C-u 5 C-n"))
(global-set-key (kbd "M-p")     (kbd "C-u 5 C-p"))
(global-set-key (kbd "M-x")     'anything-M-x)
(global-set-key (kbd "M-y")     'anything-show-kill-ring)
(global-set-key (kbd "M-;")     'comment-or-uncomment-region)
(global-set-key (kbd "M--")     'undo-tree-redo)

(global-set-key (kbd "C-x b")   'ah:menu-command)
(global-set-key (kbd "C-x e")   'resize)
(global-set-key (kbd "C-x g")   'magit-status)
(global-set-key (kbd "C-x p")   'popup-grep-buffer)
(global-set-key (kbd "C-x , ,") 'howm-menu)

(global-set-key (kbd "C-x C-b") 'anything-filelist+)
(global-set-key (kbd "C-x C-i") 'direx:jump-to-git-project-directory)
(global-set-key (kbd "C-x C-j") 'dired-jump-other-window)

(global-set-key (kbd "C-M-b")   'windmove-left)
(global-set-key (kbd "C-M-f")   'windmove-right)
(global-set-key (kbd "C-M-n")   'windmove-down)
(global-set-key (kbd "C-M-p")   'windmove-up)
(global-set-key (kbd "C-M-r")   'foreign-regexp/query-replace)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "C-t") 'switch-to-multi-term)))

;; トラックパッド用のスクロール設定
(if window-system
    (progn
      (defun scroll-down-with-lines () "" (interactive) (scroll-down 3))
      (defun scroll-up-with-lines () "" (interactive) (scroll-up 3))
      (global-set-key [wheel-up] 'scroll-down-with-lines)
      (global-set-key [wheel-down] 'scroll-up-with-lines)
      (global-set-key [double-wheel-up] 'scroll-down-with-lines)
      (global-set-key [double-wheel-down] 'scroll-up-with-lines)
      (global-set-key [triple-wheel-up] 'scroll-down-with-lines)
      (global-set-key [triple-wheel-down] 'scroll-up-with-lines)
      )
  )
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 2)))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up   2)))
(global-set-key [mouse-8] '(lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-9] '(lambda () (interactive) (scroll-up   1)))
(global-set-key [mouse-20] '(lambda () (interactive) (scroll-down (/ (window-height) 2))))
(global-set-key [mouse-21] '(lambda () (interactive) (scroll-up   (/ (window-height) 2))))

;;
;; ウィンドウ設定
;;______________________________________________________________________

(if window-system
    (progn
      (set-frame-parameter nil 'alpha 85)
      (setq ns-pop-up-frames nil)
      (scroll-bar-mode 0)
      ;;(global-linum-mode t)
      (when (require 'yascroll nil t)
        (global-yascroll-bar-mode 1)
        )
      ))
(tool-bar-mode 0)
(menu-bar-mode 0)

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
               (anything-filelist+))
              ((equal c "\C-g")
               (message "Quit")(throw 'end-flag t))))))))

;; サーバー起動
(server-start)

;;
;; テーマの読み込み
;;----------------------------------------------------------------------------------------------------
(let ((class '((class color) (min-colors 89))))
(custom-set-faces
   ;; Ensure sufficient contrast on 256-color xterms.
   `(default ((((class color) (min-colors 4096))
       ;; (:background "#313131" :foreground "#e1e1e0"))
       ;; (,class
       ;; (:background "#2a2a2a" :foreground "#e1e1e0"))))
       (:background "#2e2e2e" :foreground "#e1e1e0"))
       (,class
       (:background "#2e2e2e" :foreground "#e1e1e0"))))
   `(cursor ((,class (:background "#a4a4a4"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#2e3748"))))
   `(highlight ((,class (:background "#035f56"))))
   `(region ((,class (:background "#2d4948" :foreground "#000000"))))
   `(isearch ((,class (:background "#fcffad" :foreground "#000000"))))
   `(lazy-highlight ((,class (:background "#338f86"))))
   `(trailing-whitespace ((,class (:background "#ff4242"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "#0B2087" :foreground "#eeeeec"))))
   `(mode-line-inactive
     ((,class (:background "#878787" :foreground "#eeeeec"))))
   `(header-line ((,class (:background "#e5e5e5" :foreground "#333333"))))
   `(mode-line-name ((,class (:foreground "#ed74cd"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground "#729fcf" :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#23d7d7"))))
   `(font-lock-comment-face ((,class (:foreground "#74af68"))))
   `(font-lock-constant-face ((,class (:foreground "#008b8b"))))
   `(font-lock-function-name-face
     ((,class (:foreground "#00ede1" :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground "#ffad29" :weight bold))))
   `(font-lock-string-face ((,class (:foreground "#e67128"))))
   `(font-lock-type-face ((,class (:foreground "#34cae2"))))
   `(font-lock-variable-name-face ((,class (:foreground "#dbdb95"))))
   `(font-lock-warning-face ((,class (:foreground "#ff4242" :weight bold))))
   ;; Buttons and links
   `(button ((,class (:underline t))))
   `(link ((,class (:foreground "#59e9ff" :underline t))))
   `(link-visited ((,class (:foreground "#ed74cd" :underline t))))
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
   ))

;; term
(defface term-color-black
  '((t (:foreground "black" :background "black")))
  "Unhelpful docstring.")
(defface term-color-red
  '((t (:foreground "red3" :background "red3")))
  "Unhelpful docstring.")
(defface term-color-green
  '((t (:foreground "green3" :background "green3")))
  "Unhelpful docstring.")
(defface term-color-yellow
  '((t (:foreground "yellow3" :background "yellow3")))
  "Unhelpful docstring.")
(defface term-color-blue
  '((t (:foreground "DeepSkyBlue" :background "DeepSkyBlue")))
  "Unhelpful docstring.")
(defface term-color-magenta
  '((t (:foreground "magenta1" :background "magenta1")))
  "Unhelpful docstring.")
(defface term-color-cyan
  '((t (:foreground "cyan3" :background "cyan3")))
  "Unhelpful docstring.")
(defface term-color-white
  '((t (:foreground "white" :background "white")))
  "Unhelpful docstring.")
'(term-default-fg-color ((t (:inherit term-color-white))))
'(term-default-bg-color ((t (:inherit term-color-black))))

;; ansi-term colors
(setq ansi-term-color-vector
  [term term-color-black term-color-red term-color-green term-color-yellow
    term-color-blue term-color-magenta term-color-cyan term-color-white])

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

(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#4271ae")
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#c82829"
                    :background "#ffffff")
(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-face
                    :weight 'extra-light
                    :height 0.8
                    :foreground "#e5e5e5")
(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :foreground "#eab700"
                    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    :family "Menlo")
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :foreground "#eeeeec")
(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "white"
                    :height 0.8)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :foreground "green")
(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "black" :background "#eab700")
(set-face-attribute 'mode-line-delim-face-1 nil
                    :inherit 'mode-line-face
                    :foreground "white")
(set-face-attribute 'mode-line-git-face nil
                    :inherit 'mode-line-face
                    :foreground "green")
(set-face-attribute 'mode-line-name-face nil
                    :inherit 'mode-line-face
                    :foreground "#ed74cd")

(global-font-lock-mode t)
;;(global-hl-line-mode)

;; 全角色付け
(global-whitespace-mode 1)
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

;;
;; モードライン設定
;;---------------------------------------------------------------------------

;; git-status
(require 'git-status)

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
 '("%e"
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
   ;evil-mode-line-tag
   mode-line-position
   ;; directory and buffer/file name
   (:eval (cond
           ((string= (substring (buffer-name) 0 1) "*")
            (propertize (buffer-name) 'face 'mode-line-filename-face))
           (t
            (concat
             (propertize (shorten-directory default-directory 20) 'face 'mode-line-folder-face)
             (propertize (buffer-name) 'face 'mode-line-filename-face)))
           ))
   ;; narrow [default -- keep?]
   " %n"
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
   " "
   (:propertize user-login-name face mode-line-name-face)
   (:propertize "@" face mode-line-name-face)
   (:propertize system-name face mode-line-name-face)
   " "
   (global-mode-string global-mode-string)
   ;; " "
   ;; nyan-mode uses nyan cat as an alternative to %p
   ;; (:eval (when nyan-mode (list (nyan-create))))
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
;;______________________________________________________________________
(set-language-environment       "Japanese")
(prefer-coding-system           'utf-8-unix)
(setq                           default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system  'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(set-clipboard-coding-system    'utf-8)

;; Fontを指定
(set-face-attribute 'default nil
                    :family "menlo"
                    :height 120)
(if (display-graphic-p)
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
                  ("-cdac$" . 1.3))))))

;;
;; その他設定
;;----------------------------------------------------------------------------------------------------

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
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)
  )

;; xterm-mouse-mode
(unless (fboundp 'track-mouse)
  (defun track-mouse (e)))
(xterm-mouse-mode t)
(require 'mouse)
(require 'mwheel)
(mouse-wheel-mode t)

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
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(custom-set-variables '(read-file-name-completion-ignore-case t))

;; 同名ファイルをフォルダで識別
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; ベル音
(setq ring-bell-function 'ignore)

;; スクロール
;;(setq scroll-step 1)
(setq scroll-conservatively 2)

;; 列番号
(column-number-mode t)

;; 折り返ししない
;;(setq truncate-lines t)
;;(setq truncate-partial-width-windows t)

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

;; Trampバッファにユーザ名、ホスト名を追加
(defun tramp-my-append-buffer-name-hint ()
  "Append a hint (user, hostname) to a buffer name if visiting
file is a remote file (include directory)."
  (let ((name (or list-buffers-directory (buffer-file-name))))
    (when (and name (tramp-tramp-file-p name))
      (let* ((tramp-vec (tramp-dissect-file-name name))
             (method (tramp-file-name-method tramp-vec))
             (host (tramp-file-name-real-host tramp-vec))
             (user (or (tramp-file-name-real-user tramp-vec)
                       (nth 2 (assoc method tramp-default-user-alist))
                       tramp-default-user
                       user-real-login-name)))
        (rename-buffer (concat (buffer-name) " <" user "@" host ">") t)))))
(add-hook 'find-file-hook 'tramp-my-append-buffer-name-hint)
(add-hook 'dired-mode-hook 'tramp-my-append-buffer-name-hint)

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

(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))

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

;; grep
(defun my-ag (arg &optional topdir)
  (interactive "sgrep find: ")
  (let ((command))
    (if topdir
        (cd topdir))
    (setq command "ag --nocolor --nogroup --ignore-case --line-numbers ")
    (setq command (concat command arg))
    (grep-find command)))
(defun popup-grep-buffer ()
  (interactive)
  (let ((buffer (get-buffer "*grep*")))
    (cond ((and current-prefix-arg buffer)
           (pop-to-buffer buffer))
          (t
           (popwin:close-popup-window)
           (call-interactively 'my-ag)))
    )
  )

;;
;; パッケージ関係
;;----------------------------------------------------------------------------------------------------

;; popwin.el
;;----------------------------------------------------------------------------------------------------
(when (require 'popwin nil t)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq anything-samewindow nil)
  (setq popwin:special-display-config '(
                                        ("^\*anything.*\*$" :regexp t :height 0.5)
                                        ("*grep*" :height 0.5)
                                        (direx:direx-mode :position left :width 40 :dedicated t)
                                        ))
  )

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
  (setq direx:leaf-icon "  "
        direx:open-icon "▾ "
        direx:closed-icon "▸ ")
  (add-hook 'direx:direx-mode-hook '(lambda ()
                                      (hl-line-mode)
                                      ))
  (define-key direx:direx-mode-map (kbd "n") 'direx:next-sibling-item)
  (define-key direx:direx-mode-map (kbd "p") 'direx:previous-sibling-item)
  (define-key direx:direx-mode-map (kbd "u") 'direx:up-item)
  (define-key direx:direx-mode-map (kbd "d") 'direx:down-item)
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
  ;; diredバッファでrを押すと編集モード
  (add-hook 'dired-load-hook (lambda ()
                               (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))
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
  ;; lsの設定
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t)
  )

;; magit.el
;;----------------------------------------------------------------------------------------------------
(when (require 'magit nil t)
  ;; 色変更
  (set-face-foreground 'magit-diff-add "#b9ca4a")
  (set-face-foreground 'magit-diff-del "#d54e53")
  (set-face-background 'magit-item-highlight "#000000")
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun my/magit-quit-session ()
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)

  (defadvice git-commit-commit (after move-to-magit-buffer activate)
    (delete-window))
  )

;;
;; coffee-mode.el
;;----------------------------------------------------------------------------------------------------
(when (require 'coffee nil t)
  (add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
  (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
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

  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;;
;; multi-term.el
;;----------------------------------------------------------------------------------------------------
(when (require 'multi-term nil t)
  (setenv "TERMINFO" "~/.terminfo")
  (setq multi-term-program "/bin/zsh")
  (add-to-list 'term-unbind-key-list '"M-x")
  (add-to-list 'term-unbind-key-list '"C-t")
  (defun switch-to-multi-term ()
    (interactive)
    (let ((buffer (get-buffer "*terminal<1>*")))
      (cond ((equal buffer (current-buffer))
             (switch-to-buffer (other-buffer (current-buffer) 1)))
            (buffer
             (switch-to-buffer buffer))
            (t
             (multi-term))))
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
               ))
  )

;;
;; typescript.el
;;----------------------------------------------------------------------------------------------------
(when (require 'typescript nil t)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (require 'tss)
  ;; (tss-config-default)から抜粋したtss設定
  (loop for mode in tss-enable-modes
        for hook = (intern-soft (concat (symbol-name mode) "-hook"))
        do (add-to-list 'ac-modes mode)
        if (and hook
                (symbolp hook))
        do (add-hook hook 'tss-setup-current-buffer t))
  (add-hook 'kill-buffer-hook 'tss--delete-process t)
  )
;; 識別子の正規表現
(defvar javascript-identifier-regexp "[a-zA-Z0-9.$_]+")

;; } までの class のメソッドを列挙する関数
(defun typescript-imenu-create-method-index-1 (class bound)
  (let (result)
    (while (re-search-forward (format "^ *\\(\\(static \\|private \\|public \\|export \\)+\\|\\)\\(%s\\)\\((.*)\\) *\\(: *[A-Z]%s+\\(<[A-Z]%s+>\\|\\) *\\|\\){"
                                      javascript-identifier-regexp
                                      javascript-identifier-regexp
                                      javascript-identifier-regexp) bound t)
      (push (cons (format "%s.%s"
                          class
                          (match-string 3)
                          (match-string 4)
                          (match-string 5)) (match-beginning 1)) result))
    (nreverse result)))

;; メソッドのインデックスを作成する関数
(defun typescript-imenu-create-method-index ()
  (cons "Methods"
        (let (result)
          ;; $name = Class.create
          ;; $name = Object.extend
          ;; Object.extend($name,
          ;; $name = {
          ;; をクラスあるいはオブジェクトとする
          (dolist (pattern (list (format "^ *export class \\([A-Z]%s\\)" javascript-identifier-regexp)
                                 (format "^ *class (\\([A-Z]%s\\)" javascript-identifier-regexp)
                                 ))
            (goto-char (point-min))
            (while (re-search-forward pattern (point-max) t)
              (save-excursion
                (condition-case nil
                    ;; { を探す
                    (let ((class (replace-regexp-in-string "\.prototype$" "" (match-string 1))) ;; .prototype はとっておく
                          (try 3))
                      (if (eq (char-after) ?\()
                          (down-list))
                      (if (eq (char-before) ?{)
                          (backward-up-list))
                      (forward-list)
                      (while (and (> try 0) (not (eq (char-before) ?})))
                        (forward-list)
                        (decf try))
                      (if (eq (char-before) ?}) ;; } を見つけたら
                          (let ((bound (point)))
                            (backward-list)
                            ;; メソッドを抽出してインデックスに追加
                            (setq result (append result (typescript-imenu-create-method-index-1 class bound))))))
                  (error nil)))))
          ;; 重複を削除しておく
          (delete-duplicates result :test (lambda (a b) (= (cdr a) (cdr b)))))))
(defun typescript-imenu-create-class-index ()
  (cons "Class"
         (let (result)
           (dolist (pattern (list
                             (format "^ *export class \\([A-Z]%s\\)" javascript-identifier-regexp)
                             (format "^ *class (\\([A-Z]%s\\)" javascript-identifier-regexp)
                             ))
             (goto-char (point-min))
             (while (re-search-forward pattern (point-max) t)
               (push (cons (match-string 1) (match-beginning 1)) result)))
           (nreverse result))))
(defun typescript-imenu-create-index ()
  (list
   (typescript-imenu-create-class-index)
   (typescript-imenu-create-method-index)
   ))
(add-hook 'typescript-mode-hook (lambda () (setq imenu-create-index-function 'typescript-imenu-create-index)))

;;
;; hl-line+.el
;;----------------------------------------------------------------------------------------------------
(when (require 'hl-line+ nil t)
  (toggle-hl-line-when-idle)
  (setq hl-line-idle-interval 3)
  (set-face-background 'hl-line "#035f56")
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
;; foreign regexp.el
;;----------------------------------------------------------------------------------------------------
(when (require 'foreign-regexp nil t)
  (custom-set-variables
   ;; 正規表現、perlかrubyを選択
   '(foreign-regexp/regexp-type 'perl) ;; Choose by your preference.
   '(reb-re-syntax 'foreign-regexp)) ;; Tell re-builder to use foreign regexp.
  )

;;
;; undo-tree.el
;;----------------------------------------------------------------------------------------------------
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  )

;;
;; markdown-mode.el
;;----------------------------------------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

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
  ;;(setq ac-dwim t)
  ;; 大文字小文字を区別しない
  (setq ac-ignore-case t)
  ;; 起動モード
  (global-auto-complete-mode t)
  (add-to-list 'ac-modes 'text-mode)
  (add-to-list 'ac-modes 'fundamental-mode)
  (add-to-list 'ac-modes 'web-mode)
  (add-to-list 'ac-modes 'typescript-mode)
  (add-to-list 'ac-modes 'css-mode)
  (add-to-list 'ac-modes 'php-mode)
  (add-to-list 'ac-modes 'org-mode)

  ;; 情報源
  (defvar my-ac-sources
    '(ac-source-filename
      ac-source-yasnippet
      ac-source-abbrev
      ac-source-dictionary
      ac-source-words-in-same-mode-buffers))

  ;; lisp-mode
  (defun ac-lisp-mode-setup ()
    (setq-default ac-sources (append '(ac-source-symbols) my-ac-sources)))
  (add-hook 'emacs-lisp-mode-hook 'ac-lisp-mode-setup)

  ;; css-mode
  (defun ac-css-mode-setup ()
    (setq-default ac-sources (append '(ac-source-css-property) my-ac-sources)))
  (add-hook 'css-mode-hook 'ac-css-mode-setup)

  ;; php-mode
  (require 'php-mode)
  (require 'php-completion)
  (defun ac-php-mode-setup ()
    (php-completion-mode t)
    (setq-default ac-sources (append '(ac-source-php-completion) my-ac-sources)))
  (add-hook 'php-mode-hook 'ac-php-mode-setup)

  ;; (when (require 'auto-complete-latex nil t)
  ;;    (setq ac-l-dict-directory "~/.emacs.d/elisp/auto-complete/ac-l-dict/")
  ;;    (add-to-list 'ac-modes 'latex-mode)
  ;;    (add-hook 'LaTeX-mode-hook 'ac-l-setup))
)

;;
;; anything.el
;;----------------------------------------------------------------------------------------------------
(when (require 'anything-config nil t)
  ;; キーバインド
  (setq anything-c-filelist-file-name "/tmp/all.filelist")
  ;;(setq anything-grep-candidates-fast-directory-regexp "^/tmp")

  ;; anything-kill-ring
  (setq kill-ring-max 50)
  (setq anything-kill-ring-threshold 5)

  ;; 遅延を短く
  (setq anything-idle-delay 0.1)
  (setq anything-input-idle-delay 0.1)

  ;; anything-git-grep
  (require 'anything-git-grep)
)

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


;; howm-mode
;;----------------------------------------------------------------------------------------------------
(setq howm-prefix "\C-x,")
(setq howm-view-title-header "*")
(setq howm-insert-date-format "<%s>")
(setq howm-date-format '"%Y-%m-%d %a")
(setq howm-date-regexp-grep "[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [SMTWF][uoehra][neduit]")
(setq howm-date-regexp "\\([1-2][0-9][0-9][0-9]\\)-\\([0-1][0-9]\\)-\\([0-3][0-9]\\) [SMTWF][uoehra][neduit]")
(setq howm-reminder-regexp-grep-format (concat "<" howm-date-regexp-grep "[- :0-9]*>%s"))
(setq howm-reminder-regexp-format (concat "\\(<" howm-date-regexp "[- :0-9]*>\\)\\(\\(%s\\)\\([0-9]*\\)\\)"))
(setq howm-reminder-today-format (format howm-insert-date-format howm-date-format))
(when (require 'howm nil t)
  (setq howm-menu-lang 'ja)
  (setq howm-keyword-case-fold-search t)
  (setq font-lock-verbose nil)
  (setq howm-history-file "~/howm/.howm-history")
  (setq howm-keyword-file "~/howm/.howm-keys")
  (setq howm-file-name-format "%Y/%Y%m%d-%H%M%S.org")
  (setq howm-menu-file "~/howm/menu.org")
  (setq howm-view-summary-persistent nil)
  (setq howm-prepend t)
  (setq howm-reminder-font-lock-keywords
        `(
          (,(howm-reminder-regexp "[-]") (0 howm-reminder-normal-face prepend))
          (,(howm-reminder-regexp "[+]") (0 howm-reminder-todo-face prepend))
          (,(howm-reminder-regexp "[~]") (0 howm-reminder-defer-face prepend))
          (,(howm-reminder-regexp "[!]") (0 howm-reminder-deadline-face prepend))
          (,(howm-reminder-regexp "[@]") (0 howm-reminder-schedule-face prepend))
          (,(howm-reminder-regexp "[.]") (0 howm-reminder-done-face prepend))
          ))
  (setq howm-dtime-format (concat "<" howm-dtime-body-format ">"))
  (setq howm-highlight-date-regexp-format "%Y-%m-%d %a\\([- :0-9]*\\)")
  (setq howm-template-date-format "<%Y-%m-%d %a %H:%M>")
  (setq howm-template-file-format ">>>%s")
  (setq howm-template "* %cursor\n%date\n")
  (setq howm-schedule-sort-by-time t)
  (add-hook 'org-mode-hook 'howm-mode)
  (if (not (memq 'delete-file-if-no-contents after-save-hook))
      (setq after-save-hook
            (cons 'delete-file-if-no-contents after-save-hook)))

  (defun delete-file-if-no-contents ()
    (when (and
           (buffer-file-name (current-buffer))
           (= (point-min) (point-max)))
      (when (y-or-n-p "Delete file and kill buffer?")
        (delete-file
         (buffer-file-name (current-buffer)))
        (kill-buffer (current-buffer)))))
  )

;;
;; Org-mode
;;----------------------------------------------------------------------------------------------------
;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)
;; 画面端で改行する
(setq org-startup-truncated nil)
;; 見出しを畳んで表示する
(setq org-startup-folded nil)
;; 画面端改行トグル関数
(defun change-truncation()
  (interactive)
  (cond ((eq truncate-lines nil)
         (setq truncate-lines t))
        (t
         (setq truncate-lines nil))))

;;
;; web-mode
;;----------------------------------------------------------------------------------------------------
(setq auto-mode-alist
      (append '(
                ("\\.\\(html\\|xhtml\\|shtml\\|tpl\\|hbs\\)\\'" . web-mode)
                ("\\.php\\'" . php-mode)
                )
              auto-mode-alist))
;; php-mode
(require 'php-mode)
(defun php-mode-hook ()
  (lambda ()
    (setq php-mode-force-pear t)
))
(add-hook 'php-mode-hook 'php-mode-hook)
;; web-mode
(require 'web-mode)
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset    2)
  (setq web-mode-code-indent-offset   4)
  (setq web-mode-style-padding  0)
  (setq web-mode-script-padding 0)
  (setq web-mode-block-padding  0)
  (setq web-mode-enable-auto-pairing nil)
  )
(add-hook 'web-mode-hook 'web-mode-hook)
;; 色の設定
(custom-set-faces
 '(web-mode-doctype-face
   ((t (:foreground "#82AE46"))))                          ; doctype
 '(web-mode-html-tag-face
   ((t (:foreground "#E6B422" :weight bold))))             ; 要素名
 '(web-mode-html-attr-name-face
   ((t (:foreground "#C97586"))))                          ; 属性名など
 '(web-mode-html-attr-value-face
   ((t (:foreground "#82AE46"))))                          ; 属性値
 '(web-mode-comment-face
   ((t (:foreground "#D9333F"))))                          ; コメント
 '(web-mode-server-comment-face
   ((t (:foreground "#D9333F"))))                          ; コメント
 '(web-mode-css-rule-face
   ((t (:foreground "#A0D8EF"))))                          ; cssのタグ
 '(web-mode-css-pseudo-class-face
   ((t (:foreground "#FF7F00"))))                          ; css 疑似クラス
 '(web-mode-css-at-rule-face
   ((t (:foreground "#FF7F00"))))                          ; cssのタグ
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
  (define-key yas-minor-mode-map (kbd "C-c s v") 'yas-visit-snippet-file)

  (defun my-yas/prompt (prompt choices &optional display-fn)
    (let* ((names (loop for choice in choices
                        collect (or (and display-fn (funcall display-fn choice))
                                    choice)))
           (selected (anything-other-buffer
                      `(((name . ,(format "%s" prompt))
                         (candidates . names)
                         (action . (("Insert snippet" . (lambda (arg) arg))))))
                      "*anything yas/prompt*")))
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
;; eshell.el
;;----------------------------------------------------------------------------------------------------
(require 'eshell)
(setq eshell-banner-message " 可愛い女の子だと思った？ 残念！Eshellちゃんでした！\n\n")
;; prompt文字列
(setq eshell-highlight-prompt nil)
(setq eshell-prompt-function
      (lambda ()
        (concat
         "[" (format-time-string "%Y/%m/%d %H:%M") "]" ;; 時間
         " "
         (propertize (let ((pwd (eshell/pwd))
                           (homestring (directory-file-name (expand-file-name (getenv "HOME"))))
                           )
                       (let ((homelength (length homestring))
                             (pwdlength (length pwd)))
                         (if (>= pwdlength homelength)
                             (let ((subhome (substring pwd 0 homelength)))
                               (if (string= subhome homestring)
                                   (concat "~" (substring pwd homelength (length pwd)))
                                 pwd
                                 )
                               )
                           pwd)
                         )) 'face '(:foreground "magenta" :weight bold))
         (curr-dir-git-branch-string (eshell/pwd))
         " \n"
         (user-login-name) "@" (system-name) ;; ユーザ名@ホスト名
         " "
         (if (= (user-uid) 0)
             "#"
           (propertize "$" 'face '(:foreground "#fff")))
         (propertize " " 'read-only t 'rear-nonsticky t)
         )))
;;(setq eshell-prompt-regexp "^[^#$]*[$#] ")
(setq eshell-prompt-regexp "\\(^[^$#]*[$#] \\)\\|\\(^mysql> \\)")

(defun gitroot ()
  (interactive)
  (cd (chomp
          (shell-command-to-string "git rev-parse --show-toplevel")))
  )

(require 'vc-git)
(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat "[git:"
              (if (> (length git-output) 0)
                    (concat
                     (substring git-output 0 -1)
                     (shell-command-to-string "[[ $(git status -s | grep -e '^.M') != \"\" ]] && echo -n \"+\"")
                     (shell-command-to-string "[[ $(git status -s | grep -e '^M') != \"\" ]]  && echo -n \"!\"")
                     (shell-command-to-string "[[ $(git status -s | grep -e '^\?\?') != \"\" ]]  && echo -n \"?\"")
                     ;;(shell-command-to-string "[[ $(git status | grep \"nothing to commit\") == \"\" ]] && echo -n \"*\"")
                    )
                "(no branch)")
              "]") 'face `(:foreground "green"))
      )))

;; Emacs 起動時に Eshell を起動
;;(add-hook 'after-init-hook  (lambda ()(eshell)))

;; 補完時に大文字小文字を区別しない
(setq eshell-cmpl-ignore-case t)
;; 確認なしでヒストリ保存
(setq eshell-ask-to-save-history (quote always))
;; 1コマンドごとにヒストリ保存
(defadvice eshell-send-input (after eshell-send-input-after-advice)
  (eshell-save-some-history)
  (eshell-save-some-last-dir))
(ad-activate 'eshell-send-input)
;; 補完時にサイクルする
;;(setq eshell-cmpl-cycle-completions t)
(setq eshell-cmpl-cycle-completions nil)
;;補完候補がこの数値以下だとサイクルせずに候補表示
;;(setq eshell-cmpl-cycle-cutoff-length 5)
;; 履歴で重複を無視する
(setq eshell-hist-ignoredups t)
;; 文字を入力すれば末尾へジャンプ
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-show-maximum-output t)
(setq comint-scroll-to-bottom-on-output 'all)

;; sudoのあとも補完可能に
(defun pcomplete/sudo ()
  "Completion rules for the `sudo' command."
  (let ((pcomplete-help "complete after sudo"))
    (pcomplete-here (pcomplete-here (eshell-complete-commands-list)))))

;; トグルする設定
(defun my-toggle-term ()
  "eshell と直前のバッファを行き来する。C-u 付きで呼ぶと 今いるバッファと同じディレクトリに cd して開く"
  (interactive)
  (let ((ignore-list '("*Help*" "*Minibuf-1*" "*Messages*" "*Completions*"
                       "*terminal<1>*" "*terminal<2>*" "*terminal<3>*"))
        (dir default-directory))
    (labels
        ((_my-toggle-term (target)
                          (if (null (member (buffer-name (second target)) ignore-list))
                              (if (equal eshell-buffer-name (buffer-name (window-buffer)))
                                  (switch-to-buffer (second target))
                                (switch-to-buffer eshell-buffer-name)
                                (when current-prefix-arg
                                  (cd dir)
                                  (eshell-interactive-print (concat "cd " dir "\n"))
                                  (eshell-emit-prompt)))
                            (_my-toggle-term (cdr target)))))
      (_my-toggle-term (buffer-list)))))

;; eshellを新規で開く
(defun eshell-add-new-buffer (arg)
  (interactive "p")
  (case arg
    (4 (let ((input (read-string "eshell buffer name: ")))
         (my-eshell-create input))
       )
    (t (my-eshell-create "")))
  )
(defun my-eshell-create (input)
  (let ((bname (if (string= input "") "" (concat "<" input ">"))))
    (let ((buf (generate-new-buffer (concat eshell-buffer-name bname))))
      (switch-to-buffer buf)
      (unless (fboundp 'eshell-mode)
        (error "`eshell-auto' must be loaded before Eshell can be used"))
      (unless (eq major-mode 'eshell-mode)
        (eshell-mode))
      buf
      ))
  )

;; ファイルの場所へcdする
(defun eshell-jump-to-current-directory ()
  (interactive)
  (let ((dir default-directory))
    (eshell)
    (cd dir)
    (eshell-interactive-print (concat "cd " dir "\n"))
    (eshell-emit-prompt))
  )

;; キーバインドの変更
(add-hook 'eshell-mode-hook
          '(lambda ()
             (progn
               (eshell/export "TERM" "xterm-256color")
               (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)
               (define-key eshell-mode-map [up] 'previous-line)
               (define-key eshell-mode-map [down] 'next-line)
               (define-key eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
               (define-key eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input)
               (define-key eshell-mode-map (kbd "C-c h") 'anything-eshell-history)
               (define-key eshell-mode-map (kbd "C-c p") 'anything-esh-pcomplete)
               )
             ))

;; エスケープシーケンスを削除
(require 'ansi-color)
(require 'eshell)
(defconst escape-drop-regexp
  "\\(=\\|>\\|[0-9]\\|\\[\\?[0-9]+[hl]\\)"
  "Regexp that matches ANSI control sequences to silently drop.")
(defun escape-remove-on-region (begin end)
  "Remove escape sequence from region"
  (let ((start-marker (copy-marker begin))
        (end-marker (copy-marker end)))
    ;; First, eliminate unrecognized ANSI control sequences.
    (save-excursion
      (goto-char start-marker)
      (while (re-search-forward escape-drop-regexp end-marker t)
        (replace-match "")))))
(add-to-list 'eshell-output-filter-functions '(lambda () (escape-remove-on-region eshell-last-output-start eshell-last-output-end)))

(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
                              eshell-last-output-end))
(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)


;;----------------------------------------------------------------------------------------------------
;; マイナーモードの省略
;;----------------------------------------------------------------------------------------------------
(setcar (cdr (assq 'abbrev-mode minor-mode-alist)) " Ab")
(setcar (cdr (assq 'undo-tree-mode minor-mode-alist)) " UT")
(setcar (cdr (assq 'flymake-mode minor-mode-alist)) " FM")
(setcar (cdr (assq 'rainbow-mode minor-mode-alist)) " RW")
(setcar (cdr (assq 'php-completion-mode minor-mode-alist)) " PC")
(setcar (cdr (assq 'yas-minor-mode minor-mode-alist)) " YS")

;;
;; anything-howm.el
;;----------------------------------------------------------------------------------------------------
(require 'anything-howm)
;; 「最近のメモ」をいくつ表示するか
(setq anything-howm-recent-menu-number-limit 600)
;; howm のデータディレクトリへのパス
(setq anything-howm-data-directory "~/howm")
(defvar anything-c-source-buffers-list-howm-title
  `((name . "Buffers")
    (candidates . anything-c-buffer-list)
    (real-to-display . ah:title-real-to-display)
    (type . buffer)
    (match anything-c-buffer-match-major-mode)
    (candidate-transformer anything-c-skip-boring-buffers
                           anything-c-highlight-buffers)
    (persistent-action . anything-c-buffers-list-persistent-action)
    (keymap . ,anything-c-buffer-map)
    (volatile)
    (mode-line . anything-buffer-mode-line-string)
    (persistent-help . "Show this buffer / C-u \\[anything-execute-persistent-action]: Kill this buffer")))

;;
;; migemo.el
;;-----------------------------------------------------------------------
(when (require 'migemo)
  (require 'anything-migemo)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
  ;; 自前の情報源の定義
  (defvar my-anything-sources nil)
  (setq my-anything-sources
        '(anything-c-source-ffap-line
          anything-c-source-ffap-guesser
          anything-c-source-buffers-list-howm-title
          anything-c-source-files-in-current-dir+
          anything-c-source-recentf
          anything-c-source-bookmarks
          anything-c-source-file-cache
          anything-c-source-filelist
          ))
  (global-set-key (kbd "C-x C-b")
                  (lambda()
                    "start my-anything"
                    (interactive)
                    (anything-migemo t my-anything-sources))
                  )
  )
