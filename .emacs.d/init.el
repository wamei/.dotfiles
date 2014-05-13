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
(load-file (expand-file-name "~/.emacs.d/elisp/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

;;
;; キーバインド
;;______________________________________________________________________

(global-set-key (kbd "C-c a")   'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(global-set-key (kbd "C-h")     'backward-delete-char)
(global-set-key (kbd "C-c d")   'delete-indentation)
(global-set-key (kbd "M-g")     'goto-line)
(global-set-key (kbd "C-S-i")   'indent-region)
(global-set-key (kbd "C-m")     'newline-and-indent)
(global-set-key (kbd "C-M-r")   'replace-regexp)
(global-set-key (kbd "C-r")     'replace-string)
;;(global-set-key (kbd "C-/")     'undo)
(global-set-key (kbd "M-;")     'comment-or-uncomment-region)
(global-set-key (kbd "C-c s")   'sr-speedbar-toggle)
(global-set-key (kbd "C-c p")   'sr-speedbar-select-window)

(global-set-key (kbd "C-x p")   'popwin:display-last-buffer)

;;(define-key global-map "\C-cl" 'org-store-link)
;;(define-key global-map "\C-ca" 'org-agenda)

;; フォーカス移動
;;(windmove-default-keybindings)
(global-set-key (kbd "C-<tab>")   'other-window)
(global-set-key (kbd "C-S-<tab>") (lambda()(interactive)(other-window -1)))
(global-set-key (kbd "C-S-p") 'windmove-up)
(global-set-key (kbd "C-S-n") 'windmove-down)
(global-set-key (kbd "C-S-f") 'windmove-right)
(global-set-key (kbd "C-S-b") 'windmove-left)

(global-set-key (kbd "C-c r") 'resize)

                                        ; Tetris key map
(defvar tetris-mode-map (make-sparse-keymap 'tetris-mode-map))
(define-key tetris-mode-map "s" 'tetris-start-game)
(defvar tetris-null-map (make-sparse-keymap 'tetris-null-map))
(define-key tetris-null-map "s" 'tetris-start-game)

;;
;; ウィンドウ設定
;;______________________________________________________________________

(if window-system
    (progn
      (set-frame-parameter nil 'alpha 95) ; 透明度
      (tool-bar-mode 0)                  ; ツールバー非表示
      (setq line-spacing 0.1)              ; 行間
      (setq ns-pop-up-frames nil)))

;;起動時のフレームの大きさ
(if window-system
    (progn
;;    (set-frame-position (selected-frame) 0 0)
      (set-frame-size (selected-frame) 178 52)))

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
(if window-system (server-start))


;;
;; テーマの読み込み
;;________________________________________________________________________
(let ((class '((class color) (min-colors 89))))
(custom-set-faces
   ;; Ensure sufficient contrast on 256-color xterms.
   `(default ((((class color) (min-colors 4096))
       (:background "#2d3743" :foreground "#e1e1e0"))
       (,class
       (:background "#3a3a3a" :foreground "#e1e1e0"))))
   `(cursor ((,class (:background "#819170"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#2e3748"))))
   `(highlight ((,class (:background "#035f56"))))
   `(region ((,class (:background "#2d4948" :foreground "#000000"))))
   `(isearch ((,class (:background "#fcffad" :foreground "#000000"))))
   `(lazy-highlight ((,class (:background "#338f86"))))
   `(trailing-whitespace ((,class (:background "#ff4242"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "#212931" :foreground "#eeeeec"))))
   `(mode-line-inactive
     ((,class (:background "#878787" :foreground "#eeeeec"))))
   `(header-line ((,class (:background "#e5e5e5" :foreground "#333333"))))
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
   `(message-separator ((,class (:foreground "#23d7d7"))))))

(global-hl-line-mode)

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
                        :foreground (if dark "darkcyan" "darkseagreen")))
  (setq whitespace-space-regexp "\\(　+\\)")
  (setq whitespace-display-mappings
        '((space-mark   ?\xA0  [?\xA4]  [?_]) ; hard space - currency
          (space-mark   ?\x8A0 [?\x8A4] [?_]) ; hard space - currency
          (space-mark   ?\x920 [?\x924] [?_]) ; hard space - currency
          (space-mark   ?\xE20 [?\xE24] [?_]) ; hard space - currency
          (space-mark   ?\xF20 [?\xF24] [?_]) ; hard space - currency
          (space-mark   ?　    [?□]    [?＿]) ; full-width space - square
          (newline-mark ?\n    [?\u21B5 ?\n] [?$ ?\n])   ; eol - right quote mark
          ))
  (setq whitespace-global-modes '(not dired-mode tar-mode))
  (global-whitespace-mode 1))
(show-paren-mode t)

;; スクロールバー非表示
;;(scroll-bar-mode 1)
;;(scroll-bar-mode 0)
;;(set-scroll-bar-mode nil)
;;(toggle-scroll-bar nil)
(if window-system
    (progn
      (require 'yascroll)
      (global-yascroll-bar-mode 1)
      )
  )
;;(scroll-bar-mode 0)
;;(require 'smooth-scroll)
;;(smooth-scroll-mode t)

;; speedbar設定
(when (require 'sr-speedbar)
  (setq sr-speedbar-right-side nil)
  (setq speedbar-directory-unshown-regexp "^\\'")
  (setq speedbar-use-images nil)
  (custom-set-variables
   '(speedbar-show-unknown-files t)
   )
)
;; (defun php-imenu-create-index ()
;;   (let (index)
;;     (goto-char (point-min))
;;     (while (re-search-forward "^[ \t]*\\(public function\\|private function\\)\s+\\(\\w+\\)" (point-max) t)
;;       (push (cons (match-string 2) (match-beginning 1)) index))
;;     (nreverse index)))

;; (add-hook 'php-mode-hook
;;           '(lambda ()
;;              (setq imenu-create-index-function 'php-imenu-create-index)))


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
(if (display-graphic-p)
    (when (>= emacs-major-version 23)
      (set-face-attribute 'default nil
                          :family "menlo"
                          :height 120)
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
              ("-cdac$" . 1.3))))
  )

;;
;; その他設定
;;______________________________________________________________________

;; クリップボード共有
(setq x-select-enable-clipboard t)

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
(global-linum-mode)
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

;; C-Ret で矩形選択
;; 詳しいキーバインド操作：http://dev.ariel-networks.com/articles/emacs/part5/
(cua-mode t)
(setq cua-enable-cua-keys nil)

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

;;
;; モードライン設定
;;---------------------------------------------------------------------------
;; 行数割合表示を総行数表示
;; (defvar my-lines-page-mode t)
;; (defvar my-mode-line-format)

;; (when my-lines-page-mode
;;   (setq my-mode-line-format "%d")
;;   (if size-indication-mode
;;       (setq my-mode-line-format (concat my-mode-line-format " of %%I")))
;;   (cond ((and (eq line-number-mode t) (eq column-number-mode t))
;;          (setq my-mode-line-format (concat my-mode-line-format " (%%l,%%c)")))
;;         ((eq line-number-mode t)
;;          (setq my-mode-line-format (concat my-mode-line-format " L%%l")))
;;         ((eq column-number-mode t)
;;          (setq my-mode-line-format (concat my-mode-line-format " C%%c"))))

;;   (setq mode-line-position
;;         '(:eval (format my-mode-line-format
;;                         (count-lines (point-max) (point-min))))))

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
   (:propertize "%4l" face mode-line-position-face)
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
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b" face mode-line-filename-face)
   ;; narrow [default -- keep?]
   " %n"
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
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
   user-login-name
   "@"
   system-name
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

(set-face-attribute 'mode-line nil
    :foreground "gray80" :background "gray15"
    :inverse-video nil
    :weight 'normal
    :height 1.0
    :box '(:line-width 2 :color "gray10" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray80" :background "gray40"
    :inverse-video nil
    :weight 'extra-light
    :height 1.0
    :box '(:line-width 2 :color "gray30" :style nil))
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

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :weight 'extra-light
    :height 0.8
    :foreground "gray90")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo")
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "white")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray60"
    :height 0.8)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")
(set-face-attribute 'mode-line-delim-face-1 nil
    :inherit 'mode-line-face
    :foreground "white")

;;
;; パッケージ関係
;;;______________________________________________________________________
;;
;; popwin.el
;;-----------------------------------------------------------------------
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-height 0.5)
;; anything
(setq anything-samewindow nil)
(push '("*anything*") popwin:special-display-config)
;; dired
(push '(dired-mode :position top) popwin:special-display-config)
;; grep
(push '("*grep*" :position right :noselect t) popwin:special-display-config)

;; dash-at-point.el
(autoload 'dash-at-point "dash-at-point"
          "Search the word at point with Dash." t nil)
(global-set-key (kbd "C-c f") 'dash-at-point)
(global-set-key (kbd "C-c C-f") 'dash-at-point-with-docset)

;;
;; migemo.el
;;-----------------------------------------------------------------------
(if (eq system-type 'darwin)
    (when (require 'migemo)
      (setq migemo-command "cmigemo")
      (setq migemo-options '("-q" "--emacs"))
      (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
      (setq migemo-user-dictionary nil)
      (setq migemo-regex-dictionary nil)
      (setq migemo-coding-system 'utf-8-unix)
      (load-library "migemo")
      (migemo-init)
      ))

;;
;; auto-complete.el
;;------------------------------------------------------------------------
(when (require 'auto-complete nil t)
  (require 'auto-complete-config)

  (global-auto-complete-mode t)
  (setq ac-use-quick-help nil)

  (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
  (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
  (define-key ac-complete-mode-map [tab] 'ac-next)
  (define-key ac-complete-mode-map [S-tab] 'ac-previous)
  (define-key ac-mode-map [M-tab] 'auto-complete)
  ;; 自動的に補完しない
  ;;(setq ac-auto-start 4)
  ;; 最適なカラム計算をオフ
  ;;(setq popup-use-optimized-column-computation nil)
  (setq ac-dwim t)
  (setq ac-ignore-case t)
  ;; lisp編集情報源
  (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
  ;; ファイル名情報
  (setq-default ac-sources '(ac-source-words-in-same-mode-buffers ac-source-filename))

  ;; (when (require 'auto-complete-latex nil t)
  ;;    (setq ac-l-dict-directory "~/.emacs.d/elisp/auto-complete/ac-l-dict/")
  ;;    (add-to-list 'ac-modes 'latex-mode)
  ;;    (add-hook 'LaTeX-mode-hook 'ac-l-setup))

)

;;
;; セッションの保持
;;-------------------------------------------------------------------------
;;(when (require 'session)
  ;; session.el との併用対策
;;  (setq session-save-print-spec '(t nil nil))
;;  (add-hook 'after-init-hook 'session-initialize)
;;)

;;
;; anything.el
;;-------------------------------------------------------------------------
(when (require 'anything-config nil t)
  ;; キーバインド
;;  (global-set-key (kbd "C-x C-b")     'anything-for-files)
  (global-set-key (kbd "C-x C-b")     'anything-filelist+)
  (global-set-key (kbd "C-x c f")     'anything-filelist+)
  (global-set-key (kbd "M-y") 'anything-show-kill-ring)
  (global-set-key (kbd "M-x") 'anything-M-x)
  (setq anything-c-filelist-file-name "/tmp/all.filelist")
  ;;(setq anything-grep-candidates-fast-directory-regexp "^/tmp")

  ;; anything-kill-ring
  (setq kill-ring-max 50)
  (setq anything-kill-ring-threshold 5)

  ;; 遅延を短く
  (setq anything-idle-delay 0.1)
  (setq anything-input-idle-delay 0.1)
)

;;
;; AUCTeX
;;______________________________________________________________________
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
;; Org-mode設定
;;________________________________________________________________________
;; org-modeでの強調表示を可能にする
;;(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook
          '(lambda ()
             (turn-on-font-lock)
             (local-set-key (kbd "C-<tab>") nil)
             ))
;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)

;; 画面端で改行しない
(setq org-startup-truncated nil)
;; 画面端改行トグル関数
(defun change-truncation()
  (interactive)
  (cond ((eq truncate-lines nil)
         (setq truncate-lines t))
        (t
         (setq truncate-lines nil))))

;;
;; web-mode設定
;;________________________________________________________________________
(setq auto-mode-alist
      (append '(
                ("\\.\\(html\\|xhtml\\|shtml\\|tpl\\)\\'" . web-mode)
                ("\\.php\\'" . php-mode)
                )
              auto-mode-alist))
;; php-mode
(require 'php-mode)
(defun php-mode-hook ()
  (lambda ()
    (setq php-mode-force-pear t)
    (require 'php-completion)
    (php-completion-mode t)
    (make-local-variable 'ac-sources)
    (setq ac-sources '(
                       ac-source-words-in-same-mode-buffers
                       ac-source-php-completion
                       ac-source-filename
                       ac-source-etags))
                       ))
(add-hook 'php-mode-hook 'php-mode-hook)
;; web-mode
(require 'web-mode)
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset    4)
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


;; flymake (Emacs22から標準添付されている)
(when (require 'flymake nil t)
  (global-set-key (kbd "C-c e") 'flymake-display-err-menu-for-current-line)
  ;; PHP用設定
  (when (not (fboundp 'flymake-php-init))
    ;; flymake-php-initが未定義のバージョンだったら、自分で定義する
    (defun flymake-php-init ()
      (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
             (local-file  (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
        (list "php" (list "-f" local-file "-l"))))
    (setq flymake-allowed-file-name-masks
          (append
           flymake-allowed-file-name-masks
           '(("\\.php[345]?$" flymake-php-init))))
    (setq flymake-err-line-patterns
          (cons
           '("\\(\\(?:Parse error\\|Fatal error\\|Warning\\): .*\\) in \\(.*\\) on line \\([0-9]+\\)" 2 3 nil 1)
           flymake-err-line-patterns)))
  ;; JavaScript用設定
  (when (not (fboundp 'flymake-javascript-init))
    ;; flymake-javascript-initが未定義のバージョンだったら、自分で定義する
    (defun flymake-javascript-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        ;;(list "js" (list "-s" local-file))
        (list "jsl" (list "-process" local-file))
        ))
    (setq flymake-allowed-file-name-masks
          (append
           flymake-allowed-file-name-masks
           '(("\\.json$" flymake-javascript-init)
             ("\\.js$" flymake-javascript-init))))
    (setq flymake-err-line-patterns
          (cons
           '("\\(.+\\)(\\([0-9]+\\)): \\(?:lint \\)?\\(\\(?:warning\\|SyntaxError\\):.+\\)" 1 2 nil 3)
           flymake-err-line-patterns)))
  (add-hook 'php-mode-hook
            '(lambda() (flymake-mode t)))
  (add-hook 'javascript-mode-hook
            '(lambda() (flymake-mode t))))

;; popup.el を使って tip として表示
;; (defun my-flymake-display-err-popup.el-for-current-line ()
;;   "Display a menu with errors/warnings for current line if it has errors and/or warnings."
;;   (interactive)
;;   (let* ((line-no            (flymake-current-line-no))
;;          (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
;;          (menu-data          (flymake-make-err-menu-data line-no line-err-info-list)))
;;     (if menu-data
;;       (popup-tip (mapconcat '(lambda (e) (nth 0 e))
;;                             (nth 1 menu-data)
;;                             "\n")))
;;     ))

;; (defun flymake-display-err-menu-for-current-line ()
;;   (interactive)
;;   (let* ((line-no             (flymake-current-line-no))
;;          (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no))))
;;     (when line-err-info-list
;;       (let* ((count           (length line-err-info-list))
;;              (menu-item-text  nil))
;;         (while (> count 0)
;;           (setq menu-item-text (flymake-ler-text (nth (1- count) line-err-info-list)))
;;           (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
;;                  (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
;;             (if file
;;                 (setq menu-item-text (concat menu-item-text " - " file "(" (format "%d" line) ")"))))
;;           (setq count (1- count))
;;           (if (> count 0) (setq menu-item-text (concat menu-item-text "\n")))
;;           )
;;         (popup-tip menu-item-text)))))
;; (defadvice flymake-mode (before post-command-stuff activate compile)
;;   (set (make-local-variable 'post-command-hook)
;;     (add-hook 'post-command-hook 'flymake-display-err-menu-for-current-line)))

;; ssh-agent.el
;;--------------------------------------------------------------
(require 'ssh-agent)

;;====================
;; Eshell
;;====================
(setq eshell-banner-message " 可愛い女の子だと思った？ 残念！Eshellちゃんでした！\n\n")
(require 'vc-git)
;; prompt文字列
(setq eshell-prompt-function
      (lambda ()
        (concat
;;         "[" (format-time-string "%Y/%m/%d(%a) %H:%M") "]" ;; 時間
;;         " "
         (user-login-name) "@" (system-name) ;; ユーザ名@ホスト名
         ":"
         (let ((pwd (eshell/pwd))
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
           ))
         " ("
         (vc-git-mode-line-string (eshell/pwd))
;;         (vc-git-state (eshell/pwd))
;;         "[" (vc-git-registered (eshell/pwd)) "]"
         ")"
         " "
         (if (= (user-uid) 0)
             "#"
           "$")
         " "
         )))
(setq eshell-prompt-regexp "^[^#$]*[$#] ")
;; Emacs 起動時に Eshell を起動
(add-hook 'after-init-hook  (lambda ()
                              (cd "~")
                              (eshell)
                              ))
;; emacs 起動時に eshell バッファも一つ用意する
;;(add-hook 'after-init-hook
;;          (lambda()
;;            (eshell)
;;            (switch-to-buffer "*scratch*")
;;          ))

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

;; promptのあとも補完可能に
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
                              (if (equal "*eshell*" (buffer-name (window-buffer)))
                                  (switch-to-buffer (second target))
                                (switch-to-buffer "*eshell*")
                                (when current-prefix-arg
                                  (cd dir)
                                  (eshell-interactive-print (concat "cd " dir "\n"))
                                  (eshell-emit-prompt)))
                            (_my-toggle-term (cdr target)))))
      (_my-toggle-term (buffer-list)))))
(global-set-key (kbd "C-z") 'my-toggle-term)

;; auto-complete.elの設定
;; (require 'pcomplete)
;; (add-to-list 'ac-modes 'eshell-mode)
;; (ac-define-source pcomplete
;;   '((candidates . pcomplete-completions)))
;; (defun my-ac-eshell-mode ()
;;   (setq ac-sources
;;      '(ac-source-pcomplete
;;        ac-source-filename
;;        ac-source-files-in-current-dir
;;        ac-source-words-in-buffer
;;        ac-source-dictionary)))

;; キーバインドの変更
(add-hook 'eshell-mode-hook
          '(lambda ()
             (progn
;;             (my-ac-eshell-mode)
               (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)
               (define-key eshell-mode-map [up] 'eshell-previous-matching-input-from-input)
               (define-key eshell-mode-map [down] 'eshell-next-matching-input-from-input)
               (define-key eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
               (define-key eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input)
               (define-key eshell-mode-map (kbd "M-p") 'previous-line)
               (define-key eshell-mode-map (kbd "M-n") 'next-line)
               (define-key eshell-mode-map (kbd "C-M-p") 'anything-eshell-history)
               (define-key eshell-mode-map (kbd "C-M-n") 'anything-esh-pcomplete)
;;             (define-key eshell-mode-map (kbd "C-i") 'auto-complete)
;;             (define-key eshell-mode-map [(tab)] 'auto-complete)
               )
             ))
