;;; project-sidebar.el --- dired ベースのプロジェクトサイドバー -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; treemacs の代替。プロジェクトルートごとに 1 つの dired バッファを作り、
;; 左の side window に出す。タブ = プロジェクト (project-tabs.el) なので、
;; 各タブの window 構成が自分のプロジェクトの sidebar を持つ形になる。
;;
;; - バッファは dired そのもの。dired-tree (展開記憶・監視・D&D)、
;;   dired-git-status (色)、nerd-icons-dired、右クリックメニューがそのまま効く
;; - 見た目: 詳細を隠し、見出し行と . .. を隠し、header-line にプロジェクト名
;; - トグル (C-x C-n) は端末パネル (wamei/term-toggle) と同じ 4 態
;; - follow (Task 11)、マウス (Task 12)、desktop 復元 (Task 13)
;;
;;; Code:

(require 'dired)
(require 'dired-x)
(require 'project)
(require 'dired-tree)
(require 'project-tabs)

;; `wamei/project-sidebar-mode' は下の `define-minor-mode' で定義されるが、
;; それより前にある関数から参照するため前方宣言しておく。
(defvar wamei/project-sidebar-mode)

;;; 外観

(defgroup wamei/project-sidebar nil "dired ベースのプロジェクトサイドバー。" :group 'dired)

(defface wamei/project-sidebar-root
  '((t (:inherit font-lock-keyword-face :weight bold :height 1.3)))
  "header-line に出すプロジェクト名。")

(defvar wamei/project-sidebar-width 35 "side window の幅 (桁)。")

;;; バッファ

(defun wamei/project-sidebar--root-for (dir)
  "DIR が属するプロジェクトのルート (末尾 / あり)。プロジェクト外なら DIR。
symlink は `file-truename' で実体に解決する
(sidebar バッファの識別をシンボリックリンク越しでも同一視するため)。"
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (if-let* ((project (project-current nil)))
        (file-name-as-directory (file-truename (expand-file-name (project-root project))))
      (file-name-as-directory (file-truename default-directory)))))

(defun wamei/project-sidebar--buffer-name (root)
  "ROOT の sidebar バッファ名。先頭空白でバッファ一覧から隠す。"
  (format " *sidebar: %s*" (file-name-nondirectory (directory-file-name root))))

(defun wamei/project-sidebar--hide-header-lines ()
  "先頭のディレクトリ見出し行を overlay で隠す。total 行は dired-hide-details が隠す。
`invisible' の値は専用シンボルを使う (dired-hide-details-mode が spec をリストにする
ので t では効かないことがある)。`buffer-invisibility-spec' への登録は revert のたびに
呼ばれるとここではなく minor-mode の enable 時に 1 回だけ行う
(`add-to-invisibility-spec' は非冪等で、revert のたびに呼ぶと重複が積み上がる)。"
  (remove-overlays (point-min) (point-max) 'wamei/project-sidebar-header t)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward dired-subdir-regexp nil t)
      (let ((ov (make-overlay (point-min) (line-beginning-position 2))))
        (overlay-put ov 'wamei/project-sidebar-header t)
        (overlay-put ov 'invisible 'wamei/project-sidebar-header)
        (overlay-put ov 'evaporate t)))))

(defun wamei/project-sidebar--unadvertise ()
  "この sidebar を `dired-buffers' から外し、通常の dired に再利用させない。
`dired-unadvertise' はディレクトリ単位で消すので同じディレクトリの通常 dired まで
外してしまう。自バッファのエントリだけ消す。"
  (setq dired-buffers
        (seq-remove (lambda (entry) (eq (cdr entry) (current-buffer))) dired-buffers)))

(defun wamei/project-sidebar--decorate ()
  "readin のあとに外観を整える。"
  (when wamei/project-sidebar-mode
    (wamei/project-sidebar--hide-header-lines)
    (wamei/project-sidebar--unadvertise)))

(defun wamei/project-sidebar--create (root)
  "ROOT の sidebar バッファを新しく作る。"
  (let ((buffer (let ((dired-buffers nil))   ; 通常の dired 一覧に登録させない
                  (dired-noselect root))))
    (with-current-buffer buffer
      (rename-buffer (wamei/project-sidebar--buffer-name root) t)
      (wamei/project-sidebar-mode 1)
      (wamei/project-sidebar--decorate))
    buffer))

(defun wamei/project-sidebar-buffer (root)
  "ROOT の sidebar バッファ。無ければ作る。
ROOT は symlink かもしれないので `file-truename' で正規化してから比べる
(sidebar バッファは正規化済みの root から作るので default-directory は既に実体)。"
  (let ((root (file-name-as-directory (file-truename (expand-file-name root)))))
    (or (seq-find (lambda (buf)
                    (with-current-buffer buf
                      (and (bound-and-true-p wamei/project-sidebar-mode)
                           (equal (expand-file-name default-directory) root))))
                  (buffer-list))
        (wamei/project-sidebar--create root))))

;;; window

(defun wamei/project-sidebar-window (&optional frame)
  "FRAME の左 side window のうち sidebar を表示しているもの。無ければ nil。"
  (seq-find (lambda (win)
              (and (eq (window-parameter win 'window-side) 'left)
                   (with-current-buffer (window-buffer win)
                     (bound-and-true-p wamei/project-sidebar-mode))))
            (window-list frame 'no-minibuf)))

(defun wamei/project-sidebar-show (dir)
  "DIR のプロジェクトの sidebar を side window に出し、その window を返す。選択はしない。"
  (let ((buffer (wamei/project-sidebar-buffer (wamei/project-sidebar--root-for dir))))
    (or (get-buffer-window buffer)
        (display-buffer buffer))))

(defun wamei/project-sidebar--back-window ()
  "sidebar から戻る先。直近の通常 window。"
  (get-mru-window nil t t t))

(defun wamei/project-sidebar-toggle (&optional arg)
  "sidebar へ出入りする。

- 非表示なら開いてフォーカスする
- 表示中でフォーカスが無ければフォーカスを移す
- フォーカス中なら元の window へ戻る (開いたまま)
- ARG (C-u) 付きなら閉じる

出すプロジェクトは `wamei/project-tabs-main-window' のバッファのもの。"
  (interactive "P")
  (let ((window (wamei/project-sidebar-window)))
    (cond
     (arg
      (when window (delete-window window)))
     ((and window (eq window (selected-window)))
      (when-let* ((back (wamei/project-sidebar--back-window)))
        (select-window back)))
     (window
      (select-window window))
     (t
      (let ((dir (with-current-buffer (window-buffer (wamei/project-tabs-main-window))
                   default-directory)))
        (select-window (wamei/project-sidebar-show dir)))))))

(defun wamei/project-sidebar-quit ()
  "元の window へ戻る。sidebar は開いたまま。"
  (interactive)
  (when-let* ((back (wamei/project-sidebar--back-window)))
    (select-window back)))

;;; minor mode

(defvar wamei/project-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'wamei/project-sidebar-quit)
    map)
  "sidebar バッファのキーマップ。dired-mode-map より優先される。")

(define-minor-mode wamei/project-sidebar-mode
  "この dired バッファをプロジェクトサイドバーとして扱う。"
  :lighter nil
  :keymap wamei/project-sidebar-mode-map
  (if wamei/project-sidebar-mode
      (progn
        (wamei/dired-tree-mode 1)
        (dired-hide-details-mode 1)
        (setq-local dired-omit-verbose nil)   ; "Omitted N lines" を出さない
        (dired-omit-mode 1)                 ; . と .. を隠す (既定の dired-omit-files)
        (setq-local dired-hide-details-hide-information-lines t)
        (setq-local truncate-lines t)
        (setq-local mouse-1-click-follows-link nil)
        (setq header-line-format
              (list (propertize (concat " " (file-name-nondirectory
                                             (directory-file-name default-directory)))
                                'face 'wamei/project-sidebar-root)))
        (add-to-invisibility-spec 'wamei/project-sidebar-header)
        (add-hook 'dired-after-readin-hook #'wamei/project-sidebar--decorate 99 t))
    (remove-hook 'dired-after-readin-hook #'wamei/project-sidebar--decorate t)
    (remove-from-invisibility-spec 'wamei/project-sidebar-header)
    (remove-overlays (point-min) (point-max) 'wamei/project-sidebar-header t)
    (kill-local-variable 'header-line-format)))

;;; display-buffer

(defun wamei/project-sidebar-setup ()
  "display-buffer-alist に sidebar の出し方を登録する。init.el から 1 回呼ぶ。"
  (add-to-list 'display-buffer-alist
               `("\\` \\*sidebar: "
                 (display-buffer-in-side-window)
                 (side . left)
                 (slot . 0)
                 (window-width . ,wamei/project-sidebar-width)
                 (dedicated . t)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t))))))

(provide 'project-sidebar)
;;; project-sidebar.el ends here
