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
;; - メイン window のファイルに sidebar を追従させる follow-mode を持つ
;; - クリックでプレビュー、ダブルクリック / RET で開く。desktop 復元 (Task 13)
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

;;; follow

(defun wamei/project-sidebar--follow-target (file shown-root file-root)
  "FILE に合わせるとき sidebar をどうするか。
SHOWN-ROOT の配下なら `same'、別プロジェクト (FILE-ROOT あり) なら `switch'、
それ以外 (FILE が無い、プロジェクト外) は `none'。"
  (cond ((null file) 'none)
        ((and shown-root (wamei/dired-tree--inside-p shown-root file)) 'same)
        (file-root 'switch)
        (t 'none)))

(defun wamei/project-sidebar--reveal (window file)
  "WINDOW の sidebar で FILE の行まで展開し、window-point を移す。フォーカスは動かさない。"
  (with-current-buffer (window-buffer window)
    (save-excursion
      (when (wamei/dired-tree-expand-to file)
        (set-window-point window (point))))))

(defun wamei/project-sidebar--follow (frame)
  "FRAME のメイン window のファイルに sidebar を合わせる。
リモートのファイルには追従しない (`file-truename' が TRAMP 接続を試みて
ブロックするのを避けるため、`file-remote-p' で先に弾く)。"
  (when (frame-live-p frame)
    (with-selected-frame frame
      (when-let* ((window (wamei/project-sidebar-window frame)))
        (unless (window-parameter (selected-window) 'no-other-window)
          (let* ((buffer (window-buffer (selected-window)))
                 (file (let ((f (buffer-file-name buffer)))
                         (and f (not (file-remote-p f)) (file-truename f))))
                 (shown-root (with-current-buffer (window-buffer window)
                               (expand-file-name default-directory)))
                 (file-root (and file
                                 (let ((default-directory (file-name-directory file)))
                                   (when (project-current nil)
                                     (wamei/project-sidebar--root-for default-directory))))))
            (pcase (wamei/project-sidebar--follow-target file shown-root file-root)
              ('same (wamei/project-sidebar--reveal window file))
              ('switch
               (unwind-protect
                   (progn
                     (set-window-dedicated-p window nil)
                     (set-window-buffer window (wamei/project-sidebar-buffer file-root)))
                 (set-window-dedicated-p window t))
               (wamei/project-sidebar--reveal window file)))))))))

(defvar wamei/project-sidebar--follow-timer nil)

(defun wamei/project-sidebar--follow-soon (frame)
  "`window-buffer-change-functions' / `window-selection-change-functions' 用。
再表示中は window を触らず、次のコマンド境界で follow する。"
  (unless (timerp wamei/project-sidebar--follow-timer)
    (setq wamei/project-sidebar--follow-timer
          (run-at-time 0 nil
                       (lambda ()
                         (setq wamei/project-sidebar--follow-timer nil)
                         ;; タイマー内のエラーは呼び出し元に伝わらず素通りするので、
                         ;; ここで捕まえてメッセージにする (dired-tree の revert と同じ)。
                         (condition-case err
                             (wamei/project-sidebar--follow frame)
                           (error
                            (message "project-sidebar: follow failed: %s"
                                     (error-message-string err)))))))))

(define-minor-mode wamei/project-sidebar-follow-mode
  "メイン window のバッファに sidebar のカーソルを追従させる。"
  :global t
  (if wamei/project-sidebar-follow-mode
      (progn
        (add-hook 'window-buffer-change-functions #'wamei/project-sidebar--follow-soon)
        (add-hook 'window-selection-change-functions #'wamei/project-sidebar--follow-soon))
    (remove-hook 'window-buffer-change-functions #'wamei/project-sidebar--follow-soon)
    (remove-hook 'window-selection-change-functions #'wamei/project-sidebar--follow-soon)))

;;; 開く

(defun wamei/project-sidebar--open-in-main (file &optional select)
  "メイン window に FILE を出す。SELECT が非 nil ならそちらへフォーカスを移す。"
  (let ((window (wamei/project-tabs-main-window))
        (buffer (find-file-noselect file)))
    (set-window-buffer window buffer)
    (when select
      (select-window window))))

(defun wamei/project-sidebar-preview ()
  "point の行がファイルならメイン window に表示する。フォーカスは sidebar に残す。"
  (interactive)
  (when-let* ((file (dired-utils-get-filename)))
    (unless (file-directory-p file)
      (wamei/project-sidebar--open-in-main file))))

(defun wamei/project-sidebar-open ()
  "ファイルならメイン window で開いてフォーカスを移す。ディレクトリなら展開/折りたたみ。"
  (interactive)
  (when-let* ((file (dired-utils-get-filename)))
    (if (file-directory-p file)
        (dired-subtree-toggle)
      (wamei/project-sidebar--open-in-main file t))))

(defun wamei/project-sidebar-mouse-select (event)
  "クリックした行を選択し、ファイルならプレビューする。"
  (interactive "e")
  (mouse-set-point event)
  (wamei/project-sidebar-preview))

(defun wamei/project-sidebar-mouse-open (event)
  "ダブルクリックした行を開く (ファイル) か展開/折りたたみ (ディレクトリ)。"
  (interactive "e")
  (mouse-set-point event)
  (wamei/project-sidebar-open))

;;; minor mode

(defvar wamei/project-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'wamei/project-sidebar-quit)
    (define-key map (kbd "RET") #'wamei/project-sidebar-open)
    (define-key map [mouse-1] #'wamei/project-sidebar-mouse-select)
    (define-key map [double-mouse-1] #'wamei/project-sidebar-mouse-open)
    map)
  "sidebar バッファのキーマップ。dired-mode-map より優先される。
down-mouse-1 は束縛しない (dired の D&D に任せる)。")

(define-minor-mode wamei/project-sidebar-mode
  "この dired バッファをプロジェクトサイドバーとして扱う。"
  :lighter nil
  :keymap wamei/project-sidebar-mode-map
  (if wamei/project-sidebar-mode
      (progn
        (wamei/dired-tree-mode 1)
        (dired-hide-details-mode 1)
        (setq-local dired-omit-verbose nil)   ; "Omitted N lines" を出さない
        (setq-local dired-omit-size-limit nil) ; 行数が多くても omit を諦めない
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
