;;; delta-view.el --- diff を delta の表示 (幅があれば左右 2 列) で読む -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; magit のバッファは diff のテキストを section に分けて stage にも使うので、
;; delta の左右 2 列 (行を作り替える) にはできない。代わりに、同じ diff を
;; delta に通した読み取り専用のバッファを別に開く。
;;
;; - `wamei/delta-view-dwim' (magit の |): point の diff を開く。file / hunk なら
;;   そのファイル、staged / unstaged の見出しならその全体、commit ならその commit、
;;   revision / diff バッファならそのバッファの diff。決まらなければ全部。
;; - `wamei/delta-view-all' (magit の C-u |): git diff と git diff --cached を
;;   見出し付きで 1 つにまとめる (untracked は git diff と同じく出ない)。
;;
;; window の幅が `wamei/delta-view-side-by-side-min-width' 以上なら 2 列、狭ければ
;; 1 列。g で今の幅に合わせて作り直す。n / p で見出し・ファイル・hunk の間を、
;; M-n / M-p でファイルの間だけを移る (magit と同じ割り当て)。
;;
;; delta は .gitconfig の [delta] を読むが、features (既定の 2 列) は
;; DELTA_FEATURES を空にして外し、2 列にするかはここで決める。
;; テストは delta-view-test.el。
;;; Code:

(require 'seq)
(require 'subr-x)

(declare-function xterm-color-filter "xterm-color" (string))
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function magit-file-at-point "magit-diff" (&optional expand assert))
(declare-function magit-commit-at-point "magit-git" ())
(declare-function magit-section-match "magit-section" (condition &optional section))
(defvar magit-buffer-revision)          ; magit-mode.el
(defvar magit-buffer-diff-range)        ; magit-mode.el
(defvar magit-buffer-diff-typearg)      ; magit-mode.el
(defvar magit-buffer-diff-files)        ; magit-diff.el

(defvar wamei/delta-view-side-by-side-min-width 160
  "window の幅 (桁) がこれ以上なら左右 2 列、狭ければ 1 列で出す。")

(defface wamei/delta-view-heading
  '((t :inherit magit-section-heading))
  "まとめて出すときの見出し (Unstaged changes など)。"
  :group 'magit-diff)

(defvar-local wamei/delta-view--parts nil
  "出す diff。(見出し . git の引数) のリストで、見出しが nil なら付けない。")

;;; 行末の背景

(defconst wamei/delta-view--eol-background-re
  "\e\\[48;2;\\([0-9]+\\);\\([0-9]+\\);\\([0-9]+\\)m\e\\[0K\\(?:\e\\[0m\\)?$"
  "delta が追加・削除行の末尾に出す「背景色 + ESC[0K (行末まで消去)」。")

(defun wamei/delta-view-eol-backgrounds ()
  "カレントバッファ (delta の制御コード付き) で右端まで塗る行と色を返す。
\(行数 . \"#rrggbb\") のリスト。行数はバッファ先頭から数える。端末は ESC[0K で
その色のまま右端まで塗るが、xterm-color は捨てるので色が文字の終わりで止まる。
制御コードを色に変えた後に `wamei/delta-view-extend-backgrounds' へ渡す
\(変換でバッファは入れ直されるが行数は変わらない)。truecolor (48;2) だけを扱う。"
  (save-excursion
    (goto-char (point-min))
    (let (eols)
      (while (re-search-forward wamei/delta-view--eol-background-re nil t)
        (push (cons (count-lines (point-min) (point))
                    (apply #'format "#%02x%02x%02x"
                           (mapcar (lambda (n) (string-to-number (match-string n)))
                                   '(1 2 3))))
              eols))
      (nreverse eols))))

(defun wamei/delta-view-extend-backgrounds (eols)
  "EOLS (`wamei/delta-view-eol-backgrounds' の値) の行の改行に背景を載せる。"
  (save-excursion
    (pcase-dolist (`(,line . ,color) eols)
      (goto-char (point-min))
      (forward-line (1- line))
      (let* ((eol (line-end-position))
             (overlay (make-overlay eol (1+ eol))))
        (overlay-put overlay 'face (list :background color :extend t))
        (overlay-put overlay 'evaporate t)))))

;;; 描画

(defun wamei/delta-view--delta-args (width light)
  "WIDTH 桁で delta を走らせる引数。LIGHT が非 nil なら明るい背景向け。"
  (append (list "--paging" "never" "--true-color" "always"
                "--navigate" "--line-numbers"
                "--width" (number-to-string width)
                (if light "--light" "--dark"))
          (and (>= width wamei/delta-view-side-by-side-min-width)
               '("--side-by-side"))))

(defun wamei/delta-view--delta (raw width)
  "生の diff RAW を WIDTH 桁で delta に通した出力 (制御コード付き)。"
  (with-temp-buffer
    (insert raw)
    ;; delta はローカルで走らせる (default-directory が TRAMP でも)
    (let ((default-directory temporary-file-directory)
          (process-environment (cons "DELTA_FEATURES=" process-environment)))
      (apply #'call-process-region (point-min) (point-max) "delta" t t nil
             (wamei/delta-view--delta-args
              width (eq (frame-parameter nil 'background-mode) 'light))))
    (buffer-string)))

(defun wamei/delta-view--insert (raw width)
  "生の diff RAW を delta の表示にして point に挿入する。"
  (require 'xterm-color)
  (let ((start (point)))
    (insert (wamei/delta-view--delta raw width))
    (save-restriction
      (narrow-to-region start (point))
      (let ((eols (wamei/delta-view-eol-backgrounds)))
        (insert (xterm-color-filter (delete-and-extract-region (point-min) (point-max))))
        (wamei/delta-view-extend-backgrounds eols)
        (wamei/delta-view--mark-sections (point-min) (point-max))))
    (goto-char (point-max))))

(defun wamei/delta-view--render (parts width)
  "PARTS ((見出し . 生の diff) のリスト) を WIDTH 桁で描き直す。空の diff は飛ばす。"
  (let ((inhibit-read-only t)
        (shown nil))
    (erase-buffer)
    (pcase-dolist (`(,title . ,raw) parts)
      (unless (string-empty-p raw)
        (when shown (insert "\n"))
        (when title
          (insert (wamei/delta-view--title title) "\n\n"))
        (wamei/delta-view--insert raw width)
        (setq shown t)))
    (unless shown
      (insert "差分はありません\n"))))

;;; モード

(defun wamei/delta-view--git (args)
  "git ARGS の出力 (色なし)。default-directory のリポジトリで走らせる。"
  (with-temp-buffer
    (apply #'process-file "git" nil t nil "--no-pager" "-c" "color.ui=never" args)
    (buffer-string)))

(defun wamei/delta-view--width ()
  "delta に渡す幅。表示中の window の本文幅 (最後の桁は折り返しの印に残す)。"
  (1- (if-let* ((window (get-buffer-window)))
          (window-body-width window)
        (frame-width))))

(defun wamei/delta-view--revert (&rest _)
  "git を走らせ直し、今の window の幅で描き直す。`revert-buffer-function' 用。"
  (let ((line (line-number-at-pos)))
    (wamei/delta-view--render
     (mapcar (pcase-lambda (`(,title . ,args))
               (cons title (wamei/delta-view--git args)))
             wamei/delta-view--parts)
     (wamei/delta-view--width))
    (goto-char (point-min))
    (forward-line (1- line))))

(defconst wamei/delta-view--file-re
  "^\\(?:Δ\\|added:\\|removed:\\|renamed:\\) .*\n─"
  "ファイルの見出し。delta の --navigate の印で始まり、次の行が罫線。")

(defconst wamei/delta-view--hunk-re
  "^─*┐\n\\(• \\)"
  "hunk の見出し。delta は \"• 行番号: …\" を罫線の箱で囲む。本文の行は
行番号の列から始まるので、行頭の \"• \" と取り違えない。")

(defun wamei/delta-view--title (title)
  "まとめて出すときの見出し TITLE の文字列。セクションの印を付ける。"
  (propertize title 'face 'wamei/delta-view-heading 'wamei/delta-view-section 'title))

(defun wamei/delta-view--mark-sections (start end)
  "START から END の delta の表示で、ファイルと hunk の見出しの行に印を付ける。
印はテキストプロパティ `wamei/delta-view-section' (file / hunk) で、n / p が使う。"
  (save-excursion
    (pcase-dolist (`(,re ,group ,type) `((,wamei/delta-view--file-re 0 file)
                                         (,wamei/delta-view--hunk-re 1 hunk)))
      (goto-char start)
      (while (re-search-forward re end t)
        (let ((bol (match-beginning group)))
          (put-text-property bol (save-excursion (goto-char bol) (line-end-position))
                             'wamei/delta-view-section type))))))

(defun wamei/delta-view--find (direction types)
  "DIRECTION (1 か -1) にある、TYPES のどれかの印が付いた行の先頭。無ければ nil。"
  (save-excursion
    (beginning-of-line)
    (catch 'found
      (while (and (zerop (forward-line direction)) (not (eobp)))
        (when (memq (get-text-property (point) 'wamei/delta-view-section) types)
          (throw 'found (point)))))))

(defun wamei/delta-view--move (direction types last)
  "DIRECTION (1 か -1) へ、TYPES のどれかの印が付いた行の先頭へ移る。
無ければ動かずに LAST を伝える。"
  (if-let* ((pos (wamei/delta-view--find direction types)))
      (goto-char pos)
    (message "%s" last)))

(defun wamei/delta-view-next-section ()
  "次の見出し・ファイル・hunk へ移る。
最後のセクションからは末尾へ送り、最後の hunk の終わりまで見えるようにする。"
  (interactive)
  (if-let* ((pos (wamei/delta-view--find 1 '(title file hunk))))
      (goto-char pos)
    (if (eobp)
        (message "最後のセクションです")
      (goto-char (point-max)))))

(defun wamei/delta-view-previous-section ()
  "前の見出し・ファイル・hunk へ移る。"
  (interactive)
  (wamei/delta-view--move -1 '(title file hunk) "最初のセクションです"))

(defun wamei/delta-view-next-file ()
  "次のファイルの見出しへ移る。"
  (interactive)
  (wamei/delta-view--move 1 '(file) "最後のファイルです"))

(defun wamei/delta-view-previous-file ()
  "前のファイルの見出しへ移る。"
  (interactive)
  (wamei/delta-view--move -1 '(file) "最初のファイルです"))

(defvar wamei/delta-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'wamei/delta-view-next-section)
    (define-key map "p" #'wamei/delta-view-previous-section)
    (define-key map (kbd "M-n") #'wamei/delta-view-next-file)
    (define-key map (kbd "M-p") #'wamei/delta-view-previous-file)
    map)
  "`wamei/delta-view-mode' のキー。g (描き直し) と q (閉じる) は special-mode のもの。")

(define-derived-mode wamei/delta-view-mode special-mode "Delta"
  "diff を delta の表示で読む読み取り専用のバッファ。"
  ;; delta が幅を揃えて出すので、Emacs 側では折り返さない
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'wamei/delta-view--revert))

(defun wamei/delta-view-open (directory parts)
  "DIRECTORY のリポジトリの PARTS ((見出し . git の引数) のリスト) を開く。
バッファはリポジトリごとに 1 つで、選択中の window に出す (q で戻る)。"
  (let ((buffer (get-buffer-create
                 (format "*delta: %s*"
                         (file-name-nondirectory (directory-file-name directory))))))
    (with-current-buffer buffer
      (wamei/delta-view-mode)
      (setq default-directory directory
            wamei/delta-view--parts parts
            header-line-format
            (mapconcat (pcase-lambda (`(,_ . ,args))
                         (string-join (cons "git" args) " "))
                       parts " / ")))
    (pop-to-buffer-same-window buffer)
    ;; 幅は表示した window で決まるので、出してから描く
    (revert-buffer)
    (goto-char (point-min))))

;;; magit から開く

(defun wamei/delta-view--files-args (files)
  "FILES があれば git に渡す \"-- FILES\"。"
  (and files (cons "--" files)))

(defun wamei/delta-view--magit-parts ()
  "magit のバッファで point の diff を (見出し . git の引数) のリストで返す。
決まらなければ nil。"
  (let ((file (magit-file-at-point)))
    (cond
     ((derived-mode-p 'magit-revision-mode)
      `((nil "show" ,magit-buffer-revision
             ,@(wamei/delta-view--files-args (if file (list file) magit-buffer-diff-files)))))
     ((derived-mode-p 'magit-diff-mode)
      `((nil "diff"
             ,@(and magit-buffer-diff-typearg (list magit-buffer-diff-typearg))
             ,@(and magit-buffer-diff-range (list magit-buffer-diff-range))
             ,@(wamei/delta-view--files-args (if file (list file) magit-buffer-diff-files)))))
     ((magit-section-match [* unstaged])
      `((nil "diff" ,@(wamei/delta-view--files-args (and file (list file))))))
     ((magit-section-match [* staged])
      `((nil "diff" "--cached" ,@(wamei/delta-view--files-args (and file (list file))))))
     ((when-let* ((commit (magit-commit-at-point)))
        `((nil "show" ,commit)))))))

(defun wamei/delta-view-all ()
  "リポジトリの git diff と git diff --cached を 1 つのバッファで読む。"
  (interactive)
  (require 'magit)
  (wamei/delta-view-open (or (magit-toplevel) default-directory)
                         '(("Unstaged changes" "diff")
                           ("Staged changes" "diff" "--cached"))))

(defun wamei/delta-view-dwim (&optional all)
  "point の diff を delta の表示で読む。ALL (C-u) か決まらなければ全部。"
  (interactive "P")
  (if-let* (((not all))
            (parts (wamei/delta-view--magit-parts)))
      (wamei/delta-view-open (or (magit-toplevel) default-directory) parts)
    (wamei/delta-view-all)))

(provide 'delta-view)
;;; delta-view.el ends here
