;;; term-restore.el --- desktop で端末 (vterm) の状態を保存・復元する -*- lexical-binding: t; -*-

;;; Commentary:

;; vterm のバッファは desktop に保存されない。ここでは端末ごとに
;;   プロジェクト名 / 番号 / 作業ディレクトリ / タイトル (最後のコマンド) /
;;   スクロールバックの末尾 N 行
;; を `wamei/term-restore-saved' に記録し、desktop のグローバル変数として
;; 一緒に保存する。読み込み後は記録どおりの数の端末を作り直す。
;;
;; スクロールバックは desktop ファイルを太らせないよう別ファイル
;; (`wamei/term-restore-directory' 配下) に置き、内容が変わったときだけ書く。
;; 復元時はそのパスを環境変数 WAMEI_TERM_RESTORE に載せてシェルを起こし、
;; .zshrc が起動時に cat する。色は vterm が付けた face を SGR エスケープに
;; 写して書いておき、cat したときに端末が解釈する。

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'desktop)

(defvar wamei/term-restore-scrollback-lines 200
  "復元用に残すスクロールバックの行数。")

(defvar wamei/term-restore-directory
  (locate-user-emacs-file "term-scrollback/")
  "スクロールバックを書き出すディレクトリ。")

(defvar wamei/term-restore-saved nil
  "前回保存した端末の記録。plist のリストで、各要素は
:project (プロジェクト名) :index (番号) :directory (作業ディレクトリ)
:title (最後に報告されたタイトル、無ければ nil) :scrollback (書き出したファイル)
を持つ。`desktop-globals-to-save' 経由で desktop ファイルに書かれる。")

;;; バッファ名

(defconst wamei/term-restore--name-regexp
  "\\`\\*term: \\(.+?\\)\\(?: \\([0-9]+\\)\\)?\\*\\'"
  "端末バッファ名にマッチする正規表現。1 がプロジェクト名、2 が番号 (省略可)。")

(defun wamei/term-restore--parse-name (name)
  "端末バッファ名 NAME を (PROJECT . INDEX) に分ける。端末でなければ nil。
番号なしは 1。プロジェクト名自体が \" 2\" で終わる場合は区別できない
(端末側の命名規則と同じ制約)。"
  (when (string-match wamei/term-restore--name-regexp name)
    (cons (match-string 1 name)
          (string-to-number (or (match-string 2 name) "1")))))

;;; スクロールバック

(defun wamei/term-restore--tail (text lines)
  "TEXT の末尾 LINES 行を返す。各行の行末の空白と、末尾の空行は落とす。
vterm は画面の下端まで空行で埋めるので、そのままだと空行ばかりになる。
テキストプロパティ (色) は保つ。"
  (let ((kept (seq-drop-while
               #'string-empty-p
               (nreverse (mapcar (lambda (line)
                                   (string-trim-right line))
                                 (split-string text "\n"))))))
    (setq kept (nreverse (seq-take kept lines)))
    (if kept
        (concat (string-join kept "\n") "\n")
      "")))

;;; 色 → SGR

(defun wamei/term-restore--rgb (color)
  "色名 COLOR を (R G B) (各 0-255) にする。解決できなければ nil。
vterm は色を #rrggbb で付けるので自前で読む。`color-name-to-rgb' は
batch (端末なし) で tty の近似色に丸めるので当てにしない。"
  (cond
   ((not (stringp color)) nil)
   ((string-match "\\`#\\([[:xdigit:]]+\\)\\'" color)
    (let* ((hex (match-string 1 color))
           (width (/ (length hex) 3)))
      (when (and (> width 0) (= (* width 3) (length hex)))
        ;; 桁が多ければ上位 2 桁だけ使い (#rrrrggggbbbb → #rrggbb)、
        ;; 1 桁 (#rgb) は 17 倍して 0-255 に広げる
        (mapcar (lambda (i)
                  (let ((n (string-to-number
                            (substring hex (* i width) (+ (* i width) (min width 2)))
                            16)))
                    (if (= width 1) (* n 17) n)))
                '(0 1 2)))))
   (t (when-let* ((values (color-values color)))
        (mapcar (lambda (v) (/ v 256)) values)))))

(defun wamei/term-restore--sgr-params (face)
  "vterm の `font-lock-face' plist FACE を SGR の引数 (文字列のリスト) にする。
属性、前景色、背景色の順。vterm はデフォルト色のセルにも `default' face の
前景・背景を明示的に付けるので、それと同じ色は出さない (復元先のテーマに任せる)。"
  (let ((params nil))
    (when (eq (plist-get face :weight) 'bold) (push "1" params))
    (when (eq (plist-get face :slant) 'italic) (push "3" params))
    (when (plist-get face :underline) (push "4" params))
    (when (plist-get face :inverse-video) (push "7" params))
    (when (plist-get face :strike-through) (push "9" params))
    (pcase-dolist (`(,key ,code ,default)
                   `((:foreground "38" ,(face-foreground 'default nil t))
                     (:background "48" ,(face-background 'default nil t))))
      (when-let* ((rgb (wamei/term-restore--rgb (plist-get face key))))
        (unless (equal rgb (wamei/term-restore--rgb default))
          (push (format "%s;2;%d;%d;%d" code (nth 0 rgb) (nth 1 rgb) (nth 2 rgb))
                params))))
    (nreverse params)))

(defun wamei/term-restore--ansi (text)
  "TEXT の `font-lock-face' (vterm が色ごとに付ける plist) を SGR エスケープにして
プロパティなしの文字列で返す。face が同じ区間ごとに開始のエスケープを置き、
区間の終わりで \\e[0m に戻す。"
  (let ((pos 0)
        (parts nil))
    (while (< pos (length text))
      (let* ((next (or (next-single-property-change pos 'font-lock-face text)
                       (length text)))
             (chunk (substring-no-properties text pos next))
             (params (wamei/term-restore--sgr-params
                      (get-text-property pos 'font-lock-face text))))
        (push (if params
                  (concat "\e[" (string-join params ";") "m" chunk "\e[0m")
                chunk)
              parts)
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(defun wamei/term-restore--write-scrollback (file text)
  "TEXT を FILE に書く。既に同じ内容なら書かない。書いたら非 nil。"
  (let ((current (and (file-readable-p file)
                      (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string)))))
    (unless (equal current text)
      (make-directory (file-name-directory file) t)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region text nil file nil 'silent))
      t)))

;;; 保存

(defvar wamei/term--title)                  ; init.el (vterm ブロック) の buffer-local 変数

(defun wamei/term-restore--prompt-line-p ()
  "現在行がプロンプトの行なら非 nil。
シェルがプロンプトの各行末で OSC 51;A を出すと、vterm はそれを受けた位置の
1 文字 (行末なら改行、入力途中ならコマンドの先頭文字) に `vterm-prompt'
プロパティを付ける (.zshrc の _wamei_vterm_prompt_mark)。行内 (末尾の改行を含む)
にその印があればプロンプトの行とみなす。"
  (text-property-any (line-beginning-position)
                     (min (point-max) (1+ (line-end-position)))
                     'vterm-prompt t))

(defun wamei/term-restore--content ()
  "現在のバッファの内容を、末尾のプロンプト行と空行を除いて返す。
末尾から空行を飛ばし、プロンプトの行が続く限り遡って切る。最後の行が
プロンプトでなければ (コマンド実行中) 何も落とさない。
色 (font-lock-face) は残し、書き出すときに `wamei/term-restore--ansi' で SGR にする。"
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (forward-line 0)
    (while (and (> (point) (point-min))
                (wamei/term-restore--prompt-line-p))
      (forward-line -1))
    (let ((end (if (wamei/term-restore--prompt-line-p)
                   (point)
                 (line-beginning-position 2))))
      (buffer-substring (point-min) end))))

(defun wamei/term-restore--scrollback-file (project index)
  "PROJECT の INDEX 番目の端末のスクロールバックを置くファイル。
プロジェクト名にファイル名として扱いにくい文字があれば _ に置き換える。"
  (expand-file-name (format "%s-%d.txt"
                            (replace-regexp-in-string "[^[:alnum:]._-]" "_" project)
                            index)
                    wamei/term-restore-directory))

(defun wamei/term-restore--terminal-buffers ()
  "端末バッファを (PROJECT INDEX BUFFER) のリストでプロジェクト名・番号順に返す。"
  (sort (delq nil
              (mapcar (lambda (buffer)
                        (when-let* ((parsed (wamei/term-restore--parse-name
                                             (buffer-name buffer))))
                          (list (car parsed) (cdr parsed) buffer)))
                      (buffer-list)))
        (lambda (a b)
          (if (string= (car a) (car b))
              (< (cadr a) (cadr b))
            (string< (car a) (car b))))))

(defun wamei/term-restore--entry (project index buffer)
  "PROJECT の INDEX 番目の端末 BUFFER の記録を作り、スクロールバックを書き出す。"
  (let ((file (wamei/term-restore--scrollback-file project index)))
    (with-current-buffer buffer
      (wamei/term-restore--write-scrollback
       file
       (wamei/term-restore--ansi
        (wamei/term-restore--tail (wamei/term-restore--content)
                                  wamei/term-restore-scrollback-lines)))
      (list :project project
            :index index
            :directory default-directory
            :title (and (boundp 'wamei/term--title) wamei/term--title)
            :scrollback file))))

(defun wamei/term-restore--prune (entries)
  "`wamei/term-restore-directory' から ENTRIES が参照しないファイルを消す。"
  (when (file-directory-p wamei/term-restore-directory)
    (let ((referenced (mapcar (lambda (entry) (plist-get entry :scrollback)) entries)))
      (dolist (file (directory-files wamei/term-restore-directory t "\\`[^.]"))
        (unless (member file referenced)
          (delete-file file))))))

(defun wamei/term-restore-save ()
  "全ての端末を `wamei/term-restore-saved' に記録し、スクロールバックを書き出す。
`desktop-save-hook' から呼ぶ。"
  (let ((entries (mapcar (lambda (item) (apply #'wamei/term-restore--entry item))
                         (wamei/term-restore--terminal-buffers))))
    (wamei/term-restore--prune entries)
    (setq wamei/term-restore-saved entries)))

;;; 復元

(declare-function vterm "vterm")

(defun wamei/term-restore--buffer-name (project index)
  "PROJECT の INDEX 番目の端末バッファ名。1 は番号なし (init.el の命名規則と同じ)。"
  (if (> index 1)
      (format "*term: %s %d*" project index)
    (format "*term: %s*" project)))

(defun wamei/term-restore--create (entry)
  "記録 ENTRY の端末を作って返す。
作業ディレクトリが無くなっていればホームで作る。スクロールバックのファイルが
読めれば WAMEI_TERM_RESTORE に載せ、シェル (.zshrc) が起動時に表示する。
vterm はバッファへ切り替えるので window 構成は戻す。"
  (let* ((directory (plist-get entry :directory))
         (default-directory (if (and directory (file-directory-p directory))
                                (file-name-as-directory directory)
                              (expand-file-name "~/")))
         (scrollback (plist-get entry :scrollback))
         (process-environment
          (if (and scrollback (file-readable-p scrollback))
              (cons (concat "WAMEI_TERM_RESTORE=" scrollback) process-environment)
            process-environment))
         (buffer (save-window-excursion
                   (vterm (wamei/term-restore--buffer-name (plist-get entry :project)
                                                           (plist-get entry :index))))))
    (with-current-buffer buffer
      (setq-local wamei/term--title (plist-get entry :title)))
    buffer))

(defun wamei/term-restore-all ()
  "`wamei/term-restore-saved' の記録どおりに端末を作り直す。既にある端末は触らない。
`desktop-after-read-hook' から呼ぶ。side window の開き直し (desktop-side-windows)
より先に走らせ、パネルに出すバッファを用意しておく。"
  (dolist (entry wamei/term-restore-saved)
    (unless (get-buffer (wamei/term-restore--buffer-name (plist-get entry :project)
                                                         (plist-get entry :index)))
      (condition-case err
          (wamei/term-restore--create entry)
        (error (message "term-restore: %s %d を作れません: %s"
                        (plist-get entry :project) (plist-get entry :index)
                        (error-message-string err)))))))

;;; desktop への組み込み

(defun wamei/term-restore-setup ()
  "desktop の保存・読み込みに組み込む。"
  (add-to-list 'desktop-globals-to-save 'wamei/term-restore-saved)
  (add-hook 'desktop-save-hook #'wamei/term-restore-save)
  ;; desktop-side-windows の開き直しより先に端末を用意する
  (add-hook 'desktop-after-read-hook #'wamei/term-restore-all -10))

(provide 'term-restore)
;;; term-restore.el ends here
