;;; term-links.el --- 端末の本文に出たファイル名をリンクにする -*- lexical-binding: t; -*-

;;; Commentary:

;; ghostel は端末の文字列から path[:line[:col]] を拾ってリンクにする
;; (ghostel-links.el の `ghostel--detect-urls')。ただし拾うのは `/' を含む
;; パスだけで、`init.el' や `ai-panel.el:24' のようなファイル名だけの参照は
;; リンクにならない (`ghostel-file-detection-path-regex' の docstring にある
;; とおり、トークンごとに `file-exists-p' するのを避けるため)。
;; Codex は参照をパス付きで書くので開けるが、Claude Code は本文でファイル名
;; だけを書くことが多く、そこから開けなかった。
;;
;; ここでは ghostel の走査の後に、`/' を含まないファイル名を拾う走査を足す。
;; 名前は次の順で解決し、決まったものだけをリンクにする。
;;
;;   1. `default-directory' 直下にある
;;   2. プロジェクトのファイル一覧に、その basename のファイルが 1 つだけある
;;
;; 2 は `project-files' から作った basename の索引を引くので、トークンごとの
;; stat は 1 の 1 回だけ。索引に無い名前を引いたときは、前回の作成から
;; `wamei/term-links-index-refresh-interval' 秒過ぎていれば作り直す (Claude が
;; 作ったばかりのファイルを拾うため)。バージョン番号のように索引に決して
;; 無い名前でも作り直しが起きるので、間隔で抑えている。
;;
;; リンクは ghostel 自身と同じ fileref: なので、開き方も同じ
;; (`ghostel--open-link' が `find-file-other-window' で本体側に開く)。
;; ghostel は自分の走査で拾わなかった fileref: を「取り残されたリンク」として
;; 消すので、この走査は毎回 ghostel の走査の後に付け直す。

;;; Code:

(require 'project)

;; init.el は ghostel の :preface で読むので、ghostel 本体は require しない
;; (使うのは `wamei/term-links-mode' を ghostel のロード後に有効にしてから)。
(defvar ghostel-enable-file-detection)        ; ghostel-links.el
(defvar ghostel--soft-wrap-row-limit)         ; ghostel-links.el
(defvar ghostel--inhibit-active-line-skip)    ; ghostel-links.el
(defvar ghostel--cursor-char-pos)             ; ghostel.el (buffer-local)
(declare-function ghostel--soft-wrap-line-beginning "ghostel-links" (pos limit))
(declare-function ghostel--soft-wrap-line-end "ghostel-links" (pos limit))
(declare-function ghostel--wrap-joined-region "ghostel-links" (begin end limit))
(declare-function ghostel--wrap-offset-to-pos "ghostel-links" (offset chunks))
(declare-function ghostel--wrap-fragments "ghostel-links" (beg end))
(declare-function ghostel--skip-match-p "ghostel-links" (fragments raw active-bounds))
(declare-function ghostel--linkify "ghostel-links" (fragments uri raw))

(defvar wamei/term-links-index-refresh-interval 10
  "索引に無い名前を引いたとき、索引を作り直すまでの最短の間隔 (秒)。")

(defvar wamei/term-links--index-cache (make-hash-table :test 'equal)
  "プロジェクトのルート → (作成時刻 . basename → 絶対パスのリスト の hash)。")

(defconst wamei/term-links--bare-name-regex
  (concat "\\(?:^\\|[^[:alnum:]_./~-]\\)"
          "\\([[:alnum:]_.-]*\\.[[:alnum:]_-]+\\)"
          "\\(\\(?::[0-9]+\\(?::[0-9]+\\)?\\)?\\)")
  "`/' を含まないファイル名と、省略できる :LINE[:COL]。
名前は `.' を 1 つ以上含むもの (`init.el' / `.zshrc')。直前の文字の
除外は ghostel の `ghostel--file-detection-leading-anchor' と同じで、
パスの途中 (`/' の後) からは始まらない。")

(defun wamei/term-links--build-index (project)
  "PROJECT のファイルを basename で引ける hash にして返す。"
  (let ((index (make-hash-table :test 'equal)))
    (dolist (file (project-files project))
      (push file (gethash (file-name-nondirectory file) index)))
    index))

(defun wamei/term-links--lookup (project name)
  "PROJECT の中で basename が NAME のファイルの絶対パスのリスト。"
  (let* ((root (expand-file-name (project-root project)))
         (entry (gethash root wamei/term-links--index-cache))
         (found (and entry (gethash name (cdr entry)))))
    (when (and (not found)
               (or (not entry)
                   (>= (float-time (time-since (car entry)))
                       wamei/term-links-index-refresh-interval)))
      (setq entry (cons (current-time) (wamei/term-links--build-index project)))
      (puthash root entry wamei/term-links--index-cache)
      (setq found (gethash name (cdr entry))))
    found))

(defun wamei/term-links--resolve (name dir)
  "ファイル名 NAME を DIR から解決した絶対パス。決まらなければ nil。"
  (let ((local (expand-file-name name dir)))
    (if (file-regular-p local)
        local
      (when-let* ((project (project-current nil dir))
                  (files (wamei/term-links--lookup project name)))
        (and (null (cdr files)) (expand-file-name (car files)))))))

(defun wamei/term-links--detect-bare-names (begin end)
  "BEGIN..END の `/' を含まないファイル名をリンクにする。
ghostel の `ghostel--detect-urls' と同じく、折り返しを繋いだ論理行で探し、
プロンプト・カーソル行・他のリンクと重なる所は `ghostel--skip-match-p' で避ける。"
  (when (and ghostel-enable-file-detection
             (not (file-remote-p default-directory)))
    (let* ((inhibit-read-only t)
           (inhibit-modification-hooks t)
           (limit ghostel--soft-wrap-row-limit)
           (begin (ghostel--soft-wrap-line-beginning begin limit))
           (end (ghostel--soft-wrap-line-end end limit))
           (active-bounds
            (unless ghostel--inhibit-active-line-skip
              (let ((active-pos (or ghostel--cursor-char-pos (point))))
                (cons (ghostel--soft-wrap-line-beginning active-pos limit)
                      (ghostel--soft-wrap-line-end active-pos limit)))))
           (joined (ghostel--wrap-joined-region begin end limit))
           (text (car joined))
           (chunks (cdr joined))
           (seen (make-hash-table :test 'equal))
           (offset 0))
      (while (string-match wamei/term-links--bare-name-regex text offset)
        (setq offset (match-end 2))
        (let ((name (match-string-no-properties 1 text))
              (loc (match-string-no-properties 2 text))
              (name-beg (match-beginning 1))
              (match-end (match-end 2)))
          ;; `/' が続くならパスの先頭のディレクトリ名。パスは ghostel が拾う。
          (unless (and (< match-end (length text))
                       (eq (aref text match-end) ?/))
            (let* ((beg (ghostel--wrap-offset-to-pos name-beg chunks))
                   (mend (ghostel--wrap-offset-to-pos match-end chunks))
                   (fragments (ghostel--wrap-fragments beg mend))
                   (raw (concat name loc)))
              (unless (ghostel--skip-match-p fragments raw active-bounds)
                (let ((path (let ((cached (gethash name seen 'unset)))
                              (if (eq cached 'unset)
                                  (puthash name (wamei/term-links--resolve
                                                 name default-directory)
                                           seen)
                                cached))))
                  (when path
                    (ghostel--linkify fragments (concat "fileref:" path loc) raw)))))))))))

(defun wamei/term-links--after-detect (orig &rest args)
  "`ghostel--detect-urls' の advice。ghostel の走査の範囲でファイル名も拾う。"
  (let ((bounds (apply orig args)))
    (when (consp bounds)
      (wamei/term-links--detect-bare-names (car bounds) (cdr bounds)))
    bounds))

;;;###autoload
(define-minor-mode wamei/term-links-mode
  "ghostel の端末で `/' を含まないファイル名もリンクにする。"
  :global t
  :group 'ghostel
  (if wamei/term-links-mode
      (advice-add 'ghostel--detect-urls :around #'wamei/term-links--after-detect)
    (advice-remove 'ghostel--detect-urls #'wamei/term-links--after-detect)))

(provide 'term-links)
;;; term-links.el ends here
