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
(defvar ghostel--term-cols)                   ; ghostel.el (buffer-local)
(declare-function ghostel--soft-wrap-line-beginning "ghostel-links" (pos limit))
(declare-function ghostel--soft-wrap-line-end "ghostel-links" (pos limit))
(declare-function ghostel--wrap-joined-region "ghostel-links" (begin end limit))
(declare-function ghostel--wrap-offset-to-pos "ghostel-links" (offset chunks))
(declare-function ghostel--wrap-fragments "ghostel-links" (beg end))
(declare-function ghostel--skip-match-p "ghostel-links" (fragments raw active-bounds))
(declare-function ghostel--linkify "ghostel-links" (fragments uri raw))
(declare-function ghostel--detect-urls "ghostel-links" (&optional begin end))

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

(defvar wamei/term-links--refresh-pending (make-hash-table :test 'equal)
  "索引の作り直しを予約済みのプロジェクトのルート。")

(defvar wamei/term-links--schedule-function #'run-with-idle-timer
  "索引の作り直しを予約する関数。`run-with-idle-timer' と同じ引数を取る。
テストでは即座に呼ぶ関数に差し替える。")

(defun wamei/term-links--rescan-visible (root)
  "ROOT の中にいる ghostel の端末のうち、表示されている範囲を走査し直す。"
  (walk-windows
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (when (and (derived-mode-p 'ghostel-mode)
                  (file-in-directory-p default-directory root))
         (ghostel--detect-urls (window-start window) (window-end window t)))))
   'nomini t))

(defun wamei/term-links--refresh-index (project)
  "PROJECT の索引を作り直し、表示中の端末にリンクを付け直す。"
  (let ((root (expand-file-name (project-root project))))
    (remhash root wamei/term-links--refresh-pending)
    (puthash root (cons (current-time) (wamei/term-links--build-index project))
             wamei/term-links--index-cache)
    (wamei/term-links--rescan-visible root)))

(defun wamei/term-links--lookup (project name)
  "PROJECT の中で basename が NAME のファイルの絶対パスのリスト。
索引に無ければ、古くなった索引の作り直しをアイドル時に予約する。大きな
プロジェクトでは作り直しに 100ms 以上かかるので、出力の描画中には行わない。
作り直した後で表示中の端末を走査し直すので、そのときリンクが付く。"
  (let* ((root (expand-file-name (project-root project)))
         (entry (gethash root wamei/term-links--index-cache))
         (found (and entry (gethash name (cdr entry)))))
    (when (and (not found)
               (not (gethash root wamei/term-links--refresh-pending))
               (or (not entry)
                   (>= (float-time (time-since (car entry)))
                       wamei/term-links-index-refresh-interval)))
      (puthash root t wamei/term-links--refresh-pending)
      (funcall wamei/term-links--schedule-function
               0.5 nil #'wamei/term-links--refresh-index project)
      (setq entry (gethash root wamei/term-links--index-cache)
            found (and entry (gethash name (cdr entry)))))
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

;;;; 端末幅で改行されたパス
;;
;; Claude Code は長い行を端末の折り返しに任せず、自分で改行して次の行を
;; 字下げして続ける。ghostel の折り返しの印 (`ghostel-wrap') が付かないので、
;; 長いパスは 2 行に分かれたまま調べられ、どちらの断片も実在せずリンクに
;; ならない。端末幅ちょうどで終わる行の末尾のパスと、次の行の字下げの後の
;; 続きを繋ぎ、繋いだパスが実在するときだけ 1 つのリンクにする。

(defconst wamei/term-links--wrapped-tail-regex
  (concat "\\(?:^\\|[^[:alnum:]_./~-]\\)"
          "\\([~[:alnum:]_.-]*\\(?:/[^] \t\n\r:\"<>(){}[`']*\\)?\\)\\'")
  "行末まで続くパスの断片。始まりの条件は ghostel のパスの検出と同じ。")

(defconst wamei/term-links--wrapped-head-regex
  "\\`[ \t]*\\([^] \t\n\r:\"<>(){}[`']+\\)\\(\\(?::[0-9]+\\(?::[0-9]+\\)?\\)?\\)"
  "字下げの後に続くパスの断片と、省略できる :LINE[:COL]。")

(defun wamei/term-links--line-bounds (pos)
  "POS の行の (行頭 . 行末)。"
  (save-excursion
    (goto-char pos)
    (cons (line-beginning-position) (line-end-position))))

(defun wamei/term-links--resolve-path (path dir)
  "PATH を DIR から解決した絶対パス。`/' を含まなければ `wamei/term-links--resolve'。
末尾に付いた句読点 (文の終わり) は落としてから調べる。"
  (let ((path (replace-regexp-in-string "[。、．，.,;]+\\'" "" path)))
    (cond ((string-empty-p path) nil)
          ((string-search "/" path)
           (let ((abs (expand-file-name path dir)))
             (and (file-exists-p abs) abs)))
          (t (wamei/term-links--resolve path dir)))))

(defun wamei/term-links--wrapped-chain (line-beg)
  "LINE-BEG の行から始まる、端末幅で改行されたパスの断片。
返り値は (断片の範囲のリスト . (パス . :LINE[:COL])) か nil。"
  (let* ((bounds (wamei/term-links--line-bounds line-beg))
         (line (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when (and (= (string-width line) ghostel--term-cols)
               (string-match wamei/term-links--wrapped-tail-regex line)
               (> (length (match-string 1 line)) 0))
      (let ((fragments (list (cons (+ (car bounds) (match-beginning 1)) (cdr bounds))))
            (path (match-string 1 line))
            (loc "")
            (continue t))
        (while continue
          (setq continue nil)
          (let* ((next (save-excursion
                         (goto-char (cdr bounds))
                         (and (zerop (forward-line 1)) (not (eobp))
                              (wamei/term-links--line-bounds (point)))))
                 (text (and next (buffer-substring-no-properties (car next) (cdr next)))))
            (when (and text (string-match wamei/term-links--wrapped-head-regex text))
              (push (cons (+ (car next) (match-beginning 1))
                          (+ (car next) (match-end 2)))
                    fragments)
              (setq path (concat path (match-string 1 text))
                    loc (match-string 2 text))
              ;; 続きも行末まで埋まっていれば、さらに次の行へ続いている
              (when (and (= (match-end 0) (length text))
                         (string-empty-p loc)
                         (= (string-width text) ghostel--term-cols))
                (setq bounds next continue t)))))
        (when (cdr fragments)
          (cons (nreverse fragments) (cons path loc)))))))

(defun wamei/term-links--detect-hard-wrapped (begin end)
  "BEGIN..END にある、端末幅で改行されたパスをリンクにする。
前の行から続いているものを拾えるよう、BEGIN の 1 行前から調べる。"
  (when (and ghostel-enable-file-detection
             ghostel--term-cols
             (not (file-remote-p default-directory)))
    (let* ((inhibit-read-only t)
           (inhibit-modification-hooks t)
           (active-bounds
            (unless ghostel--inhibit-active-line-skip
              (wamei/term-links--line-bounds (or ghostel--cursor-char-pos (point)))))
           (pos (save-excursion (goto-char begin) (forward-line -1) (point))))
      (while (< pos end)
        (let ((chain (wamei/term-links--wrapped-chain pos)))
          (when chain
            (let* ((fragments (car chain))
                   (path (cadr chain))
                   (loc (cddr chain))
                   (raw (concat path loc)))
              (unless (ghostel--skip-match-p fragments raw active-bounds)
                (when-let* ((abs (wamei/term-links--resolve-path path default-directory)))
                  (ghostel--linkify
                   ;; 落とした句読点の分だけ最後の断片を縮める
                   (let* ((last (car (last fragments)))
                          (cut (- (length path)
                                  (length (replace-regexp-in-string
                                           "[。、．，.,;]+\\'" "" path)))))
                     (if (and (zerop (length loc)) (> cut 0))
                         (append (butlast fragments)
                                 (list (cons (car last) (- (cdr last) cut))))
                       fragments))
                   (concat "fileref:" abs loc)
                   raw))))))
        (setq pos (save-excursion
                    (goto-char pos)
                    (if (zerop (forward-line 1)) (point) end)))))))

(defun wamei/term-links--after-detect (orig &rest args)
  "`ghostel--detect-urls' の advice。ghostel の走査の範囲でファイル名も拾う。"
  (let ((bounds (apply orig args)))
    (when (consp bounds)
      (wamei/term-links--detect-bare-names (car bounds) (cdr bounds))
      (wamei/term-links--detect-hard-wrapped (car bounds) (cdr bounds)))
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
