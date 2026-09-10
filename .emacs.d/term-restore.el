;;; term-restore.el --- desktop で端末 (ghostel) のスクロールバックを復元する -*- lexical-binding: t; -*-

;;; Commentary:

;; 端末バッファそのものの保存・復元は ghostel-desktop.el が受け持つ
;; (`ghostel-mode' が `desktop-save-buffer' を設定し、復元ハンドラは ghostel の
;; load 時に `desktop-buffer-mode-handlers' へ登録される)。ただし復元されるのは
;; ディレクトリと identity だけで、スクロールバックは戻らない。
;;
;; ここでは端末ごとに
;;   バッファ名 / 作業ディレクトリ / タイトル (最後のコマンド) /
;;   スクロールバックの末尾 N 行
;; を `wamei/term-restore-saved' に記録し、desktop のグローバル変数として
;; 一緒に保存する。
;;
;; スクロールバックは desktop ファイルを太らせないよう別ファイル
;; (`wamei/term-restore-directory' 配下) に置き、内容が変わったときだけ書く。
;; 復元時は `ghostel-pre-spawn-hook' でそのパスを WAMEI_TERM_RESTORE に載せ、
;; .zshrc が起動時に cat する。色は ghostel が付けた face を SGR エスケープに
;; 写して書いておき、cat したときに端末が解釈する。.zshrc は cat の前に画面と
;; スクロールバックを消す。login のバナー ("Last login: ...") は .zshrc より前に
;; 出るので、消さないと復元した出力の上に残り、次の保存でそれごと巻き取られて
;; 復元のたびに 1 行ずつ増えていく。注入が効くのは起動時の
;; desktop 復元の間だけで、復元の仕上げ (`wamei/term-restore-ensure') が記録を
;; 空にして `wamei/term-restore--restoring' を下ろす。以降は同じ名前で開き直した
;; 端末に古い出力が出ることはない (記録は autosave が作り直すので、記録を
;; 空にするだけでは足りない)。`desktop-read' が desktop ファイルを読めなかった
;; セッション (ファイルが無い / 他のインスタンスがロックを持っている) では
;; `desktop-after-read-hook' が走らないので、その 2 つのフックからも窓を閉じる。
;;
;; `desktop-restore-eager' (init.el では 10) を超えた端末は desktop が idle 復元に
;; 回すため、side window の復元 (desktop-side-windows) に間に合わないことがある。
;; `wamei/term-restore-ensure' が `desktop-after-read-hook' で取りこぼしを作り、
;; タイトルを戻し、記録を空にする。

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
:name (バッファ名) :directory (作業ディレクトリ)
:title (最後に報告されたタイトル、無ければ nil) :scrollback (書き出したファイル)
を持つ。`desktop-globals-to-save' 経由で desktop ファイルに書かれる。
復元時に `wamei/term-restore--inject-scrollback' が :injected t を足し、
同じ記録からの注入を 1 回に留める (`wamei/term-restore-save' が記録を
作り直すときは付かない)。")

(defvar wamei/term-restore--restoring t
  "起動時の desktop 復元がまだ終わっていなければ非 nil。
`wamei/term-restore--inject-scrollback' はこのフラグが立っている間だけ
スクロールバックを注入し、`wamei/term-restore-ensure' が復元の仕上げで下ろす。

意図的に**セッションスコープ**にしてある (`desktop-globals-to-save' には
入れない)。記録 (`wamei/term-restore-saved') は desktop ファイルに永続化する
必要があるが、「復元中かどうか」を永続化すると再起動なしで注入が復活し、
kill した端末の出力が同じ名前で開き直した新しいシェルに再生されてしまう。
記録は `desktop-save-mode' の autosave (30 秒アイドル) が何度でも作り直すので、
記録ごとの印 (:injected) だけでは窓の外側を押さえられない。

初期値が t なのは、`ghostel-desktop' が `desktop-read' の中で端末を復元する
時点ではまだどのフックも走っていないため。窓を閉じるのは
`wamei/term-restore--finish-restoring' で、`desktop-read' がファイルを読めた
ときの `desktop-after-read-hook' (`wamei/term-restore-ensure' 経由) だけでなく、
読めなかった 2 つの分岐 (`desktop-no-desktop-file-hook' /
`desktop-not-loaded-hook') からも呼ぶ。どちらの分岐でも
`desktop-save-mode' の autosave は動き続けて記録を埋めるので、「記録が空だから
注入は起きない」では済まない。")

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
端末は画面の下端まで空行で埋めるので、そのままだと空行ばかりになる。
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
端末は色を #rrggbb で付けるので自前で読む。`color-name-to-rgb' は
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
  "ghostel の `face' plist FACE を SGR の引数 (文字列のリスト) にする。
属性、前景色、背景色の順。ghostel は装飾のないセルには face を付けないので、
ここに来る色は端末が明示したものだけ。"
  (let ((params nil))
    (when (eq (plist-get face :weight) 'bold) (push "1" params))
    (when (eq (plist-get face :slant) 'italic) (push "3" params))
    (when (plist-get face :underline) (push "4" params))
    (when (plist-get face :inverse-video) (push "7" params))
    (when (plist-get face :strike-through) (push "9" params))
    (pcase-dolist (`(,key ,code) '((:foreground "38") (:background "48")))
      (when-let* ((rgb (wamei/term-restore--rgb (plist-get face key))))
        (push (format "%s;2;%d;%d;%d" code (nth 0 rgb) (nth 1 rgb) (nth 2 rgb))
              params)))
    (nreverse params)))

(defun wamei/term-restore--ansi (text)
  "TEXT の `face' (ghostel が色ごとに付ける plist) を SGR エスケープにして
プロパティなしの文字列で返す。face が同じ区間ごとに開始のエスケープを置き、
区間の終わりで \\e[0m に戻す。"
  (let ((pos 0)
        (parts nil))
    (while (< pos (length text))
      (let* ((next (or (next-single-property-change pos 'face text)
                       (length text)))
             (chunk (substring-no-properties text pos next))
             (params (wamei/term-restore--sgr-params
                      (get-text-property pos 'face text))))
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

(defvar ghostel-title)                  ; ghostel.el (buffer-local)
(defvar ghostel-pre-spawn-hook)         ; ghostel.el
(declare-function ghostel-create "ghostel" (&optional name display identity))

;; term-panel.el と同じ理由で autoload を張る (ghostel 側に cookie が無い)。
;; ここは desktop 復元の取りこぼしを作る経路で、`condition-case' に包まれて
;; いるので void-function になっても message が出るだけで黙って捨てられる。
(autoload 'ghostel-create "ghostel")

(defun wamei/term-restore--prompt-line-p ()
  "現在行がプロンプト (と入力) の行なら非 nil。
ghostel は OSC 133 のシェル統合 (bash/fish/zsh に自動注入される) で受け取った
libghostty の行単位のセマンティック状態を写し、プロンプトの行には
`ghostel-prompt'、入力の行には `ghostel-input' を付ける。行内 (末尾の改行を
含む) にどちらかの印があればプロンプトの行とみなす。

`ghostel-prompt' だけでは足りない。プロンプト開始の印 (133;A) は
`precmd' で PROMPT に埋め込まれるが、後から PROMPT を組み直す構成では
残らないことがあり、そのときは ghostel の zle-line-init フォールバック
(133;P;k=i) が代わりに立つ。libghostty はプロンプトの行ごと INPUT として
持つので、印は `ghostel-input' だけになる。この場合に末尾のプロンプトが
落ちないと、復元のたびに空のプロンプトが 1 組ずつ溜まっていく。"
  (let ((beg (line-beginning-position))
        (end (min (point-max) (1+ (line-end-position)))))
    (or (text-property-any beg end 'ghostel-prompt t)
        (text-property-any beg end 'ghostel-input t))))

(defun wamei/term-restore--content ()
  "現在のバッファの内容を、末尾のプロンプト行と空行を除いて返す。
末尾から空行を飛ばし、プロンプトの行が続く限り遡って切る。最後の行が
プロンプトでなければ (コマンド実行中) 何も落とさない。
色 (face) は残し、書き出すときに `wamei/term-restore--ansi' で SGR にする。"
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
  "PROJECT の INDEX 番目の端末 BUFFER の記録を作り、スクロールバックを書き出す。
PROJECT と INDEX はスクロールバックのファイル名にだけ使う。"
  (let ((file (wamei/term-restore--scrollback-file project index)))
    (with-current-buffer buffer
      (wamei/term-restore--write-scrollback
       file
       (wamei/term-restore--ansi
        (wamei/term-restore--tail (wamei/term-restore--content)
                                  wamei/term-restore-scrollback-lines)))
      (list :name (buffer-name buffer)
            :directory default-directory
            ;; ghostel 未ロードで `ghostel-title' が unbound のことがある
            ;; (`wamei/term-restore-ensure' 側の `bound-and-true-p' と対称にする)
            :title (and (boundp 'ghostel-title) (buffer-local-value 'ghostel-title buffer))
            :scrollback file))))

(defun wamei/term-restore--prune (entries)
  "`wamei/term-restore-directory' から ENTRIES が参照しないファイルを消す。
`file-regular-p' で絞るのは、サブディレクトリが混じったときに
`delete-file' が `desktop-save-hook' の中で signal しないため。"
  (when (file-directory-p wamei/term-restore-directory)
    (let ((referenced (mapcar (lambda (entry) (plist-get entry :scrollback)) entries)))
      (dolist (file (directory-files wamei/term-restore-directory t "\\`[^.]"))
        (when (and (file-regular-p file)
                   (not (member file referenced)))
          (delete-file file))))))

(defun wamei/term-restore-save ()
  "全ての端末を `wamei/term-restore-saved' に記録し、スクロールバックを書き出す。
`desktop-save-hook' から呼ぶ。"
  (let ((entries (mapcar (lambda (item) (apply #'wamei/term-restore--entry item))
                         (wamei/term-restore--terminal-buffers))))
    (wamei/term-restore--prune entries)
    (setq wamei/term-restore-saved entries)))

;;; 復元

(defun wamei/term-restore--entry-for (name)
  "バッファ名 NAME の記録。無ければ nil。"
  (seq-find (lambda (entry) (equal (plist-get entry :name) name))
            wamei/term-restore-saved))

(defun wamei/term-restore--inject-scrollback ()
  "この端末に記録があれば WAMEI_TERM_RESTORE に載せる。同じ記録では 1 回だけ。
`ghostel-pre-spawn-hook' から呼ぶ。このフックは端末バッファで
`process-environment' を動的束縛した状態で呼ばれるので、`setenv' がそのまま
子プロセスに届く。

記録を消すのはここではなく `wamei/term-restore-ensure'
\(desktop の復元が終わったとき)。ここで消すと、ghostel-desktop が
`desktop-read' 中に復元した端末の記録が仕上げに届かず、タイトルを戻せない。
代わりに注入を 2 段で絞る。このフックはグローバルなので、記録が残っている間は
すべての端末の spawn で走ってしまう:

- `wamei/term-restore--restoring' (セッションスコープ): 注入が効くのは起動時の
  desktop 復元の窓の中だけ。`wamei/term-restore-ensure' が窓を閉じるので、
  その後 `desktop-save-mode' の autosave が記録を作り直しても、端末を kill して
  同じ名前で開き直しても、死んだ端末の出力は再生されない。
- 記録ごとの :injected の印: 窓の中で同じ記録から二重に注入しない。"
  (when wamei/term-restore--restoring
    (when-let* ((entry (wamei/term-restore--entry-for (buffer-name)))
                ((not (plist-get entry :injected)))
                (file (plist-get entry :scrollback)))
      (when (file-readable-p file)
        ;; entry は非 nil なので plist-put はその場で書き換わる
        ;; (`wamei/term-restore-saved' に入っている cons をそのまま触る)
        (plist-put entry :injected t)
        (setenv "WAMEI_TERM_RESTORE" file)))))

(defun wamei/term-restore--entry-directory (entry)
  "記録 ENTRY の作業ディレクトリ。無くなっていればホーム。
変数 `wamei/term-restore-directory' (スクロールバックの置き場) とは別物。"
  (let ((directory (plist-get entry :directory)))
    (if (and directory (file-directory-p directory))
        (file-name-as-directory directory)
      (expand-file-name "~/"))))

(defun wamei/term-restore--drop-lazy-queue-entry (name)
  "NAME の端末を `desktop-buffer-args-list' の遅延キューから取り除く。
`desktop-create-buffer' (desktop.el) は既存バッファ名を検査せず、名前が
食い違えば `rename-buffer' で uniquify するだけなので、`desktop-restore-eager'
を超えて lazy 復元に回された端末をここで先に作っても、キューに残ったままだと
後の idle 復元が同名をもう一つ作ってしまう (そのときは記録も空なので
スクロールバックも付かない)。キューの各要素は `desktop-create-buffer' への
引数リストで、バッファ名は (nth 2 args)。

キューが空になったら `desktop-lazy-timer' も止める。`desktop-idle-create-buffers'
のタイマ停止 (`unless desktop-buffer-args-list' → `cancel-timer') は `while' の
内側にあるので、外からキューを空にするとタイマが残り、毎アイドルで空振りする。"
  (setq desktop-buffer-args-list
        (seq-remove (lambda (args) (equal (nth 2 args) name))
                    desktop-buffer-args-list))
  (unless desktop-buffer-args-list
    (when (bound-and-true-p desktop-lazy-timer)
      (cancel-timer desktop-lazy-timer)
      (setq desktop-lazy-timer nil))))

(defun wamei/term-restore--finish-restoring ()
  "スクロールバック注入の窓を閉じる (`wamei/term-restore--restoring' を下ろす)。
`desktop-read' がファイルを読めなかった 2 つの分岐
\(`desktop-no-desktop-file-hook' / `desktop-not-loaded-hook') からも
`wamei/term-restore-ensure' の後始末からも呼ぶ。

注入は「復元された端末に前回の出力を再生する」ためだけの仕組みなので、
desktop を読まなかったセッションでは最初から注入する理由が無い。読めなかったと
分かった時点で窓を閉じておかないと、`desktop-save-mode' の autosave が記録を
埋めた後に端末を kill して同名で開き直したとき、死んだ端末の出力が新しい
シェルに再生されてしまう。"
  (setq wamei/term-restore--restoring nil))

(defun wamei/term-restore-ensure ()
  "desktop の復元の仕上げ。取りこぼした端末を作り、タイトルを戻し、記録を空にする。
`desktop-after-read-hook' から (深さ -10 で) 呼ぶ。side window の開き直し
\(desktop-side-windows) より先に走らせ、パネルに出すバッファを用意しておく。

端末の生成は `desktop-restore-eager' (init.el では 10) を超えて idle 復元に
回されたものの取りこぼし。ghostel-desktop が `desktop-read' 中に復元していれば
生成は起きず、タイトルの復元だけが効く。端末が既にタイトルを報告していれば
そちらを残す。記録を最後に空にするのは、同じ名前で開き直した端末に前回の
出力を再生しないため。あわせて `wamei/term-restore--restoring' を下ろし、
以降の spawn では注入そのものが起きないようにする (記録は autosave が何度でも
作り直すので、記録を空にするだけでは足りない)。`unwind-protect' で括るのは、
desktop 復元中の C-g (quit) でも記録とフラグが残らないようにするため
\(`condition-case' は error しか拾わない)。"
  (unwind-protect
      (dolist (entry wamei/term-restore-saved)
        (let ((name (plist-get entry :name)))
          (condition-case err
              (let ((buffer (or (get-buffer name)
                                (let ((default-directory
                                       (wamei/term-restore--entry-directory entry)))
                                  (ghostel-create name)))))
                (wamei/term-restore--drop-lazy-queue-entry name)
                (when-let* ((title (plist-get entry :title)))
                  (with-current-buffer buffer
                    (unless (bound-and-true-p ghostel-title)
                      (setq-local ghostel-title title)))))
            (error (message "term-restore: %s を復元できません: %s"
                            name (error-message-string err))))))
    (setq wamei/term-restore-saved nil)
    (wamei/term-restore--finish-restoring)))

;;; desktop への組み込み

(defun wamei/term-restore-setup ()
  "desktop の保存・読み込みと `ghostel-pre-spawn-hook' に組み込む。"
  ;; 記録は desktop ファイルに永続化するが、`wamei/term-restore--restoring'
  ;; (復元中かどうか) は意図的に永続化しない。詳しくは同変数の docstring。
  (add-to-list 'desktop-globals-to-save 'wamei/term-restore-saved)
  (add-hook 'desktop-save-hook #'wamei/term-restore-save)
  ;; 端末の起動時にスクロールバックを環境変数で渡す
  ;; (ghostel-desktop の復元経路でも wamei/term-restore-ensure でも通る)
  (add-hook 'ghostel-pre-spawn-hook #'wamei/term-restore--inject-scrollback)
  ;; desktop-side-windows の開き直しより先に、取りこぼした端末を用意して
  ;; タイトルを戻し、記録を空にする
  (add-hook 'desktop-after-read-hook #'wamei/term-restore-ensure -10)
  ;; desktop-after-read-hook は `desktop-read' がファイルを読めたときしか
  ;; 走らない。読めなかった 2 つの分岐 (ファイルが無い / 他のインスタンスが
  ;; ロックを持っている) でも注入の窓を閉じる。ここを繋がないとフラグが
  ;; セッション中ずっと t のまま残り、autosave が記録を埋めた後に端末を
  ;; kill して開き直すと死んだ端末の出力が再生される。
  (add-hook 'desktop-no-desktop-file-hook #'wamei/term-restore--finish-restoring)
  (add-hook 'desktop-not-loaded-hook #'wamei/term-restore--finish-restoring))

(provide 'term-restore)
;;; term-restore.el ends here
