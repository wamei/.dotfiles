;;; term-modeline.el --- 端末パネルの mode-line -*- lexical-binding: t; -*-

;;; Commentary:

;; フレーム下部の端末パネル (term-panel.el) に出ている ghostel バッファへ、
;; 情報を絞った 1 行の mode-line を入れる。
;;
;;   2/3  ls -al   src/lib                                    ✗1  ⠹
;;   └番号 └タイトル └cwd                                     └status └入力モード/進捗
;;
;; - 番号 (N/M): バッファ名 "*term: <project>[ N]*" から取る。端末が 1 つしか
;;   ないときは出さない。プロジェクト名は出さない (タブと重複するため)。
;; - タイトル: `ghostel-title' (OSC 0/2)。.zshrc の preexec が最後に実行した
;;   コマンドを流している。無ければシェル名。
;; - cwd: `default-directory' (OSC 7 で追従) を、その端末を開いたディレクトリ
;;   からの相対で出す。直下なら出さない。外に出たら絶対パス。
;; - 終了ステータス: OSC 133;D の公開フック `ghostel-command-finish-functions'
;;   で受けて持っておく。非 0 のときだけ出す。プロンプトの再描画も D を出すので
;;   (C を伴わない)、C を見たときだけ記録する。
;; - 入力モードと進捗: ghostel が `mode-line-process' に入れる ":Copy" /
;;   ":Emacs" / スピナーをそのまま右端に出す。mouse-1 で抜けるキーマップ付き。
;;
;; `:eval' は redisplay ごとに走るので、ここに重い処理を置かないこと。特に
;; term-panel.el の `wamei/term--buffers' や `wamei/term--index' は
;; `project-current' を通るので使わず、バッファ名の解析で済ませている。
;;
;; mode-line の右寄せと %-エスケープは claude-usage.el (Claude パネルの
;; mode-line) と共通なので、このファイルが公開関数として持つ。
;; テストは term-modeline-test.el。

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar ghostel-shell)                  ; ghostel.el
(defvar ghostel-title)                  ; ghostel.el (buffer-local)
(declare-function hide-mode-line-mode "hide-mode-line")

;;; mode-line の共通部品 (claude-usage.el と共有)

(defun wamei/term-modeline-escape (string)
  "STRING の % を二重にする。テキストプロパティは保つ。
mode-line は `:eval' が返した文字列の中の %-construct も展開するので、
そのままだと \"100%\" の % やタイトルの中の %s が消える。"
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (let ((props (text-properties-at (1- (point)))))
        (insert "%")
        (set-text-properties (1- (point)) (point) props)))
    (buffer-string)))

(defun wamei/term-modeline-align (width)
  "WIDTH 桁のものを右端に寄せるための詰め物を返す。WIDTH が 0 なら空文字列。
既に右端を越えている行では `:align-to' は後戻りできないので、詰め物は
幅 0 になり、右に置くものはそのまま続けて描かれる。"
  (if (zerop width)
      ""
    (propertize " " 'display `(space :align-to (- right ,width)))))

(defun wamei/term-modeline-process-width ()
  "`mode-line-process' を描いたときの桁数。無ければ 0。
ghostel はここに入力モードのタグ (\":Copy\" など) と OSC 9;4 の進捗を入れる。
文字列とは限らずスピナーと合成された mode-line construct のこともあるので
`format-mode-line' で測る。"
  (if mode-line-process
      (string-width (format-mode-line mode-line-process))
    0))

;;; バッファ名

(defconst wamei/term-modeline--name-regexp
  "\\`\\*term: \\(.+?\\)\\(?: \\([0-9]+\\)\\)?\\*\\'"
  "端末パネルのバッファ名。1 番目がプロジェクト名、2 番目が端末番号。
プロジェクト名は空白を含みうるので最短一致にし、末尾の数字だけを番号として
切り出す (term-panel.el の `wamei/term--buffer-name' と対の形)。")

(defun wamei/term-modeline--parse-name (name)
  "NAME が端末パネルのバッファなら (プロジェクト名 . 番号) を返す。違えば nil。"
  (when (string-match wamei/term-modeline--name-regexp name)
    (cons (match-string 1 name)
          (string-to-number (or (match-string 2 name) "1")))))

(defun wamei/term-modeline--count (project)
  "PROJECT の端末バッファの数。
`wamei/term--buffers' と違い `project-current' を通らない (redisplay ごとに
呼ばれるため)。バッファ名から見えるプロジェクト名で数えるだけ。"
  (seq-count (lambda (buffer)
               (equal project (car (wamei/term-modeline--parse-name
                                    (buffer-name buffer)))))
             (buffer-list)))

(defun wamei/term-modeline--position (index total)
  "端末番号の表示。TOTAL が 1 なら nil (番号に意味がない)。"
  (and index (> total 1) (format "%d/%d" index total)))

;;; カレントディレクトリ

(defvar-local wamei/term-modeline--root nil
  "この端末を開いたディレクトリ。cwd を相対で出すときの起点。")

(defun wamei/term-modeline--relative-dir (dir root)
  "DIR を ROOT からの相対で返す。
ROOT 直下なら nil。ROOT の外や ROOT が nil なら絶対パス (HOME は ~ に略す)。"
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (root (and root (file-name-as-directory (expand-file-name root)))))
    (cond
     ((and root (equal dir root)) nil)
     ((and root (string-prefix-p root dir))
      (directory-file-name (substring dir (length root))))
     (t (directory-file-name (abbreviate-file-name dir))))))

;;; 終了ステータス

(defvar-local wamei/term-modeline--exit-status nil
  "直前のコマンドの終了ステータス。未実行や実行中は nil。")

(defvar-local wamei/term-modeline--command-seen nil
  "OSC 133;C (コマンド開始) を見たら非 nil。
プロンプトの再描画も D を出すので、C を伴わない D は無視するために持つ。")

(defun wamei/term-modeline--on-command-start (buffer)
  "BUFFER でコマンドが始まった。`ghostel-command-start-functions' から。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq wamei/term-modeline--command-seen t
            wamei/term-modeline--exit-status nil)
      (force-mode-line-update))))

(defun wamei/term-modeline--on-command-finish (buffer status)
  "BUFFER のコマンドが STATUS で終わった。`ghostel-command-finish-functions' から。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when wamei/term-modeline--command-seen
        (setq wamei/term-modeline--command-seen nil
              wamei/term-modeline--exit-status status)
        (force-mode-line-update)))))

(defun wamei/term-modeline--status-string (status &optional separate)
  "STATUS の表示。0 と nil のときは空文字列。
右端に寄せる側の要素なので、区切りの空白は自分の頭に持つ。SEPARATE が
非 nil (= 右に入力モードタグやスピナーが続く) なら後ろにも空白を置く。"
  (if (and (integerp status) (/= status 0))
      (concat " " (propertize (format "✗%d" status) 'face 'error)
              (if separate " " ""))
    ""))

;;; 1 行の組み立て

(defconst wamei/term-modeline--title-min-width 8
  "ディレクトリを残したままタイトルに確保したい最小の桁数。
これを割るならディレクトリを捨ててタイトルに幅を回す。")

(defun wamei/term-modeline--state ()
  "1 行に出す材料を集める。"
  (let* ((parsed (wamei/term-modeline--parse-name (buffer-name)))
         (project (car parsed))
         (index (cdr parsed)))
    (list :position (and project
                         (wamei/term-modeline--position
                          index (wamei/term-modeline--count project)))
          :title (or (and (boundp 'ghostel-title) ghostel-title)
                     (file-name-nondirectory
                      (if (boundp 'ghostel-shell) ghostel-shell shell-file-name)))
          :dir (wamei/term-modeline--relative-dir
                default-directory wamei/term-modeline--root))))

(defun wamei/term-modeline--render (state width)
  "STATE を WIDTH 桁に収まる 1 行にする。"
  (let* ((position (plist-get state :position))
         (title (or (plist-get state :title) ""))
         (dir (plist-get state :dir))
         (head (if position (concat (propertize position 'face 'shadow) "  ") ""))
         (avail (max 0 (- width (string-width head))))
         (separator "   ")
         (title-width (if dir
                          (- avail (string-width dir) (length separator))
                        avail)))
    (when (and dir (< title-width wamei/term-modeline--title-min-width))
      (setq dir nil
            title-width avail))
    (concat head
            (propertize (truncate-string-to-width title (max 0 title-width) nil nil t)
                        'face 'mode-line-buffer-id)
            (when dir
              (concat separator (propertize dir 'face 'shadow))))))

;;; mode-line-format

(defun wamei/term-modeline--status ()
  "右端に出す終了ステータス。入力モードタグが続くなら区切りの空白も置く。"
  (wamei/term-modeline--status-string
   wamei/term-modeline--exit-status
   (> (wamei/term-modeline-process-width) 0)))

(defun wamei/term-modeline--right-width ()
  "右端に寄せる部分 (終了ステータス + 入力モードタグ) の桁数。"
  (+ (string-width (wamei/term-modeline--status))
     (wamei/term-modeline-process-width)))

(defun wamei/term-modeline--left ()
  "左に出す 1 行。`:eval' から呼ばれるので描画中の window の幅で詰める。"
  (wamei/term-modeline-escape
   (wamei/term-modeline--render
    (wamei/term-modeline--state)
    (max 0 (- (window-width) (wamei/term-modeline--right-width) 1)))))

(defun wamei/term-modeline--spacer ()
  "左の 1 行と右端の間を埋める詰め物。"
  (wamei/term-modeline-align (wamei/term-modeline--right-width)))

(defconst wamei/term-modeline-format
  '(" " (:eval (wamei/term-modeline--left))
    (:eval (wamei/term-modeline--spacer))
    (:eval (wamei/term-modeline--status))
    mode-line-process)
  "端末パネルのバッファに入れる `mode-line-format'。")

;;; 有効化

(defun wamei/term-modeline-setup ()
  "ghostel バッファの mode-line を決める。`ghostel-mode-hook' から呼ぶ。
端末パネルのバッファには自前の 1 行を入れ、それ以外の ghostel バッファ
\(Claude のパネルなど) は今までどおり mode-line を隠す。Claude のパネルは
claude-usage.el が改めて表に戻して使用量を出す。"
  (if (wamei/term-modeline--parse-name (buffer-name))
      (progn
        ;; 既に隠されているバッファに入れ直すとき (設定の再読み込みなど) は
        ;; 先に戻す。hide-mode-line-mode は自分を切るときに元の
        ;; `mode-line-format' (= nil) を書き戻すので、順番を逆にすると消える。
        (when (bound-and-true-p hide-mode-line-mode)
          (hide-mode-line-mode -1))
        (setq-local wamei/term-modeline--root default-directory)
        (setq-local mode-line-format wamei/term-modeline-format))
    (when (fboundp 'hide-mode-line-mode)
      (hide-mode-line-mode 1))))

(defun wamei/term-modeline-enable ()
  "終了ステータスの追従を有効にする。何度呼んでもよい。
`ghostel-command-start-functions' / `-finish-functions' は端末ごとではなく
グローバルなフック (バッファを引数に取る) なので、ここで 1 度だけ足す。"
  (add-hook 'ghostel-command-start-functions #'wamei/term-modeline--on-command-start)
  (add-hook 'ghostel-command-finish-functions #'wamei/term-modeline--on-command-finish))

(provide 'term-modeline)
;;; term-modeline.el ends here
