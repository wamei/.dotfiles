;;; term-modeline.el --- 端末パネルの mode-line -*- lexical-binding: t; -*-

;;; Commentary:

;; フレーム下部の端末パネル (term-panel.el) に出ている ghostel バッファへ、
;; 情報を絞った 1 行の mode-line を入れる。
;;
;;   2/3  ls -al  2026-09-10 22:42:15- (18s)   src/lib  ✗1  ⠹
;;   └番号 └タイトル └実行時刻                 └cwd     └status └入力モード/進捗
;;
;; - 番号 (N/M): バッファ名 "*term: <project>[ N]*" から取る。端末が 1 つしか
;;   ないときは出さない。プロジェクト名は出さない (タブと重複するため)。
;; - タイトル: `ghostel-title' (OSC 0/2)。.zshrc の preexec が最後に実行した
;;   コマンドを流している。無ければシェル名。
;; - 実行時刻: 最後のコマンドの開始時刻と終了時刻、その所要時間。OSC 133;C /
;;   133;D の公開フックで挟んで測る。上は実行中の形で、終わると開始時刻の右が
;;   埋まって "2026-09-10 22:42:15-22:45:06 (2m51s320ms)" になる。所要時間の
;;   ミリ秒は終わってから出す (実行中は 1 秒ごとにしか描き直さないので、
;;   出しても止まって見える)。
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

(defgroup wamei/term-modeline nil
  "端末パネルの mode-line。"
  :group 'tools)

(defface wamei/term-modeline-time '((t :foreground "#9B9B9B"))
  "選択中のウィンドウでの実行時刻の色。
`shadow' (doom-molokai では #555556) は mode-line の地色 #2d2e2e に沈んで
読めず、mode-line の前景色 #d6d6d4 だとタイトルと同じ強さで主張しすぎる。
その中間の灰色で、同じ理由で選んだ `wamei/claude-usage-dim' と同じ値。
地色から前景色までの 65% の位置にあたる。"
  :group 'wamei/term-modeline)

(defface wamei/term-modeline-time-inactive '((t :foreground "#3B3B3C"))
  "選択していないウィンドウでの実行時刻の色。
face に前景色を直に持たせると `mode-line-inactive' に切り替わっても暗く
ならないので、選択の有無で face ごと差し替える (`wamei/term-modeline--time-face')。
色は `wamei/term-modeline-time' と同じ置き方 — `mode-line-inactive' の
地色 #171819 から前景色 #4e4e4e までの 65% の位置。"
  :group 'wamei/term-modeline)

(defface wamei/term-modeline-dim '((t :inherit shadow))
  "選択中のウィンドウでの、番号と cwd の色。
テーマの `shadow' そのまま (doom-molokai では #555556。mode-line の地色
#2d2e2e から前景色 #d6d6d4 までの 24% の位置)。"
  :group 'wamei/term-modeline)

(defface wamei/term-modeline-dim-inactive '((t :foreground "#242526"))
  "選択していないウィンドウでの、番号と cwd の色。
`shadow' は自前の前景色を持つので `mode-line-inactive' でも暗くならず、
周りの本文 #4e4e4e より明るく浮いてしまう。`wamei/term-modeline-dim' と
同じ 24% を `mode-line-inactive' の #171819〜#4e4e4e に当てた値。"
  :group 'wamei/term-modeline)

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

(defvar-local wamei/term-modeline--start-time nil
  "最後に始まったコマンドの開始時刻 (エポック秒)。未実行なら nil。")

(defvar-local wamei/term-modeline--end-time nil
  "最後のコマンドの終了時刻 (エポック秒)。未実行と実行中は nil。")

(defun wamei/term-modeline--on-command-start (buffer)
  "BUFFER でコマンドが始まった。`ghostel-command-start-functions' から。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq wamei/term-modeline--command-seen t
            wamei/term-modeline--exit-status nil
            wamei/term-modeline--start-time (float-time)
            wamei/term-modeline--end-time nil)
      (force-mode-line-update))
    (wamei/term-modeline--start-tick buffer)))

(defun wamei/term-modeline--on-command-finish (buffer status)
  "BUFFER のコマンドが STATUS で終わった。`ghostel-command-finish-functions' から。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when wamei/term-modeline--command-seen
        (setq wamei/term-modeline--command-seen nil
              wamei/term-modeline--exit-status status
              wamei/term-modeline--end-time (float-time))
        (force-mode-line-update)))
    (wamei/term-modeline--stop-tracking buffer)))

(defun wamei/term-modeline--status-string (status &optional separate)
  "STATUS の表示。0 と nil のときは空文字列。
右端に寄せる側の要素なので、区切りの空白は自分の頭に持つ。SEPARATE が
非 nil (= 右に入力モードタグやスピナーが続く) なら後ろにも空白を置く。"
  (if (and (integerp status) (/= status 0))
      (concat " " (propertize (format "✗%d" status) 'face 'error)
              (if separate " " ""))
    ""))

;;; 実行時刻

(defconst wamei/term-modeline--duration-units
  '((3600000 . "h") (60000 . "m") (1000 . "s") (1 . "ms"))
  "所要時間を割る単位。ミリ秒あたりの大きさと接尾辞の組を、大きい順に。")

(defun wamei/term-modeline--format-duration (seconds &optional precision)
  "SECONDS を \"2m51s320ms\" の形にする。0 の単位は間にあっても出さない
\(3605 秒なら \"1h5s\")。全部 0 なら一番小さい単位で 0 を出す。
PRECISION が `milliseconds' ならミリ秒まで、既定は秒まで。実行中は 1 秒ごと
にしか描き直さないので、そこにミリ秒を出しても止まって見えるだけになる。"
  (let* ((units (if (eq precision 'milliseconds)
                    wamei/term-modeline--duration-units
                  (butlast wamei/term-modeline--duration-units)))
         (rest (max 0 (round (* 1000 (or seconds 0)))))
         (parts nil))
    (dolist (unit units)
      (let ((n (/ rest (car unit))))
        (when (> n 0)
          (push (format "%d%s" n (cdr unit)) parts))
        (setq rest (% rest (car unit)))))
    (if parts
        (string-join (nreverse parts))
      (concat "0" (cdr (car (last units)))))))

(defconst wamei/term-modeline--date-time-format "%Y-%m-%d %H:%M:%S"
  "年月日から秒まで。開始時刻はいつも、終了時刻は日をまたいだときだけこの形。")

(defconst wamei/term-modeline--time-only-format "%H:%M:%S"
  "時刻だけ。同じ日に終わった終了時刻は年月日を繰り返さない。")

(defun wamei/term-modeline--time-string (start end now &optional zone)
  "最後のコマンドの時刻。START が nil (まだ何も実行していない) なら nil。
END が nil (実行中) なら開始時刻と NOW までの経過時間、終わっていれば
開始から終了までとその所要時間を出す。ZONE は `format-time-string' に渡す
\(テストから UTC を指定するため。既定はローカル)。"
  (when start
    (let ((from (format-time-string wamei/term-modeline--date-time-format start zone)))
      (if end
          (format "%s-%s (%s)"
                  from
                  (format-time-string
                   (if (equal (format-time-string "%F" start zone)
                              (format-time-string "%F" end zone))
                       wamei/term-modeline--time-only-format
                     wamei/term-modeline--date-time-format)
                   end zone)
                  (wamei/term-modeline--format-duration (- end start) 'milliseconds))
        (format "%s- (%s)" from
                (wamei/term-modeline--format-duration (- now start)))))))

;;; 実行中の再描画

;; 実行中は経過時間が毎秒伸びるので、こちらから mode-line を叩きに行かないと
;; 止まって見える。走っている端末が 1 つも無い間はタイマーを持たない。

(defvar wamei/term-modeline--running nil
  "コマンドが走っている端末バッファ。")

(defvar wamei/term-modeline--tick-timer nil
  "経過時間を伸ばすための 1 秒ごとのタイマー。走っている端末が無ければ nil。")

(defun wamei/term-modeline--tick ()
  "走っている端末の mode-line を描き直す。
端末を殺したままコマンドが終わらないことがある (ghostel ごと消える) ので、
死んだバッファはここで落とす。全部いなくなればタイマーも止める。"
  (setq wamei/term-modeline--running
        (seq-filter #'buffer-live-p wamei/term-modeline--running))
  (dolist (buffer wamei/term-modeline--running)
    (with-current-buffer buffer
      (force-mode-line-update)))
  (unless wamei/term-modeline--running
    (wamei/term-modeline--stop-tick)))

(defun wamei/term-modeline--start-tick (buffer)
  "BUFFER を走っている端末に加え、タイマーが止まっていれば回し始める。"
  (unless (memq buffer wamei/term-modeline--running)
    (push buffer wamei/term-modeline--running))
  (unless wamei/term-modeline--tick-timer
    (setq wamei/term-modeline--tick-timer
          (run-at-time 1 1 #'wamei/term-modeline--tick))))

(defun wamei/term-modeline--stop-tracking (buffer)
  "BUFFER を走っている端末から外し、どこも走っていなければタイマーを止める。"
  (setq wamei/term-modeline--running (delq buffer wamei/term-modeline--running))
  (unless wamei/term-modeline--running
    (wamei/term-modeline--stop-tick)))

(defun wamei/term-modeline--stop-tick ()
  "タイマーを止める。何度呼んでもよい。"
  (when wamei/term-modeline--tick-timer
    (cancel-timer wamei/term-modeline--tick-timer)
    (setq wamei/term-modeline--tick-timer nil)))

;;; 1 行の組み立て

(defconst wamei/term-modeline--title-min-width 8
  "ディレクトリを残したままタイトルに確保したい最小の桁数。
これを割るならディレクトリを捨ててタイトルに幅を回す。")

(defun wamei/term-modeline--time-face (selected)
  "実行時刻の face。SELECTED が nil (非アクティブな mode-line) なら暗いほう。"
  (if selected 'wamei/term-modeline-time 'wamei/term-modeline-time-inactive))

(defun wamei/term-modeline--dim-face (selected)
  "番号と cwd の face。SELECTED が nil (非アクティブな mode-line) なら暗いほう。"
  (if selected 'wamei/term-modeline-dim 'wamei/term-modeline-dim-inactive))

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
          :selected (mode-line-window-selected-p)
          :time (wamei/term-modeline--time-string
                 wamei/term-modeline--start-time
                 wamei/term-modeline--end-time
                 (float-time))
          :dir (wamei/term-modeline--relative-dir
                default-directory wamei/term-modeline--root))))

(defun wamei/term-modeline--segment-width (separator string)
  "STRING を SEPARATOR 付きで足したときに増える桁数。STRING が nil なら 0。"
  (if string (+ (string-width separator) (string-width string)) 0))

(defun wamei/term-modeline--render (state width)
  "STATE を WIDTH 桁に収まる 1 行にする。
タイトルに `wamei/term-modeline--title-min-width' 桁を残せないときは、
cwd・実行時刻の順に捨ててタイトルへ幅を回す (どの端末かを見失うのが一番困る)。"
  (let* ((position (plist-get state :position))
         (title (or (plist-get state :title) ""))
         (time (plist-get state :time))
         (dir (plist-get state :dir))
         ;; 選択の有無が無ければアクティブ扱い (テストと、状態を作らずに
         ;; 描くとき)。`mode-line-window-selected-p' は :eval の中でしか
         ;; 正しく答えられないので、状態に採ってから渡す。
         (selected (if (plist-member state :selected)
                       (plist-get state :selected)
                     t))
         (time-face (wamei/term-modeline--time-face selected))
         (dim-face (wamei/term-modeline--dim-face selected))
         (head (if position (concat (propertize position 'face dim-face) "  ") ""))
         (avail (max 0 (- width (string-width head))))
         (time-separator "  ")
         (dir-separator "   ")
         (title-width (- avail
                         (wamei/term-modeline--segment-width time-separator time)
                         (wamei/term-modeline--segment-width dir-separator dir))))
    (when (and dir (< title-width wamei/term-modeline--title-min-width))
      (setq dir nil
            title-width (- avail (wamei/term-modeline--segment-width
                                  time-separator time))))
    (when (and time (< title-width wamei/term-modeline--title-min-width))
      (setq time nil
            title-width avail))
    (concat head
            (propertize (truncate-string-to-width title (max 0 title-width) nil nil t)
                        'face 'mode-line-buffer-id)
            (when time
              (concat time-separator (propertize time 'face time-face)))
            (when dir
              (concat dir-separator (propertize dir 'face dim-face))))))

;;; ブレイルの大きさ

;; ghostel の進捗スピナー (`ghostel-spinner-type') はブレイル。既定フォントは
;; ブレイルを持たないので Apple Braille に落ちるが、これは総高が既定と同じ
;; 18px でも descent が 5px 深い (既定は 3px)。Emacs の行高はその行の
;; max(ascent) + max(descent) なので、ブレイルが 1 文字混ざった行だけ 2px 伸びる。
;;
;; 端末バッファでこれが起きると、ghostel が端末グリッドを下端に揃えるときに
;; その 2px を `window-vscroll' として払い、選択中のウィンドウが 2px 上下に
;; 揺れる (term-panel.el の「行グリッドへの整列」)。mode-line で起きると、
;; スピナーの出入りで mode-line の高さが 1px 変わり、ウィンドウの本文高さが
;; 行グリッドから外れる。
;;
;; そこで Apple Braille 自体は既定の枠に収まる大きさへ縮め
;; (init.el の font ブロックで `face-font-rescale-alist' に入れる)、
;; mode-line に出すスピナーだけ face の :height で元の大きさへ戻す。
;; 見た目はスピナーだけ従来どおりで、行高はどこも動かない。

(defconst wamei/term-modeline-braille-rescale 0.625
  "Apple Braille を既定フォントの枠に収める倍率。
16px の 0.625 倍 = 10px で ascent 8 / descent 3 になり、既定フォントの
ascent 15 / descent 3 に収まる。`face-font-rescale-alist' に入れる値。
0.625 より大きいと descent が 4px 以上残って行が伸びる (実測)。")

(defconst wamei/term-modeline-braille-unscale
  (list :height (/ 1.0 wamei/term-modeline-braille-rescale))
  "縮めた Apple Braille を元の大きさへ戻す face。
`wamei/term-modeline-braille-rescale' の逆数。mode-line のスピナーと
高さ固定用の空白に付ける。倍率を掛けた要求サイズに rescale が掛かるので、
結果は縮める前と同じ px になる。")

;;; 行グリッドへの詰め物

;; ghostel は端末グリッドをウィンドウ下端に揃える (`ghostel--anchor-window')。
;; 表示している行の高さの合計がウィンドウの本文高さを超えるぶんは
;; `window-vscroll' として払われるので、先頭行が数 px 切れた状態が定常になる。
;; この vscroll は redisplay が point を見せるためにスクロールをやり直すと落ち、
;; 次の再描画で戻る。Claude が動いている間は毎秒数十回再描画が走るので、端数ぶん
;; 画面全体が上下に揺れる。やり直すのは選択中のウィンドウだけなので、揺れるのも
;; 選択中のウィンドウだけになる。
;;
;; 端数は本文高さが行高で割り切れないことから出る。mode-line を余りぶんだけ
;; 厚くすれば本文高さが行境界に乗り、端数は消える。フレームの高さを詰める手も
;; あるが、macOS は短時間の連続リサイズをまとめてしまい `set-frame-height' が
;; 黙って無視されることがあり、高さの違う窓が並んでいると一番高いものにしか
;; 合わせられない。mode-line はバッファごとに決まるので、窓ごとに自分の端数を
;; 吸収でき、ウィンドウマネージャと綱引きもしない。Claude のパネルは
;; claude-usage.el が mode-line の地色をパネルに合わせているので、厚くしても
;; 下端の余白に見える。

(defconst wamei/term-modeline-height-floor 24
  "mode-line の高さの下限 (ピクセル)。
詰め物はこれ以上で最小の、本文高さが行境界に乗る高さを選ぶ。中身が自然に取る
高さ (実測 22px: 使用量バーの画像とブレイルのスピナー) より小さいと詰め物が
効かず端数が残るので、少し余裕を持たせてある。無駄になる余白は
1 行ぶん未満に収まる。")

(defconst wamei/term-modeline-grid-pad-descent 5
  "詰め物が baseline より下に確保する px 数。
mode-line に出るブレイル (Apple Braille) の descent と同じ。これより浅いと
スピナーの descent がはみ出して mode-line が 1px 高くなり、整列が崩れる。")

(defvar wamei/term-modeline--grid-pad-cache nil
  "高さから詰め物の画像への alist。mode-line は毎フレーム評価されるので、
同じ高さの画像を作り直さない。")

(defun wamei/term-modeline-grid-pad-height (available line-height)
  "端末グリッドを行境界に乗せる mode-line の高さを返す。
AVAILABLE は mode-line を除いたウィンドウの高さ (ピクセル)。
`wamei/term-modeline-height-floor' 以上で最小の、(AVAILABLE - 戻り値) が
LINE-HEIGHT の倍数になる高さ。LINE-HEIGHT が取れないときは下限をそのまま返す。"
  (let ((min-height wamei/term-modeline-height-floor))
    (if (and (integerp line-height) (> line-height 0))
        (+ min-height (mod (- available min-height) line-height))
      min-height)))

(defun wamei/term-modeline-grid-pad-ascent (height)
  "高さ HEIGHT px の詰め物に渡す `:ascent' (百分率)。
Emacs は ascent の px を (高さ * 百分率 / 100) の整数除算で出すので、
百分率は切り捨てる。切り上げると descent が 1px 足りなくなり、mode-line に
出るブレイルの descent がはみ出して高さが狂う。"
  (/ (* 100 (- height wamei/term-modeline-grid-pad-descent)) height))

(defun wamei/term-modeline-grid-pad-image (height)
  "高さ HEIGHT px の見えない詰め物の画像。同じ高さならキャッシュを返す。"
  (or (cdr (assq height wamei/term-modeline--grid-pad-cache))
      (let ((image (create-image
                    (format "<svg width=\"1\" height=\"%d\" xmlns=\"http://www.w3.org/2000/svg\"></svg>"
                            height)
                    'svg t :ascent (wamei/term-modeline-grid-pad-ascent height))))
        (push (cons height image) wamei/term-modeline--grid-pad-cache)
        image)))

(defun wamei/term-modeline-grid-pad-spacer (&optional window)
  "mode-line の先頭に置く詰め物。`:eval' から呼ぶ。
描画中の WINDOW (既定は選択中の window) の高さで決まるので、フレームの
リサイズにも自動で追従する。TTY にはピクセルの概念が無いので何も出さない。"
  (let ((window (or window (selected-window))))
    (if (not (display-graphic-p (window-frame window)))
        ""
      (propertize
       " " 'display
       (wamei/term-modeline-grid-pad-image
        (wamei/term-modeline-grid-pad-height
         (- (window-pixel-height window)
            (window-tab-line-height window)
            (window-bottom-divider-width window))
         (with-selected-window window (default-line-height))))))))

;;; mode-line の高さの固定

;; mode-line の高さはそこに載る一番背の高いグリフで決まるので、スピナー
;; (ブレイル) の出入りで 18px ↔ 20px と動く。端末パネルではこれが致命的で、
;; mode-line が動くとウィンドウの本文高さが動き、本文高さを行高で割った端数も
;; 動く。ghostel は端末グリッドを下端に揃えるときこの端数を `window-vscroll'
;; として払う (term-panel.el の「下端揃えの端数」) ので、端数が動くたびに端末の
;; 中身が数 px 上下する。スピナーは実行中ずっと出ているため、コマンドを走らせる
;; たびに画面が跳ねていた。
;;
;; ブレイルと同じ descent を最初から確保した詰め物を mode-line に常駐させれば、
;; スピナーが出ても高さは変わらない。Claude のパネルは「行グリッドへの詰め物」の
;; 床 (24px) が同じ役目を兼ねている。

(defconst wamei/term-modeline-braille-descent-excess 2
  "ブレイル (Apple Braille) の descent が既定フォントより深い px 数。
既定フォントの descent 3px に対してブレイルは 5px (実測)。ascent は既定より
浅いので、ブレイルが混ざった行の高さはちょうど行高 + この値になる。")

(defun wamei/term-modeline-spinner-pad-height (line-height)
  "スピナーが出入りしても動かない mode-line の高さを返す。
LINE-HEIGHT が取れないときは nil (詰め物なし)。"
  (when (and (integerp line-height) (> line-height 0))
    (+ line-height wamei/term-modeline-braille-descent-excess)))

(defun wamei/term-modeline-spinner-pad-spacer (&optional window)
  "mode-line の先頭に置く、高さを固定する詰め物。`:eval' から呼ぶ。
TTY にはピクセルの概念が無いので何も出さない。"
  (let ((window (or window (selected-window))))
    (if (not (display-graphic-p (window-frame window)))
        ""
      (let ((height (wamei/term-modeline-spinner-pad-height
                     (with-selected-window window (default-line-height)))))
        (if height
            (propertize " " 'display (wamei/term-modeline-grid-pad-image height))
          "")))))

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
  ;; 先頭は mode-line の高さを固定する詰め物 (「mode-line の高さの固定」)。
  ;; 幅 1px の画像なので、左端の余白は続く " " で自分で置く。
  '((:eval (wamei/term-modeline-spinner-pad-spacer))
    " " (:eval (wamei/term-modeline--left))
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
