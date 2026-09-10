;;; claude-usage.el --- Claude の使用量を AI サイドバーの mode-line に出す -*- lexical-binding: t; -*-

;;; Commentary:

;; Claude Code の `/usage' が出している「今のセッション / 今週 / Fable」の消費率を
;; claude-code-ide のパネル (右サイドの ghostel バッファ) の mode-line に常時出す。
;;
;; - 取得: `/usage' が叩いている非公開エンドポイント /api/oauth/usage を
;;   OAuth のアクセストークンで直接呼ぶ。トークンは macOS の Keychain
;;   ("Claude Code-credentials")、無ければ ~/.claude/.credentials.json から
;;   取得のたびに読む (Claude Code 本体が失効時に書き換えるので自然に追従する)。
;;   `curl -H' だと argv に載って ps から見えるので、外部プロセスは使わず url.el で送る。
;; - 形: レスポンスの limits 配列から kind が session / weekly_all と、
;;   scope.model.display_name が "Fable" の weekly_scoped を拾う。非公開の
;;   エンドポイントなので、形が変わったら黙って前回値を残し、無ければダッシュを出す。
;; - 共有: エンドポイントは同一アカウントで 1〜2 分に 1 回しか通らず、その枠は
;;   Claude Code 本体のセッションとも共有になる (超えると 429)。各 Emacs が独立に
;;   投げると、後から起動した方は 429 を引き続けたまま前回値も持たないので
;;   ダッシュのままになる。取れた値は `wamei/claude-usage-cache-file' に置き、
;;   起動直後はそこから出す / interval 内に誰かが取っていれば自分は投げない。
;; - 表示: ghostel バッファの mode-line は term-modeline.el が振り分けており、
;;   端末パネル以外 (= Claude のパネル) は `hide-mode-line-mode' で消えている。
;;   Claude のバッファだけそれを外して自前の mode-line を入れる。window を増やさないので desktop の復元
;;   (desktop-side-windows.el) や slot 管理には影響しない。
;;   mode-line は改行できない (Emacs 31 でも "a\nb" は 1 行に ^J で出る) ため 1 行。
;; - バー: SVG の角丸 + グラデーション。画像が使えない端末では ░/█ の文字に落とす。
;;   redisplay ごとに作り直すと重いので、取得世代と分単位の時刻でキャッシュする。

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'face-remap)
(require 'iso8601)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'url)

;; mode-line の右寄せと %-エスケープは端末パネルと共通 (init.el が先に読む)。
(require 'term-modeline)

(declare-function claude-code-ide--buffer-session "claude-code-ide")
(declare-function claude-code-ide-mcp--active-sessions "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp-session-buffer "claude-code-ide-mcp")
(declare-function hide-mode-line-mode "hide-mode-line")

;;; 設定

(defgroup wamei/claude-usage nil
  "Claude の使用量を mode-line に出す。"
  :group 'tools)

(defcustom wamei/claude-usage-endpoint "https://api.anthropic.com/api/oauth/usage"
  "使用量を返すエンドポイント。`/usage' が叩いているものと同じ。"
  :type 'string)

(defcustom wamei/claude-usage-interval 60
  "取得する間隔 (秒)。セッションは 5 時間窓なので分単位で足りる。"
  :type 'integer)

(defcustom wamei/claude-usage-timeout 30
  "1 回の取得を諦めるまでの秒数。`wamei/claude-usage-interval' より短くする。
`url-retrieve' は、スリープ復帰などで接続だけ切れるとコールバックを呼ばないまま
終わることがある。取得中の印が残ったままだと以後の取得が全部素通りして表示が
凍るので、時間切れで見切る。"
  :type 'integer)

(defcustom wamei/claude-usage-cache-file
  (expand-file-name "claude-usage.eld" user-emacs-directory)
  "最後に取れた使用量を置くファイル。Emacs のインスタンス間で共有する。
起動直後の Emacs がここから値を出せるようにし、`wamei/claude-usage-interval'
内に誰かが取っていれば自分は投げないための置き場。"
  :type 'file)

(defcustom wamei/claude-usage-keychain-service "Claude Code-credentials"
  "アクセストークンが入っている Keychain のサービス名 (macOS)。"
  :type 'string)

(defcustom wamei/claude-usage-credentials-file
  (expand-file-name "~/.claude/.credentials.json")
  "Keychain が無い環境でアクセストークンを読むファイル。"
  :type 'file)

(defcustom wamei/claude-usage-bar-columns 6
  "バーの幅 (文字数換算)。SVG のときもこの桁数ぶんの幅で描く。"
  :type 'integer)

(defcustom wamei/claude-usage-bar-height 10
  "SVG バーの高さ (ピクセル)。"
  :type 'integer)

(defcustom wamei/claude-usage-track-opacity 0.55
  "SVG バーの溝の不透明度。mode-line の地の色に対してどれだけ浮かせるか。"
  :type 'float)

(defvar wamei/claude-usage-use-images 'auto
  "SVG でバーを描くか。`auto' なら描くフレームごとに判定する。
`auto' 以外の非 nil なら常に SVG、nil なら常に文字。
ロード時に 1 回だけ決めると、GUI の Emacs から emacsclient -nw で tty
フレームを開いたときに、画像を出せないフレームへ SVG を渡してバーが
空白になる (tty は display プロパティの画像を無視して下の文字を出す)。")

(defface wamei/claude-usage-normal '((t :inherit success))
  "余裕があるときのバーの色。")

(defface wamei/claude-usage-warning '((t :inherit warning))
  "残りが少ないときのバーの色。")

(defface wamei/claude-usage-critical '((t :inherit error))
  "ほぼ使い切っているときのバーの色。")

(defface wamei/claude-usage-dim '((t :foreground "#9B9B9B"))
  "リセット時刻と、値が取れていないときのダッシュの色。
Claude 本体が薄い説明文に使っている色 (パネルのバッファから採取した #9B9B9B)。
Emacs のテーマではなく Claude の TUI に合わせるので、色は直接書く。
`shadow' 相当だとパネルの地色 #1c1e1f に対して暗すぎ、`mode-line' の
前景色 (#d6d6d4) だと本文と同じ明るさになって主張が強すぎる。
S / W / F のラベルはバーや割合と同じ色 (`wamei/claude-usage--face') を使う。
溝の色は `wamei/claude-usage-track'。")

(defface wamei/claude-usage-track '((t :inherit shadow))
  "バーの溝の色。文字ではないので暗くてよい。")

;;; パース

(defconst wamei/claude-usage--fable-model "Fable"
  "weekly_scoped のうち拾う対象のモデル表示名。")

(defun wamei/claude-usage--time (string)
  "ISO8601 の STRING を Lisp 時刻にする。読めなければ nil。"
  (and (stringp string)
       (ignore-errors (encode-time (iso8601-parse string)))))

(defun wamei/claude-usage--entry (limit)
  "レスポンスの limits の要素 LIMIT を (:percent :severity :resets-at) にする。"
  (list :percent (or (alist-get 'percent limit) 0)
        :severity (or (alist-get 'severity limit) "normal")
        :resets-at (wamei/claude-usage--time (alist-get 'resets_at limit))))

(defun wamei/claude-usage--fable-p (limit)
  "LIMIT が Fable に紐づく weekly_scoped なら非 nil。"
  (equal (thread-last limit
                      (alist-get 'scope)
                      (alist-get 'model)
                      (alist-get 'display_name))
         wamei/claude-usage--fable-model))

(defun wamei/claude-usage--parse (body)
  "レスポンス本文 BODY (JSON 文字列) から表示に使う 3 項目を取り出す。
返り値は (:session ENTRY :weekly ENTRY :fable ENTRY) の plist。
1 つも取れなければ nil。壊れた JSON でも例外にしない。"
  (when-let* ((json (ignore-errors
                      (json-parse-string body :object-type 'alist :array-type 'list)))
              (limits (alist-get 'limits json))
              ((listp limits)))
    (let (state)
      (dolist (limit limits)
        (pcase (alist-get 'kind limit)
          ("session" (setq state (plist-put state :session
                                            (wamei/claude-usage--entry limit))))
          ("weekly_all" (setq state (plist-put state :weekly
                                               (wamei/claude-usage--entry limit))))
          ("weekly_scoped"
           (when (wamei/claude-usage--fable-p limit)
             (setq state (plist-put state :fable
                                    (wamei/claude-usage--entry limit)))))))
      state)))

;;; 整形

(defun wamei/claude-usage--format-reset (time now)
  "リセット時刻 TIME を NOW から見た文字列にする。TIME が nil なら nil。
同じ日なら時刻だけ、日をまたぐなら月日も付ける。"
  (when time
    (let ((then (decode-time time))
          (today (decode-time now)))
      (if (and (= (decoded-time-year then) (decoded-time-year today))
               (= (decoded-time-month then) (decoded-time-month today))
               (= (decoded-time-day then) (decoded-time-day today)))
          (format-time-string "%H:%M" time)
        (format-time-string "%-m/%-d %H:%M" time)))))

(defun wamei/claude-usage--steps (percent width)
  "PERCENT を WIDTH 桁 × 2 段に落としたときの段数。0..WIDTH*2 に収める。
半段を使うので桁数の 2 倍の段階が出せる (6 桁なら 12 段)。
丸めで 0 段になっても、0% でなければ半段は出す。SVG のバーも
「0 でない限り最低でも高さぶんの幅」を出しており、0% と 1% が
同じ絵になると使い始めたかどうか分からないため。"
  (let* ((clamped (max 0 (min 100 percent)))
         (steps (round (* width 2 (/ (float clamped) 100)))))
    (if (and (zerop steps) (> clamped 0))
        1
      (max 0 (min (* width 2) steps)))))

(defun wamei/claude-usage--face (entry)
  "ENTRY の severity と割合から使う face を決める。厳しい方を採る。"
  (let* ((percent (or (plist-get entry :percent) 0))
         (by-severity (pcase (plist-get entry :severity)
                        ("warning" 1)
                        ((or "critical" "exceeded" "locked") 2)
                        (_ 0)))
         (by-percent (cond ((>= percent 90) 2)
                           ((>= percent 75) 1)
                           (t 0))))
    (pcase (max by-severity by-percent)
      (2 'wamei/claude-usage-critical)
      (1 'wamei/claude-usage-warning)
      (_ 'wamei/claude-usage-normal))))

;;; バー

(defun wamei/claude-usage--hex (color)
  "COLOR を SVG が読める #rrggbb にする。読めなければそのまま返す。
`color-lighten-name' はチャンネル 16bit の #rrrrggggbbbb を返すが、
rsvg はこれを色として解釈しない。"
  (if (and (stringp color) (string-prefix-p "#" color)
           (string-match-p "\\`#[0-9a-fA-F]+\\'" color)
           (zerop (% (1- (length color)) 3)))
      (let* ((digits (/ (1- (length color)) 3))
             (scale (float (1- (expt 16 digits)))))
        (apply #'format "#%02x%02x%02x"
               (mapcar (lambda (index)
                         (round (* 255 (/ (string-to-number
                                           (substring color
                                                      (+ 1 (* index digits))
                                                      (+ 1 (* (1+ index) digits)))
                                           16)
                                          scale))))
                       '(0 1 2))))
    color))

(defun wamei/claude-usage--text-bar (percent width face)
  "PERCENT を WIDTH 桁の文字バーにする。埋まりは FACE、溝は溝の色。
太線 ━ (U+2501) で埋め、端数は半段の ╸ (U+2578)、残りは細線 ─ (U+2500)。
どれも現行の █ / ░ と同じ East Asian Ambiguous なので端末での幅は変わらない。
埋まりと溝を別の face にするのは SVG のバーと同じ理由で、1 色で塗ると
線の太さの差だけで読むことになり tty では見分けにくい。"
  (let* ((steps (wamei/claude-usage--steps percent width))
         (full (/ steps 2))
         (half (= 1 (mod steps 2))))
    (concat (propertize (concat (make-string full ?━) (if half "╸" ""))
                        'face face)
            (propertize (make-string (- width full (if half 1 0)) ?─)
                        'face 'wamei/claude-usage-track))))

(defun wamei/claude-usage--svg-bar (percent width height track fill)
  "PERCENT を WIDTH x HEIGHT ピクセルの SVG バーにする。
TRACK は溝、FILL は埋まっている部分の色。返り値は画像を display に持つ 1 文字。"
  (let* ((svg (svg-create width height))
         (radius (/ height 2.0))
         ;; 角丸の分だけ両端が細るので、0 でない限り最低でも高さぶんの幅を出す
         (raw (* width (/ (float (max 0 (min 100 percent))) 100)))
         (filled (if (> raw 0) (max raw height) 0))
         (base (wamei/claude-usage--hex fill))
         (highlight (wamei/claude-usage--hex
                     (or (ignore-errors (color-lighten-name base 18)) base))))
    (svg-rectangle svg 0 0 width height
                   :rx radius :ry radius
                   :fill (wamei/claude-usage--hex track)
                   :fill-opacity wamei/claude-usage-track-opacity)
    (when (> filled 0)
      (svg-gradient svg "bar" 'linear `((0 . ,highlight) (100 . ,base)))
      (svg-rectangle svg 0 0 filled height
                     :rx radius :ry radius :gradient "bar"))
    (propertize " " 'display (svg-image svg :scale 1 :ascent 'center))))

(defun wamei/claude-usage--images-p ()
  "いま描いているフレームで SVG バーを使うなら非 nil。
mode-line の (:eval ...) は描画中の window を `selected-window' にして呼ばれる
ので、その window のフレームで判定する。"
  (and (if (eq wamei/claude-usage-use-images 'auto)
           (display-graphic-p (window-frame))
         wamei/claude-usage-use-images)
       (image-type-available-p 'svg)
       t))

(defun wamei/claude-usage--bar (percent face)
  "PERCENT のバー。画像が使えれば SVG、駄目なら文字。色は FACE から取る。"
  (if (wamei/claude-usage--images-p)
      (wamei/claude-usage--svg-bar
       percent
       (* wamei/claude-usage-bar-columns (frame-char-width (window-frame)))
       wamei/claude-usage-bar-height
       (or (face-foreground 'wamei/claude-usage-track nil t) "#888888")
       (or (face-foreground face nil t) "#888888"))
    (wamei/claude-usage--text-bar percent wamei/claude-usage-bar-columns face)))

;;; 1 行の組み立て

(defun wamei/claude-usage--segment (label entry now)
  "LABEL と ENTRY を 1 区画にする。ENTRY が nil ならダッシュだけ。
NOW は当日かどうかの判定に使う (リセット時刻の出し方が変わる)。"
  (if (null entry)
      (propertize (concat label " —") 'face 'wamei/claude-usage-dim)
    (let* ((percent (or (plist-get entry :percent) 0))
           (face (wamei/claude-usage--face entry))
           (reset (wamei/claude-usage--format-reset
                   (plist-get entry :resets-at) now)))
      (concat (propertize label 'face face) " "
              (wamei/claude-usage--bar percent face) " "
              ;; 右揃えにすると 1 桁のときにバーとの間が 3 桁分空くので詰める。
              ;; 桁数が変わると後ろ (↻ 以降) が 1〜2 桁ずれるが、そちらを採る。
              (propertize (format "%d%%" percent) 'face face)
              (if reset
                  (propertize (concat " ↻ " reset) 'face 'wamei/claude-usage-dim)
                "")))))

(defun wamei/claude-usage--render (state now)
  "STATE を NOW から見た 1 行にする。区画ごとにリセット時刻も出す。"
  (mapconcat (lambda (segment)
               (wamei/claude-usage--segment (car segment)
                                            (plist-get state (cdr segment))
                                            now))
             '(("S" . :session) ("W" . :weekly) ("F" . :fable))
             "  "))

;;; 取得

(defvar wamei/claude-usage--state nil
  "最後に取れた使用量。形は `wamei/claude-usage--parse' の返り値。")

(defvar wamei/claude-usage--state-time nil
  "`wamei/claude-usage--state' が取れた時刻。共有ファイルとの新しさ比べに使う。")

(defvar wamei/claude-usage--generation 0
  "取得のたびに増える番号。mode-line のキャッシュを捨てるのに使う。")

(defvar wamei/claude-usage--request nil
  "取得中のリクエスト (ID BUFFER 開始時刻)。終わっていれば nil。
応答が遅いときに重ねて投げないためと、返ってこない取得を見切るために持つ。")

(defvar wamei/claude-usage--request-id 0
  "リクエストごとに増える番号。見切ったあとに遅れて返った応答を見分ける。")

;;; 取れた値の共有

(defun wamei/claude-usage--read-cache ()
  "共有ファイルの中身を (:state ... :time ...) で返す。読めなければ nil。
別のインスタンスが書いている途中や、壊れた中身でも例外にしない。"
  (when (file-readable-p wamei/claude-usage-cache-file)
    (when-let* ((cache (ignore-errors
                         (with-temp-buffer
                           (insert-file-contents wamei/claude-usage-cache-file)
                           (goto-char (point-min))
                           (read (current-buffer)))))
                ((consp cache))
                ((plist-get cache :state))
                ((plist-get cache :time)))
      cache)))

(defun wamei/claude-usage--write-cache (state time)
  "STATE と TIME を共有ファイルに書く。書けなくても取得は続ける。"
  (ignore-errors
    (with-temp-file wamei/claude-usage-cache-file
      (let ((print-level nil)
            (print-length nil))
        (prin1 (list :state state :time time) (current-buffer))
        (insert "\n")))))

(defun wamei/claude-usage--adopt-cache ()
  "共有ファイルの方が新しければ取り込む。取り込んだら非 nil。
他のインスタンスが取ってくれた値を、自分では投げずに使う。"
  (when-let* ((cache (wamei/claude-usage--read-cache))
              (time (plist-get cache :time))
              ((or (null wamei/claude-usage--state-time)
                   (time-less-p wamei/claude-usage--state-time time))))
    (setq wamei/claude-usage--state (plist-get cache :state)
          wamei/claude-usage--state-time time)
    (cl-incf wamei/claude-usage--generation)
    (force-mode-line-update t)
    t))

(defun wamei/claude-usage--fresh-p ()
  "持っている値が `wamei/claude-usage-interval' 内のものなら非 nil。"
  (and wamei/claude-usage--state-time
       (time-less-p (time-since wamei/claude-usage--state-time)
                    wamei/claude-usage-interval)))

;;; 取得

(defun wamei/claude-usage--token-from-json (body)
  "認証情報の JSON 文字列 BODY からアクセストークンを取り出す。"
  (when-let* ((json (ignore-errors
                      (json-parse-string body :object-type 'alist))))
    (thread-last json (alist-get 'claudeAiOauth) (alist-get 'accessToken))))

(defun wamei/claude-usage--token ()
  "アクセストークン。取れなければ nil。
macOS では Keychain、無ければ `wamei/claude-usage-credentials-file' から読む。"
  (or (when (executable-find "security")
        (with-temp-buffer
          (when (zerop (call-process "security" nil t nil
                                     "find-generic-password" "-w"
                                     "-s" wamei/claude-usage-keychain-service))
            (wamei/claude-usage--token-from-json (buffer-string)))))
      (when (file-readable-p wamei/claude-usage-credentials-file)
        (with-temp-buffer
          (insert-file-contents wamei/claude-usage-credentials-file)
          (wamei/claude-usage--token-from-json (buffer-string))))))

(defun wamei/claude-usage--body ()
  "`url-retrieve' のコールバック内で、カレントバッファから本文を取り出す。
ヘッダが無い・空応答なら nil。"
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (decode-coding-string (buffer-substring-no-properties (point) (point-max))
                            'utf-8))))

(defun wamei/claude-usage--abandon ()
  "返ってこない取得を見切る。見切ったら非 nil。
接続だけ切れてコールバックが来ないことがあるので、こちらから打ち切る。
残った接続とバッファも片付ける (放っておくと空の \" *http ...*\" が残る)。"
  (when-let* ((request wamei/claude-usage--request)
              ((time-less-p wamei/claude-usage-timeout
                            (time-since (nth 2 request)))))
    (setq wamei/claude-usage--request nil)
    (let ((buffer (nth 1 request)))
      (when (buffer-live-p buffer)
        (when-let* ((process (get-buffer-process buffer)))
          (set-process-sentinel process #'ignore)
          (ignore-errors (delete-process process)))
        (kill-buffer buffer)))
    t))

(defun wamei/claude-usage--receive (status id)
  "取得の応答を状態に取り込む。STATUS は `url-retrieve' のもの、ID は取得の番号。
失敗しても前回値は残す (非公開のエンドポイントなので黙って劣化させる)。
見切ったあとに遅れて返ってきた応答 (ID が古い) は、取得中の印を落とさない。"
  (when (eq id (car wamei/claude-usage--request))
    (setq wamei/claude-usage--request nil))
  (unwind-protect
      (unless (plist-get status :error)
        (when-let* ((parsed (wamei/claude-usage--parse (wamei/claude-usage--body))))
          (setq wamei/claude-usage--state parsed
                wamei/claude-usage--state-time (current-time))
          (wamei/claude-usage--write-cache parsed wamei/claude-usage--state-time)
          (cl-incf wamei/claude-usage--generation)
          (force-mode-line-update t)))
    (kill-buffer (current-buffer))))

(defun wamei/claude-usage--fetch ()
  "使用量を非同期で取りに行く。トークンが取れなければ何もしない。
前の取得が時間切れなら見切ってから投げ直す。"
  (wamei/claude-usage--abandon)
  (unless wamei/claude-usage--request
    (when-let* ((token (wamei/claude-usage--token)))
      (let ((id (cl-incf wamei/claude-usage--request-id))
            (url-request-method "GET")
            (url-request-extra-headers
             `(("Authorization" . ,(concat "Bearer " token))
               ("anthropic-beta" . "oauth-2025-04-20"))))
        ;; コールバックがその場で走ることがあるので、投げる前に印を立てる
        (setq wamei/claude-usage--request (list id nil (current-time)))
        (condition-case nil
            (let ((buffer (url-retrieve wamei/claude-usage-endpoint
                                        #'wamei/claude-usage--receive (list id) t t)))
              (when (eq id (car wamei/claude-usage--request))
                (setf (nth 1 wamei/claude-usage--request) buffer)))
          (error (when (eq id (car wamei/claude-usage--request))
                   (setq wamei/claude-usage--request nil))))))))

;;; mode-line

(defvar wamei/claude-usage--cache nil
  "描いた 1 行のキャッシュ。(KEY . STRING)。")

(defun wamei/claude-usage--invalidate (&rest _)
  "キャッシュを捨て、地色への馴染ませを引き直す。テーマを変えたときなど。
remap には色を直接入れてあるので、テーマが変わったら入れ直さないと
前のテーマの地色が残る。"
  (setq wamei/claude-usage--cache nil)
  (dolist (buffer (wamei/claude-usage--buffers))
    (with-current-buffer buffer
      (wamei/claude-usage--blend-in))))

(defun wamei/claude-usage-mode-line ()
  "mode-line に出す 1 行。redisplay ごとに呼ばれるのでキャッシュする。"
  (let* ((now (current-time))
         ;; 取得世代と分が同じなら同じ見た目になる (リセット時刻は分までしか出さない)
         ;; 画像が使えるかはフレームごとに変わる (GUI と tty で同じ 1 行を
         ;; 使い回すと、tty 側でバーが空白になる)
         (key (list wamei/claude-usage--generation
                    (floor (float-time now) 60)
                    (wamei/claude-usage--images-p))))
    (unless (equal (car wamei/claude-usage--cache) key)
      (setq wamei/claude-usage--cache
            (cons key (wamei/term-modeline-escape
                       (wamei/claude-usage--render wamei/claude-usage--state now)))))
    (cdr wamei/claude-usage--cache)))

(defconst wamei/claude-usage--mode-line-format
  ;; 先頭は行グリッドへの詰め物。mode-line を端数ぶん厚くして端末の本文高さを
  ;; 行境界に乗せる (term-modeline.el の「行グリッドへの詰め物」)。この
  ;; バッファは下の `wamei/claude-usage--mode-line-faces' で mode-line の
  ;; 地色をパネルに合わせているので、厚くしても下端の余白に見える。
  '((:eval (wamei/term-modeline-grid-pad-spacer))
    (:eval (wamei/claude-usage-mode-line))
    ;; ghostel の入力モードタグ (":Copy" など) を右端に出す。既定の
    ;; `mode-line-format' を丸ごと置き換えているので、足さないと
    ;; copy mode に入って端末が止まっていることに気づけない。
    ;; タグには ghostel が mouse-1 で抜けるキーマップを付けてある。
    (:eval (wamei/term-modeline-align (wamei/term-modeline-process-width)))
    mode-line-process)
  "Claude のバッファに入れる `mode-line-format'。")

(defconst wamei/claude-usage--mode-line-faces
  '(mode-line mode-line-active mode-line-inactive)
  "パネルの地色に合わせる mode-line の face。
Emacs 29 以降、アクティブな mode line が使うのは `mode-line' ではなく
`mode-line-active' なので、`mode-line' だけ remap しても効かない。
非アクティブ側は地色も前景色も別に持っている (doom-molokai だと #171819)
ため、これも揃えないとパネルの中で帯が浮く。")

(defvar-local wamei/claude-usage--remaps nil
  "このバッファに入れた face remap のクッキー。")

(defun wamei/claude-usage--color (color)
  "COLOR が実際の色なら返す。batch や tty の \"unspecified-bg\" なら nil。"
  (and (stringp color) (not (string-prefix-p "unspecified" color)) color))

(defun wamei/claude-usage--dim-background ()
  "auto-dim-other-buffers が非選択 window で使う背景色。使っていなければ nil。"
  (and (facep 'auto-dim-other-buffers)
       (wamei/claude-usage--color
        (face-background 'auto-dim-other-buffers nil t))))

(defun wamei/claude-usage--remap-specs (background dim)
  "mode-line に当てる face の spec を、適用する順に返す。
BACKGROUND はパネルの地色、DIM は非選択 window での暗転後の地色。
`face-remap-add-relative' は後に当てたものを優先するので DIM を後ろに置く。
フィルタは auto-dim-other-buffers 自身が `default' に使っているものと同じ条件で、
バッファ本文が暗転するのと同時に mode-line も落ちる。"
  (append (when background (list (list :background background)))
          (when dim (list (list :filtered '(:window adob--dim t)
                                (list :background dim))))))

(defun wamei/claude-usage--blend-in (&optional background dim)
  "mode-line の帯をパネルの地色に溶け込ませる。
BACKGROUND の既定はバッファの地色、DIM の既定は auto-dim-other-buffers の背景。
Claude のパネルは端末が alt-screen を全面に描いているので、mode-line だけ
別の地色だと帯が浮く。バッファローカルな face remap なので他の window には
影響しない。何度呼んでも remap は積み上がらない。"
  (mapc #'face-remap-remove-relative wamei/claude-usage--remaps)
  (setq wamei/claude-usage--remaps nil)
  (let ((specs (wamei/claude-usage--remap-specs
                (or (wamei/claude-usage--color background)
                    (wamei/claude-usage--color (face-background 'default nil t)))
                (or (wamei/claude-usage--color dim)
                    (wamei/claude-usage--dim-background)))))
    (dolist (face (seq-filter #'facep wamei/claude-usage--mode-line-faces))
      (dolist (spec specs)
        (push (face-remap-add-relative face spec) wamei/claude-usage--remaps)))))

(defun wamei/claude-usage--setup (buffer)
  "BUFFER に使用量の mode-line を付ける。何度呼んでもよい。
端末バッファは init.el の `ghostel-mode-hook' で hide-mode-line-mode が
掛かっているので先に外す。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (equal mode-line-format wamei/claude-usage--mode-line-format)
        (when (bound-and-true-p hide-mode-line-mode)
          (hide-mode-line-mode -1))
        (setq-local mode-line-format wamei/claude-usage--mode-line-format))
      (wamei/claude-usage--blend-in))))

(defun wamei/claude-usage--buffers ()
  "使用量を出している Claude のバッファ。"
  (when (fboundp 'claude-code-ide-mcp--active-sessions)
    (seq-filter #'buffer-live-p
                (mapcar #'claude-code-ide-mcp-session-buffer
                        (claude-code-ide-mcp--active-sessions)))))

(defun wamei/claude-usage--after-display (buffer)
  "表示した BUFFER に mode-line を付ける。
`claude-code-ide--display-buffer-in-side-window' の :after advice。"
  (when (get-buffer-window buffer)
    (wamei/claude-usage--setup buffer)))

;;; タイマー

(defvar wamei/claude-usage--timer nil
  "定期取得のタイマー。")

(defun wamei/claude-usage--visible-p ()
  "見えているフレームに Claude のバッファがあれば非 nil。"
  (and (fboundp 'claude-code-ide--buffer-session)
       (seq-some (lambda (frame)
                   (and (frame-visible-p frame)
                        (seq-some (lambda (window)
                                    (claude-code-ide--buffer-session
                                     (window-buffer window)))
                                  (window-list frame 'no-mini))))
                 (frame-list))))

(defun wamei/claude-usage--tick ()
  "パネルが見えているときだけ動く。
まず共有ファイルから他のインスタンスが取った値を拾い、それが
`wamei/claude-usage-interval' 内なら自分では投げない。エンドポイントの枠は
アカウント単位で狭く、Emacs を 2 つ動かすと後から起動した方が 429 を
引き続けて一度も表示できなくなる。"
  (when (wamei/claude-usage--visible-p)
    (wamei/claude-usage--adopt-cache)
    (unless (wamei/claude-usage--fresh-p)
      (wamei/claude-usage--fetch))))

;;; 有効化

(defun wamei/claude-usage-enable ()
  "使用量の表示を有効にする。何度呼んでもよい。"
  (advice-add 'claude-code-ide--display-buffer-in-side-window
              :after #'wamei/claude-usage--after-display)
  (add-hook 'enable-theme-functions #'wamei/claude-usage--invalidate)
  ;; 起動直後はまだ取れていないので、他のインスタンスが置いた値を出す
  (wamei/claude-usage--adopt-cache)
  (mapc #'wamei/claude-usage--setup (wamei/claude-usage--buffers))
  (unless wamei/claude-usage--timer
    (setq wamei/claude-usage--timer
          (run-with-timer 0 wamei/claude-usage-interval #'wamei/claude-usage--tick))))

(provide 'claude-usage)
;;; claude-usage.el ends here
