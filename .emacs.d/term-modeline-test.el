;;; term-modeline-test.el --- tests for term-modeline -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l term-modeline-test.el -f ert-run-tests-batch-and-exit
;;
;; ghostel には触らない。バッファ名の解析・相対パス・1 行の組み立てといった
;; 純関数だけを検証する。`format-mode-line' は batch では常に空文字列を返すので、
;; それに依るテストは `skip-unless' で対話時だけ走らせる (claude-usage-test.el と同じ)。
;;; Code:

(require 'ert)
(load (expand-file-name "term-modeline.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; バッファ名

(ert-deftest wamei/term-modeline-test-parse-name-without-index ()
  "番号なしの端末バッファは 1 番。"
  (should (equal (wamei/term-modeline--parse-name "*term: dotfiles*")
                 '("dotfiles" . 1))))

(ert-deftest wamei/term-modeline-test-parse-name-with-index ()
  "末尾の数字が端末番号。"
  (should (equal (wamei/term-modeline--parse-name "*term: dotfiles 3*")
                 '("dotfiles" . 3))))

(ert-deftest wamei/term-modeline-test-parse-name-keeps-spaces-in-project ()
  "プロジェクト名に空白があっても数字だけを番号として切り出す。"
  (should (equal (wamei/term-modeline--parse-name "*term: my project*")
                 '("my project" . 1)))
  (should (equal (wamei/term-modeline--parse-name "*term: my project 2*")
                 '("my project" . 2))))

(ert-deftest wamei/term-modeline-test-parse-name-rejects-other-buffers ()
  "端末パネル以外のバッファは nil。"
  (should-not (wamei/term-modeline--parse-name "*claude-code[.dotfiles]*"))
  (should-not (wamei/term-modeline--parse-name "*scratch*"))
  (should-not (wamei/term-modeline--parse-name "*terminals: dotfiles*")))

;;; 端末番号

(ert-deftest wamei/term-modeline-test-position-hidden-when-alone ()
  "端末が 1 つしかなければ番号は出さない (意味がない)。"
  (should-not (wamei/term-modeline--position 1 1)))

(ert-deftest wamei/term-modeline-test-position-shows-index-and-total ()
  "2 つ以上あるときだけ N/M を出す。"
  (should (equal (wamei/term-modeline--position 2 3) "2/3")))

;;; カレントディレクトリ

(ert-deftest wamei/term-modeline-test-relative-dir-at-root ()
  "ルート直下なら出さない。"
  (should-not (wamei/term-modeline--relative-dir "/home/u/repo/" "/home/u/repo/"))
  ;; 末尾のスラッシュの有無で変わらない
  (should-not (wamei/term-modeline--relative-dir "/home/u/repo" "/home/u/repo/")))

(ert-deftest wamei/term-modeline-test-relative-dir-inside-root ()
  "ルートより下ならルートからの相対パス。"
  (should (equal (wamei/term-modeline--relative-dir "/home/u/repo/src/lib/" "/home/u/repo/")
                 "src/lib")))

(ert-deftest wamei/term-modeline-test-relative-dir-outside-root ()
  "ルートの外へ出たら絶対パス (HOME は ~ に略す)。"
  (let ((home (expand-file-name "~/")))
    (should (equal (wamei/term-modeline--relative-dir (concat home "other/") "/tmp/repo/")
                   "~/other"))))

(ert-deftest wamei/term-modeline-test-relative-dir-without-root ()
  "ルートが分からなければ絶対パスをそのまま出す。"
  (should (equal (wamei/term-modeline--relative-dir "/tmp/x/" nil) "/tmp/x")))

(ert-deftest wamei/term-modeline-test-relative-dir-not-fooled-by-prefix ()
  "repo と repo-2 のような前方一致でルート内と誤判定しない。"
  (should (equal (wamei/term-modeline--relative-dir "/home/u/repo-2/" "/home/u/repo/")
                 "/home/u/repo-2")))

;;; 終了ステータス

(ert-deftest wamei/term-modeline-test-status-hidden-on-success ()
  "成功と未実行のときは何も出さない。"
  (should (equal (wamei/term-modeline--status-string 0) ""))
  (should (equal (wamei/term-modeline--status-string nil) "")))

(ert-deftest wamei/term-modeline-test-status-shows-failure ()
  "非 0 のときだけ error face で出す。区切りの空白を頭に持つ。"
  (let ((s (wamei/term-modeline--status-string 1)))
    (should (equal (substring-no-properties s) " ✗1"))
    (should (eq (get-text-property 1 'face s) 'error))))

(ert-deftest wamei/term-modeline-test-status-separates-from-tag ()
  "右にタグやスピナーが続くときは後ろにも空白を置く (✗1⠧ と詰まらないように)。"
  (should (equal (substring-no-properties (wamei/term-modeline--status-string 1 t))
                 " ✗1 "))
  ;; 出すものが無ければ区切りも要らない
  (should (equal (wamei/term-modeline--status-string 0 t) "")))

;;; 実行時刻

(ert-deftest wamei/term-modeline-test-format-duration-omits-empty-units ()
  "0 の単位は出さない。間の単位だけが 0 でも詰める。"
  (should (equal (wamei/term-modeline--format-duration 18) "18s"))
  (should (equal (wamei/term-modeline--format-duration 171) "2m51s"))
  (should (equal (wamei/term-modeline--format-duration 3600) "1h"))
  (should (equal (wamei/term-modeline--format-duration 3605) "1h5s"))
  (should (equal (wamei/term-modeline--format-duration 86399) "23h59m59s")))

(ert-deftest wamei/term-modeline-test-format-duration-truncates-to-seconds ()
  "既定はミリ秒を出さない。端数は切り捨てる (実行中に数字が戻らないため)。"
  (should (equal (wamei/term-modeline--format-duration 18.9) "18s")))

(ert-deftest wamei/term-modeline-test-format-duration-milliseconds ()
  "ミリ秒まで求められたら ms の単位を足す。"
  (should (equal (wamei/term-modeline--format-duration 0.045 'milliseconds) "45ms"))
  (should (equal (wamei/term-modeline--format-duration 18.412 'milliseconds) "18s412ms"))
  (should (equal (wamei/term-modeline--format-duration 171.32 'milliseconds) "2m51s320ms"))
  (should (equal (wamei/term-modeline--format-duration 3843.456 'milliseconds)
                 "1h4m3s456ms")))

(ert-deftest wamei/term-modeline-test-format-duration-zero ()
  "全部の単位が 0 なら、一番小さい単位で 0 を出す (空にしない)。"
  (should (equal (wamei/term-modeline--format-duration 0) "0s"))
  (should (equal (wamei/term-modeline--format-duration 0 'milliseconds) "0ms")))

(ert-deftest wamei/term-modeline-test-format-duration-clamps-negative ()
  "時計が巻き戻っても負の時間は出さない。"
  (should (equal (wamei/term-modeline--format-duration -5) "0s")))

(ert-deftest wamei/term-modeline-test-time-string-before-any-command ()
  "まだ何も実行していなければ出さない。"
  (should-not (wamei/term-modeline--time-string nil nil 0 t)))

(ert-deftest wamei/term-modeline-test-time-string-while-running ()
  "実行中は開始時刻 (年月日から秒まで) と、秒までの経過時間。"
  (should (equal (wamei/term-modeline--time-string 50940 nil 50958 t)
                 "1970-01-01 14:09:00- (18s)")))

(ert-deftest wamei/term-modeline-test-time-string-after-finish ()
  "終わったら終了時刻が埋まる。所要時間はここで初めてミリ秒まで出す。"
  (should (equal (wamei/term-modeline--time-string 50940 51111.32 99999 t)
                 "1970-01-01 14:09:00-14:11:51 (2m51s320ms)")))

(ert-deftest wamei/term-modeline-test-time-string-repeats-date-across-midnight ()
  "日をまたいだときだけ終了時刻にも年月日を付ける (どの日か分からなくなるため)。"
  (should (equal (wamei/term-modeline--time-string 86390 86410 99999 t)
                 "1970-01-01 23:59:50-1970-01-02 00:00:10 (20s)")))

(ert-deftest wamei/term-modeline-test-state-includes-time ()
  "1 行に出す材料に実行時刻が入る。"
  (with-temp-buffer
    (rename-buffer "*term: dotfiles*")
    (setq wamei/term-modeline--start-time 50940
          wamei/term-modeline--end-time 51111)          ; ちょうど 2m51s000ms
    ;; ここはタイムゾーンに依らせない (`--state' はローカルで出す)。
    (should (string-match-p
             "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9:]\\{8\\}-[0-9:]\\{8\\} (2m51s)\\'"
             (plist-get (wamei/term-modeline--state) :time)))))

;;; 毎秒の再描画

(defmacro wamei/term-modeline-test--with-clean-tick (&rest body)
  "タイマーの状態を持ち込まずに BODY を走らせ、残ったタイマーを片付ける。"
  (declare (indent 0))
  `(let ((wamei/term-modeline--running nil)
         (wamei/term-modeline--tick-timer nil))
     (unwind-protect (progn ,@body)
       (wamei/term-modeline--stop-tick))))

(ert-deftest wamei/term-modeline-test-tick-runs-only-while-a-command-runs ()
  "コマンドが走っている間だけ 1 秒タイマーを回す。"
  (wamei/term-modeline-test--with-clean-tick
    (with-temp-buffer
      (should-not wamei/term-modeline--tick-timer)
      (wamei/term-modeline--on-command-start (current-buffer))
      (should (memq wamei/term-modeline--tick-timer timer-list))
      (wamei/term-modeline--on-command-finish (current-buffer) 0)
      (should-not wamei/term-modeline--tick-timer))))

(ert-deftest wamei/term-modeline-test-tick-keeps-running-for-other-terminals ()
  "1 つ終わっても他の端末が走っていればタイマーは止めない。"
  (wamei/term-modeline-test--with-clean-tick
    (let ((a (generate-new-buffer " *term-a*"))
          (b (generate-new-buffer " *term-b*")))
      (unwind-protect
          (progn
            (wamei/term-modeline--on-command-start a)
            (wamei/term-modeline--on-command-start b)
            (wamei/term-modeline--on-command-finish a 0)
            (should wamei/term-modeline--tick-timer)
            (wamei/term-modeline--on-command-finish b 0)
            (should-not wamei/term-modeline--tick-timer))
        (kill-buffer a)
        (kill-buffer b)))))

(ert-deftest wamei/term-modeline-test-tick-drops-dead-buffers ()
  "端末を殺したままコマンドが終わらなくても、次の tick でタイマーは止まる。"
  (wamei/term-modeline-test--with-clean-tick
    (let ((buffer (generate-new-buffer " *term-c*")))
      (wamei/term-modeline--on-command-start buffer)
      (kill-buffer buffer)
      (wamei/term-modeline--tick)
      (should-not wamei/term-modeline--tick-timer))))

(ert-deftest wamei/term-modeline-test-command-start-records-time ()
  "開始で開始時刻が入り、終了時刻は空く。終了で終了時刻が入る。"
  (wamei/term-modeline-test--with-clean-tick
    (with-temp-buffer
      (wamei/term-modeline--on-command-start (current-buffer))
      (should wamei/term-modeline--start-time)
      (should-not wamei/term-modeline--end-time)
      (wamei/term-modeline--on-command-finish (current-buffer) 0)
      (should wamei/term-modeline--end-time))))

(ert-deftest wamei/term-modeline-test-prompt-redraw-does-not-record-end ()
  "C を伴わない D (プロンプトの再描画) では終了時刻を入れない。"
  (wamei/term-modeline-test--with-clean-tick
    (with-temp-buffer
      (wamei/term-modeline--on-command-finish (current-buffer) 0)
      (should-not wamei/term-modeline--end-time))))

;;; 1 行の組み立て

(defun wamei/term-modeline-test--state (&rest overrides)
  "テスト用の状態。OVERRIDES で上書きする。"
  (let ((state (list :position "2/3" :title "ls -al" :dir "src/lib")))
    (while overrides
      (setq state (plist-put state (pop overrides) (pop overrides))))
    state))

(ert-deftest wamei/term-modeline-test-render-full ()
  "幅が足りていれば 番号・タイトル・ディレクトリを並べる。"
  (should (equal (substring-no-properties
                  (wamei/term-modeline--render (wamei/term-modeline-test--state) 60))
                 "2/3  ls -al   src/lib")))

(ert-deftest wamei/term-modeline-test-render-without-position ()
  "端末が 1 つなら番号の分の空白も入れない。"
  (should (equal (substring-no-properties
                  (wamei/term-modeline--render
                   (wamei/term-modeline-test--state :position nil) 60))
                 "ls -al   src/lib")))

(ert-deftest wamei/term-modeline-test-render-without-dir ()
  "ルート直下ならタイトルだけ。"
  (should (equal (substring-no-properties
                  (wamei/term-modeline--render
                   (wamei/term-modeline-test--state :dir nil) 60))
                 "2/3  ls -al")))

(ert-deftest wamei/term-modeline-test-render-drops-dir-when-narrow ()
  "タイトルに 8 桁も残らないならディレクトリを捨てて、タイトルに幅を回す。"
  (let ((line (substring-no-properties
               (wamei/term-modeline--render
                (wamei/term-modeline-test--state :title "npm run build:watch") 20))))
    (should-not (string-match-p "src/lib" line))
    (should (string-prefix-p "2/3  npm run" line))
    (should (<= (string-width line) 20))))

(ert-deftest wamei/term-modeline-test-render-truncates-title ()
  "それでも収まらなければタイトルを詰める。"
  (let ((line (substring-no-properties
               (wamei/term-modeline--render
                (wamei/term-modeline-test--state :title (make-string 100 ?x) :dir nil)
                20))))
    (should (<= (string-width line) 20))
    (should (string-suffix-p "…" line))))

(ert-deftest wamei/term-modeline-test-render-faces ()
  "タイトルは mode-line-buffer-id、番号とディレクトリは控えめなほう。"
  (let ((line (wamei/term-modeline--render (wamei/term-modeline-test--state) 60)))
    (should (eq (get-text-property (string-match "2/3" line) 'face line)
                'wamei/term-modeline-dim))
    (should (eq (get-text-property (string-match "ls" line) 'face line)
                'mode-line-buffer-id))
    (should (eq (get-text-property (string-match "src" line) 'face line)
                'wamei/term-modeline-dim))))

(ert-deftest wamei/term-modeline-test-render-time-after-title ()
  "実行時刻はタイトルのすぐ右、ディレクトリの手前。"
  (should (equal (substring-no-properties
                  (wamei/term-modeline--render
                   (wamei/term-modeline-test--state
                    :time "2026-09-10 22:42:15-22:45:06 (2m51s320ms)")
                   80))
                 (concat "2/3  ls -al  2026-09-10 22:42:15-22:45:06 (2m51s320ms)"
                         "   src/lib"))))

(ert-deftest wamei/term-modeline-test-render-drops-dir-before-time ()
  "幅が足りないとき、先に捨てるのはディレクトリ。"
  (let ((line (substring-no-properties
               (wamei/term-modeline--render
                (wamei/term-modeline-test--state
                 :title "npm run build:watch"
                 :time "2026-09-10 22:42:15-22:45:06 (2m51s320ms)")
                60))))
    (should-not (string-match-p "src/lib" line))
    (should (string-match-p "2026-09-10 22:42:15" line))
    (should (<= (string-width line) 60))))

(ert-deftest wamei/term-modeline-test-render-drops-time-when-still-narrow ()
  "ディレクトリを捨てても足りなければ実行時刻も捨てて、タイトルに幅を回す。"
  (let ((line (substring-no-properties
               (wamei/term-modeline--render
                (wamei/term-modeline-test--state
                 :title "npm run build:watch"
                 :time "2026-09-10 22:42:15-22:45:06 (2m51s320ms)")
                40))))
    (should-not (string-match-p "2026-09-10" line))
    (should (string-prefix-p "2/3  npm run" line))
    (should (<= (string-width line) 40))))

(ert-deftest wamei/term-modeline-test-faces-follow-selection ()
  "非アクティブな mode-line では暗いほうの face を使う。"
  (should (eq (wamei/term-modeline--time-face t) 'wamei/term-modeline-time))
  (should (eq (wamei/term-modeline--time-face nil)
              'wamei/term-modeline-time-inactive))
  (should (eq (wamei/term-modeline--dim-face t) 'wamei/term-modeline-dim))
  (should (eq (wamei/term-modeline--dim-face nil)
              'wamei/term-modeline-dim-inactive)))

(ert-deftest wamei/term-modeline-test-render-dims-when-not-selected ()
  "選択していないウィンドウでは、時刻も番号も cwd も暗いほうへ落とす。"
  (let ((line (wamei/term-modeline--render
               (wamei/term-modeline-test--state
                :time "2026-09-10 22:42:15- (18s)" :selected nil)
               80)))
    (should (eq (get-text-property (string-match "2026" line) 'face line)
                'wamei/term-modeline-time-inactive))
    (should (eq (get-text-property (string-match "2/3" line) 'face line)
                'wamei/term-modeline-dim-inactive))
    (should (eq (get-text-property (string-match "src" line) 'face line)
                'wamei/term-modeline-dim-inactive))))

(ert-deftest wamei/term-modeline-test-render-time-face ()
  "選択の有無が状態に無ければアクティブなほう。
実行時刻は専用の face を持つ。`shadow' では薄すぎ、地の色ではタイトルと
同じ強さになる。"
  (let ((line (wamei/term-modeline--render
               (wamei/term-modeline-test--state
                :time "2026-09-10 22:42:15- (18s)")
               80)))
    (should (eq (get-text-property (string-match "2026" line) 'face line)
                'wamei/term-modeline-time))))

;;; mode-line 共通部品 (claude-usage.el と共有)

(ert-deftest wamei/term-modeline-test-escape-doubles-percent ()
  "mode-line は文字列中の %-construct を展開するので % を二重にする。"
  (should (equal (wamei/term-modeline-escape "  3%") "  3%%")))

(ert-deftest wamei/term-modeline-test-escape-keeps-properties ()
  "エスケープしてもテキストプロパティ (画像や face) を落とさない。"
  (let* ((source (concat (propertize "b" 'display '(image :type svg))
                         (propertize "9%" 'face 'bold)))
         (escaped (wamei/term-modeline-escape source)))
    (should (equal (substring-no-properties escaped) "b9%%"))
    (should (equal (get-text-property 0 'display escaped) '(image :type svg)))
    (should (eq (get-text-property 2 'face escaped) 'bold))
    (should (eq (get-text-property 3 'face escaped) 'bold))))

(ert-deftest wamei/term-modeline-test-align-without-width ()
  "右に寄せるものが無ければ詰め物も要らない。"
  (should (equal (wamei/term-modeline-align 0) "")))

(ert-deftest wamei/term-modeline-test-align-pushes-to-right-edge ()
  "WIDTH 桁だけ右端から戻した位置まで詰める。"
  (let ((spacer (wamei/term-modeline-align 5)))
    (should (equal (substring-no-properties spacer) " "))
    (should (equal (get-text-property 0 'display spacer)
                   '(space :align-to (- right 5))))))

(ert-deftest wamei/term-modeline-test-process-width ()
  "ghostel の入力モードタグとスピナーは `mode-line-process' に入っている。"
  (skip-unless (not (equal "" (format-mode-line "x"))))
  (with-temp-buffer
    (should (equal (wamei/term-modeline-process-width) 0))
    (setq mode-line-process ":Copy")
    (should (equal (wamei/term-modeline-process-width) 5))))

;;; mode-line-format

(ert-deftest wamei/term-modeline-test-format-ends-with-status-and-process ()
  "終了ステータスと入力モードタグを右端に、その手前まで詰め物で送る。"
  (should (equal (last wamei/term-modeline-format 3)
                 '((:eval (wamei/term-modeline--spacer))
                   (:eval (wamei/term-modeline--status))
                   mode-line-process))))

;;; 高さを固定する詰め物

(ert-deftest wamei/term-modeline-pad-ascent-splits-exactly ()
  "descent をちょうど必要な px だけ取り、残りを ascent にする。
Emacs は ascent px を (高さ * 百分率 / 100) の整数除算で出す。切り捨てると
ascent が 1px 足りず、その 1px を descent が余分に取るので、中身の ascent が
詰め物より高いところで行が 1px 伸びる。"
  (dolist (h (number-sequence 20 41))
    (let* ((pct (wamei/term-modeline-pad-ascent h))
           (ascent (/ (* h pct) 100)))
      (should (= (- h ascent) wamei/term-modeline-pad-descent))
      (should (<= pct 100)))))

(ert-deftest wamei/term-modeline-pad-image-is-cached ()
  "同じ高さの詰め物は作り直さない (mode-line は毎フレーム評価される)。"
  (let ((wamei/term-modeline--pad-cache nil))
    (let ((a (wamei/term-modeline-pad-image 23))
          (b (wamei/term-modeline-pad-image 23))
          (c (wamei/term-modeline-pad-image 24)))
      (should (eq a b))
      (should-not (eq a c)))))

(ert-deftest wamei/term-modeline-pad-spacer-is-empty-without-height ()
  "高さが無ければ何も出さない。"
  (should (equal (wamei/term-modeline-pad-spacer nil) "")))

(ert-deftest wamei/term-modeline-pad-spacer-is-empty-on-tty ()
  "tty にはピクセルの概念が無いので何も出さない。"
  (should (equal (wamei/term-modeline-pad-spacer 23) "")))

;;; 端末パネルの mode-line の高さ

(ert-deftest wamei/term-modeline-spinner-pad-height-reserves-braille-descent ()
  "ブレイルの深い descent ぶんだけ既定の行高より高い値を返す。
この高さで詰め物を置いておくと、スピナーが出入りしても mode-line の高さが
動かない (動くと端末の本文高さが動き、下端揃えの端数が変わって画面が跳ねる)。"
  (should (= (wamei/term-modeline-spinner-pad-height 18)
             (+ 18 wamei/term-modeline-braille-descent-excess)))
  (should (= (wamei/term-modeline-spinner-pad-height 24)
             (+ 24 wamei/term-modeline-braille-descent-excess))))

(ert-deftest wamei/term-modeline-spinner-pad-height-guards-line-height ()
  "行高が取れないときは詰め物を出さない (batch の tty など)。"
  (should-not (wamei/term-modeline-spinner-pad-height 0))
  (should-not (wamei/term-modeline-spinner-pad-height nil)))

(ert-deftest wamei/term-modeline-spinner-pad-spacer-is-empty-on-tty ()
  "tty にはピクセルの概念が無いので何も出さない。"
  (should (equal (wamei/term-modeline-spinner-pad-spacer) "")))

(ert-deftest wamei/term-modeline-format-starts-with-spinner-pad ()
  "端末パネルの mode-line の先頭は高さを固定する詰め物。"
  (should (equal (car wamei/term-modeline-format)
                 '(:eval (wamei/term-modeline-spinner-pad-spacer)))))

(provide 'term-modeline-test)
;;; term-modeline-test.el ends here
