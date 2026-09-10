;;; claude-usage-test.el --- tests for claude-usage -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l claude-usage-test.el -f ert-run-tests-batch-and-exit
;;
;; ネットワークと Keychain には触らない。取得部 (`wamei/claude-usage--fetch')
;; は差し替えず、パース・整形・描画の純関数だけを検証する。
;;; Code:

(require 'ert)
;; claude-usage.el は mode-line の共通部品を term-modeline.el から取るので先に読む
;; (init.el も ghostel ブロックで先に読んでいる)。
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "term-modeline.el" dir) nil t)
  (load (expand-file-name "claude-usage.el" dir) nil t))

;; タイムゾーンで表示が変わるので固定する
(setenv "TZ" "JST-9")

;;; フィクスチャ

(defconst wamei/claude-usage-test--json "\
{\"five_hour\":{\"utilization\":0.0,\"resets_at\":\"2026-09-07T05:50:00.215659+00:00\"},
 \"limits\":[
  {\"kind\":\"session\",\"group\":\"session\",\"percent\":0,\"severity\":\"normal\",
   \"resets_at\":\"2026-09-07T05:50:00.215659+00:00\",\"scope\":null,\"is_active\":false},
  {\"kind\":\"weekly_all\",\"group\":\"weekly\",\"percent\":29,\"severity\":\"normal\",
   \"resets_at\":\"2026-09-10T12:59:59.215680+00:00\",\"scope\":null,\"is_active\":false},
  {\"kind\":\"weekly_scoped\",\"group\":\"weekly\",\"percent\":40,\"severity\":\"warning\",
   \"resets_at\":\"2026-09-10T13:00:00.215825+00:00\",
   \"scope\":{\"model\":{\"id\":null,\"display_name\":\"Fable\"},\"surface\":null},
   \"is_active\":true}],
 \"member_dashboard_available\":false}"
  "実際の /api/oauth/usage のレスポンスから必要な部分だけ残したもの。")

(defun wamei/claude-usage-test--time (string)
  "STRING (ISO8601) を Lisp 時刻にする。"
  (encode-time (iso8601-parse string)))

(defun wamei/claude-usage-test--state ()
  "フィクスチャをパースした状態。"
  (wamei/claude-usage--parse wamei/claude-usage-test--json))

(defmacro wamei/claude-usage-test--with-cache-file (&rest body)
  "共有ファイルを一時ファイルに向けて BODY を実行する。実ファイルには触らない。"
  (declare (indent 0))
  `(let* ((wamei/claude-usage-cache-file
           (make-temp-name (expand-file-name "claude-usage-test-"
                                             temporary-file-directory)))
          (wamei/claude-usage--state nil)
          (wamei/claude-usage--state-time nil)
          (wamei/claude-usage--generation 0))
     (unwind-protect (progn ,@body)
       (when (file-exists-p wamei/claude-usage-cache-file)
         (delete-file wamei/claude-usage-cache-file)))))

;;; パース

(ert-deftest wamei/claude-usage-test-parse-picks-three-limits ()
  "session / weekly_all / Fable の weekly_scoped を拾う。"
  (let ((state (wamei/claude-usage-test--state)))
    (should (equal (plist-get (plist-get state :session) :percent) 0))
    (should (equal (plist-get (plist-get state :weekly) :percent) 29))
    (should (equal (plist-get (plist-get state :fable) :percent) 40))))

(ert-deftest wamei/claude-usage-test-parse-keeps-severity ()
  "severity をそのまま持つ。"
  (let ((state (wamei/claude-usage-test--state)))
    (should (equal (plist-get (plist-get state :session) :severity) "normal"))
    (should (equal (plist-get (plist-get state :fable) :severity) "warning"))))

(ert-deftest wamei/claude-usage-test-parse-decodes-reset-time ()
  "resets_at を Lisp 時刻にする。"
  (let ((state (wamei/claude-usage-test--state)))
    (should (equal (plist-get (plist-get state :fable) :resets-at)
                   (wamei/claude-usage-test--time "2026-09-10T13:00:00+00:00")))))

(ert-deftest wamei/claude-usage-test-parse-ignores-other-scoped-models ()
  "Fable 以外の weekly_scoped は :fable に入れない。"
  (let ((state (wamei/claude-usage--parse "\
{\"limits\":[{\"kind\":\"weekly_scoped\",\"percent\":90,\"severity\":\"normal\",
 \"resets_at\":null,\"scope\":{\"model\":{\"display_name\":\"Opus\"}}}]}")))
    (should-not (plist-get state :fable))))

(ert-deftest wamei/claude-usage-test-parse-tolerates-missing-limits ()
  "limits が無い・壊れていても例外にせず nil を返す。"
  (should-not (wamei/claude-usage--parse "{}"))
  (should-not (wamei/claude-usage--parse "not json")))

;;; リセット時刻

(ert-deftest wamei/claude-usage-test-format-reset-same-day ()
  "同じ日なら時刻だけ。"
  (should (equal (wamei/claude-usage--format-reset
                  (wamei/claude-usage-test--time "2026-09-07T14:50:00+09:00")
                  (wamei/claude-usage-test--time "2026-09-07T09:00:00+09:00"))
                 "14:50")))

(ert-deftest wamei/claude-usage-test-format-reset-other-day ()
  "日をまたぐなら月日も付ける。"
  (should (equal (wamei/claude-usage--format-reset
                  (wamei/claude-usage-test--time "2026-09-10T22:00:00+09:00")
                  (wamei/claude-usage-test--time "2026-09-07T09:00:00+09:00"))
                 "9/10 22:00")))

(ert-deftest wamei/claude-usage-test-format-reset-nil ()
  "時刻が無ければ nil。"
  (should-not (wamei/claude-usage--format-reset nil (current-time))))

;;; バー (文字)

(defun wamei/claude-usage-test--text-bar (percent width)
  "文字バーの見た目だけを取り出す (face は別のテストで見る)。"
  (substring-no-properties
   (wamei/claude-usage--text-bar percent width 'wamei/claude-usage-normal)))

(ert-deftest wamei/claude-usage-test-text-bar-width ()
  "文字バーは指定した桁数ちょうど。"
  (should (equal (length (wamei/claude-usage-test--text-bar 40 6)) 6))
  (should (equal (length (wamei/claude-usage-test--text-bar 0 6)) 6))
  (should (equal (length (wamei/claude-usage-test--text-bar 100 6)) 6)))

(ert-deftest wamei/claude-usage-test-text-bar-fill ()
  "太線で埋め、残りは細線。端は 0 / 全部。"
  (should (equal (wamei/claude-usage-test--text-bar 0 4) "────"))
  (should (equal (wamei/claude-usage-test--text-bar 100 4) "━━━━"))
  (should (equal (wamei/claude-usage-test--text-bar 50 4) "━━──")))

(ert-deftest wamei/claude-usage-test-text-bar-half-steps ()
  "桁数の 2 倍の段階を持ち、半段は ╸ で出す。"
  (should (equal (wamei/claude-usage-test--text-bar 30 6) "━━────"))
  (should (equal (wamei/claude-usage-test--text-bar 37 6) "━━────"))
  (should (equal (wamei/claude-usage-test--text-bar 45 6) "━━╸───"))
  (should (equal (wamei/claude-usage-test--text-bar 50 6) "━━━───"))
  (should (equal (wamei/claude-usage-test--text-bar 95 6) "━━━━━╸")))

(ert-deftest wamei/claude-usage-test-text-bar-shows-tiny-values ()
  "0 でなければ必ず半段は出す (SVG の「0 でない限り最低幅を出す」と同じ)。
0% と 1% が同じ絵になると、使い始めたのか分からない。"
  (should (equal (wamei/claude-usage-test--text-bar 0 6) "──────"))
  (should (equal (wamei/claude-usage-test--text-bar 1 6) "╸─────")))

(ert-deftest wamei/claude-usage-test-text-bar-splits-faces ()
  "埋まりは値の色、溝は `wamei/claude-usage-track'。
1 色で塗ると太線と細線の差だけで読むことになり、tty では見づらい。"
  (let ((bar (wamei/claude-usage--text-bar 45 6 'wamei/claude-usage-warning)))
    (should (equal (substring-no-properties bar) "━━╸───"))
    ;; 太線と半段までが値の色
    (should (eq (get-text-property 0 'face bar) 'wamei/claude-usage-warning))
    (should (eq (get-text-property 2 'face bar) 'wamei/claude-usage-warning))
    ;; 残りの細線は溝の色
    (should (eq (get-text-property 3 'face bar) 'wamei/claude-usage-track))
    (should (eq (get-text-property 5 'face bar) 'wamei/claude-usage-track))))

(ert-deftest wamei/claude-usage-test-text-bar-clamps ()
  "100 を超えても溢れない。"
  (should (equal (wamei/claude-usage-test--text-bar 150 4) "━━━━"))
  (should (equal (wamei/claude-usage-test--text-bar -10 4) "────")))

;;; 色の正規化

(ert-deftest wamei/claude-usage-test-hex-keeps-6-digits ()
  "6 桁の hex はそのまま。"
  (should (equal (wamei/claude-usage--hex "#a6e22e") "#a6e22e")))

(ert-deftest wamei/claude-usage-test-hex-narrows-16bit ()
  "`color-lighten-name' が返す 16bit/チャンネルの hex を 8bit に落とす。
SVG (rsvg) は 12 桁の #rrrrggggbbbb を色として読めない。"
  (should (equal (wamei/claude-usage--hex "#fffeffff2e14") "#ffff2e"))
  (should (equal (wamei/claude-usage--hex "#fff") "#ffffff")))

(ert-deftest wamei/claude-usage-test-hex-passes-through-unknown ()
  "読めない色はそのまま返す (SVG 側で解釈させる)。"
  (should (equal (wamei/claude-usage--hex "currentColor") "currentColor")))

;;; バー (SVG)

(ert-deftest wamei/claude-usage-test-svg-bar-colors-are-valid ()
  "SVG に埋める色は 6 桁の hex だけ。"
  (let* ((bar (wamei/claude-usage--svg-bar 40 54 10 "#8a8a8a" "#a6e22e"))
         (data (plist-get (cdr (get-text-property 0 'display bar)) :data))
         (colors nil)
         (start 0))
    ;; 属性値になっているものだけ。url(#bar) のような参照は色ではない
    (while (string-match "=\"\\(#[0-9a-fA-F]+\\)\"" data start)
      (push (match-string 1 data) colors)
      (setq start (match-end 0)))
    (should colors)
    (dolist (color colors)
      (should (equal (length color) 7)))))

(ert-deftest wamei/claude-usage-test-svg-bar-is-an-image ()
  "画像が使えるときは display プロパティに image が入る。"
  (skip-unless (image-type-available-p 'svg))
  (let* ((bar (wamei/claude-usage--svg-bar 40 54 12 "#aaaaaa" "#00ff00"))
         (image (get-text-property 0 'display bar)))
    (should (eq (car image) 'image))
    (should (eq (plist-get (cdr image) :type) 'svg))))

;;; 1 行の組み立て

(ert-deftest wamei/claude-usage-test-render-has-all-three ()
  "S / W / F と割合が並び、それぞれの後ろにリセット時刻が付く。"
  (let* ((wamei/claude-usage-use-images nil)
         (now (wamei/claude-usage-test--time "2026-09-07T09:00:00+09:00"))
         (line (substring-no-properties
                (wamei/claude-usage--render (wamei/claude-usage-test--state) now))))
    ;; バーと割合の間は値の桁数によらず 1 スペース
    (should (string-match-p "S ────── 0%" line))
    (should (string-match-p "W ━╸──── 29%" line))
    (should (string-match-p "F ━━╸─── 40%" line))
    ;; JST での各リセット時刻。S は当日なので時刻だけ
    (should (string-match-p "0% ↻ 14:50" line))
    (should (string-match-p "29% ↻ 9/10 21:59" line))
    (should (string-match-p "40% ↻ 9/10 22:00" line))
    (should (equal 3 (cl-count ?↻ line)))))

(ert-deftest wamei/claude-usage-test-render-pads-nothing ()
  "1 桁でも 3 桁でもバーと割合の間は 1 スペースのまま (右揃えの余白を出さない)。"
  (let ((wamei/claude-usage-use-images nil))
    (dolist (case '((2 . "S ╸───── 2%") (100 . "S ━━━━━━ 100%")))
      (let ((line (substring-no-properties
                   (wamei/claude-usage--render
                    (list :session (list :percent (car case) :severity "normal"
                                         :resets-at nil))
                    (current-time)))))
        (should (string-prefix-p (cdr case) line))))))

(ert-deftest wamei/claude-usage-test-render-without-state ()
  "まだ取れていなければダッシュだけを出す。"
  (let ((wamei/claude-usage-use-images nil))
    (should (equal (substring-no-properties
                    (wamei/claude-usage--render nil (current-time)))
                   "S —  W —  F —"))))

(ert-deftest wamei/claude-usage-test-render-partial-state ()
  "一部しか取れていなくても落ちない。"
  (let* ((wamei/claude-usage-use-images nil)
         (state (list :weekly (list :percent 12 :severity "normal" :resets-at nil)))
         (line (substring-no-properties
                (wamei/claude-usage--render state (current-time)))))
    (should (string-match-p "S —" line))
    (should (string-match-p "W ╸───── 12%" line))
    (should (string-match-p "F —" line))))

;;; mode-line への受け渡し

(ert-deftest wamei/claude-usage-test-mode-line-keeps-percent-sign ()
  "mode-line として展開した後も % が残る。"
  ;; batch の `format-mode-line' は常に空文字列を返すので確かめられない。
  ;; 対話 (emacsclient から M-x ert) でだけ走る。
  (skip-unless (not (equal "" (format-mode-line "x"))))
  (let* ((wamei/claude-usage-use-images nil)
         (wamei/claude-usage--cache nil)
         (wamei/claude-usage--state
          (list :weekly (list :percent 29 :severity "normal" :resets-at nil)))
         (line (substring-no-properties
                (format-mode-line wamei/claude-usage--mode-line-format))))
    (should (string-match-p "29%" line))))

(ert-deftest wamei/claude-usage-test-mode-line-format-ends-with-tag ()
  "`mode-line-process' を最後に置いて、詰め物でその手前まで送る。
詰め物は端末パネルと共通の `wamei/term-modeline-align' を使う。"
  (should (equal (last wamei/claude-usage--mode-line-format 2)
                 '((:eval (wamei/term-modeline-align
                           (wamei/term-modeline-process-width)))
                   mode-line-process))))

;;; パネルの地色への馴染ませ

(ert-deftest wamei/claude-usage-test-remap-specs-order ()
  "地色の spec を先に、暗転用を後に返す (後に当てたものが優先される)。"
  (should (equal (wamei/claude-usage--remap-specs "#1c1e1f" "#121314")
                 '((:background "#1c1e1f")
                   (:filtered (:window adob--dim t) (:background "#121314"))))))

(ert-deftest wamei/claude-usage-test-remap-specs-without-dim ()
  "auto-dim-other-buffers が無ければ地色の spec だけ。"
  (should (equal (wamei/claude-usage--remap-specs "#1c1e1f" nil)
                 '((:background "#1c1e1f"))))
  (should-not (wamei/claude-usage--remap-specs nil nil)))

(ert-deftest wamei/claude-usage-test-blend-in-follows-dim ()
  "非選択 window ではバッファ背景が暗転するので mode-line も同じ条件で落とす。"
  (with-temp-buffer
    (wamei/claude-usage--blend-in "#1c1e1f" "#121314")
    (should (equal (alist-get 'mode-line-inactive face-remapping-alist)
                   '((:filtered (:window adob--dim t) (:background "#121314"))
                     (:background "#1c1e1f")
                     mode-line-inactive)))))

(ert-deftest wamei/claude-usage-test-blend-in-remaps-all-mode-line-faces ()
  "mode-line / mode-line-active / mode-line-inactive の背景を地色に揃える。
Emacs 29 以降はアクティブな mode line が `mode-line-active' なので、
`mode-line' だけ remap しても効かない。"
  (with-temp-buffer
    (wamei/claude-usage--blend-in "#1c1e1f")
    (dolist (face '(mode-line mode-line-active mode-line-inactive))
      (should (equal (alist-get face face-remapping-alist)
                     `((:background "#1c1e1f") ,face))))))

(ert-deftest wamei/claude-usage-test-blend-in-is-idempotent ()
  "何度呼んでも remap が積み上がらない。"
  (with-temp-buffer
    (wamei/claude-usage--blend-in "#1c1e1f")
    (wamei/claude-usage--blend-in "#000000")
    (should (equal (length wamei/claude-usage--remaps) 3))
    (should (equal (alist-get 'mode-line face-remapping-alist)
                   '((:background "#000000") mode-line)))))

(ert-deftest wamei/claude-usage-test-blend-in-skips-unspecified ()
  "地色が取れない環境 (batch など) では何も remap しない。"
  (with-temp-buffer
    (wamei/claude-usage--blend-in "#1c1e1f")
    (wamei/claude-usage--blend-in "unspecified-bg")
    (should-not wamei/claude-usage--remaps)
    (should-not face-remapping-alist)))

;;; 色

(ert-deftest wamei/claude-usage-test-dim-matches-claude-dim-text ()
  "リセット時刻は Claude 本体の薄文字と同じ色にする。"
  (should (equal (face-attribute 'wamei/claude-usage-dim :foreground nil t)
                 "#9B9B9B")))

(ert-deftest wamei/claude-usage-test-label-uses-value-face ()
  "S / W / F の文字はバーや割合と同じ色にする。"
  (let* ((wamei/claude-usage-use-images nil)
         (line (wamei/claude-usage--render
                (list :session (list :percent 92 :severity "critical" :resets-at nil))
                (current-time)))
         (plain (substring-no-properties line)))
    (should (eq (get-text-property 0 'face line) 'wamei/claude-usage-critical))
    (should (eq (get-text-property (string-match "92" plain) 'face line)
                'wamei/claude-usage-critical))))

(ert-deftest wamei/claude-usage-test-reset-stays-dim ()
  "リセット時刻だけは薄いまま。"
  (let* ((wamei/claude-usage-use-images nil)
         (line (wamei/claude-usage--render
                (list :session (list :percent 10 :severity "normal"
                                     :resets-at (wamei/claude-usage-test--time
                                                 "2026-09-07T14:50:00+09:00")))
                (wamei/claude-usage-test--time "2026-09-07T09:00:00+09:00")))
         (pos (string-match "↻" (substring-no-properties line))))
    (should (eq (get-text-property pos 'face line) 'wamei/claude-usage-dim))))

(ert-deftest wamei/claude-usage-test-missing-entry-is-dim ()
  "取れていない項目はラベルもダッシュも薄く出す。"
  (let* ((wamei/claude-usage-use-images nil)
         (line (wamei/claude-usage--render nil (current-time))))
    (should (eq (get-text-property 0 'face line) 'wamei/claude-usage-dim))))

(ert-deftest wamei/claude-usage-test-face-by-severity ()
  "severity が normal 以外なら警告色に振る。"
  (should (eq (wamei/claude-usage--face '(:percent 10 :severity "normal"))
              'wamei/claude-usage-normal))
  (should (eq (wamei/claude-usage--face '(:percent 80 :severity "warning"))
              'wamei/claude-usage-warning))
  (should (eq (wamei/claude-usage--face '(:percent 99 :severity "critical"))
              'wamei/claude-usage-critical)))

;;; mode-line の高さ

(ert-deftest wamei/claude-usage-mode-line-pads-to-grid ()
  "mode-line の先頭は行グリッドへの詰め物。
ghostel は端末グリッドをウィンドウ下端に揃え、本文高さが行高で割り切れない
ぶんを window-vscroll として払う。mode-line を余りぶんだけ厚くして
本文高さを行境界に乗せる (term-modeline.el の「行グリッドへの詰め物」)。"
  (should (equal (car wamei/claude-usage--mode-line-format)
                 '(:eval (wamei/term-modeline-grid-pad-spacer)))))

(provide 'claude-usage-test)
;;; 取得の取り回し

(defmacro wamei/claude-usage-test--with-stubbed-fetch (retrieve &rest body)
  "`url-retrieve\' を RETRIEVE に、トークン取得を固定値に差し替えて BODY を実行する。
ネットワークにも Keychain にも触らない。取れた値を書く共有ファイルも
一時ファイルへ向ける (実ファイルをテストのフィクスチャで汚さない)。"
  (declare (indent 1))
  `(wamei/claude-usage-test--with-cache-file
     (let ((wamei/claude-usage--request nil)
           (wamei/claude-usage--request-id 0))
       (cl-letf (((symbol-function 'wamei/claude-usage--token) (lambda () "token"))
                 ((symbol-function 'url-retrieve) ,retrieve))
         ,@body))))

(defun wamei/claude-usage-test--expire ()
  "取得中のリクエストの開始時刻を時間切れの側へ戻す。"
  (setf (nth 2 wamei/claude-usage--request)
        (time-subtract (current-time) (1+ wamei/claude-usage-timeout))))

(ert-deftest wamei/claude-usage-test-fetch-skips-while-in-flight ()
  "取得中は重ねて投げない。"
  (let ((calls 0))
    (wamei/claude-usage-test--with-stubbed-fetch
        (lambda (&rest _) (cl-incf calls) (generate-new-buffer " *stub*"))
      (wamei/claude-usage--fetch)
      (wamei/claude-usage--fetch)
      (should (equal calls 1)))))

(ert-deftest wamei/claude-usage-test-fetch-retries-after-timeout ()
  "応答が来ないまま時間切れになったら投げ直す。
`url-retrieve\' はスリープ復帰で接続が切れるとコールバックを呼ばないまま
終わることがあり、取得中の印が残ると表示が凍る。"
  (let ((calls 0))
    (wamei/claude-usage-test--with-stubbed-fetch
        (lambda (&rest _) (cl-incf calls) (generate-new-buffer " *stub*"))
      (wamei/claude-usage--fetch)
      (wamei/claude-usage-test--expire)
      (wamei/claude-usage--fetch)
      (should (equal calls 2)))))

(ert-deftest wamei/claude-usage-test-abandon-kills-buffer ()
  "見切った取得のバッファは片付ける。"
  (let ((buffer (generate-new-buffer " *stub*")))
    (wamei/claude-usage-test--with-stubbed-fetch
        (lambda (&rest _) buffer)
      (wamei/claude-usage--fetch)
      (wamei/claude-usage-test--expire)
      (wamei/claude-usage--abandon)
      (should-not wamei/claude-usage--request)
      (should-not (buffer-live-p buffer)))))

(ert-deftest wamei/claude-usage-test-fetch-clears-after-response ()
  "応答が返ったら取得中の印を落とし、次を投げられる。
コールバックがその場で走っても印が立ちっぱなしにならない。"
  (let ((calls 0))
    (wamei/claude-usage-test--with-stubbed-fetch
        (lambda (_url callback cbargs &rest _)
          (cl-incf calls)
          (let ((buffer (generate-new-buffer " *stub*")))
            (with-current-buffer buffer
              (insert "HTTP/1.1 200 OK\n\n" wamei/claude-usage-test--json)
              (apply callback nil cbargs))
            buffer))
      (wamei/claude-usage--fetch)
      (should-not wamei/claude-usage--request)
      (should (equal (plist-get (plist-get wamei/claude-usage--state :weekly) :percent) 29))
      (wamei/claude-usage--fetch)
      (should (equal calls 2)))))

(ert-deftest wamei/claude-usage-test-late-response-keeps-new-request ()
  "見切ったあとに遅れて返ってきた応答が、新しい取得の印を落とさない。
リダイレクトなどでコールバックが別のバッファから来ると、片付けたはずの
取得があとから返ることがある。"
  (let (late)
    (wamei/claude-usage-test--with-stubbed-fetch
        (lambda (_url callback cbargs &rest _)
          (setq late (lambda ()
                       (with-current-buffer (generate-new-buffer " *late*")
                         (apply callback '(:error (error http 500)) cbargs))))
          (generate-new-buffer " *stub*"))
      (wamei/claude-usage--fetch)
      (let ((stale late))
        (wamei/claude-usage-test--expire)
        (wamei/claude-usage--fetch)
        (let ((fresh wamei/claude-usage--request))
          (should fresh)
          (funcall stale)
          (should (eq wamei/claude-usage--request fresh)))))))


;;; 取れた値のインスタンス間共有

;; /api/oauth/usage は同一アカウントで 1〜2 分に 1 回しか通らず、枠は GUI Emacs /
;; emacs -nw / 走っている Claude Code のセッションで共有になる。各 Emacs が独立に
;; 60 秒ごとに投げると後から起動した方が 429 を引き続け、前回値が無いので
;; ダッシュのままになる。取れた値はファイルに置いて、
;;   - 起動直後はそこから出す
;;   - interval 内に誰かが取っていれば自分は投げない
;; ようにする。

(ert-deftest wamei/claude-usage-test-cache-round-trip ()
  "書いた使用量と時刻をそのまま読み戻せる。"
  (wamei/claude-usage-test--with-cache-file
    (let ((state (wamei/claude-usage-test--state))
          (time (wamei/claude-usage-test--time "2026-09-07T09:00:00+09:00")))
      (wamei/claude-usage--write-cache state time)
      (let ((cache (wamei/claude-usage--read-cache)))
        (should (equal (plist-get cache :state) state))
        (should (time-equal-p (plist-get cache :time) time))))))

(ert-deftest wamei/claude-usage-test-cache-tolerates-missing-and-garbage ()
  "ファイルが無い・壊れていても例外にしない。"
  (wamei/claude-usage-test--with-cache-file
    (should (null (wamei/claude-usage--read-cache)))
    (with-temp-file wamei/claude-usage-cache-file (insert "(:state"))
    (should (null (wamei/claude-usage--read-cache)))
    (with-temp-file wamei/claude-usage-cache-file (insert "42"))
    (should (null (wamei/claude-usage--read-cache)))))

(ert-deftest wamei/claude-usage-test-adopt-takes-newer-cache ()
  "他のインスタンスが取った新しい値を拾い、mode-line のキャッシュも捨てる。"
  (wamei/claude-usage-test--with-cache-file
    (let ((state (wamei/claude-usage-test--state)))
      (wamei/claude-usage--write-cache state (current-time))
      (should (wamei/claude-usage--adopt-cache))
      (should (equal wamei/claude-usage--state state))
      (should wamei/claude-usage--state-time)
      (should (equal wamei/claude-usage--generation 1)))))

(ert-deftest wamei/claude-usage-test-adopt-keeps-newer-own-state ()
  "自分の方が新しければファイルの古い値で上書きしない。"
  (wamei/claude-usage-test--with-cache-file
    (setq wamei/claude-usage--state 'mine
          wamei/claude-usage--state-time (current-time))
    (wamei/claude-usage--write-cache (wamei/claude-usage-test--state)
                                     (time-subtract (current-time) 300))
    (should-not (wamei/claude-usage--adopt-cache))
    (should (eq wamei/claude-usage--state 'mine))
    (should (equal wamei/claude-usage--generation 0))))

(ert-deftest wamei/claude-usage-test-receive-writes-cache ()
  "取れたらファイルにも書く (他のインスタンスが起動直後から出せるように)。"
  (wamei/claude-usage-test--with-stubbed-fetch
      (lambda (_url callback cbargs &rest _)
        (with-current-buffer (generate-new-buffer " *stub-response*")
          (insert "HTTP/1.1 200 OK\n\n" wamei/claude-usage-test--json)
          (apply callback nil cbargs))
        (generate-new-buffer " *stub*"))
    (wamei/claude-usage--fetch)
    (should wamei/claude-usage--state)
    (let ((cache (wamei/claude-usage--read-cache)))
      (should (equal (plist-get cache :state) wamei/claude-usage--state))
      (should (plist-get cache :time)))))

(ert-deftest wamei/claude-usage-test-receive-keeps-cache-on-error ()
  "失敗したらファイルは触らない (前回値を壊さない)。"
  (wamei/claude-usage-test--with-stubbed-fetch
      (lambda (_url callback cbargs &rest _)
        (with-current-buffer (generate-new-buffer " *stub-response*")
          (apply callback '(:error (error http 429)) cbargs))
        (generate-new-buffer " *stub*"))
    (let ((state (wamei/claude-usage-test--state)))
      (wamei/claude-usage--write-cache state (current-time))
      (wamei/claude-usage--fetch)
      (should (equal (plist-get (wamei/claude-usage--read-cache) :state)
                     state)))))

(ert-deftest wamei/claude-usage-test-tick-skips-fetch-when-cache-is-fresh ()
  "interval 内に誰かが取っていれば投げずに拾うだけにする。"
  (wamei/claude-usage-test--with-cache-file
    (let ((fetches 0))
      (cl-letf (((symbol-function 'wamei/claude-usage--visible-p) (lambda () t))
                ((symbol-function 'wamei/claude-usage--fetch)
                 (lambda () (cl-incf fetches))))
        (wamei/claude-usage--write-cache (wamei/claude-usage-test--state)
                                         (current-time))
        (wamei/claude-usage--tick)
        (should (equal fetches 0))
        (should wamei/claude-usage--state)))))

(ert-deftest wamei/claude-usage-test-tick-fetches-when-cache-is-stale ()
  "誰も取っていない時間が続いたら自分で取りに行く。古い値は出したまま。"
  (wamei/claude-usage-test--with-cache-file
    (let ((fetches 0))
      (cl-letf (((symbol-function 'wamei/claude-usage--visible-p) (lambda () t))
                ((symbol-function 'wamei/claude-usage--fetch)
                 (lambda () (cl-incf fetches))))
        (wamei/claude-usage--write-cache
         (wamei/claude-usage-test--state)
         (time-subtract (current-time) (1+ wamei/claude-usage-interval)))
        (wamei/claude-usage--tick)
        (should (equal fetches 1))
        (should wamei/claude-usage--state)))))

(ert-deftest wamei/claude-usage-test-tick-does-nothing-while-hidden ()
  "パネルが見えていなければ拾いにも取りにも行かない。"
  (wamei/claude-usage-test--with-cache-file
    (let ((fetches 0))
      (cl-letf (((symbol-function 'wamei/claude-usage--visible-p) (lambda () nil))
                ((symbol-function 'wamei/claude-usage--fetch)
                 (lambda () (cl-incf fetches))))
        (wamei/claude-usage--write-cache (wamei/claude-usage-test--state)
                                         (current-time))
        (wamei/claude-usage--tick)
        (should (equal fetches 0))
        (should-not wamei/claude-usage--state)))))

;;; フレームごとのバー

(ert-deftest wamei/claude-usage-test-images-p-follows-frame ()
  "既定 (auto) は描くフレームで判定する。tty/batch では画像を使わない。"
  (let ((wamei/claude-usage-use-images 'auto))
    (should-not (wamei/claude-usage--images-p)))
  (let ((wamei/claude-usage-use-images nil))
    (should-not (wamei/claude-usage--images-p))))

(ert-deftest wamei/claude-usage-test-bar-falls-back-on-tty ()
  "auto のまま tty で描くと SVG ではなく文字バーになる。"
  (let* ((wamei/claude-usage-use-images 'auto)
         (bar (wamei/claude-usage--bar 50 'wamei/claude-usage-normal)))
    (should (equal (substring-no-properties bar) "━━━───"))
    (should-not (get-text-property 0 'display bar))))

(ert-deftest wamei/claude-usage-test-mode-line-cache-follows-frame ()
  "GUI と tty のフレームで同じ 1 行を使い回さない (画像は tty で見えない)。"
  (let ((wamei/claude-usage--cache nil)
        (wamei/claude-usage--state
         (list :weekly (list :percent 29 :severity "normal" :resets-at nil))))
    (cl-letf (((symbol-function 'wamei/claude-usage--images-p) (lambda () nil)))
      (should (string-match-p "━" (wamei/claude-usage-mode-line))))
    (cl-letf (((symbol-function 'wamei/claude-usage--images-p) (lambda () t)))
      (should-not (string-match-p "━" (wamei/claude-usage-mode-line))))))

;;; claude-usage-test.el ends here
