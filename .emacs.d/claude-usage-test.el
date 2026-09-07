;;; claude-usage-test.el --- tests for claude-usage -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l claude-usage-test.el -f ert-run-tests-batch-and-exit
;;
;; ネットワークと Keychain には触らない。取得部 (`wamei/claude-usage--fetch')
;; は差し替えず、パース・整形・描画の純関数だけを検証する。
;;; Code:

(require 'ert)
(load (expand-file-name "claude-usage.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

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

(ert-deftest wamei/claude-usage-test-text-bar-width ()
  "文字バーは指定した桁数ちょうど。"
  (should (equal (length (wamei/claude-usage--text-bar 40 6)) 6))
  (should (equal (length (wamei/claude-usage--text-bar 0 6)) 6))
  (should (equal (length (wamei/claude-usage--text-bar 100 6)) 6)))

(ert-deftest wamei/claude-usage-test-text-bar-fill ()
  "割合に応じて埋まる。端は 0 / 全部。"
  (should (equal (wamei/claude-usage--text-bar 0 4) "░░░░"))
  (should (equal (wamei/claude-usage--text-bar 100 4) "████"))
  (should (equal (wamei/claude-usage--text-bar 50 4) "██░░")))

(ert-deftest wamei/claude-usage-test-text-bar-clamps ()
  "100 を超えても溢れない。"
  (should (equal (wamei/claude-usage--text-bar 150 4) "████"))
  (should (equal (wamei/claude-usage--text-bar -10 4) "░░░░")))

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
    (should (string-match-p "S ░░░░░░ +0%" line))
    (should (string-match-p "W ██░░░░ +29%" line))
    (should (string-match-p "F ██░░░░ +40%" line))
    ;; JST での各リセット時刻。S は当日なので時刻だけ
    (should (string-match-p "0% ↻14:50" line))
    (should (string-match-p "29% ↻9/10 21:59" line))
    (should (string-match-p "40% ↻9/10 22:00" line))
    (should (equal 3 (cl-count ?↻ line)))))

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
    (should (string-match-p "W █░░░░░ +12%" line))
    (should (string-match-p "F —" line))))

;;; mode-line への受け渡し

(ert-deftest wamei/claude-usage-test-escape-doubles-percent ()
  "mode-line は文字列中の %-construct を展開するので % を二重にする。"
  (should (equal (wamei/claude-usage--escape "  3%") "  3%%")))

(ert-deftest wamei/claude-usage-test-escape-keeps-properties ()
  "エスケープしてもテキストプロパティ (バーの画像や face) を落とさない。"
  (let* ((source (concat (propertize "b" 'display '(image :type svg))
                         (propertize "9%" 'face 'bold)))
         (escaped (wamei/claude-usage--escape source)))
    (should (equal (substring-no-properties escaped) "b9%%"))
    (should (equal (get-text-property 0 'display escaped) '(image :type svg)))
    ;; 足した % にも元の % と同じ face が付く
    (should (eq (get-text-property 2 'face escaped) 'bold))
    (should (eq (get-text-property 3 'face escaped) 'bold))))

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

(provide 'claude-usage-test)
;;; claude-usage-test.el ends here
