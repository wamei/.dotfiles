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
  "タイトルは mode-line-buffer-id、番号とディレクトリは shadow。"
  (let ((line (wamei/term-modeline--render (wamei/term-modeline-test--state) 60)))
    (should (eq (get-text-property (string-match "2/3" line) 'face line) 'shadow))
    (should (eq (get-text-property (string-match "ls" line) 'face line)
                'mode-line-buffer-id))
    (should (eq (get-text-property (string-match "src" line) 'face line) 'shadow))))

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

(provide 'term-modeline-test)
;;; term-modeline-test.el ends here
