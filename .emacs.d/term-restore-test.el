;;; term-restore-test.el --- tests for term-restore -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l term-restore-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)

;; ghostel 本体の buffer-local 変数。ghostel を読まない batch でも
;; setq-local / buffer-local-value できるよう special にしておく。
(defvar-local ghostel-title nil
  "端末が報告したタイトル (テスト用のスタブ定義)。")

;; term-restore.el の `(defvar ghostel-pre-spawn-hook)' (値なし) は
;; その file の lexical scope 内でしか special 化されないので、この test
;; file 側で `let' 束縛しても dynamic binding にならない。値付きで
;; 再宣言して、この file の中でも special にしておく (テスト用のスタブ定義)。
(defvar ghostel-pre-spawn-hook nil
  "端末プロセスを起こす直前に呼ばれるフック (テスト用のスタブ定義)。")

(load (expand-file-name "term-restore.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; バッファ名の解析

(ert-deftest wamei/term-restore-parse-name-without-index ()
  "番号なしの端末名はプロジェクト名と 1 に分かれる。"
  (should (equal (wamei/term-restore--parse-name "*term: foo*") '("foo" . 1))))

(ert-deftest wamei/term-restore-parse-name-with-index ()
  "番号付きの端末名はプロジェクト名と番号に分かれる。"
  (should (equal (wamei/term-restore--parse-name "*term: foo 3*") '("foo" . 3))))

(ert-deftest wamei/term-restore-parse-name-rejects-other-buffers ()
  "端末以外のバッファ名は nil。"
  (should-not (wamei/term-restore--parse-name "*scratch*"))
  (should-not (wamei/term-restore--parse-name "*terminals*")))

;;; 末尾 N 行

(ert-deftest wamei/term-restore-tail-keeps-last-lines ()
  "末尾 N 行だけを残す。"
  (should (equal (wamei/term-restore--tail "a\nb\nc\nd\n" 2) "c\nd\n")))

(ert-deftest wamei/term-restore-tail-drops-trailing-blank-lines ()
  "端末が画面下端まで埋める空行と行末の空白は落とす。"
  (should (equal (wamei/term-restore--tail "a  \nb \n\n   \n\n" 10) "a\nb\n")))

(ert-deftest wamei/term-restore-tail-of-short-text ()
  "N 行に満たなければ全部残す。"
  (should (equal (wamei/term-restore--tail "a\nb\n" 5) "a\nb\n")))

;;; スクロールバックの書き出し

(ert-deftest wamei/term-restore-write-scrollback-writes-and-skips-unchanged ()
  "内容が変わったときだけ書く。戻り値は書いたかどうか。"
  (let ((file (make-temp-file "wamei-term-restore-")))
    (unwind-protect
        (progn
          (should (wamei/term-restore--write-scrollback file "x\n"))
          (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                         "x\n"))
          (should-not (wamei/term-restore--write-scrollback file "x\n"))
          (should (wamei/term-restore--write-scrollback file "y\n"))
          (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                         "y\n")))
      (delete-file file))))

;;; 色の変換

(defun wamei/term-restore-test--colored (text &rest face)
  "TEXT に ghostel が付けるのと同じ `face' plist FACE を付けて返す。"
  (propertize text 'face face))

(ert-deftest wamei/term-restore-ansi-passes-plain-text-through ()
  "face の無い文字列はそのまま。"
  (should (equal (wamei/term-restore--ansi "a\nb\n") "a\nb\n")))

(ert-deftest wamei/term-restore-ansi-emits-truecolor-foreground ()
  "前景色は 24bit の SGR にして、run の後でリセットする。"
  (should (equal (wamei/term-restore--ansi
                  (concat "x " (wamei/term-restore-test--colored "err" :foreground "#ff8000") "\n"))
                 "x \e[38;2;255;128;0merr\e[0m\n")))

(ert-deftest wamei/term-restore-ansi-emits-attributes-and-background ()
  "太字・斜体・下線・反転・取り消し線と背景色を SGR に写す。"
  (should (equal (wamei/term-restore--ansi
                  (wamei/term-restore-test--colored
                   "t" :background "#000080" :weight 'bold :underline t
                   :slant 'italic :inverse-video t :strike-through t :extend t))
                 "\e[1;3;4;7;9;48;2;0;0;128mt\e[0m")))

(ert-deftest wamei/term-restore-ansi-splits-runs-by-face ()
  "face が変わるごとに別の SGR を出す。"
  (should (equal (wamei/term-restore--ansi
                  (concat (wamei/term-restore-test--colored "a" :foreground "#ff0000")
                          (wamei/term-restore-test--colored "b" :foreground "#00ff00")
                          "c"))
                 "\e[38;2;255;0;0ma\e[0m\e[38;2;0;255;0mb\e[0mc")))

(ert-deftest wamei/term-restore-ansi-accepts-other-hex-widths ()
  "#rrrrggggbbbb は上位バイトで、#rgb は 0-255 に広げて扱う。"
  (should (equal (wamei/term-restore--ansi
                  (wamei/term-restore-test--colored "a" :foreground "#ffff80000000"))
                 "\e[38;2;255;128;0ma\e[0m"))
  (should (equal (wamei/term-restore--ansi
                  (wamei/term-restore-test--colored "a" :foreground "#f80"))
                 "\e[38;2;255;136;0ma\e[0m")))

(ert-deftest wamei/term-restore-ansi-emits-colors-equal-to-default ()
  "ghostel は装飾のないセルに face を付けないので、テーマの既定色と同じ色でも
抑止しない (face が付いている = 端末が明示した色)。`face-foreground'/
`face-background' の `default' をこの fixture と同じ色にしても出力が変わら
ないことを見て、既定色を読んで比較する抑止処理に戻っていないかを確認する。"
  (cl-letf (((symbol-function 'face-foreground)
             (lambda (face &rest _) (when (eq face 'default) "#ff0000")))
            ((symbol-function 'face-background)
             (lambda (face &rest _) (when (eq face 'default) "#000000"))))
    (should (equal (wamei/term-restore--ansi
                    (concat "plain"
                            (wamei/term-restore-test--colored
                             "red" :foreground "#ff0000")))
                   "plain\e[38;2;255;0;0mred\e[0m"))))

(ert-deftest wamei/term-restore-ansi-ignores-unresolvable-face ()
  "解決できない色しか無い face は何も出さない。"
  (should (equal (wamei/term-restore--ansi
                  (wamei/term-restore-test--colored "a" :foreground "nosuchcolor" :extend t))
                 "a")))

;;; プロンプト行の除外

(defun wamei/term-restore-test--insert-marked (text)
  "TEXT を ghostel がプロンプトに付ける `ghostel-prompt' プロパティ付きで挿入する。
ghostel は OSC 133 でプロンプトの範囲を受け取り、その文字に印を付ける。"
  (insert (propertize text 'ghostel-prompt t 'rear-nonsticky t)))

(ert-deftest wamei/term-restore-content-drops-trailing-prompt-lines ()
  "末尾に続くプロンプト行 (複数行でも) と、その後の空行は落とす。"
  (with-temp-buffer
    (insert "out\n" "~/x git:(master)")
    (wamei/term-restore-test--insert-marked "\n")
    (insert "$ ")
    (wamei/term-restore-test--insert-marked "\n")
    (insert "\n\n")
    (should (equal (wamei/term-restore--content) "out\n"))))

(ert-deftest wamei/term-restore-content-drops-typed-but-unrun-command ()
  "プロンプト行に入力途中のコマンドがあっても、その行ごと落とす。"
  (with-temp-buffer
    (insert "out\n" "$ ")
    (wamei/term-restore-test--insert-marked "l")
    (insert "s -al\n")
    (should (equal (wamei/term-restore--content) "out\n"))))

(ert-deftest wamei/term-restore-content-keeps-running-command-output ()
  "最後がプロンプトでなければ (コマンド実行中) 何も落とさない。"
  (with-temp-buffer
    (insert "$ ")
    (wamei/term-restore-test--insert-marked "n")
    (insert "pm run dev\nlistening on 3000\n")
    (should (equal (wamei/term-restore--content) "$ npm run dev\nlistening on 3000\n"))))

(ert-deftest wamei/term-restore-content-without-prompt-marks ()
  "プロンプトの印が無ければ全部残す。"
  (with-temp-buffer
    (insert "a\nb\n")
    (should (equal (wamei/term-restore--content) "a\nb\n"))))

(ert-deftest wamei/term-restore-content-keeps-faces ()
  "色 (face) は残す。復元時に SGR へ写すため。"
  (with-temp-buffer
    (insert (wamei/term-restore-test--colored "a" :foreground "#ff0000") "\n")
    (should (equal (get-text-property 0 'face (wamei/term-restore--content))
                   '(:foreground "#ff0000")))))

;;; 保存

(defmacro wamei/term-restore-test--with-saved-dir (&rest body)
  "スクロールバックの出力先を一時ディレクトリにして BODY を評価する。
`wamei/term-restore--restoring' も t に束縛して「起動時の desktop 復元中」を
既定にする。`wamei/term-restore-ensure' がこのフラグを nil にするので、
束縛しないと ensure を呼ぶテストが後続のテストに影響する。"
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "term-restore-" t)))
          (wamei/term-restore-directory dir)
          (wamei/term-restore-saved nil)
          (wamei/term-restore--restoring t))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(ert-deftest wamei/term-restore-save-records-name-directory-and-title ()
  "端末ごとにバッファ名・ディレクトリ・タイトル・スクロールバックのパスを記録する。"
  (wamei/term-restore-test--with-saved-dir
    (let ((buffer (get-buffer-create "*term: foo 2*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq default-directory "/tmp/")
              (setq-local ghostel-title "make test")
              (insert "hello\n"))
            (wamei/term-restore-save)
            (let ((entry (car wamei/term-restore-saved)))
              (should (equal (plist-get entry :name) "*term: foo 2*"))
              (should (equal (plist-get entry :directory) "/tmp/"))
              (should (equal (plist-get entry :title) "make test"))
              (should (file-readable-p (plist-get entry :scrollback)))
              (should (equal (with-temp-buffer
                               (insert-file-contents (plist-get entry :scrollback))
                               (buffer-string))
                             "hello\n"))))
        (kill-buffer buffer)))))

(ert-deftest wamei/term-restore-save-prunes-unreferenced-files ()
  "記録されていないスクロールバックのファイルは消すが、記録にあるものは残す
\(全消しに退化しても検出できるよう両方を assert する)。"
  (wamei/term-restore-test--with-saved-dir
    (let ((stale (expand-file-name "gone-1.txt" wamei/term-restore-directory))
          (buffer (get-buffer-create "*term: foo*")))
      (write-region "old\n" nil stale nil 'silent)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq default-directory "/tmp/")
              (insert "hello\n"))
            (wamei/term-restore-save)
            (should-not (file-exists-p stale))
            (should (file-exists-p (plist-get (car wamei/term-restore-saved) :scrollback))))
        (kill-buffer buffer)))))

(ert-deftest wamei/term-restore-save-prune-skips-directories ()
  "スクロールバックの置き場にディレクトリが混じっても signal しない
\(`delete-file' は `desktop-save-hook' の中で走るので、落ちると desktop の
保存ごと止まる)。"
  (wamei/term-restore-test--with-saved-dir
    (let ((subdir (expand-file-name "subdir" wamei/term-restore-directory))
          (buffer (get-buffer-create "*term: foo*")))
      (make-directory subdir t)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq default-directory "/tmp/")
              (insert "hello\n"))
            (wamei/term-restore-save)
            (should (file-directory-p subdir)))
        (kill-buffer buffer)))))

(ert-deftest wamei/term-restore-save-writes-tail-ansi-and-drops-prompt ()
  "保存は `--content' → `--tail' → `--ansi' を通して書く。末尾のプロンプト行は
落ち、色は SGR になり、行数は `wamei/term-restore-scrollback-lines' に
切り詰められる (`--entry' の合成をまとめて確認する結合テスト)。"
  (wamei/term-restore-test--with-saved-dir
    (let ((wamei/term-restore-scrollback-lines 2)
          (buffer (get-buffer-create "*term: foo*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq default-directory "/tmp/")
              (insert "one\n" "two\n")
              (insert (wamei/term-restore-test--colored "three" :foreground "#ff0000") "\n")
              (insert "$ ")
              (wamei/term-restore-test--insert-marked "\n"))
            (wamei/term-restore-save)
            (let ((file (plist-get (car wamei/term-restore-saved) :scrollback)))
              (should (equal (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string))
                             "two\n\e[38;2;255;0;0mthree\e[0m\n"))))
        (kill-buffer buffer)))))

;;; スクロールバックの注入

(ert-deftest wamei/term-restore-inject-sets-env-for-saved-buffer ()
  "記録のあるバッファ名で端末が起動するとき WAMEI_TERM_RESTORE を渡す。"
  (wamei/term-restore-test--with-saved-dir
    (let ((file (expand-file-name "foo-1.txt" wamei/term-restore-directory)))
      (write-region "old output\n" nil file nil 'silent)
      (setq wamei/term-restore-saved
            (list (list :name "*term: foo*" :directory "/tmp/"
                        :title nil :scrollback file)))
      (with-current-buffer (get-buffer-create "*term: foo*")
        (unwind-protect
            ;; ghostel-pre-spawn-hook は process-environment を動的束縛した
            ;; 状態で呼ぶので、それを模す
            (let ((process-environment (copy-sequence process-environment)))
              (wamei/term-restore--inject-scrollback)
              (should (equal (getenv "WAMEI_TERM_RESTORE") file))
              ;; 注入しても記録は残る (消すのは `-ensure' の仕上げ)
              (should wamei/term-restore-saved))
          (kill-buffer (current-buffer)))))))

(ert-deftest wamei/term-restore-inject-ignores-missing-file ()
  "記録はあるがファイルが読めないときは環境変数を立てない。"
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: foo*" :directory "/tmp/"
                      :title nil
                      :scrollback (expand-file-name "gone.txt"
                                                    wamei/term-restore-directory))))
    (with-current-buffer (get-buffer-create "*term: foo*")
      (unwind-protect
          (let ((process-environment (copy-sequence process-environment)))
            (wamei/term-restore--inject-scrollback)
            (should-not (getenv "WAMEI_TERM_RESTORE")))
        (kill-buffer (current-buffer))))))

(ert-deftest wamei/term-restore-inject-ignores-unknown-buffer ()
  "記録の無いバッファでは何もしない。"
  (wamei/term-restore-test--with-saved-dir
    (with-current-buffer (get-buffer-create "*term: other*")
      (unwind-protect
          (let ((process-environment (copy-sequence process-environment)))
            (wamei/term-restore--inject-scrollback)
            (should-not (getenv "WAMEI_TERM_RESTORE")))
        (kill-buffer (current-buffer))))))

(ert-deftest wamei/term-restore-inject-happens-only-once ()
  "同じ端末名への注入は 1 回だけ。2 回目以降は空振りする。

`ghostel-pre-spawn-hook' はグローバルなので、記録が残っている間はすべての
spawn で走る。desktop の autosave が記録を埋め直すため、端末を kill して
同じ名前で開き直すと死んだ端末の出力が再生されてしまう。注入済みの印を
付けて 1 回で打ち止めにする。"
  (wamei/term-restore-test--with-saved-dir
    (let ((file (expand-file-name "foo-1.txt" wamei/term-restore-directory)))
      (write-region "old output\n" nil file nil 'silent)
      (setq wamei/term-restore-saved
            (list (list :name "*term: foo*" :directory "/tmp/"
                        :title nil :scrollback file)))
      (with-current-buffer (get-buffer-create "*term: foo*")
        (unwind-protect
            (progn
              (let ((process-environment (copy-sequence process-environment)))
                (wamei/term-restore--inject-scrollback)
                (should (equal (getenv "WAMEI_TERM_RESTORE") file)))
              ;; 2 回目 (kill して同名で開き直した端末を模す)
              (let ((process-environment (copy-sequence process-environment)))
                (wamei/term-restore--inject-scrollback)
                (should-not (getenv "WAMEI_TERM_RESTORE"))))
          (kill-buffer (current-buffer)))))))

;;; 復元の仕上げ

(defmacro wamei/term-restore-test--with-fake-create (&rest body)
  "`ghostel-create' をバッファを作るだけの偽物にして BODY を評価する。
`calls' に (NAME . DIRECTORY) が積まれる。"
  (declare (indent 0))
  `(let ((calls nil))
     (cl-letf (((symbol-function 'ghostel-create)
                (lambda (&optional name &rest _)
                  (push (cons name default-directory) calls)
                  (get-buffer-create name))))
       ,@body)))

(ert-deftest wamei/term-restore-inject-only-during-restore-window ()
  "注入が効くのは起動時の desktop 復元の間だけ。

`wamei/term-restore-ensure' が復元の仕上げでフラグを下ろすので、その後に
`desktop-save-mode' の autosave が記録を作り直しても (端末を kill して同名で
開き直しても) 注入は起きない。:injected は記録ごとの印なので、記録が作り
直されるとリセットされてしまい、この窓の外側はそれでは押さえられない。"
  (wamei/term-restore-test--with-saved-dir
    (let ((file (expand-file-name "foo-1.txt" wamei/term-restore-directory)))
      (write-region "old output\n" nil file nil 'silent)
      (unwind-protect
          (wamei/term-restore-test--with-fake-create
            ;; 復元の仕上げ (ここでフラグが下がる)
            (wamei/term-restore-ensure)
            ;; autosave が記録を作り直したところを模す (:injected は付かない)
            (setq wamei/term-restore-saved
                  (list (list :name "*term: foo*" :directory "/tmp/"
                              :title nil :scrollback file)))
            (with-current-buffer (get-buffer-create "*term: foo*")
              (let ((process-environment (copy-sequence process-environment)))
                (wamei/term-restore--inject-scrollback)
                (should-not (getenv "WAMEI_TERM_RESTORE")))))
        (when (get-buffer "*term: foo*") (kill-buffer "*term: foo*"))))))

(ert-deftest wamei/term-restore-restoring-flag-starts-enabled ()
  "フラグの初期値は t (起動直後は desktop 復元の窓の中)。
セッションスコープなので `desktop-globals-to-save' には入れない
\(復元中かどうかを永続化すると再起動なしで注入が復活してしまう)。
`desktop-read' がファイルを読めなかったときは `desktop-after-read-hook' が
走らないが、その 2 つの分岐でも窓を閉じる (下の -closes-window-when-* を参照)。"
  (should (default-value 'wamei/term-restore--restoring))
  (let ((desktop-globals-to-save nil)
        (desktop-save-hook nil)
        (desktop-after-read-hook nil)
        (ghostel-pre-spawn-hook nil))
    (wamei/term-restore-setup)
    (should-not (memq 'wamei/term-restore--restoring desktop-globals-to-save))))

(ert-deftest wamei/term-restore-ensure-lowers-restoring-flag ()
  "復元の仕上げでフラグを下ろす。C-g (quit) で抜けても下ろす
\(記録のクリアと同じ `unwind-protect' の後始末)。"
  (wamei/term-restore-test--with-saved-dir
    (wamei/term-restore-ensure)
    (should-not wamei/term-restore--restoring))
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: foo*" :directory "/tmp/"
                      :title nil :scrollback nil)))
    (cl-letf (((symbol-function 'ghostel-create)
               (lambda (&rest _) (signal 'quit nil))))
      (condition-case nil (wamei/term-restore-ensure) (quit nil))
      (should-not wamei/term-restore--restoring))))

(ert-deftest wamei/term-restore-ensure-creates-missing-terminals ()
  "ghostel-desktop が復元しなかった端末だけを作り、タイトルを戻す。"
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: foo*" :directory "/tmp/"
                      :title "make test" :scrollback nil)))
    (wamei/term-restore-test--with-fake-create
      (unwind-protect
          (progn
            (wamei/term-restore-ensure)
            (should (equal calls '(("*term: foo*" . "/tmp/"))))
            (should (equal (buffer-local-value 'ghostel-title (get-buffer "*term: foo*"))
                           "make test")))
        (kill-buffer "*term: foo*")))))

(ert-deftest wamei/term-restore-ensure-leaves-live-terminals-alone ()
  "既にあるバッファは作り直さないが、タイトルは戻す
\(ghostel-desktop が desktop-read 中に復元した端末がこの経路に来る)。"
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: foo*" :directory "/tmp/"
                      :title "make test" :scrollback nil)))
    (let ((buffer (get-buffer-create "*term: foo*")))
      (unwind-protect
          (wamei/term-restore-test--with-fake-create
            (wamei/term-restore-ensure)
            (should-not calls)
            (should (equal (buffer-local-value 'ghostel-title buffer) "make test")))
        (kill-buffer buffer)))))

(ert-deftest wamei/term-restore-ensure-keeps-reported-title ()
  "端末が既にタイトルを報告していれば上書きしない。"
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: foo*" :directory "/tmp/"
                      :title "古いコマンド" :scrollback nil)))
    (let ((buffer (get-buffer-create "*term: foo*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer (setq-local ghostel-title "新しいコマンド"))
            (wamei/term-restore-test--with-fake-create
              (wamei/term-restore-ensure)
              (should (equal (buffer-local-value 'ghostel-title buffer) "新しいコマンド"))))
        (kill-buffer buffer)))))

(ert-deftest wamei/term-restore-ensure-clears-records ()
  "仕上げで記録を空にする。同じ名前で開き直した端末に前回の出力を再生しない。"
  (wamei/term-restore-test--with-saved-dir
    (let ((file (expand-file-name "foo-1.txt" wamei/term-restore-directory)))
      (write-region "old output\n" nil file nil 'silent)
      (setq wamei/term-restore-saved
            (list (list :name "*term: foo*" :directory "/tmp/"
                        :title nil :scrollback file)))
      (let ((buffer (get-buffer-create "*term: foo*")))
        (unwind-protect
            (wamei/term-restore-test--with-fake-create
              (wamei/term-restore-ensure)
              (should-not wamei/term-restore-saved)
              (with-current-buffer buffer
                (let ((process-environment (copy-sequence process-environment)))
                  (wamei/term-restore--inject-scrollback)
                  (should-not (getenv "WAMEI_TERM_RESTORE")))))
          (kill-buffer buffer))))))

(ert-deftest wamei/term-restore-ensure-restores-title-after-inject ()
  "注入が `-ensure' より先に済んでいても、記録は仕上げまで残っていてタイトルが戻る。

ghostel-desktop は `desktop-read' の中で端末を復元するので、pre-spawn hook
(`--inject-scrollback') は `wamei/term-restore-ensure' より先に走る。注入を
契機に記録を消す実装 (`--inject-scrollback' の中で `delq' する等) にすると、
仕上げに記録が届かず、この端末のタイトルを戻せなくなる。"
  (wamei/term-restore-test--with-saved-dir
    (let ((file (expand-file-name "foo-1.txt" wamei/term-restore-directory)))
      (write-region "old output\n" nil file nil 'silent)
      (setq wamei/term-restore-saved
            (list (list :name "*term: foo*" :directory "/tmp/"
                        :title "make test" :scrollback file)))
      (unwind-protect
          (wamei/term-restore-test--with-fake-create
            ;; ghostel-desktop が復元済みの端末を模す (注入は -ensure より前)
            (with-current-buffer (get-buffer-create "*term: foo*")
              (let ((process-environment (copy-sequence process-environment)))
                (wamei/term-restore--inject-scrollback)
                (should (equal (getenv "WAMEI_TERM_RESTORE") file))))
            (wamei/term-restore-ensure)
            ;; 復元済みなので作り直さない
            (should-not calls)
            (should (equal (buffer-local-value 'ghostel-title (get-buffer "*term: foo*"))
                           "make test"))
            (should-not wamei/term-restore-saved))
        (kill-buffer "*term: foo*")))))

(ert-deftest wamei/term-restore-ensure-continues-and-clears-after-error ()
  "ある端末の生成でエラーが起きても他の端末は処理を続け、記録は最後に空になる
\(condition-case は各エントリごとで、全体のクリアを止めない)。"
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: broken*" :directory "/tmp/"
                      :title nil :scrollback nil)
                (list :name "*term: ok*" :directory "/tmp/"
                      :title "make test" :scrollback nil)))
    (let ((calls nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-create)
                     (lambda (&optional name &rest _)
                       (push name calls)
                       (if (equal name "*term: broken*")
                           (error "boom")
                         (get-buffer-create name)))))
            (wamei/term-restore-ensure)
            (should (equal (reverse calls) '("*term: broken*" "*term: ok*")))
            (should (equal (buffer-local-value 'ghostel-title (get-buffer "*term: ok*"))
                           "make test"))
            (should-not wamei/term-restore-saved))
        (when (get-buffer "*term: ok*") (kill-buffer "*term: ok*"))))))

(ert-deftest wamei/term-restore-ensure-falls-back-to-home ()
  "記録のディレクトリが無くなっていればホームで作る。"
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: foo*" :directory "/nonexistent-dir-xyz/"
                      :title nil :scrollback nil)))
    (wamei/term-restore-test--with-fake-create
      (unwind-protect
          (progn
            (wamei/term-restore-ensure)
            (should (equal (cdar calls) (expand-file-name "~/"))))
        (kill-buffer "*term: foo*")))))

(ert-deftest wamei/term-restore-ensure-drops-lazy-queue-entry ()
  "取りこぼしを作った端末は `desktop-buffer-args-list' の遅延キューからも
除く。残っていると `desktop-restore-eager' を超えて lazy 復元に回っていた
ときに、後の idle 復元 (`desktop-idle-create-buffers' 等) が同名の
バッファをもう一つ作ってしまう (`desktop-create-buffer' が名前の重複を
検査せず `rename-buffer' で uniquify するだけなので)。無関係なキューの
要素は残す。"
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: foo*" :directory "/tmp/"
                      :title nil :scrollback nil)))
    (let* ((timer (run-with-idle-timer 100 t #'ignore))
           (desktop-lazy-timer timer)
           (desktop-buffer-args-list
            (list (list 208 nil "*term: foo*" 'term-mode nil 0 nil nil nil)
                  (list 208 nil "*scratch*" 'lisp-interaction-mode nil 0 nil nil nil))))
      (wamei/term-restore-test--with-fake-create
        (unwind-protect
            (progn
              (wamei/term-restore-ensure)
              (should (equal (mapcar (lambda (args) (nth 2 args)) desktop-buffer-args-list)
                             '("*scratch*")))
              ;; キューが残っているうちはタイマを止めない
              (should (eq desktop-lazy-timer timer)))
          (cancel-timer timer)
          (kill-buffer "*term: foo*"))))))

(ert-deftest wamei/term-restore-ensure-cancels-lazy-timer-when-queue-empties ()
  "遅延キューを空にしたら `desktop-lazy-timer' も止める。

`desktop-idle-create-buffers' のタイマ停止は `while' の内側にあるので、
外からキューを空にすると空振りのタイマが毎アイドル走り続ける。"
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: foo*" :directory "/tmp/"
                      :title nil :scrollback nil)))
    (let* ((timer (run-with-idle-timer 100 t #'ignore))
           (desktop-lazy-timer timer)
           (desktop-buffer-args-list
            (list (list 208 nil "*term: foo*" 'term-mode nil 0 nil nil nil))))
      (wamei/term-restore-test--with-fake-create
        (unwind-protect
            (progn
              (wamei/term-restore-ensure)
              (should-not desktop-buffer-args-list)
              (should-not desktop-lazy-timer)
              (should-not (memq timer timer-idle-list)))
          (when (memq timer timer-idle-list) (cancel-timer timer))
          (kill-buffer "*term: foo*"))))))

;;; desktop を読めなかった分岐

(ert-deftest wamei/term-restore-closes-window-when-no-desktop-file ()
  "desktop ファイルが無い分岐 (`desktop-no-desktop-file-hook') でも窓を閉じる。
`desktop-after-read-hook' は `desktop-read' がファイルを読めたときしか走らない。"
  (wamei/term-restore-test--with-saved-dir
    (let ((desktop-no-desktop-file-hook nil)
          (desktop-globals-to-save nil)
          (desktop-save-hook nil)
          (desktop-after-read-hook nil)
          (desktop-not-loaded-hook nil)
          (ghostel-pre-spawn-hook nil))
      (wamei/term-restore-setup)
      (run-hooks 'desktop-no-desktop-file-hook)
      (should-not wamei/term-restore--restoring))))

(ert-deftest wamei/term-restore-closes-window-when-desktop-locked ()
  "他のインスタンスがロックを持っている分岐 (`desktop-not-loaded-hook') でも窓を閉じる。"
  (wamei/term-restore-test--with-saved-dir
    (let ((desktop-not-loaded-hook nil)
          (desktop-globals-to-save nil)
          (desktop-save-hook nil)
          (desktop-after-read-hook nil)
          (desktop-no-desktop-file-hook nil)
          (ghostel-pre-spawn-hook nil))
      (wamei/term-restore-setup)
      (run-hooks 'desktop-not-loaded-hook)
      (should-not wamei/term-restore--restoring))))

(ert-deftest wamei/term-restore-no-inject-after-desktop-read-failed ()
  "desktop を読めなかったセッションでは、autosave が記録を作っても注入しない。

`desktop-save-mode' と `wamei/term-restore-setup' は無条件に有効なので、
desktop を読めなくても 30 秒アイドルの autosave が記録を埋める。窓を閉じて
おかないと、端末を kill して同名で開き直したときに死んだ端末の出力が
再生されてしまう (記録が空だから安全、という話ではない)。"
  (wamei/term-restore-test--with-saved-dir
    (let ((file (expand-file-name "foo-1.txt" wamei/term-restore-directory))
          (desktop-no-desktop-file-hook nil)
          (desktop-globals-to-save nil)
          (desktop-save-hook nil)
          (desktop-after-read-hook nil)
          (desktop-not-loaded-hook nil)
          (ghostel-pre-spawn-hook nil))
      (write-region "old output\n" nil file nil 'silent)
      (wamei/term-restore-setup)
      (run-hooks 'desktop-no-desktop-file-hook)
      (unwind-protect
          (progn
            ;; autosave が記録を作ったところを模す
            (setq wamei/term-restore-saved
                  (list (list :name "*term: foo*" :directory "/tmp/"
                              :title nil :scrollback file)))
            (with-current-buffer (get-buffer-create "*term: foo*")
              (let ((process-environment (copy-sequence process-environment)))
                (wamei/term-restore--inject-scrollback)
                (should-not (getenv "WAMEI_TERM_RESTORE")))))
        (kill-buffer "*term: foo*")))))

;;; desktop への組み込み

(ert-deftest wamei/term-restore-setup-hooks-into-desktop ()
  "desktop の保存・読み込みと ghostel-pre-spawn-hook に組み込み、記録の変数を保存対象にする。
`desktop-read' が desktop ファイルを読めなかった 2 つの分岐
\(`desktop-no-desktop-file-hook' / `desktop-not-loaded-hook') にも結線する。
ここに結線しないと `wamei/term-restore-ensure' が一度も呼ばれず、注入の窓が
セッション中ずっと開いたままになる。"
  (let ((desktop-globals-to-save nil)
        (desktop-save-hook nil)
        (desktop-after-read-hook nil)
        (desktop-no-desktop-file-hook nil)
        (desktop-not-loaded-hook nil)
        (ghostel-pre-spawn-hook nil))
    (wamei/term-restore-setup)
    (should (memq 'wamei/term-restore-saved desktop-globals-to-save))
    (should (memq #'wamei/term-restore-save desktop-save-hook))
    (should (memq #'wamei/term-restore--inject-scrollback ghostel-pre-spawn-hook))
    (should (memq #'wamei/term-restore-ensure desktop-after-read-hook))
    (should (memq #'wamei/term-restore--finish-restoring desktop-no-desktop-file-hook))
    (should (memq #'wamei/term-restore--finish-restoring desktop-not-loaded-hook))))

(provide 'term-restore-test)
;;; term-restore-test.el ends here
