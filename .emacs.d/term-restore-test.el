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
抑止しない (face が付いている = 端末が明示した色)。"
  (should (equal (wamei/term-restore--ansi
                  (concat "plain"
                          (wamei/term-restore-test--colored
                           "red" :foreground "#ff0000")))
                 "plain\e[38;2;255;0;0mred\e[0m")))

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
  "スクロールバックの出力先を一時ディレクトリにして BODY を評価する。"
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "term-restore-" t)))
          (wamei/term-restore-directory dir)
          (wamei/term-restore-saved nil))
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
  "記録されていないスクロールバックのファイルは消す。"
  (wamei/term-restore-test--with-saved-dir
    (let ((stale (expand-file-name "gone-1.txt" wamei/term-restore-directory)))
      (write-region "old\n" nil stale nil 'silent)
      (wamei/term-restore-save)
      (should-not (file-exists-p stale)))))

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
              (should (equal (getenv "WAMEI_TERM_RESTORE") file)))
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

;;; desktop への組み込み

(ert-deftest wamei/term-restore-setup-hooks-into-desktop ()
  "desktop の保存・読み込みと ghostel-pre-spawn-hook に組み込み、記録の変数を保存対象にする。"
  (let ((desktop-globals-to-save nil)
        (desktop-save-hook nil)
        (desktop-after-read-hook nil)
        (ghostel-pre-spawn-hook nil))
    (wamei/term-restore-setup)
    (should (memq 'wamei/term-restore-saved desktop-globals-to-save))
    (should (memq #'wamei/term-restore-save desktop-save-hook))
    (should (memq #'wamei/term-restore--inject-scrollback ghostel-pre-spawn-hook))
    (should (memq #'wamei/term-restore-ensure desktop-after-read-hook))))

(provide 'term-restore-test)
;;; term-restore-test.el ends here
