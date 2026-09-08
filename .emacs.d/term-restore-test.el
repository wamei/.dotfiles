;;; term-restore-test.el --- tests for term-restore -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l term-restore-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
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
  "vterm が画面下端まで埋める空行と行末の空白は落とす。"
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

(defmacro wamei/term-restore-test--with-terminals (buffers &rest body)
  "BUFFERS ((NAME CONTENT TITLE) ...) の端末風バッファと一時ディレクトリを用意して BODY を評価する。
`default-directory' は一時ディレクトリ配下の cwd/ になる。"
  (declare (indent 1))
  `(let* ((dir (file-name-as-directory (make-temp-file "wamei-term-restore-" t)))
          (cwd (file-name-as-directory (expand-file-name "cwd" dir)))
          (wamei/term-restore-directory (expand-file-name "scrollback/" dir))
          (wamei/term-restore-saved nil)
          (created nil))
     (make-directory cwd)
     (unwind-protect
         (progn
           (pcase-dolist (`(,name ,content ,title) ,buffers)
             (with-current-buffer (get-buffer-create name)
               (push (current-buffer) created)
               (setq default-directory cwd)
               (insert content)
               (setq-local wamei/term--title title)))
           ,@body)
       (mapc #'kill-buffer created)
       (delete-directory dir t))))

(ert-deftest wamei/term-restore-save-records-every-terminal ()
  "端末バッファだけを番号順に記録し、それ以外は無視する。"
  (wamei/term-restore-test--with-terminals
      '(("*term: foo 2*" "two\n" "make")
        ("*term: foo*" "one\n" nil)
        ("*not a term*" "x\n" nil))
    (wamei/term-restore-save)
    (should (equal (mapcar (lambda (entry)
                             (list (plist-get entry :project)
                                   (plist-get entry :index)
                                   (plist-get entry :directory)
                                   (plist-get entry :title)))
                           wamei/term-restore-saved)
                   `(("foo" 1 ,cwd nil)
                     ("foo" 2 ,cwd "make"))))))

(ert-deftest wamei/term-restore-save-writes-scrollback-tail ()
  "スクロールバックの末尾 N 行をプロンプト行を除いて専用ディレクトリに書き、パスを記録する。"
  (wamei/term-restore-test--with-terminals
      '(("*term: foo*" "a\nb\nc\n" nil))
    (with-current-buffer "*term: foo*"
      (insert "$ ")
      (wamei/term-restore-test--insert-marked "\n")
      (insert "\n"))
    (let ((wamei/term-restore-scrollback-lines 2))
      (wamei/term-restore-save))
    (let ((file (plist-get (car wamei/term-restore-saved) :scrollback)))
      (should (string-prefix-p wamei/term-restore-directory file))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                     "b\nc\n")))))

(ert-deftest wamei/term-restore-save-writes-colors-as-sgr ()
  "色は SGR エスケープに写して書く。行末の空白はその前に落とす。"
  (wamei/term-restore-test--with-terminals
      `(("*term: foo*" ,(concat (wamei/term-restore-test--colored "ok" :foreground "#00ff00")
                                "   \n")
         nil))
    (wamei/term-restore-save)
    (let ((file (plist-get (car wamei/term-restore-saved) :scrollback)))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                     "\e[38;2;0;255;0mok\e[0m\n")))))

(ert-deftest wamei/term-restore-save-prunes-stale-files ()
  "記録に含まれないスクロールバックのファイルは消す。"
  (wamei/term-restore-test--with-terminals
      '(("*term: foo*" "a\n" nil))
    (make-directory wamei/term-restore-directory t)
    (let ((stale (expand-file-name "stale.txt" wamei/term-restore-directory)))
      (write-region "old" nil stale nil 'silent)
      (wamei/term-restore-save)
      (should-not (file-exists-p stale))
      (should (file-exists-p (plist-get (car wamei/term-restore-saved) :scrollback))))))

;;; 復元

(defmacro wamei/term-restore-test--with-fake-vterm (calls &rest body)
  "`vterm' を、名前のバッファを作って呼び出し内容を CALLS に積む偽物に差し替えて BODY を評価する。
CALLS の各要素は (NAME DEFAULT-DIRECTORY RESTORE-ENV)。作ったバッファは後で消す。"
  (declare (indent 1))
  `(let ((,calls nil)
         (created nil))
     (cl-letf (((symbol-function 'vterm)
                (lambda (name)
                  (push (list name default-directory (getenv "WAMEI_TERM_RESTORE")) ,calls)
                  (with-current-buffer (get-buffer-create name)
                    (push (current-buffer) created)
                    (setq default-directory (file-name-as-directory default-directory))
                    (current-buffer)))))
       (unwind-protect
           (progn ,@body)
         (mapc #'kill-buffer created)))))

(ert-deftest wamei/term-restore-all-recreates-each-terminal ()
  "記録の数だけ端末を作り、作業ディレクトリとタイトルを戻す。"
  (let* ((dir (file-name-as-directory (make-temp-file "wamei-term-restore-" t)))
         (wamei/term-restore-saved
          `((:project "foo" :index 1 :directory ,dir :title "make" :scrollback nil)
            (:project "foo" :index 2 :directory ,dir :title nil :scrollback nil))))
    (unwind-protect
        (wamei/term-restore-test--with-fake-vterm calls
          (wamei/term-restore-all)
          (should (equal (mapcar (lambda (call) (list (car call) (cadr call))) (reverse calls))
                         `(("*term: foo*" ,dir) ("*term: foo 2*" ,dir))))
          (should (equal (buffer-local-value 'wamei/term--title (get-buffer "*term: foo*"))
                         "make"))
          (should-not (buffer-local-value 'wamei/term--title (get-buffer "*term: foo 2*"))))
      (delete-directory dir t))))

(ert-deftest wamei/term-restore-all-passes-scrollback-through-environment ()
  "読めるスクロールバックがあれば WAMEI_TERM_RESTORE にパスを載せる。無ければ載せない。"
  (let* ((file (make-temp-file "wamei-term-restore-" nil ".txt" "old output\n"))
         (wamei/term-restore-saved
          `((:project "foo" :index 1 :directory "~/" :scrollback ,file)
            (:project "bar" :index 1 :directory "~/" :scrollback "/nonexistent/x.txt"))))
    (unwind-protect
        (wamei/term-restore-test--with-fake-vterm calls
          (wamei/term-restore-all)
          (should (equal (nth 2 (assoc "*term: foo*" calls)) file))
          (should-not (nth 2 (assoc "*term: bar*" calls)))
          ;; 環境変数は呼び出しの間だけで、外には漏れない
          (should-not (getenv "WAMEI_TERM_RESTORE")))
      (delete-file file))))

(ert-deftest wamei/term-restore-all-falls-back-when-directory-is-gone ()
  "作業ディレクトリが無くなっていればホームで作る。"
  (let ((wamei/term-restore-saved
         '((:project "foo" :index 1 :directory "/nonexistent/dir/" :scrollback nil))))
    (wamei/term-restore-test--with-fake-vterm calls
      (wamei/term-restore-all)
      (should (equal (cadr (car calls)) (expand-file-name "~/"))))))

(ert-deftest wamei/term-restore-all-skips-existing-buffers ()
  "同名の端末が既にあれば作り直さない。"
  (let ((wamei/term-restore-saved
         '((:project "foo" :index 1 :directory "~/" :scrollback nil))))
    (with-current-buffer (get-buffer-create "*term: foo*")
      (unwind-protect
          (wamei/term-restore-test--with-fake-vterm calls
            (wamei/term-restore-all)
            (should-not calls))
        (kill-buffer)))))

(ert-deftest wamei/term-restore-setup-hooks-into-desktop ()
  "desktop の保存・読み込みに組み込み、記録の変数を保存対象にする。"
  (let ((desktop-globals-to-save nil)
        (desktop-save-hook nil)
        (desktop-after-read-hook nil))
    (wamei/term-restore-setup)
    (should (memq 'wamei/term-restore-saved desktop-globals-to-save))
    (should (memq #'wamei/term-restore-save desktop-save-hook))
    (should (memq #'wamei/term-restore-all desktop-after-read-hook))))

(provide 'term-restore-test)
;;; term-restore-test.el ends here
