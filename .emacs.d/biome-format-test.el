;;; biome-format-test.el --- tests for biome-format -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l biome-format-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
;; css-base-mode の継承関係は css-mode を読み込むまで定義されない。
(require 'css-mode)

;; apheleia 本体は読み込まず、参照する変数と関数だけを用意する。
(defvar apheleia-formatters nil)
(defvar-local apheleia-formatter nil)
(defvar wamei/biome-format-test--mode-calls nil
  "スタブの `apheleia-mode' が受け取った引数。")
(defun apheleia-mode (&optional arg)
  (push arg wamei/biome-format-test--mode-calls))

(load (expand-file-name "biome-format.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro wamei/biome-format-test--with-project (config-name &rest body)
  "一時ディレクトリに CONFIG-NAME (nil なら無し) と src/ を作って BODY を実行する。
BODY 中は `root' にプロジェクトルート、`src' に src/ が束縛される。"
  (declare (indent 1))
  `(let* ((root (file-name-as-directory
                 (make-temp-file "biome-format-test" t)))
          (src (file-name-as-directory (expand-file-name "src" root))))
     (unwind-protect
         (progn
           (make-directory src)
           (when ,config-name
             (with-temp-file (expand-file-name ,config-name root) (insert "{}")))
           ,@body)
       (delete-directory root t))))

(defmacro wamei/biome-format-test--with-file-buffer (file &rest body)
  "FILE を訪問しているかのようなバッファで BODY を実行する。"
  (declare (indent 1))
  `(with-temp-buffer
     (setq buffer-file-name ,file
           default-directory (file-name-directory ,file))
     (let ((wamei/biome-format-test--mode-calls nil))
       ,@body)))

;;; 設定ファイルの探索

(ert-deftest wamei/biome-format-config-dir-finds-biome-json-in-same-dir ()
  "同じディレクトリの biome.json を見つける。"
  (wamei/biome-format-test--with-project "biome.json"
    (should (equal (wamei/biome-format-config-dir root) root))))

(ert-deftest wamei/biome-format-config-dir-finds-biome-json-in-parent ()
  "親ディレクトリにある biome.json まで遡って見つける。"
  (wamei/biome-format-test--with-project "biome.json"
    (should (equal (wamei/biome-format-config-dir src) root))))

(ert-deftest wamei/biome-format-config-dir-finds-biome-jsonc ()
  "biome.jsonc も設定ファイルとして扱う。"
  (wamei/biome-format-test--with-project "biome.jsonc"
    (should (equal (wamei/biome-format-config-dir src) root))))

(ert-deftest wamei/biome-format-config-dir-returns-nil-without-config ()
  "設定ファイルが無ければ nil。"
  (wamei/biome-format-test--with-project nil
    (should-not (wamei/biome-format-config-dir src))))

;;; 保存時フォーマットの有効化

(ert-deftest wamei/biome-format-maybe-enable-turns-on-apheleia-with-config ()
  "設定があるプロジェクトのファイルでは biome を選んで apheleia-mode を入れる。"
  (wamei/biome-format-test--with-project "biome.json"
    (wamei/biome-format-test--with-file-buffer (expand-file-name "a.ts" src)
      (should (wamei/biome-format-maybe-enable))
      (should (eq apheleia-formatter 'biome))
      (should (equal wamei/biome-format-test--mode-calls '(1))))))

(ert-deftest wamei/biome-format-maybe-enable-does-nothing-without-config ()
  "設定が無ければ何もしない。"
  (wamei/biome-format-test--with-project nil
    (wamei/biome-format-test--with-file-buffer (expand-file-name "a.ts" src)
      (should-not (wamei/biome-format-maybe-enable))
      (should-not apheleia-formatter)
      (should-not wamei/biome-format-test--mode-calls))))

(ert-deftest wamei/biome-format-maybe-enable-does-nothing-without-file ()
  "ファイルを訪問していないバッファでは何もしない。"
  (wamei/biome-format-test--with-project "biome.json"
    (with-temp-buffer
      (setq default-directory src)
      (let ((wamei/biome-format-test--mode-calls nil))
        (should-not (wamei/biome-format-maybe-enable))
        (should-not wamei/biome-format-test--mode-calls)))))

;;; フォーマッタ定義

(ert-deftest wamei/biome-format-setup-registers-remote-capable-command ()
  "apheleia-npx を使わず、npx シンボルで node_modules/.bin を解決するコマンドに差し替える。"
  (let ((apheleia-formatters '((biome . ("apheleia-npx" "biome" "check"))
                               (prettier . ("prettier")))))
    (wamei/biome-format-setup)
    (should (equal (alist-get 'biome apheleia-formatters)
                   '(npx "biome" "check" "--write" "--linter-enabled=false"
                         "--stdin-file-path" filepath)))
    ;; 他のフォーマッタには触らない
    (should (equal (alist-get 'prettier apheleia-formatters) '("prettier")))))

(ert-deftest wamei/biome-format-setup-adds-entry-when-missing ()
  "biome エントリが無くても追加する。"
  (let ((apheleia-formatters '((prettier . ("prettier")))))
    (wamei/biome-format-setup)
    (should (eq (car (alist-get 'biome apheleia-formatters)) 'npx))))

;;; 編集時インデントの同期

(ert-deftest wamei/biome-format-probe-snippet-picks-language-by-mode ()
  "モードに応じて biome に渡す断片を選ぶ。JSON は 1 行に畳まれないよう改行入り。"
  (should (equal (wamei/biome-format-probe-snippet 'json-ts-mode) "{\n\"a\":1}"))
  (should (equal (wamei/biome-format-probe-snippet 'css-ts-mode) "a{color:red}"))
  (should (equal (wamei/biome-format-probe-snippet 'css-mode) "a{color:red}"))
  (should (equal (wamei/biome-format-probe-snippet 'typescript-ts-mode) "if(a){b()}"))
  (should (equal (wamei/biome-format-probe-snippet 'js-ts-mode) "if(a){b()}")))

(ert-deftest wamei/biome-format-parse-indentation-tab ()
  "2 行目がタブで始まればタブインデント。"
  (should (equal (wamei/biome-format-parse-indentation "if (a) {\n\tb();\n}\n")
                 '((indent_style . "tab") (indent_size . "tab")))))

(ert-deftest wamei/biome-format-parse-indentation-spaces ()
  "2 行目のスペース数が indent_size。"
  (should (equal (wamei/biome-format-parse-indentation "if (a) {\n  b();\n}\n")
                 '((indent_style . "space") (indent_size . "2"))))
  (should (equal (wamei/biome-format-parse-indentation "{\n    \"a\": 1\n}\n")
                 '((indent_style . "space") (indent_size . "4")))))

(ert-deftest wamei/biome-format-parse-indentation-rejects-garbage ()
  "2 行目が無い、またはインデントされていなければ nil。"
  (should-not (wamei/biome-format-parse-indentation ""))
  (should-not (wamei/biome-format-parse-indentation "if (a) {\n"))
  (should-not (wamei/biome-format-parse-indentation "error\nsomething\n")))

(defvar wamei/biome-format-test--process-calls nil
  "スタブの `process-file' が受け取った (PROGRAM ARGS)。")

(defmacro wamei/biome-format-test--with-process-stub (output exit &rest body)
  "`process-file' を OUTPUT を書き出して EXIT を返すスタブに差し替えて BODY を実行する。"
  (declare (indent 2))
  `(let ((wamei/biome-format-test--process-calls nil))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program _infile destination _display &rest args)
                  (push (cons program args) wamei/biome-format-test--process-calls)
                  (let ((buf (if (consp destination) (car destination) destination)))
                    (when (bufferp buf)
                      (with-current-buffer buf (insert ,output))))
                  ,exit)))
       ,@body)))

(ert-deftest wamei/biome-format-indentation-runs-biome-on-the-file ()
  "ファイルのパスを --stdin-file-path に渡して biome format を走らせ、結果を props にする。"
  (wamei/biome-format-test--with-project "biome.json"
    (wamei/biome-format-test--with-file-buffer (expand-file-name "a.ts" src)
      (setq major-mode 'typescript-ts-mode)
      (wamei/biome-format-test--with-process-stub "if (a) {\n\tb();\n}\n" 0
        (should (equal (wamei/biome-format-indentation)
                       '((indent_style . "tab") (indent_size . "tab"))))
        (let ((cmd (mapconcat #'identity (cdar wamei/biome-format-test--process-calls) " ")))
          (should (string-match-p "biome format" cmd))
          (should (string-match-p (regexp-quote (concat "--stdin-file-path=" (expand-file-name "a.ts" src))) cmd))
          (should (string-match-p (regexp-quote (shell-quote-argument "if(a){b()}")) cmd)))))))

(ert-deftest wamei/biome-format-indentation-strips-tramp-prefix ()
  "リモートのファイルはローカル名 (TRAMP プレフィックス無し) で渡す。"
  (with-temp-buffer
    (setq buffer-file-name "/ssh:host:/home/u/proj/src/a.ts"
          default-directory "/ssh:host:/home/u/proj/src/"
          major-mode 'typescript-ts-mode)
    ;; node_modules の探索はリモートへ接続してしまうので実行ファイル解決だけ止める。
    (cl-letf (((symbol-function 'wamei/biome-format--executable) (lambda () "biome")))
      (wamei/biome-format-test--with-process-stub "if (a) {\n  b();\n}\n" 0
        (should (equal (wamei/biome-format-indentation)
                       '((indent_style . "space") (indent_size . "2"))))
        (let ((cmd (mapconcat #'identity (cdar wamei/biome-format-test--process-calls) " ")))
          (should (string-match-p "--stdin-file-path=/home/u/proj/src/a.ts" cmd))
          (should-not (string-match-p "/ssh:" cmd)))))))

(ert-deftest wamei/biome-format-indentation-returns-nil-on-failure ()
  "biome が失敗したら nil。"
  (wamei/biome-format-test--with-project "biome.json"
    (wamei/biome-format-test--with-file-buffer (expand-file-name "a.ts" src)
      (setq major-mode 'typescript-ts-mode)
      (wamei/biome-format-test--with-process-stub "error\n" 1
        (should-not (wamei/biome-format-indentation))))))

(ert-deftest wamei/biome-format-hack-properties-overrides-indent-for-biome-buffers ()
  "biome 管理下のバッファでは editorconfig の props を biome の値で上書きする。"
  (wamei/biome-format-test--with-project "biome.json"
    (wamei/biome-format-test--with-file-buffer (expand-file-name "a.ts" src)
      (setq major-mode 'typescript-ts-mode)
      (setq-local apheleia-formatter 'biome)
      (let ((props (make-hash-table :test 'equal)))
        (puthash 'indent_style "space" props)
        (puthash 'indent_size "4" props)
        (wamei/biome-format-test--with-process-stub "if (a) {\n\tb();\n}\n" 0
          (wamei/biome-format-hack-editorconfig-properties props))
        (should (equal (gethash 'indent_style props) "tab"))
        (should (equal (gethash 'indent_size props) "tab"))))))

(ert-deftest wamei/biome-format-hack-properties-ignores-other-buffers ()
  "biome 管理下でなければ biome を起動せず props も触らない。"
  (wamei/biome-format-test--with-project nil
    (wamei/biome-format-test--with-file-buffer (expand-file-name "a.ts" src)
      (setq major-mode 'typescript-ts-mode)
      (let ((props (make-hash-table :test 'equal)))
        (puthash 'indent_size "4" props)
        (wamei/biome-format-test--with-process-stub "if (a) {\n\tb();\n}\n" 0
          (wamei/biome-format-hack-editorconfig-properties props)
          (should-not wamei/biome-format-test--process-calls))
        (should (equal (gethash 'indent_size props) "4"))
        (should-not (gethash 'indent_style props))))))

(ert-deftest wamei/biome-format-hack-properties-keeps-props-when-biome-fails ()
  "biome が失敗したら props は元のまま。"
  (wamei/biome-format-test--with-project "biome.json"
    (wamei/biome-format-test--with-file-buffer (expand-file-name "a.ts" src)
      (setq major-mode 'typescript-ts-mode)
      (setq-local apheleia-formatter 'biome)
      (let ((props (make-hash-table :test 'equal)))
        (puthash 'indent_size "4" props)
        (wamei/biome-format-test--with-process-stub "error\n" 1
          (wamei/biome-format-hack-editorconfig-properties props))
        (should (equal (gethash 'indent_size props) "4"))))))

(provide 'biome-format-test)
;;; biome-format-test.el ends here
