;;; project-formatter-test.el --- tests for project-formatter -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l project-formatter-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
;; css-base-mode の継承関係は css-mode を読み込むまで定義されない。
(require 'css-mode)

;; apheleia 本体は読み込まず、参照する変数と関数だけを用意する。
(defvar apheleia-formatters nil)
(defvar-local apheleia-formatter nil)
(defvar wamei/project-formatter-test--mode-calls nil
  "スタブの `apheleia-mode' が受け取った引数。")
(defun apheleia-mode (&optional arg)
  (push arg wamei/project-formatter-test--mode-calls))

(load (expand-file-name "project-formatter.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro wamei/project-formatter-test--with-project (config-name &rest body)
  "一時ディレクトリに CONFIG-NAME (nil なら無し) と src/ を作って BODY を実行する。
BODY 中は `root' にプロジェクトルート、`src' に src/ が束縛される。"
  (declare (indent 1))
  `(let* ((root (file-name-as-directory
                 (make-temp-file "project-formatter-test" t)))
          (src (file-name-as-directory (expand-file-name "src" root))))
     (unwind-protect
         (progn
           (make-directory src)
           (when ,config-name
             (with-temp-file (expand-file-name ,config-name root) (insert "{}")))
           ,@body)
       (delete-directory root t))))

(defun wamei/project-formatter-test--write (dir name content)
  "DIR/NAME に CONTENT を書く。"
  (with-temp-file (expand-file-name name dir) (insert content)))

(defmacro wamei/project-formatter-test--with-file-buffer (file &rest body)
  "FILE を訪問しているかのようなバッファで BODY を実行する。"
  (declare (indent 1))
  `(with-temp-buffer
     (setq buffer-file-name ,file
           default-directory (file-name-directory ,file))
     (let ((wamei/project-formatter-test--mode-calls nil))
       ,@body)))

;;; フォーマッタの判定

(ert-deftest wamei/project-formatter-detect-finds-biome-json-in-same-dir ()
  "同じディレクトリの biome.json を見つける。"
  (wamei/project-formatter-test--with-project "biome.json"
    (should (equal (wamei/project-formatter-detect root) (cons 'biome root)))))

(ert-deftest wamei/project-formatter-detect-finds-biome-json-in-parent ()
  "親ディレクトリにある biome.json まで遡って見つける。"
  (wamei/project-formatter-test--with-project "biome.json"
    (should (equal (wamei/project-formatter-detect src) (cons 'biome root)))))

(ert-deftest wamei/project-formatter-detect-finds-biome-jsonc ()
  "biome.jsonc も設定ファイルとして扱う。"
  (wamei/project-formatter-test--with-project "biome.jsonc"
    (should (equal (wamei/project-formatter-detect src) (cons 'biome root)))))

(ert-deftest wamei/project-formatter-detect-returns-nil-without-config ()
  "設定ファイルが無ければ nil。package.json だけあっても対象外。"
  (wamei/project-formatter-test--with-project nil
    (wamei/project-formatter-test--write root "package.json" "{\"name\":\"x\"}")
    (should-not (wamei/project-formatter-detect src))))

(ert-deftest wamei/project-formatter-detect-finds-prettierrc ()
  ".prettierrc と拡張子付きの .prettierrc.* と prettier.config.* を prettier と判定する。"
  (dolist (name '(".prettierrc" ".prettierrc.json" ".prettierrc.yaml" ".prettierrc.mjs"
                  "prettier.config.js" "prettier.config.ts"))
    (wamei/project-formatter-test--with-project name
      (should (equal (wamei/project-formatter-detect src) (cons 'prettier root))))))

(ert-deftest wamei/project-formatter-detect-finds-prettier-key-in-package-json ()
  "package.json に \"prettier\" キーがあれば prettier。"
  (wamei/project-formatter-test--with-project nil
    (wamei/project-formatter-test--write root "package.json"
                                         "{\"name\":\"x\",\"prettier\":{\"useTabs\":true}}")
    (should (equal (wamei/project-formatter-detect src) (cons 'prettier root)))))

(ert-deftest wamei/project-formatter-detect-ignores-broken-package-json ()
  "壊れた package.json はエラーにせず無視する。"
  (wamei/project-formatter-test--with-project nil
    (wamei/project-formatter-test--write root "package.json" "{ not json")
    (should-not (wamei/project-formatter-detect src))))

(ert-deftest wamei/project-formatter-detect-prefers-nearest-directory ()
  "ルートに biome、パッケージに prettier があればファイルに近い prettier が勝つ。"
  (wamei/project-formatter-test--with-project "biome.json"
    (wamei/project-formatter-test--write src ".prettierrc" "{}")
    (should (equal (wamei/project-formatter-detect src) (cons 'prettier src)))))

(ert-deftest wamei/project-formatter-detect-prefers-biome-in-same-directory ()
  "同じディレクトリに両方あれば biome。"
  (wamei/project-formatter-test--with-project "biome.json"
    (wamei/project-formatter-test--write root ".prettierrc" "{}")
    (should (equal (wamei/project-formatter-detect src) (cons 'biome root)))))

;;; 保存時フォーマットの有効化

(ert-deftest wamei/project-formatter-maybe-enable-turns-on-apheleia-with-config ()
  "設定があるプロジェクトのファイルでは biome を選んで apheleia-mode を入れる。"
  (wamei/project-formatter-test--with-project "biome.json"
    (wamei/project-formatter-test--with-file-buffer (expand-file-name "a.ts" src)
      (should (wamei/project-formatter-maybe-enable))
      (should (eq apheleia-formatter 'biome))
      (should (equal wamei/project-formatter-test--mode-calls '(1))))))

(ert-deftest wamei/project-formatter-maybe-enable-picks-prettier ()
  "prettier の設定があるプロジェクトでは prettier を選ぶ。"
  (wamei/project-formatter-test--with-project ".prettierrc"
    (wamei/project-formatter-test--with-file-buffer (expand-file-name "a.ts" src)
      (should (wamei/project-formatter-maybe-enable))
      (should (eq apheleia-formatter 'prettier))
      (should (equal wamei/project-formatter-test--mode-calls '(1))))))

(ert-deftest wamei/project-formatter-maybe-enable-does-nothing-without-config ()
  "設定が無ければ何もしない。"
  (wamei/project-formatter-test--with-project nil
    (wamei/project-formatter-test--with-file-buffer (expand-file-name "a.ts" src)
      (should-not (wamei/project-formatter-maybe-enable))
      (should-not apheleia-formatter)
      (should-not wamei/project-formatter-test--mode-calls))))

(ert-deftest wamei/project-formatter-maybe-enable-does-nothing-without-file ()
  "ファイルを訪問していないバッファでは何もしない。"
  (wamei/project-formatter-test--with-project "biome.json"
    (with-temp-buffer
      (setq default-directory src)
      (let ((wamei/project-formatter-test--mode-calls nil))
        (should-not (wamei/project-formatter-maybe-enable))
        (should-not wamei/project-formatter-test--mode-calls)))))

;;; フォーマッタ定義

(ert-deftest wamei/project-formatter-setup-registers-remote-capable-command ()
  "apheleia-npx を使わず、npx シンボルで node_modules/.bin を解決するコマンドに差し替える。"
  (let ((apheleia-formatters '((biome . ("apheleia-npx" "biome" "check"))
                               (prettier-css . ("prettier-css")))))
    (wamei/project-formatter-setup)
    (should (equal (alist-get 'biome apheleia-formatters)
                   '(npx "biome" "check" "--write" "--linter-enabled=false"
                         "--stdin-file-path" filepath)))
    ;; 他のフォーマッタには触らない
    (should (equal (alist-get 'prettier-css apheleia-formatters) '("prettier-css")))))

(ert-deftest wamei/project-formatter-setup-registers-prettier-without-emacs-indent-flags ()
  "組み込みの prettier 定義 (apheleia-npx と Emacs のインデントを渡す引数) を置き換える。"
  (let ((apheleia-formatters '((prettier . ("apheleia-npx" "prettier" "--stdin-filepath" filepath
                                            (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))))))
    (wamei/project-formatter-setup)
    (should (equal (alist-get 'prettier apheleia-formatters)
                   '(npx "prettier" "--stdin-filepath" filepath)))))

(ert-deftest wamei/project-formatter-setup-adds-entry-when-missing ()
  "biome エントリが無くても追加する。"
  (let ((apheleia-formatters '((prettier-css . ("prettier-css")))))
    (wamei/project-formatter-setup)
    (should (eq (car (alist-get 'biome apheleia-formatters)) 'npx))
    (should (eq (car (alist-get 'prettier apheleia-formatters)) 'npx))))

;;; 編集時インデントの同期

(ert-deftest wamei/project-formatter-probe-snippet-picks-language-by-mode ()
  "モードに応じて biome に渡す断片を選ぶ。JSON は 1 行に畳まれないよう改行入り。"
  (should (equal (wamei/project-formatter-probe-snippet 'json-ts-mode) "{\n\"a\":1}"))
  (should (equal (wamei/project-formatter-probe-snippet 'css-ts-mode) "a{color:red}"))
  (should (equal (wamei/project-formatter-probe-snippet 'css-mode) "a{color:red}"))
  (should (equal (wamei/project-formatter-probe-snippet 'typescript-ts-mode) "if(a){b()}"))
  (should (equal (wamei/project-formatter-probe-snippet 'js-ts-mode) "if(a){b()}")))

(ert-deftest wamei/project-formatter-parse-indentation-tab ()
  "2 行目がタブで始まればタブインデント。"
  (should (equal (wamei/project-formatter-parse-indentation "if (a) {\n\tb();\n}\n")
                 '((indent_style . "tab") (indent_size . "tab")))))

(ert-deftest wamei/project-formatter-parse-indentation-spaces ()
  "2 行目のスペース数が indent_size。"
  (should (equal (wamei/project-formatter-parse-indentation "if (a) {\n  b();\n}\n")
                 '((indent_style . "space") (indent_size . "2"))))
  (should (equal (wamei/project-formatter-parse-indentation "{\n    \"a\": 1\n}\n")
                 '((indent_style . "space") (indent_size . "4")))))

(ert-deftest wamei/project-formatter-parse-indentation-rejects-garbage ()
  "2 行目が無い、またはインデントされていなければ nil。"
  (should-not (wamei/project-formatter-parse-indentation ""))
  (should-not (wamei/project-formatter-parse-indentation "if (a) {\n"))
  (should-not (wamei/project-formatter-parse-indentation "error\nsomething\n")))

(defvar wamei/project-formatter-test--process-calls nil
  "スタブの `process-file' が受け取った (PROGRAM ARGS)。")

(defmacro wamei/project-formatter-test--with-process-stub (output exit &rest body)
  "`process-file' を OUTPUT を書き出して EXIT を返すスタブに差し替えて BODY を実行する。"
  (declare (indent 2))
  `(let ((wamei/project-formatter-test--process-calls nil))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program _infile destination _display &rest args)
                  (push (cons program args) wamei/project-formatter-test--process-calls)
                  (let ((buf (if (consp destination) (car destination) destination)))
                    (when (bufferp buf)
                      (with-current-buffer buf (insert ,output))))
                  ,exit)))
       ,@body)))

(ert-deftest wamei/project-formatter-indentation-runs-biome-on-the-file ()
  "ファイルのパスを --stdin-file-path に渡して biome format を走らせ、結果を props にする。"
  (wamei/project-formatter-test--with-project "biome.json"
    (wamei/project-formatter-test--with-file-buffer (expand-file-name "a.ts" src)
      (setq major-mode 'typescript-ts-mode)
      (wamei/project-formatter-test--with-process-stub "if (a) {\n\tb();\n}\n" 0
        (should (equal (wamei/project-formatter-indentation 'biome)
                       '((indent_style . "tab") (indent_size . "tab"))))
        (let ((cmd (mapconcat #'identity (cdar wamei/project-formatter-test--process-calls) " ")))
          (should (string-match-p "biome format" cmd))
          (should (string-match-p (regexp-quote (concat "--stdin-file-path=" (expand-file-name "a.ts" src))) cmd))
          (should (string-match-p (regexp-quote (shell-quote-argument "if(a){b()}")) cmd)))))))

(ert-deftest wamei/project-formatter-indentation-strips-tramp-prefix ()
  "リモートのファイルはローカル名 (TRAMP プレフィックス無し) で渡す。"
  (with-temp-buffer
    (setq buffer-file-name "/ssh:host:/home/u/proj/src/a.ts"
          default-directory "/ssh:host:/home/u/proj/src/"
          major-mode 'typescript-ts-mode)
    ;; node_modules の探索はリモートへ接続してしまうので実行ファイル解決だけ止める。
    (cl-letf (((symbol-function 'wamei/project-formatter--executable) (lambda (_tool) "biome")))
      (wamei/project-formatter-test--with-process-stub "if (a) {\n  b();\n}\n" 0
        (should (equal (wamei/project-formatter-indentation 'biome)
                       '((indent_style . "space") (indent_size . "2"))))
        (let ((cmd (mapconcat #'identity (cdar wamei/project-formatter-test--process-calls) " ")))
          (should (string-match-p "--stdin-file-path=/home/u/proj/src/a.ts" cmd))
          (should-not (string-match-p "/ssh:" cmd)))))))

(ert-deftest wamei/project-formatter-indentation-runs-prettier-with-stdin-filepath ()
  "prettier は --stdin-filepath でファイルを渡す。"
  (wamei/project-formatter-test--with-project ".prettierrc"
    (wamei/project-formatter-test--with-file-buffer (expand-file-name "a.ts" src)
      (setq major-mode 'typescript-ts-mode)
      (wamei/project-formatter-test--with-process-stub "if (a) {\n    b();\n}\n" 0
        (should (equal (wamei/project-formatter-indentation 'prettier)
                       '((indent_style . "space") (indent_size . "4"))))
        (let ((cmd (mapconcat #'identity (cdar wamei/project-formatter-test--process-calls) " ")))
          (should (string-match-p (regexp-quote (concat "prettier --stdin-filepath " (expand-file-name "a.ts" src))) cmd))
          (should-not (string-match-p "biome" cmd)))))))

(ert-deftest wamei/project-formatter-indentation-returns-nil-on-failure ()
  "biome が失敗したら nil。"
  (wamei/project-formatter-test--with-project "biome.json"
    (wamei/project-formatter-test--with-file-buffer (expand-file-name "a.ts" src)
      (setq major-mode 'typescript-ts-mode)
      (wamei/project-formatter-test--with-process-stub "error\n" 1
        (should-not (wamei/project-formatter-indentation 'biome))))))

(ert-deftest wamei/project-formatter-hack-properties-overrides-indent-for-biome-buffers ()
  "biome 管理下のバッファでは editorconfig の props を biome の値で上書きする。"
  (wamei/project-formatter-test--with-project "biome.json"
    (wamei/project-formatter-test--with-file-buffer (expand-file-name "a.ts" src)
      (setq major-mode 'typescript-ts-mode)
      (setq-local apheleia-formatter 'biome)
      (let ((props (make-hash-table :test 'equal)))
        (puthash 'indent_style "space" props)
        (puthash 'indent_size "4" props)
        (wamei/project-formatter-test--with-process-stub "if (a) {\n\tb();\n}\n" 0
          (wamei/project-formatter-hack-editorconfig-properties props))
        (should (equal (gethash 'indent_style props) "tab"))
        (should (equal (gethash 'indent_size props) "tab"))))))

(ert-deftest wamei/project-formatter-hack-properties-handles-prettier-buffers ()
  "prettier 管理下のバッファでも prettier を測って上書きする。"
  (wamei/project-formatter-test--with-project ".prettierrc"
    (wamei/project-formatter-test--with-file-buffer (expand-file-name "a.ts" src)
      (setq major-mode 'typescript-ts-mode)
      (setq-local apheleia-formatter 'prettier)
      (let ((props (make-hash-table :test 'equal)))
        (wamei/project-formatter-test--with-process-stub "if (a) {\n\tb();\n}\n" 0
          (wamei/project-formatter-hack-editorconfig-properties props)
          (should (string-match-p "prettier" (mapconcat #'identity (cdar wamei/project-formatter-test--process-calls) " "))))
        (should (equal (gethash 'indent_style props) "tab"))))))

(ert-deftest wamei/project-formatter-hack-properties-ignores-other-buffers ()
  "biome 管理下でなければ biome を起動せず props も触らない。"
  (wamei/project-formatter-test--with-project nil
    (wamei/project-formatter-test--with-file-buffer (expand-file-name "a.ts" src)
      (setq major-mode 'typescript-ts-mode)
      (let ((props (make-hash-table :test 'equal)))
        (puthash 'indent_size "4" props)
        (wamei/project-formatter-test--with-process-stub "if (a) {\n\tb();\n}\n" 0
          (wamei/project-formatter-hack-editorconfig-properties props)
          (should-not wamei/project-formatter-test--process-calls))
        (should (equal (gethash 'indent_size props) "4"))
        (should-not (gethash 'indent_style props))))))

(ert-deftest wamei/project-formatter-hack-properties-keeps-props-when-biome-fails ()
  "biome が失敗したら props は元のまま。"
  (wamei/project-formatter-test--with-project "biome.json"
    (wamei/project-formatter-test--with-file-buffer (expand-file-name "a.ts" src)
      (setq major-mode 'typescript-ts-mode)
      (setq-local apheleia-formatter 'biome)
      (let ((props (make-hash-table :test 'equal)))
        (puthash 'indent_size "4" props)
        (wamei/project-formatter-test--with-process-stub "error\n" 1
          (wamei/project-formatter-hack-editorconfig-properties props))
        (should (equal (gethash 'indent_size props) "4"))))))

(provide 'project-formatter-test)
;;; project-formatter-test.el ends here
