;;; project-formatter.el --- プロジェクトのフォーマッタ設定 (biome / prettier) に従う -*- lexical-binding: t; -*-

;;; Commentary:

;; apheleia を使い、biome か prettier の設定が見つかるファイルでだけ保存時に
;; そのフォーマッタを走らせる。設定が無いプロジェクトでは何もしない。
;;
;; 判定はファイルのディレクトリから上へ辿り、各ディレクトリで biome → prettier
;; の順に設定を探す。ファイルに近いディレクトリの設定が優先され、同じ
;; ディレクトリに両方あれば biome。monorepo でパッケージごとにツールが違っても
;; 自然に振り分かる。
;;
;; apheleia 組み込みの biome / prettier 定義は、パッケージ内のシェルスクリプト
;; `apheleia-npx' 経由で起動する。TRAMP でリモート実行するとそのスクリプトが
;; リモートに無いため動かないので、`npx' シンボル (apheleia が Lisp 側で
;; node_modules/.bin を解決し、無ければ PATH のコマンドを使う) を使った定義に
;; 差し替える。`filepath' は TRAMP プレフィックスを除いたパスに展開され、
;; プロセスはバッファの `default-directory' で起動するので、biome が cwd から
;; 上へ辿って見つけた設定がそのまま効く。prettier は --stdin-filepath のパスから
;; 設定を解決するので cwd に依存しない。組み込みの prettier 定義は Emacs 側の
;; インデント設定を --use-tabs / --tab-width で渡してプロジェクト設定を上書き
;; してしまうため、その点でも差し替えが必要。
;;
;; `biome check --write --linter-enabled=false' はフォーマットに加えて
;; 設定で有効な assist (import の整列など) も適用する。`files.includes' で
;; 除外されたファイルは無変更で返り、構文エラーのときは exit 1 になるので
;; apheleia はバッファを触らない。prettier も .prettierignore 対象は無変更で
;; 返り、構文エラーは exit 2 になる。
;;
;; 編集時のインデント (js-indent-level 等) は Emacs のモード既定値のままだと
;; フォーマッタの出力と食い違い、保存のたびにインデントが変わる。設定ファイルを
;; 自前で解釈する代わりに、小さな断片をフォーマッタに通して実際のインデントを
;; 測り、editorconfig のプロパティ (indent_style / indent_size) として流し込む。
;; `extends' / `overrides' / ネストした設定 / .editorconfig の取り込みはすべて
;; フォーマッタ側で解決され、モード変数への割り当ては editorconfig に任せられる。
;; editorconfig はファイルを開いた後に .editorconfig を適用するので、モードの
;; フックで変数を直接設定すると上書きされる。props を書き換えるフック
;; (`editorconfig-hack-properties-functions') に載せれば「フォーマッタの設定が
;; .editorconfig より優先」というツール自身の優先順位と一致する。

;;; Code:

(require 'cl-lib)

(defvar apheleia-formatters)
(defvar apheleia-formatter)

(declare-function apheleia-mode "apheleia")

;;; フォーマッタの判定

(defconst wamei/project-formatter-biome-config-names '("biome.json" "biome.jsonc")
  "biome の設定ファイル名。")

(defconst wamei/project-formatter-prettier-config-names
  '(".prettierrc" ".prettierrc.json" ".prettierrc.yaml" ".prettierrc.yml"
    ".prettierrc.json5" ".prettierrc.js" ".prettierrc.cjs" ".prettierrc.mjs"
    ".prettierrc.toml" "prettier.config.js" "prettier.config.cjs"
    "prettier.config.mjs" "prettier.config.ts")
  "prettier の設定ファイル名。これに加えて \"prettier\" キーを持つ package.json も設定扱い。")

(defun wamei/project-formatter--any-file-p (dir names)
  "DIR に NAMES のどれかがあれば non-nil。"
  (cl-some (lambda (name) (file-exists-p (expand-file-name name dir))) names))

(defun wamei/project-formatter--package-json-prettier-p (dir)
  "DIR の package.json に \"prettier\" キーがあれば non-nil。壊れていれば nil。"
  (let ((file (expand-file-name "package.json" dir)))
    (when (file-exists-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (let ((json (json-parse-buffer :object-type 'hash-table)))
            (and (hash-table-p json)
                 (not (eq (gethash "prettier" json 'missing) 'missing)))))))))

(defun wamei/project-formatter--tool-in-dir (dir)
  "DIR 直下の設定からフォーマッタを判定する。biome / prettier / nil。"
  (cond
   ((wamei/project-formatter--any-file-p dir wamei/project-formatter-biome-config-names)
    'biome)
   ((or (wamei/project-formatter--any-file-p dir wamei/project-formatter-prettier-config-names)
        (wamei/project-formatter--package-json-prettier-p dir))
    'prettier)))

(defun wamei/project-formatter-detect (&optional dir)
  "DIR (省略時は `default-directory') から上へ辿り、最初に見つかった設定を (TOOL . DIR) で返す。
TOOL は `biome' か `prettier'。見つからなければ nil。TRAMP のパスでも動く。"
  (let (tool)
    (when-let* ((found (locate-dominating-file
                        (or dir default-directory)
                        (lambda (d) (setq tool (wamei/project-formatter--tool-in-dir d))))))
      (cons tool (file-name-as-directory found)))))

;;; 保存時フォーマット

(defconst wamei/project-formatter-commands
  '((biome . (npx "biome" "check" "--write" "--linter-enabled=false"
                  "--stdin-file-path" filepath))
    (prettier . (npx "prettier" "--stdin-filepath" filepath)))
  "apheleia に登録するコマンド。ローカルでもリモートでも動く形。")

(defun wamei/project-formatter-maybe-enable ()
  "現在のバッファのプロジェクトにフォーマッタ設定があれば保存時フォーマットを有効にする。
有効にしたら non-nil を返す。メジャーモードのフックから呼ぶ。"
  (when-let* ((_ buffer-file-name)
              (detected (wamei/project-formatter-detect
                         (file-name-directory buffer-file-name))))
    (setq-local apheleia-formatter (car detected))
    (apheleia-mode 1)
    t))

(defun wamei/project-formatter-setup ()
  "`apheleia-formatters' の biome / prettier 定義をリモートでも動くコマンドに差し替える。"
  (pcase-dolist (`(,tool . ,command) wamei/project-formatter-commands)
    (setf (alist-get tool apheleia-formatters) command)))

;;; 編集時インデントの同期

(defun wamei/project-formatter-probe-snippet (mode)
  "MODE の言語で、フォーマットするとインデント 1 段が現れる最小の断片を返す。
JSON は改行を入れておかないと 1 行に畳まれてインデントが観測できない。"
  (cond
   ((provided-mode-derived-p mode 'json-ts-mode) "{\n\"a\":1}")
   ((provided-mode-derived-p mode 'css-base-mode) "a{color:red}")
   (t "if(a){b()}")))

(defun wamei/project-formatter-parse-indentation (output)
  "整形された OUTPUT の 2 行目の字下げを editorconfig の props にして返す。
タブなら ((indent_style . \"tab\") (indent_size . \"tab\"))、スペースなら
その個数を indent_size にする。字下げが読めなければ nil。"
  (let ((lines (split-string output "\n")))
    (when-let* ((second (nth 1 lines)))
      (cond
       ((string-prefix-p "\t" second)
        '((indent_style . "tab") (indent_size . "tab")))
       ((string-match "\\` +" second)
        `((indent_style . "space")
          (indent_size . ,(number-to-string (match-end 0)))))))))

(defun wamei/project-formatter--executable (tool)
  "プロジェクトの node_modules/.bin/TOOL があればそのローカル名、無ければ TOOL の名前。
apheleia の `npx' シンボルと同じ解決順。"
  (let ((name (symbol-name tool))
        (project (locate-dominating-file default-directory "node_modules")))
    (or (when project
          (let ((bin (expand-file-name (concat "node_modules/.bin/" name) project)))
            (when (file-executable-p bin)
              (file-local-name bin))))
        name)))

(defun wamei/project-formatter--probe-command (tool exe file)
  "TOOL を EXE で起動して FILE 用の設定で標準入力を整形するシェルコマンドの後半。"
  (pcase tool
    ('biome (concat exe " format --stdin-file-path=" (shell-quote-argument file)))
    ('prettier (concat exe " --stdin-filepath " (shell-quote-argument file)))))

(defun wamei/project-formatter-indentation (tool)
  "現在のバッファのファイルに対して TOOL が使うインデントを props で返す。
断片を TOOL に通して測る。`process-file' で走らせるので TRAMP なら
リモート側で実行される。失敗したら nil。"
  (let* ((snippet (wamei/project-formatter-probe-snippet major-mode))
         (file (file-local-name buffer-file-name))
         (exe (wamei/project-formatter--executable tool))
         (command (concat "printf %s " (shell-quote-argument snippet) " | "
                          (wamei/project-formatter--probe-command tool exe file))))
    (with-temp-buffer
      (let ((status (process-file shell-file-name nil (list (current-buffer) nil) nil
                                  shell-command-switch command)))
        (when (eql status 0)
          (wamei/project-formatter-parse-indentation (buffer-string)))))))

(defun wamei/project-formatter-hack-editorconfig-properties (props)
  "フォーマッタ管理下のバッファなら editorconfig の PROPS のインデント設定を実測値で上書きする。
`editorconfig-hack-properties-functions' に載せる。"
  (when (and buffer-file-name
             (assq apheleia-formatter wamei/project-formatter-commands))
    (pcase-dolist (`(,key . ,value)
                   (wamei/project-formatter-indentation apheleia-formatter))
      (puthash key value props))))

(provide 'project-formatter)
;;; project-formatter.el ends here
