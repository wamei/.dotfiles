;;; biome-format.el --- biome の設定があるプロジェクトだけ保存時にフォーマットする -*- lexical-binding: t; -*-

;;; Commentary:

;; apheleia を使い、`biome.json' / `biome.jsonc' が見つかるファイルでだけ保存時に
;; biome を走らせる。設定が無いプロジェクトでは何もしない。
;;
;; apheleia 組み込みの biome 定義は、パッケージ内のシェルスクリプト
;; `apheleia-npx' 経由で起動する。TRAMP でリモート実行するとそのスクリプトが
;; リモートに無いため動かないので、`npx' シンボル (apheleia が Lisp 側で
;; node_modules/.bin を解決し、無ければ PATH の biome を使う) を使った定義に
;; 差し替える。`filepath' は TRAMP プレフィックスを除いたパスに展開され、
;; プロセスはバッファの `default-directory' で起動するので、biome が cwd から
;; 上へ辿って見つけた設定がそのまま効く。
;;
;; `biome check --write --linter-enabled=false' はフォーマットに加えて
;; 設定で有効な assist (import の整列など) も適用する。`files.includes' で
;; 除外されたファイルは無変更で返り、構文エラーのときは exit 1 になるので
;; apheleia はバッファを触らない。
;;
;; 編集時のインデント (js-indent-level 等) は Emacs のモード既定値のままだと
;; biome の出力と食い違い、保存のたびにインデントが変わる。biome.jsonc を自前で
;; 解釈する代わりに、小さな断片を `biome format' に通して実際のインデントを測り、
;; editorconfig のプロパティ (indent_style / indent_size) として流し込む。
;; `extends' / `overrides' / ネストした設定 / .editorconfig の取り込みはすべて
;; biome 側で解決され、モード変数への割り当ては editorconfig に任せられる。
;; editorconfig はファイルを開いた後に .editorconfig を適用するので、モードの
;; フックで変数を直接設定すると上書きされる。props を書き換えるフック
;; (`editorconfig-hack-properties-functions') に載せれば「biome.json が
;; .editorconfig より優先」という biome 自身の優先順位と一致する。

;;; Code:

(require 'cl-lib)

(defvar apheleia-formatters)
(defvar apheleia-formatter)

(declare-function apheleia-mode "apheleia")

(defconst wamei/biome-format-config-names '("biome.json" "biome.jsonc")
  "biome の設定ファイル名。")

(defconst wamei/biome-format-command
  '(npx "biome" "check" "--write" "--linter-enabled=false"
        "--stdin-file-path" filepath)
  "apheleia に登録する biome のコマンド。ローカルでもリモートでも動く形。")

(defun wamei/biome-format-config-dir (&optional dir)
  "DIR (省略時は `default-directory') から上へ辿り、biome の設定があるディレクトリを返す。
見つからなければ nil。TRAMP のパスでも動く。"
  (locate-dominating-file
   (or dir default-directory)
   (lambda (d)
     (cl-some (lambda (name) (file-exists-p (expand-file-name name d)))
              wamei/biome-format-config-names))))

(defun wamei/biome-format-maybe-enable ()
  "現在のバッファが biome 設定のあるプロジェクトのファイルなら保存時フォーマットを有効にする。
有効にしたら non-nil を返す。メジャーモードのフックから呼ぶ。"
  (when (and buffer-file-name
             (wamei/biome-format-config-dir
              (file-name-directory buffer-file-name)))
    (setq-local apheleia-formatter 'biome)
    (apheleia-mode 1)
    t))

;;; 編集時インデントの同期

(defun wamei/biome-format-probe-snippet (mode)
  "MODE の言語で、フォーマットするとインデント 1 段が現れる最小の断片を返す。
JSON は改行を入れておかないと 1 行に畳まれてインデントが観測できない。"
  (cond
   ((provided-mode-derived-p mode 'json-ts-mode) "{\n\"a\":1}")
   ((provided-mode-derived-p mode 'css-base-mode) "a{color:red}")
   (t "if(a){b()}")))

(defun wamei/biome-format-parse-indentation (output)
  "biome が整形した OUTPUT の 2 行目の字下げを editorconfig の props にして返す。
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

(defun wamei/biome-format--executable ()
  "プロジェクトの node_modules/.bin/biome があればそのローカル名、無ければ \"biome\"。
apheleia の `npx' シンボルと同じ解決順。"
  (let ((project (locate-dominating-file default-directory "node_modules")))
    (or (when project
          (let ((bin (expand-file-name "node_modules/.bin/biome" project)))
            (when (file-executable-p bin)
              (file-local-name bin))))
        "biome")))

(defun wamei/biome-format-indentation ()
  "現在のバッファのファイルに対して biome が使うインデントを props で返す。
断片を `biome format' に通して測る。`process-file' で走らせるので TRAMP なら
リモート側で実行される。biome が失敗したら nil。"
  (let ((snippet (wamei/biome-format-probe-snippet major-mode))
        (file (file-local-name buffer-file-name))
        (exe (wamei/biome-format--executable)))
    (with-temp-buffer
      (let* ((command (concat "printf %s " (shell-quote-argument snippet)
                              " | " exe " format --stdin-file-path="
                              (shell-quote-argument file)))
             (status (process-file shell-file-name nil (list (current-buffer) nil) nil
                                   shell-command-switch command)))
        (when (eql status 0)
          (wamei/biome-format-parse-indentation (buffer-string)))))))

(defun wamei/biome-format-hack-editorconfig-properties (props)
  "biome 管理下のバッファなら editorconfig の PROPS のインデント設定を biome の値で上書きする。
`editorconfig-hack-properties-functions' に載せる。"
  (when (and buffer-file-name (eq apheleia-formatter 'biome))
    (pcase-dolist (`(,key . ,value) (wamei/biome-format-indentation))
      (puthash key value props))))

(defun wamei/biome-format-setup ()
  "`apheleia-formatters' の biome 定義をリモートでも動くコマンドに差し替える。"
  (setf (alist-get 'biome apheleia-formatters) wamei/biome-format-command))

(provide 'biome-format)
;;; biome-format.el ends here
