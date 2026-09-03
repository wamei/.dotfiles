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

(defun wamei/biome-format-setup ()
  "`apheleia-formatters' の biome 定義をリモートでも動くコマンドに差し替える。"
  (setf (alist-get 'biome apheleia-formatters) wamei/biome-format-command))

(provide 'biome-format)
;;; biome-format.el ends here
