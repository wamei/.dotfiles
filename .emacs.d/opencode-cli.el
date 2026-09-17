;;; opencode-cli.el --- opencode run を Emacs から使う -*- lexical-binding: t; -*-
;;; Commentary:
;; `opencode run' を非同期プロセスとして呼ぶ backend。プロンプトは stdin で渡す。
;; モデルと provider は opencode の設定 (~/.config/opencode/opencode.jsonc) に任せる
;; ので、社内 LLM を既定にしていればそれが使われる。
;;
;; - `wamei/opencode-cli-run'  llm-cli.el の backend。`wamei/claude-cli-run' と同じ形。
;; - `wamei/opencode'          リージョンや minibuffer の prompt を投げる汎用コマンド。
;;
;; claude との違いが 2 つある:
;; - `--system-prompt' に当たる引数が無いので、システムプロンプトは入力の先頭に置く。
;; - 出力の先頭に `> build · MODEL' というヘッダ行と ANSI が付くので取り除く。
;;; Code:

(require 'subr-x)
;; batch-byte-compile 時は load-file-name も buffer-file-name も nil になるため byte-compile-current-file を併用する
(require 'llm-cli
         (expand-file-name "llm-cli"
                           (file-name-directory
                            (or load-file-name
                                (bound-and-true-p byte-compile-current-file)
                                buffer-file-name))))

;;; 設定

(defvar wamei/opencode-cli-program "opencode"
  "opencode CLI の実行ファイル。")

;;; プロセス

(defun wamei/opencode-cli--command (model)
  "MODEL で単発生成する opencode のコマンドライン。prompt は stdin で渡す。
MODEL が nil なら `--model' を渡さず opencode の設定の既定モデルに任せる。
`--pure' は外部プラグインを読まない指定で、起動を軽くする
\(設定ファイルの provider は npm 依存なので `--pure' でも生きている)。"
  (append (list wamei/opencode-cli-program "run" "--pure")
          (when model (list "--model" model))))

(defun wamei/opencode-cli--clean (output)
  "opencode の OUTPUT から ANSI エスケープと先頭のヘッダ行を取り除く。
ヘッダは `> build · MODEL' の形で毎回 1 行だけ付く。本文中の引用は残したいので
落とすのは先頭の 1 行だけに限る。"
  (let* ((text (replace-regexp-in-string "\033\\[[0-9;]*[A-Za-z]" "" output))
         (text (replace-regexp-in-string "\\`[ \t\n]*> [^\n]*\n" "" text)))
    (string-trim text)))

(defun wamei/opencode-cli-run (model input callback &optional system-prompt)
  "MODEL に INPUT を stdin で渡して非同期に実行し、成功したら CALLBACK を出力で呼ぶ。
SYSTEM-PROMPT があれば INPUT の先頭に置く (opencode に相当する引数が無いため)。
失敗時は CALLBACK を呼ばず stderr を `message' で知らせる。プロセスを返す。"
  (wamei/llm-cli-run-process
   (wamei/opencode-cli--command model)
   (if system-prompt (concat system-prompt "\n\n" input) input)
   callback
   nil
   #'wamei/opencode-cli--clean
   (format "opencode (%s)" (or model "default"))))

;;; 汎用コマンド

(defun wamei/opencode (&optional insert)
  "opencode に prompt を投げる。C-u 付きで結果をポイント位置に挿入する。
詳細は `wamei/llm-cli-prompt' を参照。"
  (interactive "P")
  (wamei/llm-cli-prompt "opencode" insert))

(provide 'opencode-cli)
;;; opencode-cli.el ends here
