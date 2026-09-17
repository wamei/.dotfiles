;;; claude-cli.el --- claude -p を Emacs から使う -*- lexical-binding: t; -*-
;;; Commentary:
;; `claude -p --model MODEL' を非同期プロセスとして呼ぶ backend。
;; claude-code-ide (対話セッション) とは独立で、単発の生成にだけ使う。
;; プロセスの配管とコミットメッセージ生成は llm-cli.el が持つ。
;;
;; - `wamei/claude-cli-run'  llm-cli.el の backend。claude-complete.el も使う。
;; - `wamei/claude-haiku' / `wamei/claude-sonnet' / `wamei/claude-opus'
;;   リージョンや minibuffer の prompt を投げて結果をバッファに表示する。
;;   C-u 付きならポイント位置に挿入する。
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

(defvar wamei/claude-cli-program "claude"
  "claude CLI の実行ファイル。")

(defvar wamei/claude-cli-models '("haiku" "sonnet" "opus")
  "コマンドを用意するモデル別名。要素ごとに `wamei/claude-MODEL' が定義される。")

(defvar wamei/claude-cli-thinking-tokens 0
  "claude の拡張思考に許すトークン数。MAX_THINKING_TOKENS として渡す。
0 で思考を無効化する。既定では haiku でも 1000 トークン前後考えてから答え、
コミットメッセージ 1 件に 13-15 秒かかっていた。無効化すると 2 秒程度になる。
--effort は haiku には効かなかったので使わない。nil なら環境を触らない。")

;;; プロセス

(defun wamei/claude-cli--command (model &optional system-prompt)
  "MODEL で単発生成する claude のコマンドライン。prompt は stdin で渡す。
SYSTEM-PROMPT があれば Claude Code 既定のシステムプロンプトをそれで置き換える。
既定のものはコーディングエージェント向けで、「I'll check the staged changes...」と
作業を実況したり bash を書き出したりするため、単発生成には向かない。
ツールは全て無効にし、セッションも残さない。
MCP サーバーと user/project 設定 (プラグイン・hook) も読まない。これらの起動処理が
haiku の応答そのものより長く (計測で 6-7 秒中の 4 秒程度) かかるため。
--bare はさらに速いが keychain を読まず認証に失敗するので使わない。"
  (append
   (list wamei/claude-cli-program "-p" "--model" model
         "--output-format" "text"
         "--tools" ""
         "--no-session-persistence"
         "--strict-mcp-config"
         "--setting-sources" "")
   (when system-prompt (list "--system-prompt" system-prompt))))

(defun wamei/claude-cli--environment ()
  "claude プロセスに渡す環境。`wamei/claude-cli-thinking-tokens' を反映する。"
  (if wamei/claude-cli-thinking-tokens
      (cons (format "MAX_THINKING_TOKENS=%d" wamei/claude-cli-thinking-tokens)
            process-environment)
    process-environment))

(defun wamei/claude-cli-run (model input callback &optional system-prompt)
  "MODEL に INPUT を stdin で渡して非同期に実行し、成功したら CALLBACK を出力で呼ぶ。
SYSTEM-PROMPT は `wamei/claude-cli--command' に渡す。
失敗時は CALLBACK を呼ばず stderr を `message' で知らせる。プロセスを返す。
プロセスに `wamei/llm-cli-cancelled' プロパティが付いていれば失敗を知らせない。"
  (wamei/llm-cli-run-process
   (wamei/claude-cli--command model system-prompt)
   input
   callback
   (wamei/claude-cli--environment)
   nil
   (format "claude (%s)" model)))

;;; 汎用コマンド

(defun wamei/claude-cli-define-commands ()
  "`wamei/claude-cli-models' の各モデルに `wamei/claude-MODEL' コマンドを定義する。"
  (dolist (model wamei/claude-cli-models)
    (defalias (intern (format "wamei/claude-%s" model))
      (lambda (&optional insert)
        (interactive "P")
        (wamei/llm-cli-prompt model insert))
      (format "claude %s に prompt を投げる。C-u 付きで結果をポイント位置に挿入する。
詳細は `wamei/llm-cli-prompt' を参照。" model))))

(wamei/claude-cli-define-commands)

(provide 'claude-cli)
;;; claude-cli.el ends here
