;;; claude-complete.el --- claude -p によるゴーストテキスト補完 -*- lexical-binding: t; -*-
;;; Commentary:
;; 入力が止まったとき、カーソル位置の続きを `claude -p' (haiku) に生成させ、
;; 薄い色の overlay (ゴーストテキスト) で見せる。TAB で受け入れ、他のコマンドで消える。
;; eglot 管理下のバッファでは LSP の補完候補 (その位置で有効な識別子名) をプロンプトに
;; 混ぜ、識別子の捏造を抑える。
;;
;; - `wamei/claude-complete-mode'   バッファ単位の minor mode。prog-mode で有効化する想定
;; - `wamei/claude-complete'        手動トリガー (C-c C-.)
;; - `wamei/claude-complete-accept' 表示中の提案を挿入 (TAB)
;; - `wamei/claude-complete-dismiss' 提案と進行中の要求を捨てる
;;
;; プロセス実行は claude-cli.el の `wamei/claude-cli-run' に委ねる。
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'claude-cli
         (expand-file-name "claude-cli"
                           (file-name-directory (or load-file-name buffer-file-name))))

;;; 設定

(defvar wamei/claude-complete-model "haiku"
  "補完に使うモデル別名。")

(defvar wamei/claude-complete-idle-delay 1.0
  "入力が止まってから自動で補完を要求するまでの秒数。
要求 1 回ごとに Claude Code の利用枠を 1 リクエスト消費する。")

(defvar wamei/claude-complete-auto t
  "non-nil なら idle 時に自動で補完を要求する。nil なら手動トリガーのみ。")

(defvar wamei/claude-complete-prefix-chars 3000
  "プロンプトに含める点より手前の文字数。")

(defvar wamei/claude-complete-suffix-chars 1000
  "プロンプトに含める点より後ろの文字数。")

;;; 文脈抽出

(defun wamei/claude-complete--language ()
  "`major-mode' から言語名を導く。typescript-ts-mode → typescript。"
  (string-remove-suffix
   "-mode" (string-remove-suffix "-ts-mode" (symbol-name major-mode))))

(defun wamei/claude-complete--path ()
  "プロンプトに載せるファイルパス。プロジェクト相対、無ければファイル名、無ければバッファ名。"
  (cond
   ((null buffer-file-name) (buffer-name))
   ((project-current)
    (file-relative-name buffer-file-name (project-root (project-current))))
   (t (file-name-nondirectory buffer-file-name))))

(defun wamei/claude-complete--context ()
  "点の前後のテキストとファイル情報を plist で返す。"
  (list :prefix (buffer-substring-no-properties
                 (max (point-min) (- (point) wamei/claude-complete-prefix-chars))
                 (point))
        :suffix (buffer-substring-no-properties
                 (point)
                 (min (point-max) (+ (point) wamei/claude-complete-suffix-chars)))
        :path (wamei/claude-complete--path)
        :language (wamei/claude-complete--language)))

(provide 'claude-complete)
;;; claude-complete.el ends here
