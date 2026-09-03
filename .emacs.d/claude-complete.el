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

;;; プロンプト

(defvar wamei/claude-complete-system-prompt
  "You are a code completion engine inside a text editor. You receive one file \
with the cursor position marked as <CURSOR>, and optionally an <identifiers> list \
of names that are valid at the cursor according to the language server. Reply with \
exactly the code that should be inserted at <CURSOR> and nothing else: no code \
fences, no explanation, no commentary, and do not repeat code that already appears \
before or after the cursor. Continue the code in the same style and indentation. \
Prefer names from <identifiers> over inventing new ones. Keep the completion short: \
finish the current statement or block, typically one to five lines."
  "補完で Claude Code 既定のシステムプロンプトを置き換える文。")

(defun wamei/claude-complete--prompt (context identifiers)
  "CONTEXT (`wamei/claude-complete--context' の plist) と IDENTIFIERS から stdin 本文を作る。
IDENTIFIERS が nil なら <identifiers> ブロックを出さない。"
  (concat (format "<file path=\"%s\" language=\"%s\">\n"
                  (plist-get context :path) (plist-get context :language))
          (plist-get context :prefix) "<CURSOR>" (plist-get context :suffix)
          "\n</file>\n"
          (when identifiers
            (concat "<identifiers>\n" (string-join identifiers ", ") "\n</identifiers>\n"))))

;;; 出力整形

(defun wamei/claude-complete--strip-fences (text)
  "TEXT の先頭行と末尾行がコードフェンスなら両方を落とす。"
  (let ((lines (split-string (string-trim-right text) "\n")))
    (if (and (>= (length lines) 2)
             (string-prefix-p "```" (string-trim (car lines)))
             (string-prefix-p "```" (string-trim (car (last lines)))))
        (string-join (butlast (cdr lines)) "\n")
      text)))

(defun wamei/claude-complete--strip-line-head (text prefix)
  "PREFIX の最終行 (点のある行の点より手前) を TEXT が繰り返していれば落とす。
行頭が空白だけでも、その空白を TEXT が繰り返していれば落とす (インデントの二重化を防ぐ)。
空白付きで一致しなければ、空白を除いた行頭でも試す。"
  (let* ((head (car (last (split-string prefix "\n"))))
         (bare (string-trim-left head)))
    (cond
     ((string-empty-p head) text)
     ((string-prefix-p head text) (substring text (length head)))
     ((and (not (string-empty-p bare)) (string-prefix-p bare text))
      (substring text (length bare)))
     (t text))))

(defun wamei/claude-complete--strip-suffix-overlap (text suffix)
  "SUFFIX の先頭行 (先行する改行を含む) と TEXT の末尾が重なっていれば、重なりを落とす。"
  (let* ((head (if (string-match "\\`\n*[^\n]*" suffix) (match-string 0 suffix) ""))
         (max (min (length text) (length head))))
    (cl-loop for k from max downto 1
             when (string= (substring text (- (length text) k)) (substring head 0 k))
             return (substring text 0 (- (length text) k))
             finally return text)))

(defun wamei/claude-complete--clean (text context)
  "モデルの出力 TEXT を挿入可能な形に整える。空になれば nil。
CONTEXT は `wamei/claude-complete--context' の plist。"
  (let* ((text (wamei/claude-complete--strip-fences text))
         (text (wamei/claude-complete--strip-line-head text (plist-get context :prefix)))
         (text (wamei/claude-complete--strip-suffix-overlap text (plist-get context :suffix)))
         (text (string-trim-right text)))
    (unless (string-empty-p text) text)))

(provide 'claude-complete)
;;; claude-complete.el ends here
