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

(defvar wamei/claude-complete-max-identifiers 50
  "eglot の補完候補からプロンプトに載せる識別子名の上限。")

(defvar wamei/claude-complete-eglot-timeout 0.3
  "eglot に補完候補を求めるときのタイムアウト秒。超えたら識別子なしで進む。")

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

;;; 状態

(defvar-local wamei/claude-complete--overlay nil
  "表示中のゴーストテキスト overlay。")

(defvar-local wamei/claude-complete--process nil
  "走行中の claude プロセス。")

(defvar-local wamei/claude-complete--timer nil
  "自動要求の idle timer。")

(defvar-local wamei/claude-complete--request nil
  "要求時点の (tick . point)。応答が返ったとき同じでなければ捨てる。")

(defun wamei/claude-complete--stamp ()
  "現在の (buffer-chars-modified-tick . point)。"
  (cons (buffer-chars-modified-tick) (point)))

(defun wamei/claude-complete--cancel-timer ()
  "idle timer を止める。"
  (when wamei/claude-complete--timer
    (cancel-timer wamei/claude-complete--timer)
    (setq wamei/claude-complete--timer nil)))

(defun wamei/claude-complete--cancel-process ()
  "走行中の claude プロセスを静かに止める。"
  (when-let* ((process wamei/claude-complete--process))
    (when (process-live-p process)
      (process-put process 'wamei/claude-cli-cancelled t)
      (delete-process process))
    (setq wamei/claude-complete--process nil)))

;;; 表示

(defun wamei/claude-complete--visible-p ()
  "ゴーストテキストが表示中なら non-nil。"
  (and wamei/claude-complete--overlay
       (overlay-buffer wamei/claude-complete--overlay)))

(defun wamei/claude-complete--delete-overlay ()
  "ゴーストテキストを消す。"
  (when wamei/claude-complete--overlay
    (delete-overlay wamei/claude-complete--overlay)
    (setq wamei/claude-complete--overlay nil)))

(defun wamei/claude-complete--show (text)
  "TEXT を点の直後にゴーストテキストとして表示する。"
  (wamei/claude-complete--delete-overlay)
  (let ((overlay (make-overlay (point) (point) nil t t)))
    (overlay-put overlay 'after-string (propertize text 'face 'shadow))
    (overlay-put overlay 'wamei/claude-complete-text text)
    (setq wamei/claude-complete--overlay overlay)))

(defun wamei/claude-complete-accept ()
  "表示中のゴーストテキストを挿入する。"
  (interactive)
  (when (wamei/claude-complete--visible-p)
    (let ((text (overlay-get wamei/claude-complete--overlay 'wamei/claude-complete-text)))
      (wamei/claude-complete--delete-overlay)
      (insert text))))

(defun wamei/claude-complete-dismiss ()
  "ゴーストテキストと進行中の要求を捨てる。"
  (interactive)
  (wamei/claude-complete--teardown))

(defun wamei/claude-complete--pre-command ()
  "accept 以外のコマンドが走る前にゴーストテキストを消す。"
  (unless (eq this-command 'wamei/claude-complete-accept)
    (wamei/claude-complete--delete-overlay)))

;;; eglot 文脈

(declare-function eglot-managed-p "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function jsonrpc-async-request "jsonrpc")

(defun wamei/claude-complete--completion-labels (result)
  "LSP の textDocument/completion の RESULT から :label を集める。重複を除き上限で切る。"
  (let* ((items (if (and (listp result) (plist-member result :items))
                    (plist-get result :items)
                  result))
         (items (if (vectorp items) items (vconcat items)))
         (labels (cl-loop for item across items
                          for label = (plist-get item :label)
                          when (stringp label) collect label)))
    (seq-take (delete-dups labels) wamei/claude-complete-max-identifiers)))

(defun wamei/claude-complete--eglot-server ()
  "eglot が補完に応えられる状態なら server、そうでなければ nil。"
  (and (fboundp 'eglot-managed-p)
       (eglot-managed-p)
       (eglot-server-capable :completionProvider)
       (eglot-current-server)))

(defun wamei/claude-complete--eglot-identifiers (callback)
  "eglot に点の位置の補完候補を求め、識別子名のリストで CALLBACK を 1 回呼ぶ。
eglot が無い・非対応・エラー・タイムアウトのときは nil で呼ぶ。"
  (let ((server (wamei/claude-complete--eglot-server))
        (done nil))
    (cl-flet ((finish (identifiers)
                (unless done
                  (setq done t)
                  (funcall callback identifiers))))
      (if (null server)
          (finish nil)
        (condition-case nil
            (jsonrpc-async-request
             server :textDocument/completion (eglot--TextDocumentPositionParams)
             :success-fn (lambda (result)
                           (finish (wamei/claude-complete--completion-labels result)))
             :error-fn (lambda (&rest _) (finish nil))
             :timeout-fn (lambda (&rest _) (finish nil))
             :timeout wamei/claude-complete-eglot-timeout)
          (error (finish nil)))))))

;;; 要求

(defun wamei/claude-complete--allowed-p ()
  "いま補完を要求してよければ non-nil。
corfu のポップアップ表示中 (`completion-in-region-mode')、読み取り専用、minibuffer では要求しない。"
  (not (or completion-in-region-mode
           buffer-read-only
           (minibufferp))))

(defun wamei/claude-complete--start (stamp identifiers)
  "現在バッファの文脈と IDENTIFIERS から claude を起動する。
応答時に STAMP が `wamei/claude-complete--request' と現在位置の両方に一致すれば表示する。"
  (let ((buffer (current-buffer))
        (context (wamei/claude-complete--context)))
    (setq wamei/claude-complete--process
          (wamei/claude-cli-run
           wamei/claude-complete-model
           (wamei/claude-complete--prompt context identifiers)
           (lambda (output)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq wamei/claude-complete--process nil)
                 (when (and (equal stamp wamei/claude-complete--request)
                            (equal stamp (wamei/claude-complete--stamp)))
                   (when-let* ((text (wamei/claude-complete--clean output context)))
                     (wamei/claude-complete--show text))))))
           wamei/claude-complete-system-prompt))))

(defun wamei/claude-complete-request ()
  "点の位置の補完を要求する。走行中の要求があれば置き換える。"
  (when (wamei/claude-complete--allowed-p)
    (wamei/claude-complete--cancel-process)
    (let ((buffer (current-buffer))
          (stamp (wamei/claude-complete--stamp)))
      (setq wamei/claude-complete--request stamp)
      (wamei/claude-complete--eglot-identifiers
       (lambda (identifiers)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (equal stamp wamei/claude-complete--request)
                        (equal stamp (wamei/claude-complete--stamp)))
               (wamei/claude-complete--start stamp identifiers)))))))))

(defun wamei/claude-complete ()
  "いまの位置の続きを claude に提案させる。"
  (interactive)
  (wamei/claude-complete-request))

;;; idle 自動トリガー

(defun wamei/claude-complete--on-idle (buffer)
  "idle timer から呼ばれる。BUFFER が選択ウィンドウのバッファで、表示中でも走行中でもなければ要求する。"
  (when (and (buffer-live-p buffer)
             (eq buffer (window-buffer (selected-window))))
    (with-current-buffer buffer
      (setq wamei/claude-complete--timer nil)
      (when (and wamei/claude-complete-mode
                 (not (wamei/claude-complete--visible-p))
                 (not (process-live-p wamei/claude-complete--process)))
        (wamei/claude-complete-request)))))

(defun wamei/claude-complete--post-command ()
  "要求時点からバッファか点が動いていれば走行中の要求を捨て、idle timer を張り直す。"
  (when (and wamei/claude-complete--request
             (not (equal wamei/claude-complete--request (wamei/claude-complete--stamp))))
    (wamei/claude-complete--cancel-process)
    (setq wamei/claude-complete--request nil))
  (wamei/claude-complete--cancel-timer)
  (when wamei/claude-complete-auto
    (setq wamei/claude-complete--timer
          (run-with-idle-timer wamei/claude-complete-idle-delay nil
                               #'wamei/claude-complete--on-idle (current-buffer)))))

;;; minor mode

(declare-function wamei/claude-complete "claude-complete")

(defun wamei/claude-complete--tab-filter (command)
  "ゴーストテキスト表示中だけ COMMAND を返す。他は既定の TAB に任せる。"
  (and (wamei/claude-complete--visible-p) command))

(defvar wamei/claude-complete-mode-map
  (let ((map (make-sparse-keymap))
        (accept '(menu-item "" wamei/claude-complete-accept
                            :filter wamei/claude-complete--tab-filter)))
    (define-key map (kbd "C-c C-.") #'wamei/claude-complete)
    (define-key map (kbd "TAB") accept)
    (define-key map (kbd "<tab>") accept)
    map)
  "`wamei/claude-complete-mode' のキーマップ。")

(defun wamei/claude-complete--teardown ()
  "timer・プロセス・overlay をすべて片付ける。"
  (wamei/claude-complete--cancel-timer)
  (wamei/claude-complete--cancel-process)
  (wamei/claude-complete--delete-overlay)
  (setq wamei/claude-complete--request nil))

(define-minor-mode wamei/claude-complete-mode
  "claude によるゴーストテキスト補完。"
  :lighter " Claude"
  :keymap wamei/claude-complete-mode-map
  (if wamei/claude-complete-mode
      (progn
        (add-hook 'pre-command-hook #'wamei/claude-complete--pre-command nil t)
        (add-hook 'post-command-hook #'wamei/claude-complete--post-command nil t)
        (add-hook 'kill-buffer-hook #'wamei/claude-complete--teardown nil t))
    (remove-hook 'pre-command-hook #'wamei/claude-complete--pre-command t)
    (remove-hook 'post-command-hook #'wamei/claude-complete--post-command t)
    (remove-hook 'kill-buffer-hook #'wamei/claude-complete--teardown t)
    (wamei/claude-complete--teardown)))

(provide 'claude-complete)
;;; claude-complete.el ends here
