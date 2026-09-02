;;; claude-cli.el --- claude -p を Emacs から使う -*- lexical-binding: t; -*-
;;; Commentary:
;; `claude -p --model MODEL' を非同期プロセスとして呼び、結果をバッファに出したり
;; コミットメッセージとして挿入したりする。claude-code-ide (対話セッション) とは
;; 独立で、単発の生成にだけ使う。
;;
;; - `wamei/claude-haiku' / `wamei/claude-sonnet' / `wamei/claude-opus'
;;   リージョンや minibuffer の prompt を投げて *claude* バッファに表示する。
;;   C-u 付きならポイント位置に挿入する。
;; - `wamei/claude-commit-message'
;;   git-commit バッファで、staged diff と直近ログからメッセージを生成して挿入する。
;;; Code:

(require 'cl-lib)
(require 'subr-x)

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

(defvar wamei/claude-cli-result-buffer "*claude*"
  "汎用コマンドの結果を表示するバッファ名。")

(defvar wamei/claude-commit-message-model "haiku"
  "`wamei/claude-commit-message' が既定で使うモデル。")

(defvar wamei/claude-commit-message-log-count 10
  "スタイルの手本として prompt に含める直近コミットの件数。")

(defvar wamei/claude-commit-message-max-diff-chars 60000
  "prompt に含める staged diff の最大文字数。超えた分は切り捨てて注記する。
lock ファイル等の巨大な diff でコンテキストを溢れさせないための上限。")

;;; プロセス

(defun wamei/claude-cli--command (model)
  "MODEL で単発生成する claude のコマンドライン。prompt は stdin で渡す。
ツールは全て無効にし、セッションも残さない。
MCP サーバーと user/project 設定 (プラグイン・hook) も読まない。これらの起動処理が
haiku の応答そのものより長く (計測で 6-7 秒中の 4 秒程度) かかるため。
--bare はさらに速いが keychain を読まず認証に失敗するので使わない。"
  (list wamei/claude-cli-program "-p" "--model" model
        "--output-format" "text"
        "--tools" ""
        "--no-session-persistence"
        "--strict-mcp-config"
        "--setting-sources" ""))

(defun wamei/claude-cli--environment ()
  "claude プロセスに渡す環境。`wamei/claude-cli-thinking-tokens' を反映する。"
  (if wamei/claude-cli-thinking-tokens
      (cons (format "MAX_THINKING_TOKENS=%d" wamei/claude-cli-thinking-tokens)
            process-environment)
    process-environment))

(defun wamei/claude-cli-run (model input callback)
  "MODEL に INPUT を stdin で渡して非同期に実行し、成功したら CALLBACK を出力で呼ぶ。
失敗時は CALLBACK を呼ばず stderr を `message' で知らせる。プロセスを返す。"
  (let* ((process-environment (wamei/claude-cli--environment))
         (stdout (generate-new-buffer " *claude-cli*" t))
         (stderr (generate-new-buffer " *claude-cli-stderr*" t))
         ;; :stderr にバッファを直接渡すと既定の sentinel が終了メッセージを
         ;; 書き込むので、sentinel を持たない pipe プロセスを介す。
         (stderr-process (make-pipe-process :name "claude-cli-stderr"
                                            :buffer stderr
                                            :sentinel #'ignore
                                            :noquery t))
         (process
          (make-process
           :name "claude-cli"
           :buffer stdout
           :stderr stderr-process
           :command (wamei/claude-cli--command model)
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (proc _event)
             (unless (process-live-p proc)
               (when (process-live-p stderr-process)
                 (accept-process-output stderr-process 0.2))
               (let ((status (process-exit-status proc))
                     (output (with-current-buffer stdout (buffer-string)))
                     (errors (with-current-buffer stderr
                               (string-trim (buffer-string)))))
                 (delete-process stderr-process)
                 (kill-buffer stdout)
                 (kill-buffer stderr)
                 (if (and (eq (process-status proc) 'exit) (zerop status))
                     (funcall callback (string-trim-right output "\n+"))
                   (message "claude (%s) failed: %s" model
                            (if (string-empty-p errors)
                                (format "exit status %d" status)
                              errors)))))))))
    (process-send-string process input)
    (process-send-eof process)
    process))

;;; 汎用コマンド

(defun wamei/claude-cli--show-result (model text)
  "TEXT を MODEL の見出し付きで `wamei/claude-cli-result-buffer' の末尾に追記して表示する。"
  (let ((buffer (get-buffer-create wamei/claude-cli-result-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp) (insert "\n"))
        (insert (format "## %s\n\n%s\n" model text))))
    (display-buffer buffer)))

(defun wamei/claude-cli-prompt (model &optional insert)
  "MODEL に prompt を投げる。
リージョンがあればその内容を入力にし、minibuffer で読んだ文字列を指示として前置する
(空なら リージョンをそのまま送る)。リージョンがなければ minibuffer の文字列が prompt。
INSERT が非 nil なら結果をポイント位置に挿入し、nil なら `wamei/claude-cli-result-buffer'
に表示する。"
  (let* ((region (when (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))))
         (instruction (read-string
                       (format "claude (%s)%s: " model (if region " on region" ""))))
         (input (cond ((not region) instruction)
                      ((string-empty-p instruction) region)
                      (t (concat instruction "\n\n" region))))
         (marker (when insert (copy-marker (if region (region-end) (point))))))
    (when (string-empty-p (string-trim input))
      (user-error "Empty prompt"))
    (deactivate-mark)
    (message "claude (%s): thinking..." model)
    (wamei/claude-cli-run
     model input
     (lambda (text)
       (if (and marker (buffer-live-p (marker-buffer marker)))
           (with-current-buffer (marker-buffer marker)
             (save-excursion
               (goto-char marker)
               (insert text)))
         (wamei/claude-cli--show-result model text))
       (message "claude (%s): done" model)))))

(defun wamei/claude-cli-define-commands ()
  "`wamei/claude-cli-models' の各モデルに `wamei/claude-MODEL' コマンドを定義する。"
  (dolist (model wamei/claude-cli-models)
    (defalias (intern (format "wamei/claude-%s" model))
      (lambda (&optional insert)
        (interactive "P")
        (wamei/claude-cli-prompt model insert))
      (format "claude %s に prompt を投げる。C-u 付きで結果をポイント位置に挿入する。
詳細は `wamei/claude-cli-prompt' を参照。" model))))

(wamei/claude-cli-define-commands)

;;; コミットメッセージ

(defun wamei/claude-commit-message--truncate-diff (diff)
  "DIFF が `wamei/claude-commit-message-max-diff-chars' を超えていれば切り詰めて注記する。"
  (if (<= (length diff) wamei/claude-commit-message-max-diff-chars)
      diff
    (concat (substring diff 0 wamei/claude-commit-message-max-diff-chars)
            "\n\n[diff truncated: "
            (number-to-string (- (length diff) wamei/claude-commit-message-max-diff-chars))
            " more characters omitted]")))

(defun wamei/claude-commit-message--prompt (diff log)
  "staged DIFF と直近の LOG からコミットメッセージ生成の prompt を組み立てる。"
  (concat
   "Write a git commit message for the staged changes below.\n"
   "Match the language, tone and format of the recent commit messages in this "
   "repository: if they are Japanese, write Japanese; if they use a prefix "
   "convention, use it too.\n"
   "Output only the commit message itself: one summary line under 72 characters, "
   "then optionally a blank line and a short body. No code fences, no quotes, "
   "no explanation.\n\n"
   "## Recent commit messages\n\n" log "\n\n"
   "## Staged diff\n\n" (wamei/claude-commit-message--truncate-diff diff)))

(defun wamei/claude-commit-message--first-line-empty-p ()
  "現在のバッファの 1 行目が空白だけなら非 nil。2 行目以降は見ない。"
  (save-excursion
    (goto-char (point-min))
    (looking-at "[ \t]*$")))

(defun wamei/claude-commit-message--insert (text comment-char)
  "TEXT をコミットメッセージとして挿入し、先頭へ移動する。
1 行目が空ならその行を TEXT に置き換え、2 行目以降 (手書きのメモや trailer) は残す。
1 行目に既に要約があれば、COMMENT-CHAR のコメント塊より前の既存メッセージ全体を
TEXT に置き換える。どちらも TEXT とその後ろの内容の間に空行を 1 つ確保する。"
  (goto-char (point-min))
  (if (wamei/claude-commit-message--first-line-empty-p)
      ;; 空の 1 行目は改行ごと消し、TEXT の後ろで空行を作り直す。
      (delete-region (point-min) (min (1+ (line-end-position)) (point-max)))
    (delete-region (point-min)
                   (if (re-search-forward (concat "^" (regexp-quote comment-char)) nil t)
                       (line-beginning-position)
                     (point-max))))
  (goto-char (point-min))
  (insert text "\n")
  (unless (or (eobp) (looking-at "[ \t]*$"))
    (insert "\n"))
  (goto-char (point-min)))

(defun wamei/claude-commit-message--git (&rest args)
  "カレントディレクトリのリポジトリで git ARGS を実行し、標準出力を返す。"
  (with-temp-buffer
    (let ((status (apply #'call-process "git" nil '(t nil) nil args)))
      (unless (zerop status)
        (user-error "git %s failed (exit %d)" (string-join args " ") status)))
    (buffer-string)))

(defun wamei/claude-commit-message--comment-char ()
  "現在のバッファのコメント文字。git-commit-mode が設定する `comment-start' を使う。"
  (let ((start (and (boundp 'comment-start) comment-start
                    (string-trim comment-start))))
    (if (and start (not (string-empty-p start))) start "#")))

(defun wamei/claude-commit-message (&optional model)
  "staged な変更からコミットメッセージを生成してバッファ先頭に挿入する。
MODEL は省略時 `wamei/claude-commit-message-model'。C-u 付きで対話的に選ぶ。
1 行目が空ならそのまま挿入し、既に要約が書かれていれば置き換えるか確認する。"
  (interactive
   (list (when current-prefix-arg
           (completing-read "Model: " wamei/claude-cli-models nil nil nil nil
                            wamei/claude-commit-message-model))))
  (let* ((model (or model wamei/claude-commit-message-model))
         (comment-char (wamei/claude-commit-message--comment-char))
         (buffer (current-buffer)))
    (when (or (wamei/claude-commit-message--first-line-empty-p)
              (y-or-n-p "Replace the current commit message? "))
      (let ((diff (wamei/claude-commit-message--git "diff" "--cached" "--no-color"))
            (log (wamei/claude-commit-message--git
                  "log" "-n" (number-to-string wamei/claude-commit-message-log-count)
                  "--format=%B---")))
        (when (string-empty-p (string-trim diff))
          (user-error "No staged changes"))
        (message "claude (%s): generating commit message..." model)
        (wamei/claude-cli-run
         model (wamei/claude-commit-message--prompt diff log)
         (lambda (text)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (wamei/claude-commit-message--insert text comment-char))
             (message "claude (%s): commit message inserted" model))))))))

(provide 'claude-cli)
;;; claude-cli.el ends here
