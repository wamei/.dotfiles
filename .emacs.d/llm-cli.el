;;; llm-cli.el --- CLI の LLM を Emacs から使う共通部分 -*- lexical-binding: t; -*-
;;; Commentary:
;; claude / opencode のような「プロンプトを stdin で渡して答えを stdout で受け取る」
;; CLI を非同期で呼ぶための土台。backend ごとの差 (コマンドライン・環境変数・出力の
;; 整形) は claude-cli.el / opencode-cli.el が持ち、ここには共通部分だけを置く。
;;
;; - `wamei/llm-cli-run-process'
;;   コマンドを非同期に起動し、出力を callback へ渡す。全 backend の土台。
;; - `wamei/llm-cli-models' / `wamei/llm-cli--resolve'
;;   表示名 (haiku / opencode など) から backend 関数とモデル名を引く。
;; - `wamei/llm-cli-prompt'
;;   リージョンや minibuffer の prompt を投げて結果をバッファに出す汎用コマンドの中身。
;; - `wamei/llm-commit-message'
;;   git-commit バッファで、staged diff と直近ログからメッセージを生成して挿入する。
;;   C-u 付きで backend を選べる。
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function wamei/claude-cli-run "claude-cli")
(declare-function wamei/opencode-cli-run "opencode-cli")

;;; 設定

(defvar wamei/llm-cli-models
  '(("haiku"    wamei/claude-cli-run   "haiku")
    ("sonnet"   wamei/claude-cli-run   "sonnet")
    ("opus"     wamei/claude-cli-run   "opus")
    ("opencode" wamei/opencode-cli-run nil))
  "選べるモデルの一覧。要素は (NAME RUNNER MODEL)。
NAME は completing-read に出す表示名。RUNNER は (MODEL INPUT CALLBACK
&optional SYSTEM-PROMPT) を取る backend 関数。MODEL は RUNNER に渡すモデル名で、
nil なら backend 側の既定 (opencode なら opencode.jsonc の model) に任せる。")

(defvar wamei/llm-cli-default-runner #'wamei/claude-cli-run
  "`wamei/llm-cli-models' に無い名前を指定されたときに使う backend 関数。
名前はモデル名としてそのまま渡す。claude の新しいモデル別名を一覧に足さずに
その場で使えるようにするための逃げ道。")

(defvar wamei/llm-cli-result-buffer "*llm*"
  "汎用コマンドの結果を表示するバッファ名。backend をまたいで 1 つに集める。")

;;; プロセス

(defun wamei/llm-cli-run-process (command input callback
                                          &optional environment filter label)
  "COMMAND を非同期に起動し、INPUT を stdin で渡し、成功したら CALLBACK を出力で呼ぶ。
ENVIRONMENT があれば `process-environment' の代わりに使う。
FILTER があれば出力に適用してから CALLBACK に渡す。
LABEL は失敗を知らせる `message' に出す名前 (既定はプログラム名)。
失敗時は CALLBACK を呼ばず stderr を `message' で知らせる。プロセスを返す。
プロセスに `wamei/llm-cli-cancelled' プロパティが付いていれば失敗を知らせない。"
  (let* ((process-environment (or environment process-environment))
         (label (or label (file-name-nondirectory (car command))))
         (stdout (generate-new-buffer " *llm-cli*" t))
         (stderr (generate-new-buffer " *llm-cli-stderr*" t))
         ;; :stderr にバッファを直接渡すと既定の sentinel が終了メッセージを
         ;; 書き込むので、sentinel を持たない pipe プロセスを介す。
         (stderr-process (make-pipe-process :name "llm-cli-stderr"
                                            :buffer stderr
                                            :sentinel #'ignore
                                            :noquery t))
         (process
          (make-process
           :name "llm-cli"
           :buffer stdout
           :stderr stderr-process
           :command command
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
                 (cond
                  ((and (eq (process-status proc) 'exit) (zerop status))
                   (funcall callback
                            (string-trim-right (if filter (funcall filter output) output)
                                               "\n+")))
                  ;; 呼び出し側が意図的に止めたプロセス。失敗として知らせない。
                  ((process-get proc 'wamei/llm-cli-cancelled) nil)
                  (t
                   (message "%s failed: %s" label
                            (if (string-empty-p errors)
                                (format "exit status %d" status)
                              errors))))))))))
    (process-send-string process input)
    (process-send-eof process)
    process))

;;; モデルの解決

(defun wamei/llm-cli--resolve (name)
  "表示名 NAME から (RUNNER . MODEL) を返す。
`wamei/llm-cli-models' に無い NAME は `wamei/llm-cli-default-runner' に
モデル名としてそのまま渡す。"
  (if-let* ((entry (assoc name wamei/llm-cli-models)))
      (cons (nth 1 entry) (nth 2 entry))
    (cons wamei/llm-cli-default-runner name)))

(defun wamei/llm-cli-run (name input callback &optional system-prompt)
  "表示名 NAME の backend に INPUT を投げ、成功したら CALLBACK を出力で呼ぶ。
SYSTEM-PROMPT の扱いは backend による。プロセスを返す。"
  (let ((runner (wamei/llm-cli--resolve name)))
    (funcall (car runner) (cdr runner) input callback system-prompt)))

(defun wamei/llm-cli-read-model (&optional default)
  "モデルを minibuffer で読む。DEFAULT は初期入力に使う。
一覧に無い名前も受け付ける (`wamei/llm-cli--resolve' が既定 backend に渡す)。"
  (completing-read "Model: " (mapcar #'car wamei/llm-cli-models)
                   nil nil nil nil default))

;;; 汎用コマンド

(defun wamei/llm-cli--show-result (name text)
  "TEXT を NAME の見出し付きで `wamei/llm-cli-result-buffer' の末尾に追記して表示する。"
  (let ((buffer (get-buffer-create wamei/llm-cli-result-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp) (insert "\n"))
        (insert (format "## %s\n\n%s\n" name text))))
    (display-buffer buffer)))

(defun wamei/llm-cli-prompt (name &optional insert)
  "表示名 NAME のモデルに prompt を投げる。
リージョンがあればその内容を入力にし、minibuffer で読んだ文字列を指示として前置する
\(空なら リージョンをそのまま送る)。リージョンがなければ minibuffer の文字列が prompt。
INSERT が非 nil なら結果をポイント位置に挿入し、nil なら `wamei/llm-cli-result-buffer'
に表示する。"
  (let* ((region (when (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))))
         (instruction (read-string
                       (format "%s%s: " name (if region " on region" ""))))
         (input (cond ((not region) instruction)
                      ((string-empty-p instruction) region)
                      (t (concat instruction "\n\n" region))))
         (marker (when insert (copy-marker (if region (region-end) (point))))))
    (when (string-empty-p (string-trim input))
      (user-error "Empty prompt"))
    (deactivate-mark)
    (message "%s: thinking..." name)
    (wamei/llm-cli-run
     name input
     (lambda (text)
       (if (and marker (buffer-live-p (marker-buffer marker)))
           (with-current-buffer (marker-buffer marker)
             (save-excursion
               (goto-char marker)
               (insert text)))
         (wamei/llm-cli--show-result name text))
       (message "%s: done" name)))))

;;; コミットメッセージ

(defvar wamei/llm-commit-message-model "haiku"
  "`wamei/llm-commit-message' が既定で使うモデル。`wamei/llm-cli-models' の表示名。")

(defvar wamei/llm-commit-message-system-prompt
  "You are a commit message generator. You receive recent commit messages and a \
staged diff, and you reply with a single git commit message wrapped in <commit> and \
</commit> tags, and nothing else: no greeting, no explanation, no code fences, \
no analysis of what you are about to do. Match the language, tone and format of \
the recent commit messages."
  "コミットメッセージ生成で backend 既定のシステムプロンプトを置き換える文。")

(defvar wamei/llm-commit-message-log-count 10
  "スタイルの手本として prompt に含める直近コミットの件数。")

(defvar wamei/llm-commit-message-max-diff-chars 60000
  "prompt に含める staged diff の最大文字数。超えた分は切り捨てて注記する。
lock ファイル等の巨大な diff でコンテキストを溢れさせないための上限。")

(defun wamei/llm-commit-message--truncate-diff (diff)
  "DIFF が `wamei/llm-commit-message-max-diff-chars' を超えていれば切り詰めて注記する。"
  (if (<= (length diff) wamei/llm-commit-message-max-diff-chars)
      diff
    (concat (substring diff 0 wamei/llm-commit-message-max-diff-chars)
            "\n\n[diff truncated: "
            (number-to-string (- (length diff) wamei/llm-commit-message-max-diff-chars))
            " more characters omitted]")))

(defun wamei/llm-commit-message--prompt (diff log)
  "staged DIFF と直近の LOG からコミットメッセージ生成の prompt を組み立てる。"
  (concat
   "Write a git commit message for the staged changes below.\n"
   "Match the language, tone and format of the recent commit messages in this "
   "repository: if they are Japanese, write Japanese; if they use a prefix "
   "convention, use it too.\n"
   "Write one summary line under 72 characters, then optionally a blank line and "
   "a short body. Wrap the whole message in <commit> and </commit> tags and output "
   "nothing outside the tags: no code fences, no quotes, no explanation.\n\n"
   "## Recent commit messages\n\n" log "\n\n"
   "## Staged diff\n\n" (wamei/llm-commit-message--truncate-diff diff)))

(defun wamei/llm-commit-message--extract (output)
  "OUTPUT からコミットメッセージ本体を取り出す。
<commit>...</commit> があればその中身、なければ全体。前置きの説明が混ざっても
タグの外は捨てる。残ったコードフェンスと前後の空白は取り除く。"
  (let* ((body (if (string-match "<commit>\\(\\(?:.\\|\n\\)*?\\)</commit>" output)
                   (match-string 1 output)
                 output))
         (body (replace-regexp-in-string "\\`[ \t\n]*```[^\n]*\n\\|\n```[ \t\n]*\\'" "" body)))
    (string-trim body)))

(defun wamei/llm-commit-message--first-line-empty-p ()
  "現在のバッファの 1 行目が空白だけなら非 nil。2 行目以降は見ない。"
  (save-excursion
    (goto-char (point-min))
    (looking-at "[ \t]*$")))

(defun wamei/llm-commit-message--insert (text comment-char)
  "TEXT をコミットメッセージとして挿入し、先頭へ移動する。
1 行目が空ならその行を TEXT に置き換え、2 行目以降 (手書きのメモや trailer) は残す。
1 行目に既に要約があれば、COMMENT-CHAR のコメント塊より前の既存メッセージ全体を
TEXT に置き換える。どちらも TEXT とその後ろの内容の間に空行を 1 つ確保する。"
  (goto-char (point-min))
  (if (wamei/llm-commit-message--first-line-empty-p)
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

(defun wamei/llm-commit-message--git (&rest args)
  "カレントディレクトリのリポジトリで git ARGS を実行し、標準出力を返す。"
  (with-temp-buffer
    (let ((status (apply #'call-process "git" nil '(t nil) nil args)))
      (unless (zerop status)
        (user-error "git %s failed (exit %d)" (string-join args " ") status)))
    (buffer-string)))

(defun wamei/llm-commit-message--comment-char ()
  "現在のバッファのコメント文字。git-commit-mode が設定する `comment-start' を使う。"
  (let ((start (and (boundp 'comment-start) comment-start
                    (string-trim comment-start))))
    (if (and start (not (string-empty-p start))) start "#")))

(defun wamei/llm-commit-message (&optional model)
  "staged な変更からコミットメッセージを生成してバッファ先頭に挿入する。
MODEL は `wamei/llm-cli-models' の表示名。省略時は `wamei/llm-commit-message-model'。
C-u 付きで対話的に選ぶ。
1 行目が空ならそのまま挿入し、既に要約が書かれていれば置き換えるか確認する。"
  (interactive
   (list (when current-prefix-arg
           (wamei/llm-cli-read-model wamei/llm-commit-message-model))))
  (let* ((model (or model wamei/llm-commit-message-model))
         (comment-char (wamei/llm-commit-message--comment-char))
         (buffer (current-buffer)))
    (when (or (wamei/llm-commit-message--first-line-empty-p)
              (y-or-n-p "Replace the current commit message? "))
      (let ((diff (wamei/llm-commit-message--git "diff" "--cached" "--no-color"))
            (log (wamei/llm-commit-message--git
                  "log" "-n" (number-to-string wamei/llm-commit-message-log-count)
                  "--format=%B---")))
        (when (string-empty-p (string-trim diff))
          (user-error "No staged changes"))
        (message "%s: generating commit message..." model)
        (wamei/llm-cli-run
         model (wamei/llm-commit-message--prompt diff log)
         (lambda (output)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (wamei/llm-commit-message--insert
                (wamei/llm-commit-message--extract output) comment-char))
             (message "%s: commit message inserted" model)))
         wamei/llm-commit-message-system-prompt)))))

(provide 'llm-cli)
;;; llm-cli.el ends here
