# claude-complete 実装計画

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Emacs のコードバッファで `claude -p`（haiku）にカーソル位置の続きを生成させ、ゴーストテキストとして表示し TAB で受け入れられる minor mode を作る。

**Architecture:** 新規 `claude-complete.el` に buffer-local minor mode `wamei/claude-complete-mode` を置く。プロセス実行は既存 `wamei/claude-cli-run`（claude-cli.el）を再利用し、claude-cli.el には「キャンセル済みプロセスの失敗を message しない」変更だけ入れる。純粋関数（文脈抽出・プロンプト生成・出力整形）、buffer-local 状態（overlay / process / timer / request stamp）、副作用層（overlay 表示・eglot 問い合わせ・timer）を分ける。

**Tech Stack:** Emacs 31 Lisp、ERT（batch）、claude CLI 2.1、eglot / jsonrpc（Emacs 同梱）、leaf（init.el）

**Spec:** `docs/superpowers/specs/2026-09-03-claude-complete-design.md`

## Global Constraints

- 名前空間は既存に合わせ `wamei/claude-complete-…`、内部関数・変数は `wamei/claude-complete--…`
- ファイルは `.emacs.d/` 直下、ヘッダに `-*- lexical-binding: t; -*-`、コメントとテスト名の説明は日本語
- テストは `emacs -Q --batch -l .emacs.d/claude-complete-test.el -f ert-run-tests-batch-and-exit` で走ること（claude-cli-test.el と同形式）
- claude の起動フラグは `wamei/claude-cli--command` のものを変えない（ツール無効・セッション非永続・設定非読込）
- コミットメッセージは日本語（このリポジトリの既存コミットに合わせる）。コミット末尾に `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>` を付ける
- init.el の leaf はブロック内エラーを握りつぶすので、init.el 変更後は `Warning (leaf)` / `Error (leaf)` を grep で確認する（Task 9 に手順あり）

## ファイル構成

| ファイル | 責務 |
|---|---|
| `.emacs.d/claude-cli.el`（変更） | `wamei/claude-cli-run` の sentinel が `wamei/claude-cli-cancelled` プロパティ付きプロセスの失敗を message しない |
| `.emacs.d/claude-cli-test.el`（変更） | 上記のテスト |
| `.emacs.d/claude-complete.el`（新規） | 変数、文脈抽出、プロンプト生成、出力整形、overlay 表示、request/start、idle timer、eglot 問い合わせ、minor mode |
| `.emacs.d/claude-complete-test.el`（新規） | 上記の ERT |
| `.emacs.d/init.el`（変更） | `leaf claude-complete` を `leaf claude-cli` の直後に追加、`prog-mode-hook` で有効化 |

テストは 1 ファイルに集約する。Task 2 で骨格（ヘッダ、load、共通フィクスチャ）を作り、以降の Task は既存テストファイルの末尾（`;;; claude-complete-test.el ends here` の直前）に追記する。実装も同様に `(provide 'claude-complete)` の直前に追記する。

---

### Task 1: claude-cli.el — キャンセル済みプロセスの失敗を無音にする

**Files:**
- Modify: `.emacs.d/claude-cli.el:100-117`（sentinel）
- Test: `.emacs.d/claude-cli-test.el`

**Interfaces:**
- Produces: プロセスに `(process-put proc 'wamei/claude-cli-cancelled t)` を付けてから `delete-process` すると、sentinel は `message` を出さない。後続 Task の `wamei/claude-complete--cancel-process` がこれに依存する。

- [ ] **Step 1: 失敗テストを書く**

`.emacs.d/claude-cli-test.el` の `;;; 非同期実行` セクション末尾（`wamei/claude-cli-run-trims-trailing-newline` の後）に追記:

```elisp
(ert-deftest wamei/claude-cli-run-reports-failure-with-message ()
  (wamei/claude-cli-test--with-stub "exit 3"
    (let* ((messages nil)
           (process (cl-letf (((symbol-function 'message)
                               (lambda (fmt &rest args)
                                 (when fmt (push (apply #'format fmt args) messages)))))
                      (let ((process (wamei/claude-cli-run "haiku" "" #'ignore)))
                        (wamei/claude-cli-test--wait process)
                        process))))
      (ignore process)
      (should (cl-some (lambda (m) (string-match-p "failed" m)) messages)))))

(ert-deftest wamei/claude-cli-run-stays-silent-when-cancelled ()
  (wamei/claude-cli-test--with-stub "sleep 5"
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) messages)))))
        (let ((process (wamei/claude-cli-run "haiku" "" #'ignore)))
          (process-put process 'wamei/claude-cli-cancelled t)
          (delete-process process)
          (wamei/claude-cli-test--wait process)))
      (should-not messages))))
```

- [ ] **Step 2: 失敗を確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-cli-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -5`
Expected: `wamei/claude-cli-run-stays-silent-when-cancelled` が FAILED（`messages` に "claude (haiku) failed: ..." が入る）。`reports-failure-with-message` は PASS（現状の挙動確認）。

- [ ] **Step 3: sentinel を変更する**

`.emacs.d/claude-cli.el` の sentinel 内、`(if (and (eq (process-status proc) 'exit) (zerop status))` の分岐を次に置き換える:

```elisp
                 (cond
                  ((and (eq (process-status proc) 'exit) (zerop status))
                   (funcall callback (string-trim-right output "\n+")))
                  ;; 呼び出し側が意図的に止めたプロセス。失敗として知らせない。
                  ((process-get proc 'wamei/claude-cli-cancelled) nil)
                  (t
                   (message "claude (%s) failed: %s" model
                            (if (string-empty-p errors)
                                (format "exit status %d" status)
                              errors))))
```

docstring（`wamei/claude-cli-run`）の末尾に 1 行足す:

```
プロセスに `wamei/claude-cli-cancelled' プロパティが付いていれば失敗を知らせない。
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-cli-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3`
Expected: `Ran 27 tests, 27 results as expected, 0 unexpected`

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/claude-cli.el .emacs.d/claude-cli-test.el
git commit -m "claude-cli にキャンセル済みプロセスの失敗を無音にするプロパティを追加

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 2: claude-complete.el 骨格と文脈抽出

**Files:**
- Create: `.emacs.d/claude-complete.el`
- Create: `.emacs.d/claude-complete-test.el`

**Interfaces:**
- Consumes: `claude-cli.el` の feature `claude-cli`
- Produces:
  - `wamei/claude-complete-prefix-chars` (integer, 3000) / `wamei/claude-complete-suffix-chars` (integer, 1000)
  - `(wamei/claude-complete--language)` → string。`major-mode` から `-ts-mode` / `-mode` を除いたもの
  - `(wamei/claude-complete--path)` → string。プロジェクト相対パス / ファイル名 / バッファ名
  - `(wamei/claude-complete--context)` → plist `(:prefix STR :suffix STR :path STR :language STR)`

- [ ] **Step 1: テストファイルの骨格と文脈抽出テストを書く**

`.emacs.d/claude-complete-test.el` を新規作成:

```elisp
;;; claude-complete-test.el --- tests for claude-complete -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "claude-cli.el" dir) nil t)
  (load (expand-file-name "claude-complete.el" dir) nil t))

;;; フィクスチャ

(defun wamei/claude-complete-test--stub (script)
  "SCRIPT を本文とする実行可能なシェルスクリプトを作り、そのパスを返す。"
  (let ((path (make-temp-file "claude-stub-" nil ".sh"
                              (concat "#!/bin/sh\n" script "\n"))))
    (set-file-modes path #o755)
    path))

(defmacro wamei/claude-complete-test--with-stub (script &rest body)
  "SCRIPT のスタブを `wamei/claude-cli-program' にして BODY を評価する。"
  (declare (indent 1))
  `(let ((wamei/claude-cli-program (wamei/claude-complete-test--stub ,script)))
     (unwind-protect (progn ,@body)
       (delete-file wamei/claude-cli-program))))

(defun wamei/claude-complete-test--wait (process)
  "PROCESS が終了し sentinel が走り終わるまで待つ。"
  (while (process-live-p process)
    (accept-process-output process 0.1))
  (accept-process-output nil 0.1))

(defmacro wamei/claude-complete-test--with-buffer (text &rest body)
  "TEXT を入れた一時バッファで BODY を評価する。
TEXT 中の `|' を取り除いてその位置に点を置く。`|' が無ければ末尾。"
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,text)
     (goto-char (point-min))
     (if (search-forward "|" nil t)
         (delete-char -1)
       (goto-char (point-max)))
     ,@body))

;;; 文脈抽出

(ert-deftest wamei/claude-complete-language-strips-ts-mode-and-mode-suffix ()
  (with-temp-buffer
    (setq major-mode 'typescript-ts-mode)
    (should (equal (wamei/claude-complete--language) "typescript"))
    (setq major-mode 'emacs-lisp-mode)
    (should (equal (wamei/claude-complete--language) "emacs-lisp"))
    (setq major-mode 'tsx-ts-mode)
    (should (equal (wamei/claude-complete--language) "tsx"))))

(ert-deftest wamei/claude-complete-path-falls-back-to-buffer-name ()
  (with-temp-buffer
    (rename-buffer "*scratch-x*" t)
    (should (equal (wamei/claude-complete--path) (buffer-name)))))

(ert-deftest wamei/claude-complete-path-uses-file-name-without-project ()
  (let ((dir (make-temp-file "claude-complete-" t)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "sub/util.ts" dir))
          (should (equal (wamei/claude-complete--path) "util.ts")))
      (delete-directory dir t))))

(ert-deftest wamei/claude-complete-path-is-relative-to-project-root ()
  (let ((dir (make-temp-file "claude-complete-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (make-directory (expand-file-name "src" dir))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "src/util.ts" dir))
            (setq default-directory (expand-file-name "src/" dir))
            (should (equal (wamei/claude-complete--path) "src/util.ts"))))
      (delete-directory dir t))))

(ert-deftest wamei/claude-complete-context-splits-at-point ()
  (wamei/claude-complete-test--with-buffer "abc|def"
    (let ((context (wamei/claude-complete--context)))
      (should (equal (plist-get context :prefix) "abc"))
      (should (equal (plist-get context :suffix) "def"))
      (should (equal (plist-get context :language) "emacs-lisp"))
      (should (stringp (plist-get context :path))))))

(ert-deftest wamei/claude-complete-context-limits-prefix-and-suffix-length ()
  (wamei/claude-complete-test--with-buffer "0123456789|abcdefghij"
    (let* ((wamei/claude-complete-prefix-chars 4)
           (wamei/claude-complete-suffix-chars 3)
           (context (wamei/claude-complete--context)))
      (should (equal (plist-get context :prefix) "6789"))
      (should (equal (plist-get context :suffix) "abc")))))

(provide 'claude-complete-test)
;;; claude-complete-test.el ends here
```

- [ ] **Step 2: 失敗を確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -5`
Expected: `claude-complete.el` が無いので load でエラー（`Cannot open load file`）。

- [ ] **Step 3: claude-complete.el を作る**

`.emacs.d/claude-complete.el` を新規作成:

```elisp
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
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3`
Expected: `Ran 6 tests, 6 results as expected, 0 unexpected`

`wamei/claude-complete-path-is-relative-to-project-root` だけ失敗し、原因が `project-current` が `.git` ディレクトリを認識しないことなら、そのテスト内で `(require 'vc-git)` を先頭に足して再実行する。それでも通らなければテストを削除せず、原因を報告して止まる。

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/claude-complete.el .emacs.d/claude-complete-test.el
git commit -m "claude-complete.el を追加して補完文脈の抽出を実装

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 3: プロンプト生成

**Files:**
- Modify: `.emacs.d/claude-complete.el`（`;;; 文脈抽出` の後に `;;; プロンプト` セクションを追加）
- Test: `.emacs.d/claude-complete-test.el`

**Interfaces:**
- Consumes: Task 2 の context plist
- Produces:
  - `wamei/claude-complete-system-prompt` (string)
  - `(wamei/claude-complete--prompt CONTEXT IDENTIFIERS)` → string。IDENTIFIERS は文字列リストまたは nil

- [ ] **Step 1: 失敗テストを書く**

`.emacs.d/claude-complete-test.el` の `(provide 'claude-complete-test)` の直前に追記:

```elisp
;;; プロンプト

(ert-deftest wamei/claude-complete-prompt-wraps-file-and-marks-cursor ()
  (let ((prompt (wamei/claude-complete--prompt
                 '(:prefix "abc" :suffix "def" :path "src/a.ts" :language "typescript")
                 nil)))
    (should (equal prompt
                   "<file path=\"src/a.ts\" language=\"typescript\">\nabc<CURSOR>def\n</file>\n"))))

(ert-deftest wamei/claude-complete-prompt-lists-identifiers-when-given ()
  (let ((prompt (wamei/claude-complete--prompt
                 '(:prefix "" :suffix "" :path "a.ts" :language "typescript")
                 '("clamp" "Math"))))
    (should (string-suffix-p "</file>\n<identifiers>\nclamp, Math\n</identifiers>\n" prompt))))

(ert-deftest wamei/claude-complete-prompt-omits-identifiers-block-when-empty ()
  (let ((prompt (wamei/claude-complete--prompt
                 '(:prefix "" :suffix "" :path "a.ts" :language "typescript")
                 nil)))
    (should-not (string-match-p "<identifiers>" prompt))))

(ert-deftest wamei/claude-complete-system-prompt-forbids-fences-and-mentions-cursor ()
  (should (string-match-p "<CURSOR>" wamei/claude-complete-system-prompt))
  (should (string-match-p "fence" wamei/claude-complete-system-prompt))
  (should (string-match-p "<identifiers>" wamei/claude-complete-system-prompt)))
```

- [ ] **Step 2: 失敗を確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E 'FAILED|Ran'`
Expected: 4 件 FAILED（`void-function wamei/claude-complete--prompt` / `void-variable`）

- [ ] **Step 3: 実装する**

`.emacs.d/claude-complete.el` の `(provide 'claude-complete)` の直前に追記:

```elisp
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
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3`
Expected: `Ran 10 tests, 10 results as expected, 0 unexpected`

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/claude-complete.el .emacs.d/claude-complete-test.el
git commit -m "claude-complete にプロンプト生成を追加

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 4: 出力整形

**Files:**
- Modify: `.emacs.d/claude-complete.el`（`;;; 出力整形` セクションを追加）
- Test: `.emacs.d/claude-complete-test.el`

**Interfaces:**
- Consumes: Task 2 の context plist（`:prefix` と `:suffix` を使う）
- Produces: `(wamei/claude-complete--clean TEXT CONTEXT)` → string または nil

- [ ] **Step 1: 失敗テストを書く**

`.emacs.d/claude-complete-test.el` の `(provide 'claude-complete-test)` の直前に追記:

```elisp
;;; 出力整形

(defun wamei/claude-complete-test--clean (text prefix suffix)
  (wamei/claude-complete--clean text (list :prefix prefix :suffix suffix
                                           :path "a.ts" :language "typescript")))

(ert-deftest wamei/claude-complete-clean-strips-code-fences ()
  (should (equal (wamei/claude-complete-test--clean "```ts\nreturn 1;\n```" "" "")
                 "return 1;"))
  (should (equal (wamei/claude-complete-test--clean "```\nreturn 1;\nfoo();\n```\n" "" "")
                 "return 1;\nfoo();")))

(ert-deftest wamei/claude-complete-clean-keeps-text-without-fences ()
  (should (equal (wamei/claude-complete-test--clean "return 1;" "" "") "return 1;")))

(ert-deftest wamei/claude-complete-clean-drops-repeated-line-head ()
  ;; 点の手前が「  return 」で、モデルが行全体を返してきた場合
  (should (equal (wamei/claude-complete-test--clean "  return Math.max(a, b);"
                                                    "function f() {\n  return " "\n}")
                 "Math.max(a, b);"))
  ;; インデント無しで行を繰り返した場合
  (should (equal (wamei/claude-complete-test--clean "return Math.max(a, b);"
                                                    "function f() {\n  return " "\n}")
                 "Math.max(a, b);")))

(ert-deftest wamei/claude-complete-clean-keeps-output-when-line-head-is-blank ()
  (should (equal (wamei/claude-complete-test--clean "  return 1;" "function f() {\n  " "\n}")
                 "return 1;"))
  (should (equal (wamei/claude-complete-test--clean "return 1;" "function f() {\n" "\n}")
                 "return 1;")))

(ert-deftest wamei/claude-complete-clean-drops-overlap-with-suffix ()
  ;; foo(<CURSOR>) で「a, b)」が返った場合、末尾の ) は既にある
  (should (equal (wamei/claude-complete-test--clean "a, b)" "foo(" ")") "a, b"))
  ;; 閉じ括弧の行が既にある場合
  (should (equal (wamei/claude-complete-test--clean "return x;\n}" "{\n  " "\n}")
                 "return x;")))

(ert-deftest wamei/claude-complete-clean-ignores-suffix-beyond-first-line ()
  (should (equal (wamei/claude-complete-test--clean "return x;" "{\n  " "\n}\nreturn x;")
                 "return x;")))

(ert-deftest wamei/claude-complete-clean-trims-trailing-whitespace ()
  (should (equal (wamei/claude-complete-test--clean "return 1;  \n\n" "" "") "return 1;")))

(ert-deftest wamei/claude-complete-clean-returns-nil-when-empty ()
  (should-not (wamei/claude-complete-test--clean "" "" ""))
  (should-not (wamei/claude-complete-test--clean "   \n" "" ""))
  (should-not (wamei/claude-complete-test--clean "```\n```" "" ""))
  (should-not (wamei/claude-complete-test--clean ")" "foo(" ")")))
```

- [ ] **Step 2: 失敗を確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E 'FAILED|Ran'`
Expected: 8 件 FAILED（`void-function wamei/claude-complete--clean`）

- [ ] **Step 3: 実装する**

`.emacs.d/claude-complete.el` の `(provide 'claude-complete)` の直前に追記:

```elisp
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
インデント付きとインデント無しの両方を試す。空白だけの行頭は対象にしない。"
  (let* ((head (car (last (split-string prefix "\n"))))
         (bare (string-trim-left head)))
    (cond
     ((string-empty-p bare) text)
     ((string-prefix-p head text) (substring text (length head)))
     ((string-prefix-p bare text) (substring text (length bare)))
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
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3`
Expected: `Ran 18 tests, 18 results as expected, 0 unexpected`

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/claude-complete.el .emacs.d/claude-complete-test.el
git commit -m "claude-complete に出力整形を追加

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 5: overlay 表示・確定・破棄と minor mode

**Files:**
- Modify: `.emacs.d/claude-complete.el`（`;;; 状態` `;;; 表示` `;;; minor mode` セクションを追加）
- Test: `.emacs.d/claude-complete-test.el`

**Interfaces:**
- Produces:
  - buffer-local 変数 `wamei/claude-complete--overlay` `wamei/claude-complete--process` `wamei/claude-complete--timer` `wamei/claude-complete--request`
  - `(wamei/claude-complete--show TEXT)` 点に overlay を置く
  - `(wamei/claude-complete--visible-p)` → overlay 表示中なら non-nil
  - `(wamei/claude-complete--delete-overlay)`
  - コマンド `wamei/claude-complete-accept` / `wamei/claude-complete-dismiss`
  - `wamei/claude-complete-mode` と `wamei/claude-complete-mode-map`（TAB は overlay 表示中だけ accept に効く。`C-c C-.` は Task 6 で定義する `wamei/claude-complete` に束縛するので、この Task では未定義の関数をシンボルで束縛しておく）
  - `(wamei/claude-complete--teardown)` timer / process / overlay を片付ける
  - `(wamei/claude-complete--cancel-process)` `(wamei/claude-complete--cancel-timer)`
  - `pre-command-hook` 用 `wamei/claude-complete--pre-command`（accept 以外のコマンドで overlay を消す）

- [ ] **Step 1: 失敗テストを書く**

`.emacs.d/claude-complete-test.el` の `(provide 'claude-complete-test)` の直前に追記:

```elisp
;;; 表示・確定・破棄

(ert-deftest wamei/claude-complete-show-places-shadow-overlay-at-point ()
  (wamei/claude-complete-test--with-buffer "foo(|)"
    (wamei/claude-complete--show "a, b")
    (should (wamei/claude-complete--visible-p))
    (let ((overlay wamei/claude-complete--overlay))
      (should (= (overlay-start overlay) (point)))
      (should (= (overlay-end overlay) (point)))
      (should (equal (overlay-get overlay 'after-string) "a, b"))
      (should (eq (get-text-property 0 'face (overlay-get overlay 'after-string)) 'shadow)))
    ;; バッファ本文は変わらない
    (should (equal (buffer-string) "foo()"))))

(ert-deftest wamei/claude-complete-show-replaces-previous-overlay ()
  (wamei/claude-complete-test--with-buffer "x|"
    (wamei/claude-complete--show "one")
    (let ((first wamei/claude-complete--overlay))
      (wamei/claude-complete--show "two")
      (should-not (overlay-buffer first))
      (should (equal (overlay-get wamei/claude-complete--overlay 'after-string) "two")))))

(ert-deftest wamei/claude-complete-accept-inserts-text-and-removes-overlay ()
  (wamei/claude-complete-test--with-buffer "foo(|)"
    (wamei/claude-complete--show "a, b")
    (wamei/claude-complete-accept)
    (should (equal (buffer-string) "foo(a, b)"))
    (should (= (point) 9))
    (should-not (wamei/claude-complete--visible-p))))

(ert-deftest wamei/claude-complete-accept-does-nothing-without-overlay ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete-accept)
    (should (equal (buffer-string) "foo"))))

(ert-deftest wamei/claude-complete-dismiss-removes-overlay ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete--show "bar")
    (wamei/claude-complete-dismiss)
    (should-not (wamei/claude-complete--visible-p))
    (should (equal (buffer-string) "foo"))))

(ert-deftest wamei/claude-complete-pre-command-dismisses-unless-accept ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete--show "bar")
    (let ((this-command 'wamei/claude-complete-accept))
      (wamei/claude-complete--pre-command))
    (should (wamei/claude-complete--visible-p))
    (let ((this-command 'self-insert-command))
      (wamei/claude-complete--pre-command))
    (should-not (wamei/claude-complete--visible-p))))

;;; minor mode

(ert-deftest wamei/claude-complete-mode-binds-tab-only-while-visible ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete-mode 1)
    (should-not (eq (key-binding (kbd "TAB")) 'wamei/claude-complete-accept))
    (wamei/claude-complete--show "bar")
    (should (eq (key-binding (kbd "TAB")) 'wamei/claude-complete-accept))
    (should (eq (key-binding (kbd "<tab>")) 'wamei/claude-complete-accept))
    (wamei/claude-complete--delete-overlay)
    (should-not (eq (key-binding (kbd "TAB")) 'wamei/claude-complete-accept))))

(ert-deftest wamei/claude-complete-mode-binds-manual-trigger ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete-mode 1)
    (should (eq (key-binding (kbd "C-c C-.")) 'wamei/claude-complete))))

(ert-deftest wamei/claude-complete-mode-installs-and-removes-hooks ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete-mode 1)
    (should (memq #'wamei/claude-complete--pre-command pre-command-hook))
    (should (memq #'wamei/claude-complete--post-command post-command-hook))
    (should (memq #'wamei/claude-complete--teardown kill-buffer-hook))
    (wamei/claude-complete-mode -1)
    (should-not (memq #'wamei/claude-complete--pre-command pre-command-hook))
    (should-not (memq #'wamei/claude-complete--post-command post-command-hook))
    (should-not (memq #'wamei/claude-complete--teardown kill-buffer-hook))))

(ert-deftest wamei/claude-complete-mode-off-clears-overlay-timer-and-request ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (wamei/claude-complete-mode 1)
    (wamei/claude-complete--show "bar")
    (setq wamei/claude-complete--timer (run-with-idle-timer 100 nil #'ignore))
    (setq wamei/claude-complete--request (cons 1 1))
    (wamei/claude-complete-mode -1)
    (should-not (wamei/claude-complete--visible-p))
    (should-not wamei/claude-complete--timer)
    (should-not wamei/claude-complete--request)))
```

- [ ] **Step 2: 失敗を確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E 'FAILED|Ran'`
Expected: 10 件 FAILED（`void-function wamei/claude-complete--show` など）

- [ ] **Step 3: 実装する**

`.emacs.d/claude-complete.el` の `(provide 'claude-complete)` の直前に追記。`wamei/claude-complete--post-command` は Task 7 で本実装するが、hook 登録テストを通すためにここでは timer を触らない最小版を置く。

```elisp
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
  (wamei/claude-complete--cancel-timer)
  (wamei/claude-complete--cancel-process)
  (wamei/claude-complete--delete-overlay)
  (setq wamei/claude-complete--request nil))

(defun wamei/claude-complete--pre-command ()
  "accept 以外のコマンドが走る前にゴーストテキストを消す。"
  (unless (eq this-command 'wamei/claude-complete-accept)
    (wamei/claude-complete--delete-overlay)))

(defun wamei/claude-complete--post-command ()
  "コマンド後の処理。Task 7 で idle timer の張り直しを実装する。"
  nil)

;;; minor mode

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
```

`wamei/claude-complete` は Task 6 で定義する。この Task の時点では `#'wamei/claude-complete` は未定義シンボルへの束縛だが、キーマップは定義済みでなくても束縛できる。byte-compile 時の警告を避けるため、`;;; minor mode` の先頭に次を足す:

```elisp
(declare-function wamei/claude-complete "claude-complete")
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3`
Expected: `Ran 28 tests, 28 results as expected, 0 unexpected`

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/claude-complete.el .emacs.d/claude-complete-test.el
git commit -m "claude-complete にゴーストテキストの表示と minor mode を追加

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 6: 要求の発行と応答の表示（手動トリガー）

**Files:**
- Modify: `.emacs.d/claude-complete.el`（`;;; 要求` セクションを `;;; minor mode` の前に追加）
- Test: `.emacs.d/claude-complete-test.el`

**Interfaces:**
- Consumes: `wamei/claude-cli-run MODEL INPUT CALLBACK &optional SYSTEM-PROMPT` → process。Task 3 の `--prompt`、Task 4 の `--clean`、Task 5 の `--show` / `--stamp` / `--cancel-process`
- Produces:
  - `(wamei/claude-complete--allowed-p)` → 要求してよいなら non-nil
  - `(wamei/claude-complete--eglot-identifiers CALLBACK)` CALLBACK を識別子リスト（この Task では常に nil）で 1 回呼ぶ。Task 8 で eglot 対応に置き換える
  - `(wamei/claude-complete--start STAMP IDENTIFIERS)` claude を起動し `wamei/claude-complete--process` に保持
  - `(wamei/claude-complete-request)` ガード → stamp 記録 → identifiers 取得 → start
  - コマンド `wamei/claude-complete`（手動トリガー）

- [ ] **Step 1: 失敗テストを書く**

`.emacs.d/claude-complete-test.el` の `;;; minor mode` セクションの前に追記:

```elisp
;;; 要求

(ert-deftest wamei/claude-complete-request-shows-cleaned-output ()
  (wamei/claude-complete-test--with-stub "printf '```ts\\nreturn 1;\\n```'"
    (wamei/claude-complete-test--with-buffer "function f() {\n  |\n}"
      (wamei/claude-complete-request)
      (should (process-live-p wamei/claude-complete--process))
      (wamei/claude-complete-test--wait wamei/claude-complete--process)
      (should (wamei/claude-complete--visible-p))
      (should (equal (overlay-get wamei/claude-complete--overlay 'wamei/claude-complete-text)
                     "return 1;"))
      (should-not wamei/claude-complete--process))))

(ert-deftest wamei/claude-complete-request-sends-prompt-and-system-prompt ()
  ;; stub は引数と stdin をまとめて返す。応答は整形されてそのまま overlay に載る
  (wamei/claude-complete-test--with-stub "printf '%s\\n' \"$@\"; cat"
    (wamei/claude-complete-test--with-buffer "abc|def"
      (wamei/claude-complete-request)
      (wamei/claude-complete-test--wait wamei/claude-complete--process)
      (let ((shown (overlay-get wamei/claude-complete--overlay 'wamei/claude-complete-text)))
        (should (string-match-p "--model\nhaiku\n" shown))
        (should (string-match-p (regexp-quote wamei/claude-complete-system-prompt) shown))
        (should (string-match-p "abc<CURSOR>def" shown))))))

(ert-deftest wamei/claude-complete-request-discards-stale-response ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (wamei/claude-complete-request)
      (let ((process wamei/claude-complete--process))
        ;; 応答を受け取る前にバッファを変える
        (insert "x")
        (wamei/claude-complete-test--wait process))
      (should-not (wamei/claude-complete--visible-p))
      (should (equal (buffer-string) "foox")))))

(ert-deftest wamei/claude-complete-request-discards-response-after-point-moves ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "fo|o"
      (wamei/claude-complete-request)
      (let ((process wamei/claude-complete--process))
        (forward-char 1)
        (wamei/claude-complete-test--wait process))
      (should-not (wamei/claude-complete--visible-p)))))

(ert-deftest wamei/claude-complete-request-does-not-show-empty-output ()
  (wamei/claude-complete-test--with-stub "printf '   '"
    (wamei/claude-complete-test--with-buffer "foo|"
      (wamei/claude-complete-request)
      (wamei/claude-complete-test--wait wamei/claude-complete--process)
      (should-not (wamei/claude-complete--visible-p)))))

(ert-deftest wamei/claude-complete-request-cancels-previous-process-silently ()
  (wamei/claude-complete-test--with-stub "sleep 5"
    (wamei/claude-complete-test--with-buffer "foo|"
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (when fmt (push (apply #'format fmt args) messages)))))
          (wamei/claude-complete-request)
          (let ((first wamei/claude-complete--process))
            (wamei/claude-complete-request)
            (should-not (eq first wamei/claude-complete--process))
            (should-not (process-live-p first))
            (wamei/claude-complete-test--wait first)
            (wamei/claude-complete--cancel-process)
            (accept-process-output nil 0.1)))
        (should-not messages)))))

(ert-deftest wamei/claude-complete-request-is-blocked-while-completion-in-region ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (let ((completion-in-region-mode t))
        (wamei/claude-complete-request))
      (should-not wamei/claude-complete--process)
      (should-not wamei/claude-complete--request))))

(ert-deftest wamei/claude-complete-request-is-blocked-in-read-only-buffer ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (setq buffer-read-only t)
      (wamei/claude-complete-request)
      (should-not wamei/claude-complete--process))))

(ert-deftest wamei/claude-complete-command-is-interactive-and-requests ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (should (commandp #'wamei/claude-complete))
      (call-interactively #'wamei/claude-complete)
      (wamei/claude-complete-test--wait wamei/claude-complete--process)
      (should (equal (overlay-get wamei/claude-complete--overlay 'wamei/claude-complete-text)
                     "bar")))))
```

- [ ] **Step 2: 失敗を確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E 'FAILED|Ran'`
Expected: 9 件 FAILED（`void-function wamei/claude-complete-request` など）

- [ ] **Step 3: 実装する**

`.emacs.d/claude-complete.el` の `;;; minor mode` セクション（`declare-function` の行）の前に追記:

```elisp
;;; 要求

(defun wamei/claude-complete--allowed-p ()
  "いま補完を要求してよければ non-nil。
corfu のポップアップ表示中 (`completion-in-region-mode')、読み取り専用、minibuffer では要求しない。"
  (not (or completion-in-region-mode
           buffer-read-only
           (minibufferp))))

(defun wamei/claude-complete--eglot-identifiers (callback)
  "CALLBACK を識別子リストで 1 回呼ぶ。Task 8 で eglot の補完候補を返すようにする。"
  (funcall callback nil))

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
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3`
Expected: `Ran 37 tests, 37 results as expected, 0 unexpected`

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/claude-complete.el .emacs.d/claude-complete-test.el
git commit -m "claude-complete に手動トリガーの要求と応答表示を追加

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 7: idle 自動トリガー

**Files:**
- Modify: `.emacs.d/claude-complete.el`（`wamei/claude-complete--post-command` を置き換え、`wamei/claude-complete--on-idle` を追加）
- Test: `.emacs.d/claude-complete-test.el`

**Interfaces:**
- Consumes: Task 5 の `--cancel-timer` / `--cancel-process` / `--stamp` / `--visible-p`、Task 6 の `wamei/claude-complete-request`
- Produces:
  - `(wamei/claude-complete--post-command)` 要求時点から変化があれば process をキャンセルし、`wamei/claude-complete-auto` なら idle timer を張り直す
  - `(wamei/claude-complete--on-idle BUFFER)` timer から呼ばれ、条件を満たせば `wamei/claude-complete-request`

- [ ] **Step 1: 失敗テストを書く**

`.emacs.d/claude-complete-test.el` の `;;; minor mode` セクションの前に追記:

```elisp
;;; idle 自動トリガー

(ert-deftest wamei/claude-complete-post-command-arms-idle-timer ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (let ((wamei/claude-complete-idle-delay 100))
      (unwind-protect
          (progn
            (wamei/claude-complete--post-command)
            (should (timerp wamei/claude-complete--timer))
            (should (eq (timer--function wamei/claude-complete--timer)
                        #'wamei/claude-complete--on-idle))
            (should (equal (timer--args wamei/claude-complete--timer)
                           (list (current-buffer))))
            (should (memq wamei/claude-complete--timer timer-idle-list)))
        (wamei/claude-complete--cancel-timer)))))

(ert-deftest wamei/claude-complete-post-command-replaces-existing-timer ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (let ((wamei/claude-complete-idle-delay 100))
      (unwind-protect
          (progn
            (wamei/claude-complete--post-command)
            (let ((first wamei/claude-complete--timer))
              (wamei/claude-complete--post-command)
              (should-not (eq first wamei/claude-complete--timer))
              (should-not (memq first timer-idle-list))))
        (wamei/claude-complete--cancel-timer)))))

(ert-deftest wamei/claude-complete-post-command-does-not-arm-timer-when-auto-off ()
  (wamei/claude-complete-test--with-buffer "foo|"
    (let ((wamei/claude-complete-auto nil))
      (wamei/claude-complete--post-command)
      (should-not wamei/claude-complete--timer))))

(ert-deftest wamei/claude-complete-post-command-cancels-process-after-edit ()
  (wamei/claude-complete-test--with-stub "sleep 5"
    (wamei/claude-complete-test--with-buffer "foo|"
      (let ((wamei/claude-complete-auto nil))
        (wamei/claude-complete-request)
        (let ((process wamei/claude-complete--process))
          (insert "x")
          (wamei/claude-complete--post-command)
          (should-not (process-live-p process))
          (should-not wamei/claude-complete--process)
          (should-not wamei/claude-complete--request)
          (wamei/claude-complete-test--wait process))))))

(ert-deftest wamei/claude-complete-post-command-keeps-process-when-unchanged ()
  (wamei/claude-complete-test--with-stub "sleep 5"
    (wamei/claude-complete-test--with-buffer "foo|"
      (let ((wamei/claude-complete-auto nil))
        (wamei/claude-complete-request)
        (let ((process wamei/claude-complete--process))
          (wamei/claude-complete--post-command)
          (should (process-live-p process))
          (should (eq process wamei/claude-complete--process))
          (wamei/claude-complete--cancel-process)
          (wamei/claude-complete-test--wait process))))))

(ert-deftest wamei/claude-complete-on-idle-requests-in-current-buffer ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (wamei/claude-complete-mode 1)
      (let ((wamei/claude-complete-auto nil))
        (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
          (wamei/claude-complete--on-idle (current-buffer)))
        (should wamei/claude-complete--process)
        (wamei/claude-complete-test--wait wamei/claude-complete--process)
        (should (wamei/claude-complete--visible-p))))))

(ert-deftest wamei/claude-complete-on-idle-skips-when-buffer-not-selected ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (wamei/claude-complete-mode 1)
      (let ((other (generate-new-buffer " *other*")))
        (unwind-protect
            (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) other)))
              (wamei/claude-complete--on-idle (current-buffer))
              (should-not wamei/claude-complete--process))
          (kill-buffer other))))))

(ert-deftest wamei/claude-complete-on-idle-skips-while-visible-or-running ()
  (wamei/claude-complete-test--with-stub "sleep 5"
    (wamei/claude-complete-test--with-buffer "foo|"
      (wamei/claude-complete-mode 1)
      (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
        ;; 表示中は要求しない
        (wamei/claude-complete--show "bar")
        (wamei/claude-complete--on-idle (current-buffer))
        (should-not wamei/claude-complete--process)
        (wamei/claude-complete--delete-overlay)
        ;; 走行中は要求しない
        (wamei/claude-complete-request)
        (let ((process wamei/claude-complete--process))
          (wamei/claude-complete--on-idle (current-buffer))
          (should (eq process wamei/claude-complete--process))
          (wamei/claude-complete--cancel-process)
          (wamei/claude-complete-test--wait process))))))

(ert-deftest wamei/claude-complete-on-idle-skips-when-mode-off ()
  (wamei/claude-complete-test--with-stub "printf 'bar'"
    (wamei/claude-complete-test--with-buffer "foo|"
      (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
        (wamei/claude-complete--on-idle (current-buffer))
        (should-not wamei/claude-complete--process)))))
```

- [ ] **Step 2: 失敗を確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E 'FAILED|Ran'`
Expected: `post-command-arms-idle-timer` `post-command-replaces-existing-timer` `post-command-cancels-process-after-edit` と `on-idle-*` 4 件が FAILED。`does-not-arm-timer-when-auto-off` と `keeps-process-when-unchanged` は最小版でも通る。

- [ ] **Step 3: 実装する**

`.emacs.d/claude-complete.el` の Task 5 で置いた最小版 `wamei/claude-complete--post-command` を削除し、`;;; 要求` セクションの末尾（`wamei/claude-complete` コマンドの後）に次を追記:

```elisp
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
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3`
Expected: `Ran 46 tests, 46 results as expected, 0 unexpected`

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/claude-complete.el .emacs.d/claude-complete-test.el
git commit -m "claude-complete に idle 自動トリガーを追加

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 8: eglot の補完候補をプロンプトに混ぜる

**Files:**
- Modify: `.emacs.d/claude-complete.el`（Task 6 の `wamei/claude-complete--eglot-identifiers` を置き換え、`;;; eglot 文脈` セクションにする。変数 `wamei/claude-complete-max-identifiers` / `wamei/claude-complete-eglot-timeout` を `;;; 設定` に追加）
- Test: `.emacs.d/claude-complete-test.el`

**Interfaces:**
- Consumes: eglot の `eglot-managed-p` `eglot-server-capable` `eglot-current-server` `eglot--TextDocumentPositionParams`、jsonrpc の `jsonrpc-async-request SERVER METHOD PARAMS &key :success-fn :error-fn :timeout-fn :timeout`
- Produces:
  - `wamei/claude-complete-max-identifiers` (integer, 50) / `wamei/claude-complete-eglot-timeout` (float, 0.3)
  - `(wamei/claude-complete--completion-labels RESULT)` → 文字列リスト。RESULT は LSP の `CompletionItem[]`（vector）か `CompletionList`（`:items` を持つ plist）
  - `(wamei/claude-complete--eglot-identifiers CALLBACK)` 非同期。常に CALLBACK を 1 回だけ呼ぶ

- [ ] **Step 1: 失敗テストを書く**

`.emacs.d/claude-complete-test.el` の `;;; minor mode` セクションの前に追記:

```elisp
;;; eglot 文脈

(require 'eglot)

(ert-deftest wamei/claude-complete-completion-labels-reads-item-array ()
  (should (equal (wamei/claude-complete--completion-labels
                  [(:label "clamp" :kind 3) (:label "Math" :kind 6)])
                 '("clamp" "Math"))))

(ert-deftest wamei/claude-complete-completion-labels-reads-completion-list ()
  (should (equal (wamei/claude-complete--completion-labels
                  '(:isIncomplete :json-false :items [(:label "a") (:label "b")]))
                 '("a" "b"))))

(ert-deftest wamei/claude-complete-completion-labels-dedupes-and-limits ()
  (let ((wamei/claude-complete-max-identifiers 2))
    (should (equal (wamei/claude-complete--completion-labels
                    [(:label "a") (:label "a") (:label "b") (:label "c")])
                   '("a" "b")))))

(ert-deftest wamei/claude-complete-completion-labels-handles-nil-and-missing-label ()
  (should-not (wamei/claude-complete--completion-labels nil))
  (should (equal (wamei/claude-complete--completion-labels [(:kind 3) (:label "x")])
                 '("x"))))

(defmacro wamei/claude-complete-test--with-fake-eglot (respond &rest body)
  "eglot が動いているように見せ、`jsonrpc-async-request' を RESPOND で置き換えて BODY を評価する。
RESPOND は (lambda (server method params &rest keys)) で、keys から :success-fn 等を取り出して呼ぶ。"
  (declare (indent 1))
  `(cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
             ((symbol-function 'eglot-server-capable) (lambda (&rest _) t))
             ((symbol-function 'eglot-current-server) (lambda () 'fake-server))
             ((symbol-function 'eglot--TextDocumentPositionParams) (lambda () '(:fake t)))
             ((symbol-function 'jsonrpc-async-request) ,respond))
     ,@body))

(ert-deftest wamei/claude-complete-eglot-identifiers-returns-nil-when-not-managed ()
  (cl-letf (((symbol-function 'eglot-managed-p) (lambda () nil)))
    (let ((calls nil))
      (wamei/claude-complete--eglot-identifiers (lambda (ids) (push ids calls)))
      (should (equal calls '(nil))))))

(ert-deftest wamei/claude-complete-eglot-identifiers-returns-labels-on-success ()
  (let ((calls nil) (seen nil))
    (wamei/claude-complete-test--with-fake-eglot
        (lambda (server method params &rest keys)
          (setq seen (list server method params (plist-get keys :timeout)))
          (funcall (plist-get keys :success-fn) [(:label "clamp") (:label "Math")])
          '(1))
      (wamei/claude-complete--eglot-identifiers (lambda (ids) (push ids calls))))
    (should (equal calls '(("clamp" "Math"))))
    (should (equal seen (list 'fake-server :textDocument/completion '(:fake t)
                              wamei/claude-complete-eglot-timeout)))))

(ert-deftest wamei/claude-complete-eglot-identifiers-returns-nil-on-error-or-timeout ()
  (dolist (key '(:error-fn :timeout-fn))
    (let ((calls nil))
      (wamei/claude-complete-test--with-fake-eglot
          (lambda (_server _method _params &rest keys)
            (funcall (plist-get keys key) (list :code -1 :message "boom"))
            '(1))
        (wamei/claude-complete--eglot-identifiers (lambda (ids) (push ids calls))))
      (should (equal calls '(nil))))))

(ert-deftest wamei/claude-complete-eglot-identifiers-calls-back-once-even-if-both-fire ()
  (let ((calls nil))
    (wamei/claude-complete-test--with-fake-eglot
        (lambda (_server _method _params &rest keys)
          (funcall (plist-get keys :timeout-fn))
          (funcall (plist-get keys :success-fn) [(:label "late")])
          '(1))
      (wamei/claude-complete--eglot-identifiers (lambda (ids) (push ids calls))))
    (should (equal calls '(nil)))))

(ert-deftest wamei/claude-complete-eglot-identifiers-returns-nil-when-request-signals ()
  (let ((calls nil))
    (wamei/claude-complete-test--with-fake-eglot
        (lambda (&rest _) (signal 'error '("no server")))
      (wamei/claude-complete--eglot-identifiers (lambda (ids) (push ids calls))))
    (should (equal calls '(nil)))))

(ert-deftest wamei/claude-complete-request-puts-eglot-labels-into-prompt ()
  (wamei/claude-complete-test--with-stub "cat"
    (wamei/claude-complete-test--with-buffer "abc|"
      (wamei/claude-complete-test--with-fake-eglot
          (lambda (_server _method _params &rest keys)
            (funcall (plist-get keys :success-fn) [(:label "clamp") (:label "Math")])
            '(1))
        (wamei/claude-complete-request))
      (wamei/claude-complete-test--wait wamei/claude-complete--process)
      (should (string-match-p "<identifiers>\nclamp, Math\n</identifiers>"
                              (overlay-get wamei/claude-complete--overlay
                                           'wamei/claude-complete-text))))))
```

- [ ] **Step 2: 失敗を確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E 'FAILED|Ran'`
Expected: `completion-labels-*` 4 件と `returns-labels-on-success` `calls-back-once…` `request-puts-eglot-labels…` が FAILED。`returns-nil-*` 系は最小版でも通る。

- [ ] **Step 3: 実装する**

`.emacs.d/claude-complete.el` の `;;; 設定` セクション末尾に追加:

```elisp
(defvar wamei/claude-complete-max-identifiers 50
  "eglot の補完候補からプロンプトに載せる識別子名の上限。")

(defvar wamei/claude-complete-eglot-timeout 0.3
  "eglot に補完候補を求めるときのタイムアウト秒。超えたら識別子なしで進む。")
```

Task 6 で置いた最小版 `wamei/claude-complete--eglot-identifiers` を削除し、`;;; 要求` セクションの前に次を追記:

```elisp
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
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3`
Expected: `Ran 56 tests, 56 results as expected, 0 unexpected`

- [ ] **Step 5: byte-compile で警告が無いことを確認する**

Run: `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -L . -f batch-byte-compile claude-complete.el 2>&1; rm -f claude-complete.elc`
Expected: `Warning` 行が出ない。出た場合は該当箇所を直す（未宣言関数なら `declare-function` を追加、未使用変数なら `_` 接頭辞）。

- [ ] **Step 6: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/claude-complete.el .emacs.d/claude-complete-test.el
git commit -m "claude-complete に eglot の補完候補をプロンプトへ混ぜる処理を追加

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 9: init.el への組み込みと実機確認

**Files:**
- Modify: `.emacs.d/init.el:963-977`（`leaf claude-cli` ブロックの直後に `leaf claude-complete` を追加）

**Interfaces:**
- Consumes: `wamei/claude-complete-mode`（Task 5）

- [ ] **Step 1: leaf ブロックを追加する**

`.emacs.d/init.el` の `leaf claude-cli` ブロック（`(define-key git-commit-mode-map (kbd "C-c C-m") #'wamei/claude-commit-message)))` で終わる）の直後に追記:

```elisp

(leaf claude-complete
  :doc "claude -p によるゴーストテキスト補完"
  :ensure nil
  :after claude-cli
  :preface
  ;; 実体は claude-complete.el。claude-cli と同じく init.el の実体の隣から読む。
  (load (expand-file-name "claude-complete"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  ;; prog-mode 全体で有効化する。eglot が無いバッファでも動き、eglot 管理下なら
  ;; 補完候補の識別子名がプロンプトに加わる。
  :hook (prog-mode-hook . wamei/claude-complete-mode))
```

- [ ] **Step 2: batch で init.el を読み込み leaf の警告が無いことを確認する**

Run:

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch \
  --eval '(setq user-init-file "~/.emacs.d/init.el")' \
  --eval '(load "~/.emacs.d/init.el")' 2>&1 | grep -E 'leaf|claude-complete|Error|Warning' | grep -v 'Warning (leaf) .*mini-frame\|desktop'
```

Expected: `claude-complete` に関する `Warning (leaf)` / `Error (leaf)` が出ない。他ブロックの既知の偽陽性（`desktop` / `mini-frame` の `arrayp, nil`）は無視してよい。

- [ ] **Step 3: モードが有効になることを batch で確認する**

Run:

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch \
  --eval '(setq user-init-file "~/.emacs.d/init.el")' \
  --eval '(load "~/.emacs.d/init.el")' \
  --eval '(with-temp-buffer (emacs-lisp-mode) (run-hooks (quote prog-mode-hook)) (message "mode=%s key=%s" wamei/claude-complete-mode (key-binding (kbd "C-c C-."))))' 2>&1 | grep '^mode='
```

Expected: `mode=t key=wamei/claude-complete`

- [ ] **Step 4: 実機で確認する（ユーザーに依頼する内容）**

executor は次を人間に依頼し、結果を待つ:

1. Emacs を再起動するか、init.el の `leaf claude-complete` ブロックを `C-M-x` で評価する
2. `.ts` ファイルを開き、関数本体の途中で 1 秒ほど入力を止める。2〜3 秒後に薄い色の提案が出る
3. TAB で挿入される。別のキーを打つと消える
4. `C-c C-.` で手動でも出る
5. corfu のポップアップが出ている間は提案が出ない
6. `M-: (setq wamei/claude-complete-auto nil)` で自動が止まる

問題があれば systematic-debugging skill で原因を追う。`*Messages*` に `claude (haiku) failed:` が出ていればプロセス側、何も出ず表示もされないなら `--clean` が nil を返しているか stamp 照合で捨てている。

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/init.el
git commit -m "init.el に claude-complete を組み込んで prog-mode でゴーストテキスト補完を有効化

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

## 自己レビュー結果

- **仕様カバレッジ**: 文脈抽出 (T2)、プロンプト (T3)、整形 (T4)、状態と overlay と mode (T5)、要求・鮮度判定・ガード・キャンセル無音 (T1, T6)、idle (T7)、eglot (T8)、init.el (T9)、teardown と kill-buffer-hook (T5)。仕様の「TAB は overlay の keymap で束縛」は mode map の `:filter` 付き `menu-item` に変更した（長さ 0 の overlay の `keymap` プロパティは点の位置で拾われないため）。仕様書側にも同じ修正を反映する
- **型の一貫性**: `wamei/claude-cli-run` の引数順 (model input callback system-prompt) は既存どおり。`--request` と `--stamp` は共に `(tick . point)` の cons。`--eglot-identifiers` の callback 引数は文字列リストか nil で T6 / T8 で一致
