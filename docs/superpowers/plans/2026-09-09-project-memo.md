# org メモ (プロジェクト別 + 全体) 実装計画

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `~/org/` に置く org メモ (プロジェクト別 + 全体) を自動保存・自動復元で運用し、プロジェクトタブを開いた直後の画面を「dired サイドバー + そのプロジェクトのメモ」にする。

**Architecture:** 新規 `~/.dotfiles/.emacs.d/project-memo.el` に、パス解決・メモバッファ生成・表示トグル・自動保存・タブ初期画面をまとめる。init.el 側は `leaf org` (最小) と `leaf project-memo` の追加、`leaf project` の `project-switch-commands` 差し替えだけ。自動保存は Emacs 標準の `auto-save-visited-mode` を述語でメモバッファに絞って使い、自動復元は desktop に任せる (専用処理を持たない)。

**Tech Stack:** Emacs 31.1 (macOS NS ビルド), Emacs Lisp (lexical binding), ert (batch), 既存モジュール `project-tabs.el` / `project-sidebar.el`, leaf.el

**Spec:** `docs/superpowers/specs/2026-09-09-project-memo-design.md`

## Global Constraints

- Emacs 31.1。`auto-save-visited-mode` / `auto-save-visited-predicate` / `auto-save-visited-interval` は標準にある。
- メモの置き場は `~/org/` のフラット構成。プロジェクトメモ `~/org/<project-name>.org`、全体メモ `~/org/global.org`。
- 同名 repo が複数あると同じメモを共有する。これは仕様 (回避しない)。
- 新規ファイルは `~/.dotfiles/.emacs.d/` に置く。init.el は `~/.emacs.d/init.el` への symlink なので、init.el からの load は必ず `(file-name-directory (file-truename user-init-file))` を起点にする。
- テストは `emacs -Q --batch -l <file>-test.el -f ert-run-tests-batch-and-exit` で走る形にする。既存の `project-sidebar-test.el` の流儀に合わせる。
- **テストは実ユーザーの `~/org/` を絶対に触らない。** フィクスチャで `wamei/project-memo-directory` を一時ディレクトリに let 束縛する。
- コメントと docstring は日本語。既存モジュールと同じ密度で「なぜそうしたか」を書く。
- TDD (Red → Green → Refactoring)。各タスクの最後にコミットする。
- コミットメッセージは英語。末尾に以下を付ける:

  ```
  Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01FcQ4DZ1S7uxjgYpB2XJYdC
  ```

---

## ファイル構成

| ファイル | 役割 |
| --- | --- |
| `.emacs.d/project-memo.el` (新規) | パス解決 / メモバッファ / 表示トグル / 自動保存 / タブ初期画面 |
| `.emacs.d/project-memo-test.el` (新規) | 上記の ert テスト |
| `.emacs.d/init.el` (変更) | `leaf org` 追加、`leaf project-memo` 追加、`leaf project` の `project-switch-commands` 差し替え |

`project-memo.el` の依存は `project` / `project-tabs` / `project-sidebar`。循環は無い (`project-sidebar` → `project-tabs` の一方向)。

---

## Task 1: パス解決とメモバッファ判定

**Files:**
- Create: `.emacs.d/project-memo.el`
- Test: `.emacs.d/project-memo-test.el`

**Interfaces:**
- Consumes: なし
- Produces:
  - `wamei/project-memo-directory` (defcustom, string, 既定 `"~/org/"`)
  - `wamei/project-memo-global-name` (defcustom, string, 既定 `"global.org"`)
  - `(wamei/project-memo-file PROJECT)` → 絶対パス文字列
  - `(wamei/project-memo-global-file)` → 絶対パス文字列
  - `(wamei/project-memo-buffer-p &optional BUFFER)` → boolean

- [ ] **Step 1: 失敗するテストを書く**

`.emacs.d/project-memo-test.el` を新規作成する。

```elisp
;;; project-memo-test.el --- tests for project-memo -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'project)
(package-initialize)
(require 'dired-subtree)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "project-tabs.el" dir) nil t)
  (load (expand-file-name "dired-tree.el" dir) nil t)
  (load (expand-file-name "project-sidebar.el" dir) nil t)
  (load (expand-file-name "project-memo.el" dir) nil t))

(wamei/project-sidebar-setup)

;;; フィクスチャ

(defmacro wamei/project-memo-test--with-project (var &rest body)
  "一時ディレクトリを transient プロジェクトにして VAR に束縛し BODY を評価する。

`wamei/project-memo-directory' も一時ディレクトリに差し替える。実ユーザーの
~/org/ にテストが書き込まないようにするため、この束縛は必須。
VAR は `file-truename' 済み (macOS では make-temp-file の結果が
/var/folders/… → /private/var/… と symlink 越しになる)。"
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (file-truename (make-temp-file "memo-proj-" t))))
          (wamei/project-memo-directory
           (file-name-as-directory (file-truename (make-temp-file "memo-org-" t))))
          (project-find-functions
           (list (lambda (dir)
                   (when (string-prefix-p ,var (file-truename (expand-file-name dir)))
                     (cons 'transient ,var))))))
     (unwind-protect
         (progn ,@body)
       ;; メモバッファを (未保存でも聞かれないように) 片付けてからディレクトリを消す
       (dolist (buf (buffer-list))
         (when-let* ((file (buffer-file-name buf))
                     ((string-prefix-p wamei/project-memo-directory file)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory ,var t)
       (delete-directory wamei/project-memo-directory t))))

(defun wamei/project-memo-test--project (root)
  "ROOT のプロジェクトオブジェクト。"
  (project-current nil root))

;;; パス解決

(ert-deftest wamei/project-memo-file-is-project-name-under-memo-directory ()
  (wamei/project-memo-test--with-project root
    (should (equal (wamei/project-memo-file (wamei/project-memo-test--project root))
                   (expand-file-name
                    (concat (file-name-nondirectory (directory-file-name root)) ".org")
                    wamei/project-memo-directory)))))

(ert-deftest wamei/project-memo-file-replaces-slash-in-name ()
  (wamei/project-memo-test--with-project root
    (cl-letf (((symbol-function 'project-name) (lambda (_project) "group/app")))
      (should (equal (wamei/project-memo-file (wamei/project-memo-test--project root))
                     (expand-file-name "group-app.org" wamei/project-memo-directory))))))

(ert-deftest wamei/project-memo-global-file-is-global-name ()
  (wamei/project-memo-test--with-project root
    (should (equal (wamei/project-memo-global-file)
                   (expand-file-name "global.org" wamei/project-memo-directory)))))

(ert-deftest wamei/project-memo-file-creates-memo-directory ()
  (wamei/project-memo-test--with-project root
    (delete-directory wamei/project-memo-directory t)
    (should-not (file-directory-p wamei/project-memo-directory))
    (wamei/project-memo-global-file)
    (should (file-directory-p wamei/project-memo-directory))))

;;; メモバッファの判定

(ert-deftest wamei/project-memo-buffer-p-matches-org-under-memo-directory ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (find-file-noselect (wamei/project-memo-global-file))))
      (should (wamei/project-memo-buffer-p buffer)))))

(ert-deftest wamei/project-memo-buffer-p-rejects-other-files ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (should-not (wamei/project-memo-buffer-p buffer))
        (kill-buffer buffer)))))

(ert-deftest wamei/project-memo-buffer-p-rejects-non-org-in-memo-directory ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (find-file-noselect
                   (expand-file-name "notes.txt" wamei/project-memo-directory))))
      (should-not (wamei/project-memo-buffer-p buffer)))))

(provide 'project-memo-test)
;;; project-memo-test.el ends here
```

- [ ] **Step 2: テストが失敗することを確認**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL。`project-memo.el` がまだ無いので load が黙って何もせず、`wamei/project-memo-file` が void-function になる。

- [ ] **Step 3: 最小の実装を書く**

`.emacs.d/project-memo.el` を新規作成する。

```elisp
;;; project-memo.el --- org のメモ (プロジェクト別 / 全体) -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; プロジェクトごとのメモと、プロジェクトに紐づかない全体メモを org で持つ。
;;
;; - 実体は `wamei/project-memo-directory' (既定 ~/org/) 直下のフラットな
;;   org ファイル。プロジェクトメモは <project-name>.org、全体メモは global.org
;; - 保存は意識しなくてよい。アイドル中は `auto-save-visited-mode' が、
;;   メモから離れるときは `wamei/project-memo-save-all' が実ファイルへ書く
;; - 復元は desktop に任せる。メモは通常のファイルバッファなので専用処理は要らない
;; - プロジェクトタブを開いた直後の画面 (`wamei/project-memo-switch-setup') は
;;   左に sidebar、本文 window にそのプロジェクトのメモ
;;
;; 置き場をフラットにしたので、同名の repo が複数あると同じメモを共有する。
;; 「どこにある repo でも扱えること」を優先した結果として受け入れている。
;;
;;; Code:

(require 'project)
(require 'project-tabs)

(defgroup wamei/project-memo nil
  "org のメモ (プロジェクト別 / 全体)。"
  :group 'convenience)

(defcustom wamei/project-memo-directory "~/org/"
  "メモを置くディレクトリ。"
  :type 'directory
  :group 'wamei/project-memo)

(defcustom wamei/project-memo-global-name "global.org"
  "全体メモのファイル名。`wamei/project-memo-directory' からの相対。"
  :type 'string
  :group 'wamei/project-memo)

;;; パス解決

(defun wamei/project-memo--sanitize (name)
  "NAME をファイル名に使える形にする。ディレクトリ区切りを - に潰す。"
  (replace-regexp-in-string "/" "-" name))

(defun wamei/project-memo--directory ()
  "メモのディレクトリ (末尾 / 付き)。無ければ作る。"
  (let ((dir (file-name-as-directory (expand-file-name wamei/project-memo-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun wamei/project-memo-file (project)
  "PROJECT のメモファイルの絶対パス。

名前は `project-name' を使う。タブに出ている名前 (project-tabs.el) と
同じものにして、タブとメモの対応を見た目から追えるようにする。"
  (expand-file-name (concat (wamei/project-memo--sanitize (project-name project)) ".org")
                    (wamei/project-memo--directory)))

(defun wamei/project-memo-global-file ()
  "全体メモの絶対パス。"
  (expand-file-name wamei/project-memo-global-name (wamei/project-memo--directory)))

(defun wamei/project-memo-buffer-p (&optional buffer)
  "BUFFER (既定はカレント) がメモファイルを訪れているか。

バッファローカルの目印ではなくパスで判定する。desktop から復元された
メモバッファには目印が付かないが、自動保存はそれにも効く必要がある。"
  (let ((file (buffer-file-name (or buffer (current-buffer)))))
    (and file
         (equal (file-name-extension file) "org")
         (file-in-directory-p file (expand-file-name wamei/project-memo-directory))
         t)))

(provide 'project-memo)
;;; project-memo.el ends here
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: PASS (7 tests)

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles
git add .emacs.d/project-memo.el .emacs.d/project-memo-test.el
git commit -m "$(cat <<'EOF'
Add memo path resolution and memo buffer predicate

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FcQ4DZ1S7uxjgYpB2XJYdC
EOF
)"
```

---

## Task 2: メモバッファの生成 (テンプレートとプロジェクトの結びつけ)

**Files:**
- Modify: `.emacs.d/project-memo.el` (「パス解決」節の後ろに「バッファ」節を足す)
- Test: `.emacs.d/project-memo-test.el` (「メモバッファの判定」節の後ろに足す)

**Interfaces:**
- Consumes: `wamei/project-memo-file`, `wamei/project-memo-global-file` (Task 1)
- Produces: `(wamei/project-memo-buffer &optional PROJECT)` → buffer。PROJECT が nil なら全体メモ。

**背景 (この設計の理由):** メモバッファが訪れているのは `~/org/foo.org` でプロジェクト外なので、そのまま本文 window に出すと `wamei/tab-bar-tab-name-project` の判定が外れ、タブ名が `foo.org` に化ける (`wamei/project-tabs-pin-name` も走らない)。プロジェクトメモのバッファに `project-current-directory-override` をバッファローカルで持たせて、`project-current` が対象プロジェクトを返すようにする。

- [ ] **Step 1: 失敗するテストを書く**

`project-memo-test.el` に足す。

```elisp
;;; メモバッファ

(ert-deftest wamei/project-memo-buffer-inserts-title-for-new-file ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (wamei/project-memo-buffer (wamei/project-memo-test--project root))))
      (with-current-buffer buffer
        (should (string-prefix-p
                 (concat "#+title: " (file-name-nondirectory (directory-file-name root)))
                 (buffer-string)))))))

(ert-deftest wamei/project-memo-buffer-keeps-existing-content ()
  (wamei/project-memo-test--with-project root
    (let ((file (wamei/project-memo-file (wamei/project-memo-test--project root))))
      (with-temp-file file (insert "既存の中身\n"))
      (let ((buffer (wamei/project-memo-buffer (wamei/project-memo-test--project root))))
        (with-current-buffer buffer
          (should (equal (buffer-string) "既存の中身\n")))))))

(ert-deftest wamei/project-memo-buffer-overrides-project-for-project-memo ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (wamei/project-memo-buffer (wamei/project-memo-test--project root))))
      (with-current-buffer buffer
        (should (local-variable-p 'project-current-directory-override))
        (should (equal (project-root (project-current nil)) root))))))

(ert-deftest wamei/project-memo-buffer-does-not-override-project-for-global ()
  (wamei/project-memo-test--with-project root
    (let ((buffer (wamei/project-memo-buffer nil)))
      (with-current-buffer buffer
        (should-not (local-variable-p 'project-current-directory-override))
        (should (equal (buffer-file-name) (wamei/project-memo-global-file)))))))

(ert-deftest wamei/project-memo-buffer-is-org-mode ()
  (wamei/project-memo-test--with-project root
    (with-current-buffer (wamei/project-memo-buffer nil)
      (should (derived-mode-p 'org-mode)))))
```

- [ ] **Step 2: テストが失敗することを確認**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL (`wamei/project-memo-buffer` が void-function)

- [ ] **Step 3: 最小の実装を書く**

`project-memo.el` の `wamei/project-memo-buffer-p` の後ろ、`(provide 'project-memo)` の前に足す。

```elisp
;;; バッファ

(defun wamei/project-memo-buffer (&optional project)
  "PROJECT のメモバッファ。PROJECT が nil なら全体メモ。

ファイルがまだ無ければ #+title: の 1 行だけ入れる。ファイルは最初の保存で
生まれる (自動保存があるので、開いたまま数秒放置すれば実体ができる)。

プロジェクトメモには `project-current-directory-override' をバッファ
ローカルで持たせる。メモの実体は ~/org/ にあってプロジェクト外なので、
これが無いとタブ名の判定 (`wamei/tab-bar-tab-name-project') が外れ、
project-find-file などの起点もメモのディレクトリになってしまう。"
  (let* ((file (if project
                   (wamei/project-memo-file project)
                 (wamei/project-memo-global-file)))
         (new (not (file-exists-p file)))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (when (and new (zerop (buffer-size)))
        (insert "#+title: " (if project (project-name project) (file-name-base file)) "\n\n"))
      (if project
          (setq-local project-current-directory-override
                      (file-name-as-directory (expand-file-name (project-root project))))
        (kill-local-variable 'project-current-directory-override)))
    buffer))
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: PASS (12 tests)

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles
git add .emacs.d/project-memo.el .emacs.d/project-memo-test.el
git commit -m "$(cat <<'EOF'
Create memo buffers with a title line and a project override

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FcQ4DZ1S7uxjgYpB2XJYdC
EOF
)"
```

---

## Task 3: 表示トグル (`C-x C-m`)

**Files:**
- Modify: `.emacs.d/project-memo.el` (「バッファ」節の後ろに「表示」節を足す)
- Test: `.emacs.d/project-memo-test.el`

**Interfaces:**
- Consumes: `wamei/project-memo-buffer` (Task 2), `wamei/project-memo-buffer-p` (Task 1), `wamei/project-tabs-main-window` / `wamei/project-tabs-current-root` (既存 `project-tabs.el`)
- Produces:
  - `(wamei/project-memo--project)` → project または nil
  - `(wamei/project-memo-toggle &optional GLOBAL)` (interactive, `"P"`)

- [ ] **Step 1: 失敗するテストを書く**

`project-memo-test.el` に足す。

```elisp
;;; 表示トグル

(ert-deftest wamei/project-memo-toggle-shows-project-memo-in-main-window ()
  (wamei/project-memo-test--with-project root
    (let ((main (selected-window)))
      (with-current-buffer (window-buffer main)
        (setq default-directory root))
      (wamei/project-memo-toggle)
      (should (eq (selected-window) main))
      (should (equal (buffer-file-name (window-buffer main))
                     (wamei/project-memo-file (wamei/project-memo-test--project root)))))))

(ert-deftest wamei/project-memo-toggle-returns-to-previous-buffer ()
  (wamei/project-memo-test--with-project root
    (let* ((main (selected-window))
           (work (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            (set-window-buffer main work)
            (wamei/project-memo-toggle)
            (should (wamei/project-memo-buffer-p (window-buffer main)))
            (wamei/project-memo-toggle)
            (should (eq (window-buffer main) work)))
        (kill-buffer work)))))

(ert-deftest wamei/project-memo-toggle-with-prefix-shows-global-memo ()
  (wamei/project-memo-test--with-project root
    (let ((main (selected-window)))
      (with-current-buffer (window-buffer main)
        (setq default-directory root))
      (wamei/project-memo-toggle '(4))
      (should (equal (buffer-file-name (window-buffer main))
                     (wamei/project-memo-global-file))))))

(ert-deftest wamei/project-memo-toggle-outside-project-shows-global-memo ()
  (wamei/project-memo-test--with-project root
    (let ((main (selected-window))
          (scratch (get-buffer-create "*memo-test-scratch*")))
      (unwind-protect
          (progn
            (with-current-buffer scratch (setq default-directory temporary-file-directory))
            (set-window-buffer main scratch)
            (wamei/project-memo-toggle)
            (should (equal (buffer-file-name (window-buffer main))
                           (wamei/project-memo-global-file))))
        (kill-buffer scratch)))))

(ert-deftest wamei/project-memo-toggle-from-global-to-project-keeps-back-buffer ()
  (wamei/project-memo-test--with-project root
    (let* ((main (selected-window))
           (work (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            (set-window-buffer main work)
            (wamei/project-memo-toggle '(4))   ; 全体メモ
            (wamei/project-memo-toggle)        ; プロジェクトメモ (メモ → メモ)
            (should (equal (buffer-file-name (window-buffer main))
                           (wamei/project-memo-file (wamei/project-memo-test--project root))))
            (wamei/project-memo-toggle)        ; 戻り先は work のまま
            (should (eq (window-buffer main) work)))
        (kill-buffer work)))))
```

- [ ] **Step 2: テストが失敗することを確認**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL (`wamei/project-memo-toggle` が void-function)

- [ ] **Step 3: 最小の実装を書く**

`project-memo.el` の「バッファ」節の後ろに足す。

```elisp
;;; 表示

(defun wamei/project-memo--project ()
  "メモの対象にするプロジェクト。無ければ nil。

タブに紐づいた root (project-tabs.el) を先に見る。本文 window に別
プロジェクトのファイルや *scratch* が出ていても、タブの宣言に従わせる。"
  (if-let* ((root (wamei/project-tabs-current-root)))
      (project-current nil root)
    (with-current-buffer (window-buffer (wamei/project-tabs-main-window))
      (project-current nil))))

(defun wamei/project-memo--restore (window)
  "WINDOW をメモを出す前のバッファに戻す。記録が無ければ直前のバッファ。"
  (let ((back (window-parameter window 'wamei/project-memo-back)))
    (set-window-parameter window 'wamei/project-memo-back nil)
    (if (buffer-live-p back)
        (set-window-buffer window back)
      (switch-to-prev-buffer window))
    (select-window window)))

(defun wamei/project-memo-toggle (&optional global)
  "本文 window にメモを出す。既に出ていれば元のバッファに戻る。

GLOBAL (`C-u') が非 nil なら全体メモ。タブがプロジェクトに紐づいて
いないときは GLOBAL 無しでも全体メモになる。

出す先は `wamei/project-tabs-main-window'。sidebar や端末パネルに
フォーカスがあっても本文 window に出す。

戻り先は window パラメータに退避する。メモから別のメモへ切り替えた
ときは上書きせず、最初にメモを出す前のバッファを保つ。"
  (interactive "P")
  (let* ((project (unless global (wamei/project-memo--project)))
         (buffer (wamei/project-memo-buffer project))
         (window (wamei/project-tabs-main-window)))
    (if (eq (window-buffer window) buffer)
        (wamei/project-memo--restore window)
      (unless (wamei/project-memo-buffer-p (window-buffer window))
        (set-window-parameter window 'wamei/project-memo-back (window-buffer window)))
      (set-window-buffer window buffer)
      (select-window window))))
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: PASS (17 tests)

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles
git add .emacs.d/project-memo.el .emacs.d/project-memo-test.el
git commit -m "$(cat <<'EOF'
Toggle the project and global memo in the main window

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FcQ4DZ1S7uxjgYpB2XJYdC
EOF
)"
```

---

## Task 4: 自動保存

**Files:**
- Modify: `.emacs.d/project-memo.el` (「表示」節の後ろに「自動保存」節を足す)
- Test: `.emacs.d/project-memo-test.el`

**Interfaces:**
- Consumes: `wamei/project-memo-buffer-p` (Task 1)
- Produces:
  - `(wamei/project-memo--auto-save-p)` → boolean (`auto-save-visited-predicate` 用)
  - `(wamei/project-memo-save-all &rest _)` → nil (hook 用。引数は受け流す)
  - `(wamei/project-memo-autosave-setup)` → nil (init.el から 1 回呼ぶ)

- [ ] **Step 1: 失敗するテストを書く**

`project-memo-test.el` に足す。

```elisp
;;; 自動保存

(ert-deftest wamei/project-memo-auto-save-p-only-for-memo-buffers ()
  (wamei/project-memo-test--with-project root
    (let ((memo (wamei/project-memo-buffer nil))
          (work (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            (should (with-current-buffer memo (wamei/project-memo--auto-save-p)))
            (should-not (with-current-buffer work (wamei/project-memo--auto-save-p))))
        (kill-buffer work)))))

(ert-deftest wamei/project-memo-save-all-writes-modified-memo ()
  (wamei/project-memo-test--with-project root
    (let ((memo (wamei/project-memo-buffer nil)))
      (with-current-buffer memo
        (goto-char (point-max))
        (insert "書きかけ\n")
        (should (buffer-modified-p)))
      (wamei/project-memo-save-all)
      (should-not (buffer-modified-p memo))
      (should (file-exists-p (wamei/project-memo-global-file)))
      (with-temp-buffer
        (insert-file-contents (wamei/project-memo-global-file))
        (should (string-match-p "書きかけ" (buffer-string)))))))

(ert-deftest wamei/project-memo-save-all-leaves-other-buffers-alone ()
  (wamei/project-memo-test--with-project root
    (let ((work (find-file-noselect (expand-file-name "main.el" root))))
      (unwind-protect
          (progn
            (with-current-buffer work (insert ";; 未保存\n"))
            (wamei/project-memo-save-all)
            (should (buffer-modified-p work))
            (should-not (file-exists-p (expand-file-name "main.el" root))))
        (with-current-buffer work (set-buffer-modified-p nil))
        (kill-buffer work)))))

(ert-deftest wamei/project-memo-save-all-accepts-hook-arguments ()
  (wamei/project-memo-test--with-project root
    ;; window-selection-change-functions は frame を渡す。
    (should-not (wamei/project-memo-save-all (selected-frame)))))

(ert-deftest wamei/project-memo-autosave-setup-installs-predicate-and-hooks ()
  (let ((auto-save-visited-predicate nil)
        (window-selection-change-functions nil)
        (kill-emacs-hook nil)
        (after-focus-change-function #'ignore)
        (auto-save-visited-mode nil))
    (cl-letf (((symbol-function 'auto-save-visited-mode) (lambda (&rest _) t)))
      (wamei/project-memo-autosave-setup)
      (should (eq auto-save-visited-predicate #'wamei/project-memo--auto-save-p))
      (should (memq #'wamei/project-memo-save-all window-selection-change-functions))
      (should (memq #'wamei/project-memo-save-all kill-emacs-hook)))))
```

- [ ] **Step 2: テストが失敗することを確認**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL (`wamei/project-memo--auto-save-p` が void-function)

- [ ] **Step 3: 最小の実装を書く**

`project-memo.el` の「表示」節の後ろに足す。

```elisp
;;; 自動保存

(defun wamei/project-memo--auto-save-p ()
  "`auto-save-visited-predicate' 用。メモバッファだけ実ファイルへ保存する。

`auto-save-visited-mode' はグローバルなので、述語を置かないと全部の
ファイルが勝手に保存されるようになる。"
  (wamei/project-memo-buffer-p))

(defun wamei/project-memo-save-all (&rest _)
  "変更のあるメモバッファを全て保存する。

`window-selection-change-functions' (frame を受け取る)、
`after-focus-change-function'、`kill-emacs-hook' から呼ぶので引数は受け流す。
アイドル中の保存は `auto-save-visited-mode' が見るため、ここは
「メモから離れた瞬間」を埋めるためにある。"
  (let ((save-silently t))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (wamei/project-memo-buffer-p) (buffer-modified-p))
          (save-buffer)))))
  nil)

(defun wamei/project-memo-autosave-setup ()
  "メモの自動保存を有効にする。init.el から 1 回呼ぶ。"
  (setq auto-save-visited-predicate #'wamei/project-memo--auto-save-p)
  (auto-save-visited-mode 1)
  (add-hook 'window-selection-change-functions #'wamei/project-memo-save-all)
  (add-function :after after-focus-change-function #'wamei/project-memo-save-all)
  (add-hook 'kill-emacs-hook #'wamei/project-memo-save-all))
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: PASS (22 tests)

- [ ] **Step 5: `before-save-hook` に org へ効くものが無いことを確認**

実際の設定で org バッファの `before-save-hook` を見る。フォーマッタ (apheleia / project-formatter) が org に噛むと、離脱時保存のたびにバッファが書き換わる。

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch --eval '(princ (format "%S" (progn (require (quote org)) (with-temp-buffer (org-mode) (list before-save-hook (default-value (quote before-save-hook)))))))'
```

さらに、実際の init.el を読んだ状態でも確認する (稼働中の Emacs があれば `emacsclient` で):

```bash
emacsclient -e '(with-temp-buffer (org-mode) (format "%S" (append before-save-hook (default-value (quote before-save-hook)))))'
```

Expected: org バッファを書き換える hook が無いこと。もし `apheleia` / `project-formatter` 系が居たら、`wamei/project-memo-save-all` の中で該当 hook を外すか、org を対象外にする設定を足す。見つかった内容は結果として報告する。

- [ ] **Step 6: コミット**

```bash
cd ~/.dotfiles
git add .emacs.d/project-memo.el .emacs.d/project-memo-test.el
git commit -m "$(cat <<'EOF'
Save memo buffers on idle and when leaving them

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FcQ4DZ1S7uxjgYpB2XJYdC
EOF
)"
```

---

## Task 5: プロジェクトタブの初期画面

**Files:**
- Modify: `.emacs.d/project-memo.el` (「自動保存」節の後ろに「タブの初期画面」節を足す。冒頭の `require` に `project-sidebar` を足す)
- Test: `.emacs.d/project-memo-test.el`

**Interfaces:**
- Consumes: `wamei/project-memo-buffer` (Task 2), `wamei/project-tabs-main-window` (既存), `wamei/project-sidebar-show` (既存 `project-sidebar.el`)
- Produces: `(wamei/project-memo-switch-setup)` (interactive, 引数なし)

**背景:** `project-switch-project` は `project-switch-commands` がシンボルならそれを `call-interactively` する。このとき呼び出し元バッファに `project-current-directory-override` がバッファローカルで設定されている (`default-directory` は変わらない)。なので対象プロジェクトは `(project-current)` で取る。`select-window` は選択した window のバッファをカレントにするので、プロジェクトの取得は `select-window` より **前** に行う。

- [ ] **Step 1: 失敗するテストを書く**

`project-memo-test.el` に足す。

```elisp
;;; タブの初期画面

(ert-deftest wamei/project-memo-switch-setup-shows-sidebar-and-memo ()
  (wamei/project-memo-test--with-project root
    (let ((main (selected-window)))
      (with-current-buffer (window-buffer main)
        (setq default-directory root))
      (unwind-protect
          (progn
            (wamei/project-memo-switch-setup)
            ;; 本文 window にプロジェクトメモ
            (should (equal (buffer-file-name (window-buffer main))
                           (wamei/project-memo-file (wamei/project-memo-test--project root))))
            ;; 左に sidebar が出ていて、フォーカスは本文に残る
            (let ((side (wamei/project-sidebar-window)))
              (should side)
              (should (eq (window-parameter side 'window-side) 'left)))
            (should (eq (selected-window) main)))
        (when-let* ((side (wamei/project-sidebar-window)))
          (delete-window side))))))

(ert-deftest wamei/project-memo-switch-setup-uses-directory-override ()
  (wamei/project-memo-test--with-project root
    ;; project-switch-project と同じ状況: default-directory は別で、
    ;; project-current-directory-override だけが対象プロジェクトを指す。
    (let ((main (selected-window))
          (caller (get-buffer-create "*memo-test-caller*")))
      (unwind-protect
          (progn
            (with-current-buffer caller
              (setq default-directory temporary-file-directory)
              (setq-local project-current-directory-override root)
              (set-window-buffer main caller)
              (wamei/project-memo-switch-setup))
            (should (equal (buffer-file-name (window-buffer main))
                           (wamei/project-memo-file (wamei/project-memo-test--project root)))))
        (when-let* ((side (wamei/project-sidebar-window)))
          (delete-window side))
        (kill-buffer caller)))))
```

- [ ] **Step 2: テストが失敗することを確認**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL (`wamei/project-memo-switch-setup` が void-function)

- [ ] **Step 3: 最小の実装を書く**

`project-memo.el` 冒頭の require を次にする。

```elisp
(require 'project)
(require 'project-tabs)
(require 'project-sidebar)
```

「自動保存」節の後ろに足す。

```elisp
;;; タブの初期画面

(defun wamei/project-memo-switch-setup ()
  "プロジェクトを開いた直後の画面を作る。

`project-switch-commands' に置いて `project-switch-project' から
`call-interactively' で呼ばれる。左に sidebar、本文 window にその
プロジェクトのメモを出し、フォーカスは本文に残す。

対象プロジェクトは `project-current' から取る。呼び出し元バッファに
`project-current-directory-override' がバッファローカルで設定されて
いるため (`default-directory' は変わらない)。`select-window' は選択した
window のバッファをカレントにするので、取得はその前に済ませる。"
  (interactive)
  (let* ((project (project-current nil))
         (root (and project (project-root project)))
         (window (wamei/project-tabs-main-window))
         (buffer (wamei/project-memo-buffer project)))
    (select-window window)
    (delete-other-windows window)
    (set-window-buffer window buffer)
    (when root
      (wamei/project-sidebar-show root))
    (select-window window)))
```

- [ ] **Step 4: テストが通ることを確認**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit
```

Expected: PASS (24 tests)

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles
git add .emacs.d/project-memo.el .emacs.d/project-memo-test.el
git commit -m "$(cat <<'EOF'
Open the sidebar and the project memo when entering a project

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FcQ4DZ1S7uxjgYpB2XJYdC
EOF
)"
```

---

## Task 6: init.el への配線と実機確認

**Files:**
- Modify: `.emacs.d/init.el`
  - `leaf project` (643 行付近) の `:custom` の `project-switch-commands`
  - `leaf project-sidebar` (1105 行付近) の直後に `leaf project-memo` を追加
  - `leaf org` を新規追加 (`leaf project-memo` の直前)

**Interfaces:**
- Consumes: `wamei/project-memo-toggle` (Task 3), `wamei/project-memo-autosave-setup` (Task 4), `wamei/project-memo-switch-setup` (Task 5)
- Produces: なし (配線のみ)

- [ ] **Step 1: `project-switch-commands` を差し替える**

`leaf project` の `:custom` を書き換える。

変更前:

```elisp
  :custom
  ;; 切り替え先で何をするかをミニバッファで選ばせず、ルートを dired で開く。
  ;; シンボルを入れるとその command を即実行する (dispatch メニューを出さない)。
  (project-switch-commands . #'project-dired)
```

変更後:

```elisp
  :custom
  ;; 切り替え先で何をするかをミニバッファで選ばせず、sidebar + プロジェクトメモの
  ;; 画面を作る (project-memo.el)。シンボルを入れるとその command を即実行する
  ;; (dispatch メニューを出さない)。root の dired が要るときは C-x C-j がある。
  (project-switch-commands . #'wamei/project-memo-switch-setup)
```

- [ ] **Step 2: `leaf org` と `leaf project-memo` を追加する**

`leaf project-sidebar` ブロックの直後 (`leaf dired-toggle-sudo` の直前) に挿入する。`project-memo.el` は `project-sidebar` を require するので、順序はこの位置でなければならない。

```elisp
(leaf org
  :doc "メモに使う分だけの org 設定 (agenda / capture は入れない)"
  :ensure nil
  :custom
  (org-directory . "~/org/")
  (org-startup-indented . t)          ; 見出しの深さをインデントで見せる
  (org-startup-folded . 'showall)     ; メモなので畳まずに開く
  :hook
  (org-mode-hook . visual-line-mode)) ; 長い行は折り返して表示する

(leaf project-memo
  :doc "org のメモ (プロジェクト別 / 全体)"
  :ensure nil
  :bind (("C-x C-m" . wamei/project-memo-toggle))
  :preface
  ;; init.el は ~/.emacs.d/init.el への symlink なので実体の隣から読む。
  (load (expand-file-name "project-memo"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  :config
  ;; アイドル中の保存 (auto-save-visited-mode) と、メモから離れたときの保存。
  (wamei/project-memo-autosave-setup))
```

- [ ] **Step 3: init.el が構文として読めることを確認**

括弧の対応だけ先に見る (実際に動くかは次の隔離 daemon で確認する)。

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch --eval '(with-temp-buffer (insert-file-contents "init.el") (goto-char (point-min)) (condition-case err (while t (read (current-buffer))) (end-of-file (princ "read ok")) (error (princ (format "READ ERROR: %S" err)))))'
```

Expected: `read ok`

- [ ] **Step 4: 隔離 daemon で実際に起動して確認**

稼働中の Emacs を壊さないよう、隔離した `--init-directory` で daemon を起動して確認する (elpa と .cache は symlink で共有する)。

```bash
D=$(mktemp -d)
ln -s ~/.emacs.d/elpa "$D/elpa" 2>/dev/null
ln -s ~/.emacs.d/.cache "$D/.cache" 2>/dev/null
ln -s ~/.dotfiles/.emacs.d/init.el "$D/init.el"
ln -s ~/.dotfiles/.emacs.d/early-init.el "$D/early-init.el"
emacs --init-directory="$D" --daemon=memo-check 2>&1 | tail -20
```

起動できたら、次を順に確認する。

```bash
# 1. モジュールが読めていて、キーが割り当たっている
emacsclient -s memo-check -e '(list (fboundp (quote wamei/project-memo-toggle)) (key-binding (kbd "C-x C-m")))'
# Expected: (t wamei/project-memo-toggle)

# 2. 自動保存の述語が入っている
emacsclient -s memo-check -e '(list auto-save-visited-mode auto-save-visited-predicate auto-save-visited-interval)'
# Expected: (t wamei/project-memo--auto-save-p 5)

# 3. project-switch-commands が差し替わっている
emacsclient -s memo-check -e 'project-switch-commands'
# Expected: wamei/project-memo-switch-setup

# 4. org バッファの before-save-hook にフォーマッタが居ない
emacsclient -s memo-check -e '(with-temp-buffer (org-mode) (format "%S" (append before-save-hook (default-value (quote before-save-hook)))))'
```

終わったら PID で止める (`pkill -f daemon=` では落ちないことがある)。

```bash
emacsclient -s memo-check -e '(kill-emacs)' || pkill -f "daemon=memo-check"
```

- [ ] **Step 5: GUI で手触りを確認する (ユーザーに依頼する)**

バッチで見えない部分はユーザーに確認してもらう。次の項目を伝える。

1. `C-x C-p` で新しいプロジェクトを開く → 左に sidebar、本文にメモ、フォーカスは本文
2. そのときタブ名がプロジェクト名になっている (`<name>.org` になっていない)
3. メモに何か書いて 5 秒放置 → `~/org/<name>.org` ができている
4. メモに書いて別の window / タブへ移動 → その時点で保存されている
5. `C-x C-m` でメモ ↔ 元のバッファを往復できる
6. `C-u C-x C-m` で全体メモが出る
7. Emacs を再起動 → メモバッファが元のタブに戻っている (desktop)
8. メモバッファで `C-x C-f` (project-find-file) がそのプロジェクトの候補を出す

- [ ] **Step 6: コミット**

```bash
cd ~/.dotfiles
git add .emacs.d/init.el
git commit -m "$(cat <<'EOF'
Open the sidebar and a memo instead of dired on project entry

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FcQ4DZ1S7uxjgYpB2XJYdC
EOF
)"
```

---

## 完了条件

- `emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit` が全て PASS
- 隔離 daemon で Task 6 Step 4 の 4 項目が期待どおり
- ユーザーが Task 6 Step 5 の 8 項目を確認済み
