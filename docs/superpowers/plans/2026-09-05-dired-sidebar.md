# dired ベースのプロジェクトサイドバー 実装計画

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** treemacs をやめ、dired + dired-subtree の上にプロジェクトごとのサイドバーを作り、dired に git 状態の色分け・変更の自動検知・右クリックメニュー・D&D・macOS の open を足す。

**Architecture:** 新規 3 ファイル。`dired-tree.el` は dired-subtree を使う dired バッファ共通の「木」の振る舞い (展開記憶、展開ディレクトリの file-notify、path までの展開、D&D の drop 先)。`dired-git-status.el` は `git status --porcelain` の非同期取得と行への face 付け。`project-sidebar.el` はプロジェクトごとの sidebar バッファ、左 side window への表示とトグル、follow、マウス操作、desktop 復元。いずれも純関数 (テスト対象) と副作用層を分ける。init.el には数行の設定と leaf だけ置く。最後に treemacs 関連を全部削除する。

**Tech Stack:** Emacs 31.1 (macOS NS ビルド) Lisp、ERT (batch)、dired / dired-aux / dired-x / filenotify / dnd (Emacs 同梱)、dired-subtree + dired-hacks-utils (MELPA)、nerd-icons-dired、leaf (init.el)

**Spec:** `docs/superpowers/specs/2026-09-05-dired-sidebar-design.md`

## Global Constraints

- 名前空間は既存に合わせる: 公開は `wamei/dired-tree-…` / `wamei/dired-git-status-…` / `wamei/project-sidebar-…`、内部は `…--…`。spec の短い名前 (`dired-tree-mode` 等) はこの接頭辞付きで実装する
- ファイルは `.emacs.d/` 直下、ヘッダに `-*- lexical-binding: t; -*-`、Commentary・docstring・テスト名の説明は日本語
- テストは `cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l <name>-test.el -f ert-run-tests-batch-and-exit` で走ること (project-tabs-test.el と同形式。`package-initialize` して MELPA のパッケージを使う)
- init.el は `~/.emacs.d/init.el` への symlink。ローカル .el は `(load (expand-file-name "<name>" (file-name-directory (file-truename user-init-file))) nil t)` で読む (既存の project-tabs と同じ)
- init.el の leaf はブロック内エラーを握りつぶす。init.el を変えたら `emacs --batch -l ~/.emacs.d/init.el --eval '(kill-emacs 0)' 2>&1 | grep -E "Warning \(leaf\)|Error \(leaf\)"` で確認する (Task 14 まで treemacs-tab-bar の batch 起因のバックトレースが出るのは既知、無視する)
- 動作確認は起動中の Emacs に `emacsclient --eval '<form>'` で流す。init.el を編集したブロックは `(load-file "~/.emacs.d/init.el")` ではなく該当 leaf の式だけ `emacsclient --eval` で評価する (init.el 全体の再読込は副作用が多い)
- D&D は Emacs 内の dired 同士のみ。Finder で個別に「表示」する機能は入れない
- コミットメッセージは日本語 (このリポジトリの既存コミットに合わせる)。末尾に `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>` を付ける。作業は master で行う (init.el の symlink が master を指すため)

## ファイル構成

| ファイル | 責務 |
|---|---|
| `.emacs.d/dired-tree.el` (新規) | 展開記憶 `wamei/dired-tree--expanded`、展開ディレクトリの file-notify、`wamei/dired-tree-expand-to`、drop 先の決定と dnd ハンドラ、minor mode `wamei/dired-tree-mode` |
| `.emacs.d/dired-tree-test.el` (新規) | 上記の ERT |
| `.emacs.d/dired-git-status.el` (新規) | porcelain のパースと伝播 (純関数)、ルート単位の非同期取得とキャッシュ、行への overlay、minor mode `wamei/dired-git-status-mode`、magit 連携 |
| `.emacs.d/dired-git-status-test.el` (新規) | 上記の ERT |
| `.emacs.d/project-sidebar.el` (新規) | sidebar バッファの生成と外観、side window 表示とトグル、follow、マウス / キー、desktop restorer、minor mode `wamei/project-sidebar-mode` |
| `.emacs.d/project-sidebar-test.el` (新規) | 上記の ERT |
| `.emacs.d/project-tabs.el` (変更) | `wamei/project-tabs--name-window` を `wamei/project-tabs-main-window` として公開、treemacs ガードを削除 |
| `.emacs.d/project-tabs-test.el` (変更) | treemacs ガードのテスト 3 本と `(require 'treemacs)` を削除 |
| `.emacs.d/init.el` (変更) | dired / dired-subtree の設定、3 つの leaf 追加、treemacs 4 leaf の削除、hide-mode-line の hook 付け替え、desktop の restorer と除外の差し替え |

---

### Task 1: spike — macOS で同一フレーム内の dired D&D が動くか

**Files:** なし (起動中の Emacs で確認するだけ)

- [ ] **Step 1: 一時ディレクトリと dired 2 窓を用意する**

```bash
mkdir -p /tmp/dnd-spike/src /tmp/dnd-spike/dst && touch /tmp/dnd-spike/src/a.txt
emacsclient --eval '(progn (setq dired-mouse-drag-files t) (delete-other-windows) (dired "/tmp/dnd-spike/src") (split-window-right) (other-window 1) (dired "/tmp/dnd-spike/dst") (other-window 1))'
```

- [ ] **Step 2: ユーザーに手で drag してもらう**

左の dired の `a.txt` を右の dired の中へドラッグ & ドロップしてもらう。
期待: 右に `a.txt` がコピーされる (macOS の drop は `private` で届き、dired 既定では copy になる。Shift 押下で move にはならないはず)。

- [ ] **Step 3: 結果を記録する**

```bash
ls /tmp/dnd-spike/src /tmp/dnd-spike/dst
```

コピーされていれば D&D の設計はそのまま (Task 6 で「既定 move」に置き換える)。
何も起きなければ、ここで止めて設計を見直す (spec の「D&D」節を修正してから続行)。

- [ ] **Step 4: 後片付け**

```bash
rm -rf /tmp/dnd-spike
```

---

### Task 2: init.el — dired と dired-subtree の設定

**Files:**
- Modify: `.emacs.d/init.el:1159-1189` (leaf dired) と直後に leaf dired-subtree を追加

**Interfaces:**
- Produces: `wamei/dired-context-menu-extras (menu click)` (context-menu-functions 用)、dired バッファで `TAB` = `dired-subtree-toggle`、`<backtab>` = `dired-subtree-cycle`、`C-c o` = `dired-do-open`

- [ ] **Step 1: dired-subtree を elpa にインストールする**

```bash
emacs --batch --eval '(progn (package-initialize) (package-refresh-contents) (package-install (quote dired-subtree)))' 2>&1 | tail -2
ls ~/.emacs.d/elpa | grep dired-subtree
```

Expected: `dired-subtree-2024…` が表示される。

- [ ] **Step 2: leaf dired を書き換える**

`.emacs.d/init.el` の `(leaf dired` ブロックを次に置き換える (既存の `:bind` / `:preface` の内容は保ち、追加分を足す)。

```elisp
(leaf dired
  :doc "diredの設定"
  :leaf-defer nil
  :bind (("C-x C-j" . dired-toggle-current-or-project-directory)
         (:dired-mode-map
         ("C-c C-s" . dired-toggle-sudo)
         ("C-c o" . dired-do-open)
         ("RET" . dired-find-file)
         ("a" . dired-find-alternate-file)
         ("^" . dired-up-directory)
         ("C-b" . backward-char)
         ("C-f" . forward-char)))
  :preface
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-isearch-filenames t)
  (setq dired-auto-revert-buffer t)
  (setq dired-listing-switches "--color=auto --group-directories-first -alLv")
  (setq insert-directory-program "/opt/homebrew/bin/gls")
  (put 'dired-find-alternate-file 'disabled nil)
  ;; ファイルを掴んで別の dired バッファへ落とせるようにする (down-mouse-1)。
  ;; 動かさずに離したときは mouse-1 が押し戻されるので通常のクリックと両立する。
  (setq dired-mouse-drag-files t)
  ;; 外部でのファイル変更に追従する (file-notify 経由)。
  (setq auto-revert-verbose nil)

  (defun dired-toggle-current-or-project-directory (n)
    "N が 1 ならカレントファイルの位置、4 (C-u) ならプロジェクトルートを dired で開く。"
    (interactive "p")
    (let ((project (project-current nil)))
      (cond ((= n 1)
             (dired-jump))
            ((= n 4)
             (if project
                 (project-dired)
               (dired-jump)))
            )))

  (defun wamei/dired-context-menu-extras (menu click)
    "右クリックメニューに dired のファイル操作を足す。`context-menu-functions' 用。
dired 組み込みの `dired-context-menu' (Find / Open / Open With) に続けて、
コピー・改名・削除・新規作成、ディレクトリ行なら展開/折りたたみを出す。"
    (when (and (derived-mode-p 'dired-mode)
               (mouse-posn-property (event-start click) 'dired-filename))
      ;; 右クリックした行に point を移す。メニューの各コマンドは point の
      ;; ファイル (またはマーク) に効くので、save-excursion で戻さない。
      (mouse-set-point click)
      (let ((file (dired-get-filename nil t)))
        (define-key menu [wamei-dired-separator] menu-bar-separator)
        (when (and file (file-directory-p file) (fboundp 'dired-subtree-toggle))
          (define-key menu [wamei-dired-toggle]
                      '(menu-item "Expand / Collapse" dired-subtree-toggle)))
        ;; dired-copy-filename-as-kill は引数 0 で絶対パスをコピーする
        (define-key menu [wamei-dired-copy-path]
                    '(menu-item "Copy Path" (lambda () (interactive) (dired-copy-filename-as-kill 0))))
        (define-key menu [wamei-dired-copy] '(menu-item "Copy…" dired-do-copy))
        (define-key menu [wamei-dired-rename] '(menu-item "Rename…" dired-do-rename))
        (define-key menu [wamei-dired-delete] '(menu-item "Delete…" dired-do-delete))
        (define-key menu [wamei-dired-new-file] '(menu-item "New File…" dired-create-empty-file))
        (define-key menu [wamei-dired-new-dir] '(menu-item "New Directory…" dired-create-directory))))
    menu)
  :hook
  (dired-mode-hook . auto-revert-mode)
  :config
  ;; 右クリックメニュー。dired-mode では dired-context-menu が組み込みで足される。
  (context-menu-mode 1)
  (add-hook 'context-menu-functions #'wamei/dired-context-menu-extras))

(leaf dired-subtree
  :doc "dired でディレクトリをその場で展開する"
  :ensure t
  :after dired
  :bind (:dired-mode-map
         ("TAB" . dired-subtree-toggle)
         ("<backtab>" . dired-subtree-cycle))
  :custom
  ;; 背景色で深さを表すのはやめ、line-prefix のインデントだけにする
  (dired-subtree-use-backgrounds . nil)
  :config
  ;; nerd-icons-dired は dired-after-readin-hook でしか付け直さないので、
  ;; 展開した行にもアイコンを付ける。revert 時の復元も dired-subtree-insert を
  ;; 通るのでここ一箇所で足りる。
  (with-eval-after-load 'nerd-icons-dired
    (add-hook 'dired-subtree-after-insert-hook #'nerd-icons-dired--refresh)))
```

注意: `:hook (dired-mode-hook . auto-revert-mode)` は leaf の書式どおり `(hook . function)` の形にする (メモリ: `:hook` は補完なし、書式ミスは黙って無視される)。

- [ ] **Step 3: leaf の警告が無いことを確認する**

```bash
emacs --batch -l ~/.emacs.d/init.el --eval '(kill-emacs 0)' 2>&1 | grep -E "Warning \(leaf\)|Error \(leaf\)"
```

Expected: 出力なし (treemacs-tab-bar のバックトレースは grep で落ちる)。

- [ ] **Step 4: 起動中の Emacs で確認する**

```bash
emacsclient --eval '(progn (setq dired-mouse-drag-files t auto-revert-verbose nil) (context-menu-mode 1) (add-hook (quote context-menu-functions) (function wamei/dired-context-menu-extras)) (require (quote dired-subtree)) (setq dired-subtree-use-backgrounds nil) (define-key dired-mode-map (kbd "TAB") (function dired-subtree-toggle)) (define-key dired-mode-map (kbd "<backtab>") (function dired-subtree-cycle)) (define-key dired-mode-map (kbd "C-c o") (function dired-do-open)) (add-hook (quote dired-subtree-after-insert-hook) (function nerd-icons-dired--refresh)) (add-hook (quote dired-mode-hook) (function auto-revert-mode)) (dired "~/.dotfiles"))'
```

ただし上の式を流す前に、init.el の `wamei/dired-context-menu-extras` の defun を `emacsclient --eval` で評価しておく (init.el から該当 defun をコピーして流す)。

ユーザー確認項目: `TAB` でディレクトリが展開され、展開行にアイコンが付く / 右クリックで Copy Path 等が出る / `C-c o` で Finder や既定アプリが開く / 別ターミナルで `touch ~/.dotfiles/x` すると数秒以内に一覧に現れる (確認後 `rm ~/.dotfiles/x`)。

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/init.el && git commit -m "$(cat <<'EOF'
dired に右クリックメニュー・D&D・auto-revert・dired-subtree を導入する

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: dired-tree.el — 展開記憶

**Files:**
- Create: `.emacs.d/dired-tree.el`
- Test: `.emacs.d/dired-tree-test.el`

**Interfaces:**
- Produces:
  - `wamei/dired-tree--normalize (dir)` → 絶対パス、末尾 `/` なし
  - `wamei/dired-tree--expanded-add (expanded dir)` / `wamei/dired-tree--expanded-remove (expanded dir)` → 新しいリスト (非破壊)
  - `wamei/dired-tree--children-to-reopen (expanded children)` → children のうち expanded に含まれるもの (出現順)
  - `wamei/dired-tree--subdirs-in (ov)` → overlay 範囲内のディレクトリ行の絶対パス
  - buffer-local `wamei/dired-tree--expanded`
  - minor mode `wamei/dired-tree-mode`

- [ ] **Step 1: テストファイルを作り、純関数のテストを書く**

`.emacs.d/dired-tree-test.el`:

```elisp
;;; dired-tree-test.el --- tests for dired-tree -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l dired-tree-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(package-initialize)
(require 'dired-subtree)
(load (expand-file-name "dired-tree.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; フィクスチャ

(defmacro wamei/dired-tree-test--with-tree (var &rest body)
  "一時ディレクトリに a/b/c.txt と a/d.txt と e.txt を作り、VAR に束縛して BODY を評価する。"
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "dired-tree-" t))))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "a/b" ,var) t)
           (write-region "" nil (expand-file-name "a/b/c.txt" ,var))
           (write-region "" nil (expand-file-name "a/d.txt" ,var))
           (write-region "" nil (expand-file-name "e.txt" ,var))
           ,@body)
       (delete-directory ,var t))))

(defmacro wamei/dired-tree-test--with-dired (root var &rest body)
  "ROOT の dired バッファ (wamei/dired-tree-mode 有効) を VAR に束縛して BODY を評価し、後で kill する。"
  (declare (indent 2))
  `(let ((,var (dired-noselect ,root)))
     (unwind-protect
         (with-current-buffer ,var
           (wamei/dired-tree-mode 1)
           ,@body)
       (kill-buffer ,var))))

;;; 展開記憶 (純関数)

(ert-deftest wamei/dired-tree-normalize-strips-trailing-slash ()
  (should (equal (wamei/dired-tree--normalize "/tmp/x/") "/tmp/x"))
  (should (equal (wamei/dired-tree--normalize "/tmp/x") "/tmp/x")))

(ert-deftest wamei/dired-tree-expanded-add-is-idempotent ()
  (let ((e (wamei/dired-tree--expanded-add nil "/tmp/x/")))
    (should (equal e '("/tmp/x")))
    (should (equal (wamei/dired-tree--expanded-add e "/tmp/x") e))))

(ert-deftest wamei/dired-tree-expanded-remove-keeps-descendants ()
  (let ((e '("/tmp/x/y" "/tmp/x" "/tmp/z")))
    (should (equal (wamei/dired-tree--expanded-remove e "/tmp/x/")
                   '("/tmp/x/y" "/tmp/z")))))

(ert-deftest wamei/dired-tree-children-to-reopen-keeps-order ()
  (should (equal (wamei/dired-tree--children-to-reopen
                  '("/tmp/x/c" "/tmp/x/a")
                  '("/tmp/x/a" "/tmp/x/b" "/tmp/x/c"))
                 '("/tmp/x/a" "/tmp/x/c"))))

;;; 展開記憶 (dired バッファ)

(ert-deftest wamei/dired-tree-insert-records-and-remove-forgets-only-self ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-insert)
      (dired-utils-goto-line (expand-file-name "a/b" root))
      (dired-subtree-insert)
      (should (equal (sort (copy-sequence wamei/dired-tree--expanded) #'string<)
                     (list (expand-file-name "a" root) (expand-file-name "a/b" root))))
      ;; 親 a を閉じる
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-toggle)
      (should (equal wamei/dired-tree--expanded (list (expand-file-name "a/b" root)))))))

(ert-deftest wamei/dired-tree-reinsert-reopens-remembered-children ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-insert)
      (dired-utils-goto-line (expand-file-name "a/b" root))
      (dired-subtree-insert)
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-toggle)
      (should-not (dired-utils-goto-line (expand-file-name "a/b/c.txt" root)))
      ;; もう一度開くと a/b も開いた状態で戻る
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-insert)
      (should (dired-utils-goto-line (expand-file-name "a/b/c.txt" root))))))

(provide 'dired-tree-test)
;;; dired-tree-test.el ends here
```

- [ ] **Step 2: 失敗を確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l dired-tree-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -5
```

Expected: `dired-tree.el` が無いので load エラー。

- [ ] **Step 3: dired-tree.el を書く**

```elisp
;;; dired-tree.el --- dired-subtree を木として扱う -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; dired-subtree でディレクトリを展開する dired バッファに共通する振る舞い。
;;
;; 1. 展開記憶 (`wamei/dired-tree--expanded')
;;    dired-subtree-remove は範囲を削除して中の overlay をまとめて捨てるので、
;;    親を閉じると子孫の展開状態が消える。展開中ディレクトリの集合をバッファ
;;    ローカルに持ち、展開時に子孫を開き直す。閉じたときは自身だけ忘れる。
;;
;; 2. 展開ディレクトリの監視 (Task 5)
;; 3. path までの展開 (Task 4)
;; 4. D&D の drop 先 (Task 6)
;;
;;; Code:

(require 'dired)
(require 'dired-subtree)
(require 'dired-hacks-utils)
(require 'seq)

;;; 展開記憶

(defvar-local wamei/dired-tree--expanded nil
  "展開中 (または閉じた親の下で展開したまま) のディレクトリ。絶対パス、末尾 / なし。")

(defun wamei/dired-tree--normalize (dir)
  "DIR を絶対パス・末尾 / なしに正規化する。"
  (directory-file-name (expand-file-name dir)))

(defun wamei/dired-tree--expanded-add (expanded dir)
  "EXPANDED に DIR を加えた新しいリスト。既にあればそのまま。"
  (let ((d (wamei/dired-tree--normalize dir)))
    (if (member d expanded) expanded (cons d expanded))))

(defun wamei/dired-tree--expanded-remove (expanded dir)
  "EXPANDED から DIR だけを外した新しいリスト。子孫は残す。"
  (remove (wamei/dired-tree--normalize dir) expanded))

(defun wamei/dired-tree--children-to-reopen (expanded children)
  "CHILDREN (絶対パス) のうち EXPANDED に入っているものを出現順で返す。"
  (seq-filter (lambda (c) (member (wamei/dired-tree--normalize c) expanded))
              children))

(defun wamei/dired-tree--subdirs-in (ov)
  "subtree overlay OV の範囲にあるディレクトリ行の絶対パス。"
  (let (dirs)
    (save-excursion
      (goto-char (overlay-start ov))
      (while (< (point) (overlay-end ov))
        (when (and (dired-subtree--dired-line-is-directory-or-link-p)
                   (dired-utils-get-filename))
          (push (dired-utils-get-filename) dirs))
        (forward-line 1)))
    (nreverse dirs)))

(defun wamei/dired-tree--after-insert ()
  "`dired-subtree-after-insert-hook' 用。展開を記憶し、覚えている子孫を開き直す。"
  (when wamei/dired-tree-mode
    (when-let* ((ov (dired-subtree--get-ov)))
      (let ((dir (overlay-get ov 'dired-subtree-name)))
        (setq wamei/dired-tree--expanded
              (wamei/dired-tree--expanded-add wamei/dired-tree--expanded dir))
        (dolist (child (wamei/dired-tree--children-to-reopen
                        wamei/dired-tree--expanded
                        (wamei/dired-tree--subdirs-in ov)))
          (save-excursion
            (when (and (dired-utils-goto-line child)
                       (not (dired-subtree--is-expanded-p)))
              (dired-subtree-insert))))))))

(defun wamei/dired-tree--before-remove (&rest _)
  "`dired-subtree-remove' の :before advice。閉じるディレクトリ自身だけ忘れる。"
  (when wamei/dired-tree-mode
    (when-let* ((ov (dired-subtree--get-ov)))
      (setq wamei/dired-tree--expanded
            (wamei/dired-tree--expanded-remove
             wamei/dired-tree--expanded (overlay-get ov 'dired-subtree-name))))))

;;; minor mode

(define-minor-mode wamei/dired-tree-mode
  "dired-subtree の展開を記憶し、展開ディレクトリを監視し、D&D の落下先を行から決める。"
  :lighter nil
  (if wamei/dired-tree-mode
      (progn
        (add-hook 'dired-subtree-after-insert-hook #'wamei/dired-tree--after-insert nil t)
        (advice-add 'dired-subtree-remove :before #'wamei/dired-tree--before-remove))
    (remove-hook 'dired-subtree-after-insert-hook #'wamei/dired-tree--after-insert t)))

(provide 'dired-tree)
;;; dired-tree.el ends here
```

注意: advice はグローバルだが中で `wamei/dired-tree-mode` を見るので他バッファには効かない。after-insert hook は buffer-local に足す。

- [ ] **Step 4: テストが通ることを確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l dired-tree-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

Expected: `Ran 6 tests, 6 results as expected`。

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/dired-tree.el .emacs.d/dired-tree-test.el && git commit -m "$(cat <<'EOF'
dired-tree を追加し、dired-subtree の展開状態を親を閉じても記憶する

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: dired-tree.el — path までの展開とカーソル保持

**Files:**
- Modify: `.emacs.d/dired-tree.el`
- Test: `.emacs.d/dired-tree-test.el`

**Interfaces:**
- Produces:
  - `wamei/dired-tree--inside-p (root file)` → FILE が ROOT 配下 (ROOT 自身は含まない) なら t
  - `wamei/dired-tree--ancestors (root file)` → ROOT 直下から FILE の親までのディレクトリ列 (絶対パス、末尾 / なし)。ROOT 直下のファイルなら nil
  - `wamei/dired-tree-expand-to (file)` → 祖先を展開して FILE の行へ移動、成功なら非 nil
  - `wamei/dired-tree-revert ()` → カーソル行のファイルを保って revert-buffer

- [ ] **Step 1: テストを追加する**

`dired-tree-test.el` の `(provide …)` の前に追加:

```elisp
;;; 祖先と展開

(ert-deftest wamei/dired-tree-inside-p ()
  (should (wamei/dired-tree--inside-p "/tmp/r/" "/tmp/r/a/b.txt"))
  (should-not (wamei/dired-tree--inside-p "/tmp/r" "/tmp/r"))
  (should-not (wamei/dired-tree--inside-p "/tmp/r" "/tmp/rx/a.txt"))
  (should-not (wamei/dired-tree--inside-p "/tmp/r" "/tmp/other/a.txt")))

(ert-deftest wamei/dired-tree-ancestors-lists-dirs-below-root ()
  (should (equal (wamei/dired-tree--ancestors "/tmp/r/" "/tmp/r/a/b/c.txt")
                 '("/tmp/r/a" "/tmp/r/a/b")))
  (should (equal (wamei/dired-tree--ancestors "/tmp/r" "/tmp/r/e.txt") nil)))

(ert-deftest wamei/dired-tree-expand-to-opens-ancestors-and-lands-on-file ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (should (wamei/dired-tree-expand-to (expand-file-name "a/b/c.txt" root)))
      (should (equal (dired-utils-get-filename) (expand-file-name "a/b/c.txt" root)))
      (should (equal (sort (copy-sequence wamei/dired-tree--expanded) #'string<)
                     (list (expand-file-name "a" root) (expand-file-name "a/b" root)))))))

(ert-deftest wamei/dired-tree-expand-to-returns-nil-outside-root ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (should-not (wamei/dired-tree-expand-to "/etc/hosts")))))

(ert-deftest wamei/dired-tree-revert-keeps-point-on-subtree-line ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (wamei/dired-tree-expand-to (expand-file-name "a/b/c.txt" root))
      (wamei/dired-tree-revert)
      (should (equal (dired-utils-get-filename) (expand-file-name "a/b/c.txt" root))))))
```

- [ ] **Step 2: 失敗を確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l dired-tree-test.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E "FAILED|Ran"
```

Expected: 新しい 5 本が FAILED (void-function)。

- [ ] **Step 3: 実装する**

`dired-tree.el` の `;;; minor mode` の前に追加:

```elisp
;;; path までの展開

(defun wamei/dired-tree--inside-p (root file)
  "FILE が ROOT の配下 (ROOT 自身を除く) なら t。"
  (let ((root (file-name-as-directory (expand-file-name root)))
        (file (expand-file-name file)))
    (and (string-prefix-p root file)
         (not (equal (directory-file-name root) (directory-file-name file))))))

(defun wamei/dired-tree--ancestors (root file)
  "ROOT 直下から FILE の親までのディレクトリ列 (絶対パス、末尾 / なし)。
FILE が ROOT 直下なら nil。FILE が ROOT 外でも nil。"
  (when (wamei/dired-tree--inside-p root file)
    (let ((root (wamei/dired-tree--normalize root))
          (dir (directory-file-name (file-name-directory (expand-file-name file))))
          acc)
      (while (not (equal dir root))
        (push dir acc)
        (setq dir (directory-file-name (file-name-directory dir))))
      acc)))

(defun wamei/dired-tree-expand-to (file)
  "FILE までの祖先ディレクトリを展開し、FILE の行へ移動する。
FILE がこのバッファのルート外、または途中の行が見つからなければ nil。"
  (let ((root (expand-file-name default-directory)))
    (when (wamei/dired-tree--inside-p root file)
      (catch 'missing
        (dolist (dir (wamei/dired-tree--ancestors root file))
          (unless (dired-utils-goto-line dir)
            (throw 'missing nil))
          (unless (dired-subtree--is-expanded-p)
            (dired-subtree-insert)))
        (dired-utils-goto-line (wamei/dired-tree--normalize file))))))

;;; カーソル保持

(defun wamei/dired-tree-revert ()
  "カーソル行のファイルを保って `revert-buffer' する。
dired 標準の復元は subtree 行では効かないので `dired-utils-goto-line' で戻す。"
  (let ((file (dired-utils-get-filename)))
    (revert-buffer)
    (when file
      (or (dired-utils-goto-line file)
          (dired-goto-file file)))))
```

- [ ] **Step 4: テストが通ることを確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l dired-tree-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

Expected: `Ran 11 tests, 11 results as expected`。

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/dired-tree.el .emacs.d/dired-tree-test.el && git commit -m "$(cat <<'EOF'
dired-tree に path までの展開とカーソル保持つき revert を追加する

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```

---

### Task 5: dired-tree.el — 展開ディレクトリの file-notify 監視

**Files:**
- Modify: `.emacs.d/dired-tree.el`
- Test: `.emacs.d/dired-tree-test.el`

**Interfaces:**
- Produces:
  - `wamei/dired-tree--watch-diff (current wanted)` → `(to-add . to-remove)`。両方ディレクトリの絶対パスのリスト
  - `wamei/dired-tree--visible-expanded ()` → いま overlay で展開中のディレクトリ
  - `wamei/dired-tree--reconcile-watches ()` → 監視を visible-expanded に合わせる
  - `wamei/dired-tree-refresh-hook` (変数): 監視による revert 後に呼ぶ関数のリスト (Task 8 で dired-git-status が登録する)

- [ ] **Step 1: テストを追加する**

```elisp
;;; 監視

(ert-deftest wamei/dired-tree-watch-diff ()
  (let ((diff (wamei/dired-tree--watch-diff '("/a" "/b") '("/b" "/c"))))
    (should (equal (car diff) '("/c")))
    (should (equal (cdr diff) '("/a")))))

(ert-deftest wamei/dired-tree-visible-expanded-follows-overlays ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (wamei/dired-tree-expand-to (expand-file-name "a/b/c.txt" root))
      (should (equal (sort (wamei/dired-tree--visible-expanded) #'string<)
                     (list (expand-file-name "a" root) (expand-file-name "a/b" root))))
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-toggle)
      (should-not (wamei/dired-tree--visible-expanded)))))

(ert-deftest wamei/dired-tree-reconcile-registers-watch-per-visible-dir ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (wamei/dired-tree-expand-to (expand-file-name "a/b/c.txt" root))
      (should (= (hash-table-count wamei/dired-tree--watches) 2))
      (dired-utils-goto-line (expand-file-name "a" root))
      (dired-subtree-toggle)
      (should (= (hash-table-count wamei/dired-tree--watches) 0)))))
```

- [ ] **Step 2: 失敗を確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l dired-tree-test.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E "FAILED|Ran"
```

- [ ] **Step 3: 実装する**

`dired-tree.el` に `(require 'filenotify)` と `(require 'subr-x)` (`hash-table-keys` 用) を追加し、`;;; minor mode` の前に追加:

```elisp
;;; 展開ディレクトリの監視

(defvar wamei/dired-tree-refresh-hook nil
  "監視による revert のあとに呼ぶ関数。dired-git-status が色の再取得に使う。")

(defvar wamei/dired-tree-revert-delay 0.3
  "file-notify の通知から revert までの待ち時間 (秒)。連続する通知をまとめる。")

(defvar-local wamei/dired-tree--watches nil
  "ディレクトリ → file-notify の descriptor。top ディレクトリは auto-revert が見るので含めない。")

(defvar-local wamei/dired-tree--revert-timer nil)

(defun wamei/dired-tree--watch-diff (current wanted)
  "CURRENT を WANTED に合わせるための (追加するもの . 外すもの)。"
  (cons (seq-remove (lambda (d) (member d current)) wanted)
        (seq-remove (lambda (d) (member d wanted)) current)))

(defun wamei/dired-tree--visible-expanded ()
  "overlay で展開中のディレクトリ (絶対パス、末尾 / なし)。"
  (delete-dups
   (mapcar (lambda (ov) (overlay-get ov 'dired-subtree-name))
           (dired-subtree--get-all-ovs))))

(defun wamei/dired-tree--schedule-revert (buffer)
  "BUFFER の revert を `wamei/dired-tree-revert-delay' 後に予約する。既存の予約は延ばす。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (timerp wamei/dired-tree--revert-timer)
        (cancel-timer wamei/dired-tree--revert-timer))
      (setq wamei/dired-tree--revert-timer
            (run-at-time wamei/dired-tree-revert-delay nil
                         (lambda ()
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (setq wamei/dired-tree--revert-timer nil)
                               (wamei/dired-tree-revert)
                               (run-hooks 'wamei/dired-tree-refresh-hook)))))))))

(defun wamei/dired-tree--watch-callback (buffer _event)
  "file-notify のコールバック。BUFFER の revert を予約する。"
  (wamei/dired-tree--schedule-revert buffer))

(defun wamei/dired-tree--reconcile-watches ()
  "監視対象を、いま見えている展開ディレクトリに合わせる。"
  (unless wamei/dired-tree--watches
    (setq wamei/dired-tree--watches (make-hash-table :test 'equal)))
  (let* ((current (hash-table-keys wamei/dired-tree--watches))
         (diff (wamei/dired-tree--watch-diff current (wamei/dired-tree--visible-expanded)))
         (buffer (current-buffer)))
    (dolist (dir (car diff))
      (when (file-directory-p dir)
        (ignore-errors
          (puthash dir
                   (file-notify-add-watch
                    dir '(change)
                    (lambda (event) (wamei/dired-tree--watch-callback buffer event)))
                   wamei/dired-tree--watches))))
    (dolist (dir (cdr diff))
      (ignore-errors (file-notify-rm-watch (gethash dir wamei/dired-tree--watches)))
      (remhash dir wamei/dired-tree--watches))))

(defun wamei/dired-tree--remove-all-watches ()
  "全ての監視を外す。バッファ kill 用。"
  (when wamei/dired-tree--watches
    (maphash (lambda (_dir desc) (ignore-errors (file-notify-rm-watch desc)))
             wamei/dired-tree--watches)
    (clrhash wamei/dired-tree--watches)))
```

そして既存の関数と minor mode を次のように変える:

- `wamei/dired-tree--after-insert` の末尾 (子孫の開き直しの後) に `(wamei/dired-tree--reconcile-watches)` を足す
- `wamei/dired-tree--after-remove` を新設して `dired-subtree-after-remove-hook` (buffer-local) に登録: `(when wamei/dired-tree-mode (wamei/dired-tree--reconcile-watches))`
- `dired-after-readin-hook` (buffer-local、depth 90 で dired-subtree の復元より後) にも `wamei/dired-tree--after-remove` を登録 (revert で消えたディレクトリの監視を外すため)
- minor mode の有効化で `(add-hook 'kill-buffer-hook #'wamei/dired-tree--remove-all-watches nil t)`、無効化で `remove-hook` と `wamei/dired-tree--remove-all-watches`

```elisp
(defun wamei/dired-tree--after-remove ()
  "`dired-subtree-after-remove-hook' / `dired-after-readin-hook' 用。監視を現状に合わせる。"
  (when wamei/dired-tree-mode
    (wamei/dired-tree--reconcile-watches)))

(define-minor-mode wamei/dired-tree-mode
  "dired-subtree の展開を記憶し、展開ディレクトリを監視し、D&D の落下先を行から決める。"
  :lighter nil
  (if wamei/dired-tree-mode
      (progn
        (add-hook 'dired-subtree-after-insert-hook #'wamei/dired-tree--after-insert nil t)
        (add-hook 'dired-subtree-after-remove-hook #'wamei/dired-tree--after-remove nil t)
        (add-hook 'dired-after-readin-hook #'wamei/dired-tree--after-remove 90 t)
        (add-hook 'kill-buffer-hook #'wamei/dired-tree--remove-all-watches nil t)
        (advice-add 'dired-subtree-remove :before #'wamei/dired-tree--before-remove))
    (remove-hook 'dired-subtree-after-insert-hook #'wamei/dired-tree--after-insert t)
    (remove-hook 'dired-subtree-after-remove-hook #'wamei/dired-tree--after-remove t)
    (remove-hook 'dired-after-readin-hook #'wamei/dired-tree--after-remove t)
    (remove-hook 'kill-buffer-hook #'wamei/dired-tree--remove-all-watches t)
    (wamei/dired-tree--remove-all-watches)))
```

- [ ] **Step 4: テストが通ることを確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l dired-tree-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

Expected: `Ran 14 tests, 14 results as expected`。

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/dired-tree.el .emacs.d/dired-tree-test.el && git commit -m "$(cat <<'EOF'
dired-tree で展開中ディレクトリを file-notify で監視し、変更時に revert する

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```

---

### Task 6: dired-tree.el — D&D の drop 先と init.el への組み込み

**Files:**
- Modify: `.emacs.d/dired-tree.el`
- Modify: `.emacs.d/init.el` (leaf dired-subtree の直後に leaf dired-tree)
- Test: `.emacs.d/dired-tree-test.el`

**Interfaces:**
- Produces:
  - `wamei/dired-tree--drop-target (directory-p file parent top)` → 落下先ディレクトリ (末尾 / あり)
  - `wamei/dired-tree--drop-destination (from target-dir)` → 移動先のフルパス
  - `wamei/dired-tree-drop-action` (変数、既定 `move`): drop が `private` / `copy` で届いたときに使う操作
  - `wamei/dired-tree-dnd-handle-file (uri action)` → dnd-protocol-alist 用ハンドラ

- [ ] **Step 1: テストを追加する**

```elisp
;;; drop 先

(ert-deftest wamei/dired-tree-drop-target-prefers-directory-line ()
  (should (equal (wamei/dired-tree--drop-target t "/r/a" "/r/" "/r/") "/r/a/"))
  (should (equal (wamei/dired-tree--drop-target nil "/r/a/x.txt" "/r/a" "/r/") "/r/a/"))
  (should (equal (wamei/dired-tree--drop-target nil nil nil "/r/") "/r/")))

(ert-deftest wamei/dired-tree-drop-destination-keeps-basename ()
  (should (equal (wamei/dired-tree--drop-destination "/src/x.txt" "/r/a/") "/r/a/x.txt"))
  (should (equal (wamei/dired-tree--drop-destination "/src/dir/" "/r/a/") "/r/a/dir")))

(ert-deftest wamei/dired-tree-resolve-action-maps-private-to-default ()
  (let ((wamei/dired-tree-drop-action 'move))
    (should (eq (wamei/dired-tree--resolve-action 'private) 'move))
    (should (eq (wamei/dired-tree--resolve-action 'copy) 'move))
    (should (eq (wamei/dired-tree--resolve-action 'link) 'link))))

(ert-deftest wamei/dired-tree-dnd-moves-file-into-subtree-directory ()
  (wamei/dired-tree-test--with-tree root
    (wamei/dired-tree-test--with-dired root buf
      (wamei/dired-tree-expand-to (expand-file-name "a/d.txt" root))
      ;; point は a/d.txt の行。落下先はその親 a/
      (let ((wamei/dired-tree-drop-action 'move))
        (wamei/dired-tree-dnd-handle-file
         (concat "file://" (expand-file-name "e.txt" root)) 'private))
      (should (file-exists-p (expand-file-name "a/e.txt" root)))
      (should-not (file-exists-p (expand-file-name "e.txt" root))))))
```

- [ ] **Step 2: 失敗を確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l dired-tree-test.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E "FAILED|Ran"
```

- [ ] **Step 3: 実装する**

`dired-tree.el` に `(require 'dired-aux)` `(require 'dnd)` を追加し、`;;; minor mode` の前に追加:

```elisp
;;; D&D の drop 先

(defvar wamei/dired-tree-drop-action 'move
  "drop が private / copy で届いたときの操作。move / copy のいずれか。
macOS の drop イベントは常に private で届き、dired 既定では copy になる。
Finder と同じく既定は移動にする。")

(defun wamei/dired-tree--drop-target (directory-p file parent top)
  "落下先ディレクトリ (末尾 / あり)。
行がディレクトリ (DIRECTORY-P) ならその FILE、ファイルなら PARENT、行に何も無ければ TOP。"
  (file-name-as-directory
   (cond ((and directory-p file) file)
         (file parent)
         (t top))))

(defun wamei/dired-tree--drop-destination (from target-dir)
  "FROM を TARGET-DIR に落としたときのフルパス。"
  (concat (file-name-as-directory target-dir)
          (file-name-nondirectory (directory-file-name from))))

(defun wamei/dired-tree--resolve-action (action)
  "dnd の ACTION を実際の操作に直す。private / copy は `wamei/dired-tree-drop-action'。"
  (if (memq action '(private copy)) wamei/dired-tree-drop-action action))

(defun wamei/dired-tree-drop-directory-at-point ()
  "point の行から落下先ディレクトリを決める。"
  (let* ((file (dired-utils-get-filename))
         (ov (dired-subtree--get-ov))
         (parent (if ov (overlay-get ov 'dired-subtree-name) (dired-current-directory))))
    (wamei/dired-tree--drop-target (and file (file-directory-p file))
                                   file parent (dired-current-directory))))

(defun wamei/dired-tree-dnd-handle-file (uri action)
  "URI のローカルファイルを point の行の落下先へ ACTION で運ぶ。`dnd-protocol-alist' 用。
`dired-dnd-handle-file' は落下先を `dired-current-directory' (top) に固定するので、
subtree 行を見て決める版。終わったら revert して行を作り直す。"
  (let* ((from (dnd-get-local-file-name uri t))
         (action (wamei/dired-tree--resolve-action action)))
    (when from
      (let ((to (wamei/dired-tree--drop-destination
                 from (wamei/dired-tree-drop-directory-at-point))))
        (unless (equal (directory-file-name from) (directory-file-name to))
          (let ((overwrite (and (file-exists-p to)
                                (y-or-n-p (format-message "Overwrite existing file `%s'? " to)))))
            (when (or overwrite (not (file-exists-p to)))
              (pcase action
                ('move (dired-rename-file from to overwrite))
                ('copy (dired-copy-file from to overwrite))
                ('link (make-symbolic-link from to overwrite)))
              (wamei/dired-tree-revert)
              (run-hooks 'wamei/dired-tree-refresh-hook))))
        action))))

(defun wamei/dired-tree--setup-dnd ()
  "このバッファの `dnd-protocol-alist' の先頭に自前のハンドラを置く。"
  (setq-local dnd-protocol-alist
              (cons '("^file:" . wamei/dired-tree-dnd-handle-file)
                    (default-value 'dnd-protocol-alist))))
```

minor mode の有効化に `(wamei/dired-tree--setup-dnd)`、無効化に `(kill-local-variable 'dnd-protocol-alist)` を足す。

注意: `dired-rename-file` はソース側の dired バッファの行も直す (dired-buffers 経由)。移動元が別バッファでも auto-revert が拾う。

- [ ] **Step 4: テストが通ることを確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l dired-tree-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

Expected: `Ran 18 tests, 18 results as expected`。

- [ ] **Step 5: init.el に leaf dired-tree を足す**

leaf dired-subtree の直後:

```elisp
(leaf dired-tree
  :doc "dired-subtree の展開記憶、展開ディレクトリの監視、D&D の落下先"
  :ensure nil
  ;; :after は付けない。dired-tree.el が dired-subtree を require するので
  ;; :preface の load 時点で両方読み込まれる。:after を付けると :hook の登録が
  ;; eval-after-load に包まれて、読む順が変わったときに黙って効かなくなる。
  :preface
  ;; 実体は dired-tree.el。init.el は symlink なので実体の隣から読む。
  (load (expand-file-name "dired-tree"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  :hook
  (dired-mode-hook . wamei/dired-tree-mode))
```

- [ ] **Step 6: leaf の警告確認と GUI 確認**

```bash
emacs --batch -l ~/.emacs.d/init.el --eval '(kill-emacs 0)' 2>&1 | grep -E "Warning \(leaf\)|Error \(leaf\)"
emacsclient --eval '(progn (load "~/.emacs.d/dired-tree.el") (add-hook (quote dired-mode-hook) (function wamei/dired-tree-mode)) (dired "~/.dotfiles"))'
```

ユーザー確認: `TAB` で `.emacs.d` を展開 → その中の `docs` 相当のディレクトリも展開 → 親を閉じて再度開くと子も開いたまま / 展開中ディレクトリで外部からファイルを作ると一覧が更新される / 別 dired からファイルを subtree の行へ drop すると移動される。

- [ ] **Step 7: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/dired-tree.el .emacs.d/dired-tree-test.el .emacs.d/init.el && git commit -m "$(cat <<'EOF'
dired-tree で D&D の落下先を subtree 行から決め、既定を移動にする

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```

---

### Task 7: dired-git-status.el — porcelain のパースと伝播

**Files:**
- Create: `.emacs.d/dired-git-status.el`
- Test: `.emacs.d/dired-git-status-test.el`

**Interfaces:**
- Produces:
  - `wamei/dired-git-status--parse (output root)` → hash (絶対パス、末尾 / なし → state)。state は `modified` / `added` / `untracked` / `renamed` / `conflict`
  - `wamei/dired-git-status--propagate (table root)` → 親ディレクトリに状態を足した新しい hash
  - `wamei/dired-git-status--code-to-state (xy)` → 2 文字の XY から state (無視なら nil)

- [ ] **Step 1: テストを書く**

`.emacs.d/dired-git-status-test.el`:

```elisp
;;; dired-git-status-test.el --- tests for dired-git-status -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l dired-git-status-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(package-initialize)
(load (expand-file-name "dired-git-status.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun wamei/dired-git-status-test--alist (table)
  "TABLE をソート済み alist にする。"
  (let (acc)
    (maphash (lambda (k v) (push (cons k v) acc)) table)
    (sort acc (lambda (a b) (string< (car a) (car b))))))

;;; XY → state

(ert-deftest wamei/dired-git-status-code-to-state ()
  (should (eq (wamei/dired-git-status--code-to-state "??") 'untracked))
  (should (eq (wamei/dired-git-status--code-to-state " M") 'modified))
  (should (eq (wamei/dired-git-status--code-to-state "M ") 'modified))
  (should (eq (wamei/dired-git-status--code-to-state " D") 'modified))
  (should (eq (wamei/dired-git-status--code-to-state "A ") 'added))
  (should (eq (wamei/dired-git-status--code-to-state "AM") 'added))
  (should (eq (wamei/dired-git-status--code-to-state "R ") 'renamed))
  (should (eq (wamei/dired-git-status--code-to-state "UU") 'conflict))
  (should (eq (wamei/dired-git-status--code-to-state "AA") 'conflict))
  (should-not (wamei/dired-git-status--code-to-state "!!")))

;;; パース

(ert-deftest wamei/dired-git-status-parse-handles-z-separated-entries ()
  (let ((table (wamei/dired-git-status--parse
                (concat " M src/a.el\0?? new.txt\0A  b.el\0")
                "/r/")))
    (should (equal (wamei/dired-git-status-test--alist table)
                   '(("/r/b.el" . added)
                     ("/r/new.txt" . untracked)
                     ("/r/src/a.el" . modified))))))

(ert-deftest wamei/dired-git-status-parse-rename-uses-new-path ()
  ;; -z ではリネームは "R  新\0旧\0" の順で来る
  (let ((table (wamei/dired-git-status--parse "R  new.el\0old.el\0" "/r")))
    (should (equal (wamei/dired-git-status-test--alist table)
                   '(("/r/new.el" . renamed))))))

(ert-deftest wamei/dired-git-status-parse-ignores-empty-output ()
  (should (= (hash-table-count (wamei/dired-git-status--parse "" "/r")) 0)))

;;; 伝播

(ert-deftest wamei/dired-git-status-propagate-marks-ancestors-modified ()
  (let* ((table (wamei/dired-git-status--parse " M src/deep/a.el\0" "/r"))
         (out (wamei/dired-git-status--propagate table "/r")))
    (should (equal (wamei/dired-git-status-test--alist out)
                   '(("/r/src" . modified)
                     ("/r/src/deep" . modified)
                     ("/r/src/deep/a.el" . modified))))))

(ert-deftest wamei/dired-git-status-propagate-conflict-wins ()
  (let* ((table (wamei/dired-git-status--parse " M src/a.el\0UU src/b.el\0" "/r"))
         (out (wamei/dired-git-status--propagate table "/r")))
    (should (eq (gethash "/r/src" out) 'conflict))))

(ert-deftest wamei/dired-git-status-propagate-does-not-mark-root ()
  (let* ((table (wamei/dired-git-status--parse "?? x.txt\0" "/r"))
         (out (wamei/dired-git-status--propagate table "/r")))
    (should-not (gethash "/r" out))))

(provide 'dired-git-status-test)
;;; dired-git-status-test.el ends here
```

- [ ] **Step 2: 失敗を確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l dired-git-status-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

- [ ] **Step 3: 実装する**

`.emacs.d/dired-git-status.el`:

```elisp
;;; dired-git-status.el --- dired のファイル名に git の状態で色を付ける -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; treemacs-git-mode の代替。プロジェクトルートで `git status --porcelain=v1 -z'
;; を非同期に取り、path → state の表にして、変更を含むディレクトリにも伝播させ、
;; dired の各行 (dired-subtree の展開行を含む) のファイル名に overlay で face を当てる。
;;
;; - パース (`wamei/dired-git-status--parse') と伝播 (`--propagate') は純関数
;; - 取得はルート単位で 1 回。結果は `wamei/dired-git-status--cache' に置き、
;;   同じルートを見る全 dired バッファに配る
;; - 更新契機: バッファ表示 / revert / magit の refresh / dired-tree の監視
;;
;;; Code:

(require 'dired)
(require 'project)
(require 'subr-x)

;;; face

(defgroup wamei/dired-git-status nil "dired のファイル名を git の状態で色分けする。" :group 'dired)

(defface wamei/dired-git-status-modified '((t (:foreground "#e5c07b")))
  "変更されたファイル、または変更を含むディレクトリ。")
(defface wamei/dired-git-status-added '((t (:foreground "#98c379")))
  "index に追加されたファイル。")
(defface wamei/dired-git-status-untracked '((t (:foreground "#7ec699")))
  "未追跡のファイル。")
(defface wamei/dired-git-status-renamed '((t (:foreground "#61afef")))
  "リネームされたファイル。")
(defface wamei/dired-git-status-conflict '((t (:foreground "#e06c75" :weight bold)))
  "コンフリクト中のファイル、またはそれを含むディレクトリ。")

(defconst wamei/dired-git-status--faces
  '((modified . wamei/dired-git-status-modified)
    (added . wamei/dired-git-status-added)
    (untracked . wamei/dired-git-status-untracked)
    (renamed . wamei/dired-git-status-renamed)
    (conflict . wamei/dired-git-status-conflict)))

;;; パース (純関数)

(defun wamei/dired-git-status--code-to-state (xy)
  "porcelain の 2 文字 XY を state に直す。無視するもの (!!) は nil。"
  (let ((x (aref xy 0)) (y (aref xy 1)))
    (cond ((and (eq x ??) (eq y ??)) 'untracked)
          ((and (eq x ?!) (eq y ?!)) nil)
          ((or (eq x ?U) (eq y ?U) (and (eq x ?A) (eq y ?A)) (and (eq x ?D) (eq y ?D))) 'conflict)
          ((memq x '(?R ?C)) 'renamed)
          ((eq x ?A) 'added)
          (t 'modified))))

(defun wamei/dired-git-status--parse (output root)
  "`git status --porcelain=v1 -z' の OUTPUT を ROOT からの絶対パス → state の hash にする。
エントリは \"XY path\\0\"、リネームは \"XY new\\0old\\0\"。"
  (let ((table (make-hash-table :test 'equal))
        (root (file-name-as-directory (expand-file-name root)))
        (fields (split-string output "\0" t)))
    (while fields
      (let* ((entry (pop fields))
             (xy (substring entry 0 2))
             (path (substring entry 3))
             (state (wamei/dired-git-status--code-to-state xy)))
        (when (memq (aref xy 0) '(?R ?C))
          (pop fields))                 ; 旧パスは捨てる
        (when state
          (puthash (directory-file-name (concat root path)) state table))))
    table))

(defun wamei/dired-git-status--propagate (table root)
  "TABLE の各パスの祖先 (ROOT 自身は除く) に状態を足した新しい hash。
子に conflict があれば conflict、それ以外は modified。"
  (let ((out (copy-hash-table table))
        (root (directory-file-name (expand-file-name root))))
    (maphash
     (lambda (path state)
       (let ((dir (directory-file-name (file-name-directory path)))
             (mark (if (eq state 'conflict) 'conflict 'modified)))
         (while (and (not (equal dir root))
                     (string-prefix-p (concat root "/") (concat dir "/")))
           (unless (eq (gethash dir out) 'conflict)
             (puthash dir mark out))
           (setq dir (directory-file-name (file-name-directory dir))))))
     table)
    out))

(provide 'dired-git-status)
;;; dired-git-status.el ends here
```

注意: `--untracked-files=all` なのでディレクトリ自体のエントリは来ない。祖先は「既に conflict なら保つ、それ以外は mark で上書き」だけでよい。テスト `propagate-conflict-wins` は maphash の順序に依らず `/r/src` が `conflict` になることを確かめる。

- [ ] **Step 4: テストが通ることを確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l dired-git-status-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

Expected: `Ran 8 tests, 8 results as expected`。

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/dired-git-status.el .emacs.d/dired-git-status-test.el && git commit -m "$(cat <<'EOF'
dired-git-status を追加し、git status --porcelain のパースと親への伝播を実装する

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```

---

### Task 8: dired-git-status.el — 非同期取得・キャッシュ・描画・init.el

**Files:**
- Modify: `.emacs.d/dired-git-status.el`
- Modify: `.emacs.d/init.el` (leaf dired-tree の直後に leaf dired-git-status)
- Test: `.emacs.d/dired-git-status-test.el`

**Interfaces:**
- Produces:
  - `wamei/dired-git-status--root ()` → このバッファの git ルート (絶対パス、末尾 / なし) か nil
  - `wamei/dired-git-status--decorate (table)` → このバッファの各行に overlay を張る
  - `wamei/dired-git-status-refresh ()` (interactive) → このバッファのルートを再取得して配る
  - `wamei/dired-git-status-mode` (minor mode)
  - `wamei/dired-git-status--cache` (root → propagate 済み hash)

- [ ] **Step 1: テストを追加する**

```elisp
;;; 描画とルート判定

(ert-deftest wamei/dired-git-status-root-is-nil-outside-git ()
  (let ((dir (file-name-as-directory (make-temp-file "dgs-" t))))
    (unwind-protect
        (with-current-buffer (dired-noselect dir)
          (should-not (wamei/dired-git-status--root))
          (kill-buffer))
      (delete-directory dir t))))

(ert-deftest wamei/dired-git-status-decorate-puts-face-on-filename ()
  (let ((dir (file-name-as-directory (make-temp-file "dgs-" t))))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "a.el" dir))
          (write-region "" nil (expand-file-name "b.el" dir))
          (with-current-buffer (dired-noselect dir)
            (let ((table (make-hash-table :test 'equal)))
              (puthash (expand-file-name "a.el" dir) 'modified table)
              (wamei/dired-git-status--decorate table)
              (dired-goto-file (expand-file-name "a.el" dir))
              (should (seq-find (lambda (ov) (overlay-get ov 'wamei/dired-git-status-overlay))
                                (overlays-at (point))))
              (should (eq (overlay-get (seq-find (lambda (ov) (overlay-get ov 'wamei/dired-git-status-overlay))
                                                 (overlays-at (point)))
                                       'face)
                          'wamei/dired-git-status-modified))
              (dired-goto-file (expand-file-name "b.el" dir))
              (should-not (seq-find (lambda (ov) (overlay-get ov 'wamei/dired-git-status-overlay))
                                    (overlays-at (point)))))
            (kill-buffer)))
      (delete-directory dir t))))

(ert-deftest wamei/dired-git-status-decorate-replaces-old-overlays ()
  (let ((dir (file-name-as-directory (make-temp-file "dgs-" t))))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "a.el" dir))
          (with-current-buffer (dired-noselect dir)
            (let ((table (make-hash-table :test 'equal)))
              (puthash (expand-file-name "a.el" dir) 'modified table)
              (wamei/dired-git-status--decorate table)
              (wamei/dired-git-status--decorate (make-hash-table :test 'equal))
              (should-not (seq-find (lambda (ov) (overlay-get ov 'wamei/dired-git-status-overlay))
                                    (overlays-in (point-min) (point-max)))))
            (kill-buffer)))
      (delete-directory dir t))))

(ert-deftest wamei/dired-git-status-fetch-colors-modified-file-in-real-repo ()
  "実際に git init したリポジトリで非同期取得が終わるまで待ち、色が付くこと。"
  (skip-unless (executable-find "git"))
  (let ((dir (file-name-as-directory (make-temp-file "dgs-" t))))
    (unwind-protect
        (let ((default-directory dir))
          (call-process "git" nil nil nil "init" "-q")
          (write-region "x" nil (expand-file-name "tracked.el" dir))
          (call-process "git" nil nil nil "add" "tracked.el")
          (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "init")
          (write-region "y" nil (expand-file-name "tracked.el" dir))
          (write-region "" nil (expand-file-name "new.el" dir))
          (with-current-buffer (dired-noselect dir)
            (wamei/dired-git-status-mode 1)
            (let ((deadline (+ (float-time) 5)))
              (while (and (< (float-time) deadline)
                          (not (gethash (directory-file-name dir) wamei/dired-git-status--cache)))
                (accept-process-output nil 0.1)))
            (let ((table (gethash (directory-file-name dir) wamei/dired-git-status--cache)))
              (should table)
              (should (eq (gethash (expand-file-name "tracked.el" dir) table) 'modified))
              (should (eq (gethash (expand-file-name "new.el" dir) table) 'untracked)))
            (kill-buffer)))
      (delete-directory dir t))))
```

- [ ] **Step 2: 失敗を確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l dired-git-status-test.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E "FAILED|Ran"
```

- [ ] **Step 3: 実装する**

`dired-git-status.el` の `(provide …)` の前に追加:

```elisp
;;; ルート

(defun wamei/dired-git-status--root ()
  "このバッファの git ルート (絶対パス、末尾 / なし)。git 管理外なら nil。
project-current を使い、その root に .git が無ければ nil。"
  (when-let* ((project (project-current nil))
              (root (directory-file-name (expand-file-name (project-root project)))))
    (when (file-exists-p (expand-file-name ".git" root))
      root)))

;;; 取得とキャッシュ

(defvar wamei/dired-git-status--cache (make-hash-table :test 'equal)
  "ルート → propagate 済みの path → state 表。")

(defvar wamei/dired-git-status--running (make-hash-table :test 'equal)
  "ルート → 実行中のプロセス。値が `again' 付きなら完了後にもう 1 回走らせる。")

(defun wamei/dired-git-status--buffers-for (root)
  "ROOT を見ている、mode 有効な dired バッファ。"
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (and (derived-mode-p 'dired-mode)
                       (bound-and-true-p wamei/dired-git-status-mode)
                       (equal (wamei/dired-git-status--root) root))))
              (buffer-list)))

(defun wamei/dired-git-status--distribute (root)
  "ROOT のキャッシュを、ROOT を見ている全バッファに描く。"
  (when-let* ((table (gethash root wamei/dired-git-status--cache)))
    (dolist (buf (wamei/dired-git-status--buffers-for root))
      (with-current-buffer buf
        (wamei/dired-git-status--decorate table)))))

(defun wamei/dired-git-status--fetch (root)
  "ROOT で git status を非同期に走らせ、終わったらキャッシュして配る。
実行中なら完了後にもう 1 回だけ走るよう印を付ける。"
  (if (gethash root wamei/dired-git-status--running)
      (process-put (gethash root wamei/dired-git-status--running) 'again t)
    (let* ((buffer (generate-new-buffer " *dired-git-status*"))
           (default-directory (file-name-as-directory root))
           (process
            (make-process
             :name "dired-git-status"
             :buffer buffer
             :command '("git" "status" "--porcelain=v1" "-z" "--untracked-files=all")
             :noquery t
             :sentinel
             (lambda (proc _event)
               (unless (process-live-p proc)
                 (let ((again (process-get proc 'again)))
                   (remhash root wamei/dired-git-status--running)
                   (when (and (zerop (process-exit-status proc)) (buffer-live-p buffer))
                     (puthash root
                              (wamei/dired-git-status--propagate
                               (wamei/dired-git-status--parse
                                (with-current-buffer buffer (buffer-string)) root)
                               root)
                              wamei/dired-git-status--cache)
                     (wamei/dired-git-status--distribute root))
                   (when (buffer-live-p buffer) (kill-buffer buffer))
                   (when again (wamei/dired-git-status--fetch root))))))))
      (puthash root process wamei/dired-git-status--running))))

;;; 描画

(defun wamei/dired-git-status--clear ()
  "このバッファの overlay を全部消す。"
  (remove-overlays (point-min) (point-max) 'wamei/dired-git-status-overlay t))

(defun wamei/dired-git-status--decorate (table)
  "TABLE に従い、このバッファの各行のファイル名に face を当てる。"
  (wamei/dired-git-status--clear)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let* ((file (dired-get-filename nil t))
                  (state (gethash (directory-file-name file) table))
                  (face (alist-get state wamei/dired-git-status--faces))
                  (beg (dired-move-to-filename))
                  (end (dired-move-to-end-of-filename t)))
        (let ((ov (make-overlay beg end)))
          (overlay-put ov 'wamei/dired-git-status-overlay t)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'face face)))
      (forward-line 1))))

;;; 更新契機

(defun wamei/dired-git-status-refresh ()
  "このバッファのルートの git 状態を再取得して、同じルートの全バッファに配る。"
  (interactive)
  (when-let* ((root (wamei/dired-git-status--root)))
    (wamei/dired-git-status--fetch root)))

(defun wamei/dired-git-status--redecorate ()
  "readin / subtree 展開のあと、キャッシュがあれば描き直し、無ければ取得する。"
  (when wamei/dired-git-status-mode
    (when-let* ((root (wamei/dired-git-status--root)))
      (if-let* ((table (gethash root wamei/dired-git-status--cache)))
          (wamei/dired-git-status--decorate table)
        (wamei/dired-git-status--fetch root)))))

(defun wamei/dired-git-status--refresh-all-visible ()
  "表示中の dired バッファのルートを全部再取得する。magit の refresh 後に呼ぶ。"
  (let (roots)
    (dolist (win (window-list nil 'no-minibuf))
      (with-current-buffer (window-buffer win)
        (when (and (derived-mode-p 'dired-mode) (bound-and-true-p wamei/dired-git-status-mode))
          (when-let* ((root (wamei/dired-git-status--root)))
            (cl-pushnew root roots :test #'equal)))))
    (mapc #'wamei/dired-git-status--fetch roots)))

(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook #'wamei/dired-git-status--refresh-all-visible))

;;; minor mode

(define-minor-mode wamei/dired-git-status-mode
  "dired のファイル名に git の状態で色を付ける。"
  :lighter nil
  (if wamei/dired-git-status-mode
      (progn
        (add-hook 'dired-after-readin-hook #'wamei/dired-git-status--redecorate 95 t)
        (add-hook 'dired-subtree-after-insert-hook #'wamei/dired-git-status--redecorate 95 t)
        (add-hook 'wamei/dired-tree-refresh-hook #'wamei/dired-git-status-refresh nil t)
        (wamei/dired-git-status-refresh))
    (remove-hook 'dired-after-readin-hook #'wamei/dired-git-status--redecorate t)
    (remove-hook 'dired-subtree-after-insert-hook #'wamei/dired-git-status--redecorate t)
    (remove-hook 'wamei/dired-tree-refresh-hook #'wamei/dired-git-status-refresh t)
    (wamei/dired-git-status--clear)))
```

`(require 'cl-lib)` をファイル冒頭に足す (`cl-pushnew`)。`dired-subtree-after-insert-hook` と `wamei/dired-tree-refresh-hook` は未ロードでも `add-hook` は変数を作るだけなので require 不要。

- [ ] **Step 4: テストが通ることを確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l dired-git-status-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

Expected: `Ran 12 tests, 12 results as expected`。

- [ ] **Step 5: init.el に leaf を足す**

leaf dired-tree の直後:

```elisp
(leaf dired-git-status
  :doc "dired のファイル名を git の状態で色分けする (treemacs-git-mode の代替)"
  :ensure nil
  :preface
  (load (expand-file-name "dired-git-status"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  :hook
  (dired-mode-hook . wamei/dired-git-status-mode))
```

- [ ] **Step 6: leaf の警告確認と GUI 確認**

```bash
emacs --batch -l ~/.emacs.d/init.el --eval '(kill-emacs 0)' 2>&1 | grep -E "Warning \(leaf\)|Error \(leaf\)"
emacsclient --eval '(progn (load "~/.emacs.d/dired-git-status.el") (add-hook (quote dired-mode-hook) (function wamei/dired-git-status-mode)) (dired "~/.dotfiles"))'
```

ユーザー確認: `~/.dotfiles` で変更中のファイル (この作業の init.el など) が黄色、`.emacs.d` ディレクトリも黄色 / `touch ~/.dotfiles/zz` で緑になり、`rm` で消える / `TAB` 展開した行にも色が付く。

- [ ] **Step 7: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/dired-git-status.el .emacs.d/dired-git-status-test.el .emacs.d/init.el && git commit -m "$(cat <<'EOF'
dired-git-status で git の状態を非同期に取得し、dired の行に色を付ける

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```

---

### Task 9: project-tabs.el — 基準 window の関数を公開する

**Files:**
- Modify: `.emacs.d/project-tabs.el:40-46`
- Test: `.emacs.d/project-tabs-test.el`

**Interfaces:**
- Produces: `wamei/project-tabs-main-window ()` → 選択 window が side window (no-other-window) なら直近の通常 window、そうでなければ選択 window。旧名 `wamei/project-tabs--name-window` は alias で残す

- [ ] **Step 1: テストを追加する**

`project-tabs-test.el` の `;;; タブ名` テスト群の後に:

```elisp
(ert-deftest wamei/project-tabs-main-window-skips-side-window ()
  (let* ((main (selected-window))
         (side (split-window main nil 'left)))
    (unwind-protect
        (progn
          (set-window-parameter side 'no-other-window t)
          (select-window side)
          (should (eq (wamei/project-tabs-main-window) main))
          (select-window main)
          (should (eq (wamei/project-tabs-main-window) main)))
      (delete-window side))))
```

- [ ] **Step 2: 失敗を確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-tabs-test.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E "FAILED|Ran"
```

Expected: 1 本 FAILED (void-function)。

- [ ] **Step 3: 実装する**

`project-tabs.el` の `wamei/project-tabs--name-window` を次に置き換える:

```elisp
(defun wamei/project-tabs-main-window ()
  "タブの本文とみなす window。
選択 window が side window (no-other-window 付き) なら直近の通常 window。
タブ名の根拠、サイドバーが従うバッファ、サイドバーからファイルを開く先に使う。"
  (if (window-parameter (selected-window) 'no-other-window)
      (or (get-mru-window nil nil t t) (selected-window))
    (selected-window)))

(defalias 'wamei/project-tabs--name-window #'wamei/project-tabs-main-window)
```

- [ ] **Step 4: テストが通ることを確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-tabs-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

Expected: 全部 pass (treemacs ガードのテストはまだ残っていて pass する)。

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/project-tabs.el .emacs.d/project-tabs-test.el && git commit -m "$(cat <<'EOF'
project-tabs の基準 window の判定を wamei/project-tabs-main-window として公開する

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```

---

### Task 10: project-sidebar.el — バッファ・表示・トグル

**Files:**
- Create: `.emacs.d/project-sidebar.el`
- Test: `.emacs.d/project-sidebar-test.el`
- Modify: `.emacs.d/init.el` (leaf treemacs-tab-bar の直後に leaf project-sidebar。`C-x C-n` は後に定義されるこちらが勝つ)

**Interfaces:**
- Consumes: `wamei/project-tabs-main-window` (Task 9)、`wamei/dired-tree-mode` (Task 3)
- Produces:
  - `wamei/project-sidebar--buffer-name (root)` → `" *sidebar: <name>*"`
  - `wamei/project-sidebar--root-for (dir)` → DIR のプロジェクトルート (末尾 / あり)、無ければ DIR
  - `wamei/project-sidebar-buffer (root)` → ROOT の sidebar バッファ (無ければ作る)
  - `wamei/project-sidebar-window (&optional frame)` → FRAME の左 side window で sidebar を表示しているもの、無ければ nil
  - `wamei/project-sidebar-show (dir)` → DIR のプロジェクトの sidebar を side window に出し、window を返す (選択しない)
  - `wamei/project-sidebar-toggle (&optional arg)` (interactive)
  - `wamei/project-sidebar-mode` (minor mode、buffer-local)、`wamei/project-sidebar-mode-map`
  - `wamei/project-sidebar-root` (face)

- [ ] **Step 1: テストを書く**

`.emacs.d/project-sidebar-test.el`:

```elisp
;;; project-sidebar-test.el --- tests for project-sidebar -*- lexical-binding: t; -*-
;;; Commentary:
;; emacs -Q --batch -l project-sidebar-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'project)
(package-initialize)
(require 'dired-subtree)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "project-tabs.el" dir) nil t)
  (load (expand-file-name "dired-tree.el" dir) nil t)
  (load (expand-file-name "project-sidebar.el" dir) nil t))

;;; フィクスチャ

(defmacro wamei/project-sidebar-test--with-project (var &rest body)
  "一時ディレクトリを transient プロジェクトにして VAR に束縛し BODY を評価する。
中に src/main.el と README を作る。"
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (make-temp-file "sidebar-" t)))
          (project-find-functions
           (list (lambda (dir)
                   (when (string-prefix-p ,var (expand-file-name dir))
                     (cons 'transient ,var))))))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "src" ,var) t)
           (write-region "" nil (expand-file-name "src/main.el" ,var))
           (write-region "" nil (expand-file-name "README" ,var))
           ,@body)
       (dolist (buf (buffer-list))
         (when (string-prefix-p " *sidebar: " (buffer-name buf))
           (kill-buffer buf)))
       (delete-directory ,var t))))

;;; バッファ

(ert-deftest wamei/project-sidebar-buffer-name-uses-project-name ()
  (should (equal (wamei/project-sidebar--buffer-name "/tmp/proj-a/") " *sidebar: proj-a*")))

(ert-deftest wamei/project-sidebar-root-for-falls-back-to-dir ()
  (let ((project-find-functions nil))
    (should (equal (wamei/project-sidebar--root-for "/tmp/nowhere/") "/tmp/nowhere/"))))

(ert-deftest wamei/project-sidebar-buffer-is-dired-with-modes ()
  (wamei/project-sidebar-test--with-project root
    (let ((buf (wamei/project-sidebar-buffer root)))
      (with-current-buffer buf
        (should (derived-mode-p 'dired-mode))
        (should wamei/project-sidebar-mode)
        (should wamei/dired-tree-mode)
        (should dired-hide-details-mode)
        (should (equal (expand-file-name default-directory) root))
        (should (string-match-p (file-name-nondirectory (directory-file-name root))
                                (format-mode-line header-line-format)))))))

(ert-deftest wamei/project-sidebar-buffer-is-reused ()
  (wamei/project-sidebar-test--with-project root
    (should (eq (wamei/project-sidebar-buffer root) (wamei/project-sidebar-buffer root)))))

(ert-deftest wamei/project-sidebar-buffer-does-not-hijack-plain-dired ()
  (wamei/project-sidebar-test--with-project root
    (let ((sidebar (wamei/project-sidebar-buffer root))
          (plain (dired-noselect root)))
      (unwind-protect
          (should-not (eq sidebar plain))
        (kill-buffer plain)))))

;;; 表示とトグル

(ert-deftest wamei/project-sidebar-show-displays-in-left-side-window ()
  (wamei/project-sidebar-test--with-project root
    (let ((win (wamei/project-sidebar-show root)))
      (unwind-protect
          (progn
            (should (window-live-p win))
            (should (eq (window-parameter win 'window-side) 'left))
            (should (window-parameter win 'no-other-window))
            (should (eq (wamei/project-sidebar-window) win)))
        (delete-window win)))))

(ert-deftest wamei/project-sidebar-toggle-cycles-open-focus-back-close ()
  (wamei/project-sidebar-test--with-project root
    (let ((main (selected-window)))
      (with-current-buffer (window-buffer main)
        (setq default-directory root))
      ;; 非表示 → 開いてフォーカス
      (wamei/project-sidebar-toggle)
      (let ((win (wamei/project-sidebar-window)))
        (should win)
        (should (eq (selected-window) win))
        ;; フォーカス中 → 元へ戻る (開いたまま)
        (wamei/project-sidebar-toggle)
        (should (eq (selected-window) main))
        (should (window-live-p win))
        ;; 表示中で未フォーカス → フォーカス
        (wamei/project-sidebar-toggle)
        (should (eq (selected-window) win))
        ;; C-u → 閉じる
        (wamei/project-sidebar-toggle '(4))
        (should-not (wamei/project-sidebar-window))))))

(provide 'project-sidebar-test)
;;; project-sidebar-test.el ends here
```

- [ ] **Step 2: 失敗を確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-sidebar-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

- [ ] **Step 3: project-sidebar.el を書く**

```elisp
;;; project-sidebar.el --- dired ベースのプロジェクトサイドバー -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; treemacs の代替。プロジェクトルートごとに 1 つの dired バッファを作り、
;; 左の side window に出す。タブ = プロジェクト (project-tabs.el) なので、
;; 各タブの window 構成が自分のプロジェクトの sidebar を持つ形になる。
;;
;; - バッファは dired そのもの。dired-tree (展開記憶・監視・D&D)、
;;   dired-git-status (色)、nerd-icons-dired、右クリックメニューがそのまま効く
;; - 見た目: 詳細を隠し、見出し行と . .. を隠し、header-line にプロジェクト名
;; - トグル (C-x C-n) は端末パネル (wamei/term-toggle) と同じ 4 態
;; - follow (Task 11)、マウス (Task 12)、desktop 復元 (Task 13)
;;
;;; Code:

(require 'dired)
(require 'dired-x)
(require 'project)
(require 'dired-tree)
(require 'project-tabs)

;;; 外観

(defface wamei/project-sidebar-root
  '((t (:inherit font-lock-keyword-face :weight bold :height 1.3)))
  "header-line に出すプロジェクト名。")

(defvar wamei/project-sidebar-width 35 "side window の幅 (桁)。")

;;; バッファ

(defun wamei/project-sidebar--root-for (dir)
  "DIR が属するプロジェクトのルート (末尾 / あり)。プロジェクト外なら DIR。"
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (if-let* ((project (project-current nil)))
        (file-name-as-directory (expand-file-name (project-root project)))
      default-directory)))

(defun wamei/project-sidebar--buffer-name (root)
  "ROOT の sidebar バッファ名。先頭空白でバッファ一覧から隠す。"
  (format " *sidebar: %s*" (file-name-nondirectory (directory-file-name root))))

(defun wamei/project-sidebar--hide-header-lines ()
  "先頭のディレクトリ見出し行を overlay で隠す。total 行は dired-hide-details が隠す。
`invisible' の値は専用シンボルにして `buffer-invisibility-spec' に足す
(dired-hide-details-mode が spec をリストにするので t では効かないことがある)。"
  (add-to-invisibility-spec 'wamei/project-sidebar-header)
  (remove-overlays (point-min) (point-max) 'wamei/project-sidebar-header t)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward dired-subdir-regexp nil t)
      (let ((ov (make-overlay (point-min) (line-beginning-position 2))))
        (overlay-put ov 'wamei/project-sidebar-header t)
        (overlay-put ov 'invisible 'wamei/project-sidebar-header)
        (overlay-put ov 'evaporate t)))))

(defun wamei/project-sidebar--unadvertise ()
  "この sidebar を `dired-buffers' から外し、通常の dired に再利用させない。
`dired-unadvertise' はディレクトリ単位で消すので同じディレクトリの通常 dired まで
外してしまう。自バッファのエントリだけ消す。"
  (setq dired-buffers
        (seq-remove (lambda (entry) (eq (cdr entry) (current-buffer))) dired-buffers)))

(defun wamei/project-sidebar--decorate ()
  "readin のあとに外観を整える。"
  (when wamei/project-sidebar-mode
    (wamei/project-sidebar--hide-header-lines)
    (wamei/project-sidebar--unadvertise)))

(defun wamei/project-sidebar--create (root)
  "ROOT の sidebar バッファを新しく作る。"
  (let ((buffer (let ((dired-buffers nil))   ; 通常の dired 一覧に登録させない
                  (dired-noselect root))))
    (with-current-buffer buffer
      (rename-buffer (wamei/project-sidebar--buffer-name root) t)
      (wamei/project-sidebar-mode 1)
      (wamei/project-sidebar--decorate))
    buffer))

(defun wamei/project-sidebar-buffer (root)
  "ROOT の sidebar バッファ。無ければ作る。"
  (let ((root (file-name-as-directory (expand-file-name root))))
    (or (seq-find (lambda (buf)
                    (with-current-buffer buf
                      (and (bound-and-true-p wamei/project-sidebar-mode)
                           (equal (expand-file-name default-directory) root))))
                  (buffer-list))
        (wamei/project-sidebar--create root))))

;;; window

(defun wamei/project-sidebar-window (&optional frame)
  "FRAME の左 side window のうち sidebar を表示しているもの。無ければ nil。"
  (seq-find (lambda (win)
              (and (eq (window-parameter win 'window-side) 'left)
                   (with-current-buffer (window-buffer win)
                     (bound-and-true-p wamei/project-sidebar-mode))))
            (window-list frame 'no-minibuf)))

(defun wamei/project-sidebar-show (dir)
  "DIR のプロジェクトの sidebar を side window に出し、その window を返す。選択はしない。"
  (let ((buffer (wamei/project-sidebar-buffer (wamei/project-sidebar--root-for dir))))
    (or (get-buffer-window buffer)
        (display-buffer buffer))))

(defun wamei/project-sidebar--back-window ()
  "sidebar から戻る先。直近の通常 window。"
  (get-mru-window nil t t t))

(defun wamei/project-sidebar-toggle (&optional arg)
  "sidebar へ出入りする。

- 非表示なら開いてフォーカスする
- 表示中でフォーカスが無ければフォーカスを移す
- フォーカス中なら元の window へ戻る (開いたまま)
- ARG (C-u) 付きなら閉じる

出すプロジェクトは `wamei/project-tabs-main-window' のバッファのもの。"
  (interactive "P")
  (let ((window (wamei/project-sidebar-window)))
    (cond
     (arg
      (when window (delete-window window)))
     ((and window (eq window (selected-window)))
      (when-let* ((back (wamei/project-sidebar--back-window)))
        (select-window back)))
     (window
      (select-window window))
     (t
      (let ((dir (with-current-buffer (window-buffer (wamei/project-tabs-main-window))
                   default-directory)))
        (select-window (wamei/project-sidebar-show dir)))))))

(defun wamei/project-sidebar-quit ()
  "元の window へ戻る。sidebar は開いたまま。"
  (interactive)
  (when-let* ((back (wamei/project-sidebar--back-window)))
    (select-window back)))

;;; minor mode

(defvar wamei/project-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'wamei/project-sidebar-quit)
    map)
  "sidebar バッファのキーマップ。dired-mode-map より優先される。")

(define-minor-mode wamei/project-sidebar-mode
  "この dired バッファをプロジェクトサイドバーとして扱う。"
  :lighter nil
  :keymap wamei/project-sidebar-mode-map
  (when wamei/project-sidebar-mode
    (wamei/dired-tree-mode 1)
    (dired-hide-details-mode 1)
    (dired-omit-mode 1)                 ; . と .. を隠す (既定の dired-omit-files)
    (setq-local dired-hide-details-hide-information-lines t)
    (setq-local truncate-lines t)
    (setq-local mouse-1-click-follows-link nil)
    (setq header-line-format
          (list (propertize (concat " " (file-name-nondirectory
                                         (directory-file-name default-directory)))
                            'face 'wamei/project-sidebar-root)))
    (add-hook 'dired-after-readin-hook #'wamei/project-sidebar--decorate 99 t)))

;;; display-buffer

(defun wamei/project-sidebar-setup ()
  "display-buffer-alist に sidebar の出し方を登録する。init.el から 1 回呼ぶ。"
  (add-to-list 'display-buffer-alist
               `("\\` \\*sidebar: "
                 (display-buffer-in-side-window)
                 (side . left)
                 (slot . 0)
                 (window-width . ,wamei/project-sidebar-width)
                 (dedicated . t)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t))))))

(provide 'project-sidebar)
;;; project-sidebar.el ends here
```

テストの `show` / `toggle` は `display-buffer-alist` の登録が必要なので、テストファイルの load の後に `(wamei/project-sidebar-setup)` を 1 行足す。

- [ ] **Step 4: テストが通ることを確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-sidebar-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

Expected: `Ran 7 tests, 7 results as expected`。batch では `display-buffer-in-side-window` が動く (frame はある)。`dired-omit-mode` が batch で警告を出すなら `(let ((dired-omit-verbose nil)) …)` で黙らせる。

- [ ] **Step 5: init.el に leaf を足す**

`(leaf dired-git-status …)` の直後 (project-sidebar.el は dired-tree と project-tabs を require するので、両方が load された後でなければならない。treemacs の leaf より後ろなので `C-x C-n` はこちらが勝つ):

```elisp
(leaf project-sidebar
  :doc "dired ベースのプロジェクトサイドバー (treemacs の代替)"
  :ensure nil
  :bind (("C-x C-n" . wamei/project-sidebar-toggle))
  :preface
  (load (expand-file-name "project-sidebar"
                          (file-name-directory (file-truename user-init-file)))
        nil t)
  :init
  (wamei/project-sidebar-setup)
  :hook
  (wamei/project-sidebar-mode-hook . hide-mode-line-mode))
```

- [ ] **Step 6: leaf の警告確認と GUI 確認**

```bash
emacs --batch -l ~/.emacs.d/init.el --eval '(kill-emacs 0)' 2>&1 | grep -E "Warning \(leaf\)|Error \(leaf\)"
emacsclient --eval '(progn (load "~/.emacs.d/project-tabs.el") (load "~/.emacs.d/project-sidebar.el") (wamei/project-sidebar-setup) (global-set-key (kbd "C-x C-n") (function wamei/project-sidebar-toggle)) (add-hook (quote wamei/project-sidebar-mode-hook) (function hide-mode-line-mode)))'
```

ユーザー確認: treemacs を `C-u C-x C-n` で閉じてから `C-x C-n` → 左に dired の sidebar が出てフォーカス、header にプロジェクト名 / `C-x C-n` で本文へ戻る / `q` も同じ / `C-u C-x C-n` で閉じる / タブを切り替えても別タブには出ない。

- [ ] **Step 7: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/project-sidebar.el .emacs.d/project-sidebar-test.el .emacs.d/init.el && git commit -m "$(cat <<'EOF'
project-sidebar を追加し、プロジェクトごとの dired を左 side window に出す

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```

---

### Task 11: project-sidebar.el — follow

**Files:**
- Modify: `.emacs.d/project-sidebar.el`
- Modify: `.emacs.d/init.el` (leaf project-sidebar の `:config`)
- Test: `.emacs.d/project-sidebar-test.el`

**Interfaces:**
- Consumes: `wamei/dired-tree-expand-to`、`wamei/dired-tree--inside-p`
- Produces:
  - `wamei/project-sidebar--follow-target (file shown-root file-root)` → `same` / `switch` / `none`
  - `wamei/project-sidebar--follow (frame)` → FRAME の sidebar をメイン window のファイルに合わせる
  - `wamei/project-sidebar--follow-soon (frame)` → window change functions 用
  - `wamei/project-sidebar-follow-mode` (global minor mode) → hook の登録/解除

- [ ] **Step 1: テストを追加する**

```elisp
;;; follow

(ert-deftest wamei/project-sidebar-follow-target ()
  (should (eq (wamei/project-sidebar--follow-target "/r/a.el" "/r/" "/r/") 'same))
  (should (eq (wamei/project-sidebar--follow-target "/o/a.el" "/r/" "/o/") 'switch))
  (should (eq (wamei/project-sidebar--follow-target "/o/a.el" "/r/" nil) 'none))
  (should (eq (wamei/project-sidebar--follow-target nil "/r/" "/r/") 'none)))

(ert-deftest wamei/project-sidebar-follow-expands-to-visited-file ()
  (wamei/project-sidebar-test--with-project root
    (let* ((main (selected-window))
           (file (expand-file-name "src/main.el" root))
           (buf (find-file-noselect file)))
      (unwind-protect
          (progn
            (set-window-buffer main buf)
            (let ((win (wamei/project-sidebar-show root)))
              (wamei/project-sidebar--follow (selected-frame))
              (with-current-buffer (window-buffer win)
                (should (equal (save-excursion
                                 (goto-char (window-point win))
                                 (dired-utils-get-filename))
                               file)))
              (delete-window win)))
        (kill-buffer buf)))))

(ert-deftest wamei/project-sidebar-follow-switches-to-other-project ()
  (wamei/project-sidebar-test--with-project root-a
    (wamei/project-sidebar-test--with-project root-b
      (let* ((project-find-functions
              (list (lambda (dir)
                      (cond ((string-prefix-p root-a (expand-file-name dir)) (cons 'transient root-a))
                            ((string-prefix-p root-b (expand-file-name dir)) (cons 'transient root-b))))))
             (main (selected-window))
             (buf (find-file-noselect (expand-file-name "README" root-b))))
        (unwind-protect
            (let ((win (wamei/project-sidebar-show root-a)))
              (set-window-buffer main buf)
              (wamei/project-sidebar--follow (selected-frame))
              (should (eq (window-buffer win) (wamei/project-sidebar-buffer root-b)))
              (delete-window win))
          (kill-buffer buf))))))
```

- [ ] **Step 2: 失敗を確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-sidebar-test.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E "FAILED|Ran"
```

- [ ] **Step 3: 実装する**

`project-sidebar.el` の `;;; minor mode` の前に追加:

```elisp
;;; follow

(defun wamei/project-sidebar--follow-target (file shown-root file-root)
  "FILE に合わせるとき sidebar をどうするか。
SHOWN-ROOT の配下なら `same'、別プロジェクト (FILE-ROOT あり) なら `switch'、
それ以外 (FILE が無い、プロジェクト外) は `none'。"
  (cond ((null file) 'none)
        ((and shown-root (wamei/dired-tree--inside-p shown-root file)) 'same)
        (file-root 'switch)
        (t 'none)))

(defun wamei/project-sidebar--reveal (window file)
  "WINDOW の sidebar で FILE の行まで展開し、window-point を移す。フォーカスは動かさない。"
  (with-current-buffer (window-buffer window)
    (save-excursion
      (when (wamei/dired-tree-expand-to file)
        (set-window-point window (point))))))

(defun wamei/project-sidebar--follow (frame)
  "FRAME のメイン window のファイルに sidebar を合わせる。"
  (when (frame-live-p frame)
    (with-selected-frame frame
      (when-let* ((window (wamei/project-sidebar-window frame)))
        (unless (window-parameter (selected-window) 'no-other-window)
          (let* ((buffer (window-buffer (selected-window)))
                 (file (buffer-file-name buffer))
                 (shown-root (with-current-buffer (window-buffer window)
                               (expand-file-name default-directory)))
                 (file-root (and file
                                 (let ((default-directory (file-name-directory file)))
                                   (when-let* ((p (project-current nil)))
                                     (file-name-as-directory (expand-file-name (project-root p))))))))
            (pcase (wamei/project-sidebar--follow-target file shown-root file-root)
              ('same (wamei/project-sidebar--reveal window file))
              ('switch
               (set-window-dedicated-p window nil)
               (set-window-buffer window (wamei/project-sidebar-buffer file-root))
               (set-window-dedicated-p window t)
               (wamei/project-sidebar--reveal window file)))))))))

(defvar wamei/project-sidebar--follow-timer nil)

(defun wamei/project-sidebar--follow-soon (frame)
  "`window-buffer-change-functions' / `window-selection-change-functions' 用。
再表示中は window を触らず、次のコマンド境界で follow する。"
  (unless (timerp wamei/project-sidebar--follow-timer)
    (setq wamei/project-sidebar--follow-timer
          (run-at-time 0 nil
                       (lambda ()
                         (setq wamei/project-sidebar--follow-timer nil)
                         (wamei/project-sidebar--follow frame))))))

(define-minor-mode wamei/project-sidebar-follow-mode
  "メイン window のバッファに sidebar のカーソルを追従させる。"
  :global t
  (if wamei/project-sidebar-follow-mode
      (progn
        (add-hook 'window-buffer-change-functions #'wamei/project-sidebar--follow-soon)
        (add-hook 'window-selection-change-functions #'wamei/project-sidebar--follow-soon))
    (remove-hook 'window-buffer-change-functions #'wamei/project-sidebar--follow-soon)
    (remove-hook 'window-selection-change-functions #'wamei/project-sidebar--follow-soon)))
```

- [ ] **Step 4: テストが通ることを確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-sidebar-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

Expected: `Ran 10 tests, 10 results as expected`。

- [ ] **Step 5: init.el の leaf に follow を足す**

leaf project-sidebar に `:config (wamei/project-sidebar-follow-mode 1)` を追加。

- [ ] **Step 6: GUI 確認**

```bash
emacsclient --eval '(progn (load "~/.emacs.d/project-sidebar.el") (wamei/project-sidebar-follow-mode 1))'
```

ユーザー確認: sidebar を開いた状態で `C-x C-f` で深いファイルを開くと sidebar が展開してその行にカーソル (フォーカスは本文のまま) / 別プロジェクトのファイルを開くと sidebar のプロジェクトが切り替わる。

- [ ] **Step 7: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/project-sidebar.el .emacs.d/project-sidebar-test.el .emacs.d/init.el && git commit -m "$(cat <<'EOF'
project-sidebar をメイン window のファイルに追従させる

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```

---

### Task 12: project-sidebar.el — マウスとキー

**Files:**
- Modify: `.emacs.d/project-sidebar.el`
- Test: `.emacs.d/project-sidebar-test.el`

**Interfaces:**
- Produces:
  - `wamei/project-sidebar--open-in-main (file &optional select)` → メイン window に FILE を出す。SELECT ならフォーカスも移す
  - `wamei/project-sidebar-preview ()` → point の行がファイルならメイン window に表示 (フォーカスは残す)
  - `wamei/project-sidebar-open ()` → ファイルは開いてフォーカス、ディレクトリは toggle
  - `wamei/project-sidebar-mouse-select (event)` / `wamei/project-sidebar-mouse-open (event)`

- [ ] **Step 1: テストを追加する**

```elisp
;;; 開く

(ert-deftest wamei/project-sidebar-preview-shows-file-without-focus ()
  (wamei/project-sidebar-test--with-project root
    (let* ((main (selected-window))
           (win (wamei/project-sidebar-show root)))
      (unwind-protect
          (progn
            (select-window win)
            (wamei/dired-tree-expand-to (expand-file-name "README" root))
            (wamei/project-sidebar-preview)
            (should (eq (selected-window) win))
            (should (equal (buffer-file-name (window-buffer main))
                           (expand-file-name "README" root))))
        (kill-buffer (window-buffer main))
        (delete-window win)))))

(ert-deftest wamei/project-sidebar-open-selects-main-for-file ()
  (wamei/project-sidebar-test--with-project root
    (let* ((main (selected-window))
           (win (wamei/project-sidebar-show root)))
      (unwind-protect
          (progn
            (select-window win)
            (wamei/dired-tree-expand-to (expand-file-name "README" root))
            (wamei/project-sidebar-open)
            (should (eq (selected-window) main))
            (should (equal (buffer-file-name (window-buffer main))
                           (expand-file-name "README" root))))
        (kill-buffer (window-buffer main))
        (delete-window win)))))

(ert-deftest wamei/project-sidebar-open-toggles-directory ()
  (wamei/project-sidebar-test--with-project root
    (let ((win (wamei/project-sidebar-show root)))
      (unwind-protect
          (progn
            (select-window win)
            (dired-utils-goto-line (expand-file-name "src" root))
            (wamei/project-sidebar-open)
            (should (eq (selected-window) win))
            (should (dired-utils-goto-line (expand-file-name "src/main.el" root))))
        (delete-window win)))))
```

- [ ] **Step 2: 失敗を確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-sidebar-test.el -f ert-run-tests-batch-and-exit 2>&1 | grep -E "FAILED|Ran"
```

- [ ] **Step 3: 実装する**

`project-sidebar.el` の `;;; minor mode` の前に追加し、keymap にキーを足す:

```elisp
;;; 開く

(defun wamei/project-sidebar--open-in-main (file &optional select)
  "メイン window に FILE を出す。SELECT が非 nil ならそちらへフォーカスを移す。"
  (let ((window (wamei/project-tabs-main-window))
        (buffer (find-file-noselect file)))
    (set-window-buffer window buffer)
    (when select
      (select-window window))))

(defun wamei/project-sidebar-preview ()
  "point の行がファイルならメイン window に表示する。フォーカスは sidebar に残す。"
  (interactive)
  (when-let* ((file (dired-utils-get-filename)))
    (unless (file-directory-p file)
      (wamei/project-sidebar--open-in-main file))))

(defun wamei/project-sidebar-open ()
  "ファイルならメイン window で開いてフォーカスを移す。ディレクトリなら展開/折りたたみ。"
  (interactive)
  (when-let* ((file (dired-utils-get-filename)))
    (if (file-directory-p file)
        (dired-subtree-toggle)
      (wamei/project-sidebar--open-in-main file t))))

(defun wamei/project-sidebar-mouse-select (event)
  "クリックした行を選択し、ファイルならプレビューする。"
  (interactive "e")
  (mouse-set-point event)
  (wamei/project-sidebar-preview))

(defun wamei/project-sidebar-mouse-open (event)
  "ダブルクリックした行を開く (ファイル) か展開/折りたたみ (ディレクトリ)。"
  (interactive "e")
  (mouse-set-point event)
  (wamei/project-sidebar-open))
```

keymap:

```elisp
(defvar wamei/project-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'wamei/project-sidebar-quit)
    (define-key map (kbd "RET") #'wamei/project-sidebar-open)
    (define-key map [mouse-1] #'wamei/project-sidebar-mouse-select)
    (define-key map [double-mouse-1] #'wamei/project-sidebar-mouse-open)
    map)
  "sidebar バッファのキーマップ。dired-mode-map より優先される。
down-mouse-1 は束縛しない (dired の D&D に任せる)。")
```

- [ ] **Step 4: テストが通ることを確認する**

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-sidebar-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

Expected: `Ran 13 tests, 13 results as expected`。

- [ ] **Step 5: GUI 確認**

```bash
emacsclient --eval '(load "~/.emacs.d/project-sidebar.el")'
```

既に開いている sidebar バッファは kill してから `C-x C-n` で作り直す (keymap の変更は minor mode の再有効化で効くが、作り直す方が確実)。

ユーザー確認: シングルクリックで行が選ばれ、ファイルなら本文に出る (フォーカスは sidebar) / ダブルクリックでフォーカスが本文へ / ディレクトリのダブルクリックで展開 / `RET` も同じ / ドラッグで別 dired に落とせる / 右クリックメニューが出る。

- [ ] **Step 6: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/project-sidebar.el .emacs.d/project-sidebar-test.el && git commit -m "$(cat <<'EOF'
project-sidebar にクリックでプレビュー、ダブルクリック / RET で開く操作を足す

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```

---

### Task 13: desktop 連携

**Files:**
- Modify: `.emacs.d/init.el` (leaf desktop: `wamei/desktop--restore-treemacs` → `wamei/desktop--restore-sidebar`、`wamei/desktop-side-restorers`、`desktop-modes-not-to-save`、`desktop-buffers-not-to-save`)

**Interfaces:**
- Consumes: `wamei/project-sidebar-show (dir)` → window、`wamei/desktop-side-resize (window size)`

- [ ] **Step 1: `desktop-buffers-not-to-save` の既定値を確認する**

```bash
emacs -Q --batch --eval '(progn (require (quote desktop)) (message "%S" desktop-buffers-not-to-save))' 2>&1 | tail -1
```

nil か正規表現文字列のどちらかが出る。以下のコードはどちらでも動く。

- [ ] **Step 2: init.el の leaf desktop を書き換える**

`wamei/desktop--restore-treemacs` の defun を次に置き換える:

```elisp
  (defun wamei/desktop--restore-sidebar (spec)
    "sidebar を開き直し、幅を SPEC の :size に合わせる。
desktop-side-windows が SPEC の :directory を default-directory に束縛して呼ぶので、
そのディレクトリのプロジェクトの sidebar が出る。"
    (wamei/desktop-side-resize (wamei/project-sidebar-show default-directory)
                               (plist-get spec :size)))
```

`:custom` の `desktop-modes-not-to-save` から `treemacs-mode` の行を消し、コメント「treemacs はツリーを自前で再構築するので復元すると壊れる。」も消す。

`:config` の `wamei/desktop-side-restorers` の treemacs 行を差し替える:

```elisp
  ;; バッファ名で開き直し方を選ぶ。sidebar は " *sidebar: " で始まる。
  (setq wamei/desktop-side-restorers
        '(("\\` \\*sidebar: " . wamei/desktop--restore-sidebar)
          ("\\`\\*term: " . wamei/desktop--restore-term)
          ("\\`\\*terminals\\*\\'" . ignore)
          ("\\`\\*claude-code\\[" . wamei/desktop--restore-claude)))
  ;; sidebar は dired バッファなので desktop が普通の dired として保存してしまう。
  ;; 除外して restorer に任せる。
  (setq desktop-buffers-not-to-save
        (if desktop-buffers-not-to-save
            (concat "\\` \\*sidebar: \\|" desktop-buffers-not-to-save)
          "\\` \\*sidebar: "))
```

- [ ] **Step 3: leaf の警告確認**

```bash
emacs --batch -l ~/.emacs.d/init.el --eval '(kill-emacs 0)' 2>&1 | grep -E "Warning \(leaf\)|Error \(leaf\)"
```

- [ ] **Step 4: 復元関数の単体確認 (起動中の Emacs)**

```bash
emacsclient --eval '(progn (defun wamei/desktop--restore-sidebar (spec) (wamei/desktop-side-resize (wamei/project-sidebar-show default-directory) (plist-get spec :size))) (let ((default-directory "~/.dotfiles/")) (wamei/desktop--restore-sidebar (list :size 40))) (window-total-width (wamei/project-sidebar-window)))'
```

Expected: `40`。

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/init.el && git commit -m "$(cat <<'EOF'
desktop の side window 復元を treemacs から project-sidebar に切り替える

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```

---

### Task 14: treemacs の削除

**Files:**
- Modify: `.emacs.d/init.el` (leaf treemacs / treemacs-nerd-icons / treemacs-magit / treemacs-tab-bar、hide-mode-line の hook、コメント)
- Modify: `.emacs.d/project-tabs.el` (Commentary 2 と 3、declare-function、ガード)
- Modify: `.emacs.d/project-tabs-test.el` (`(require 'treemacs)`、ガードのテスト 3 本とそのフィクスチャ)
- Modify: `.emacs.d/desktop-side-windows.el` (コメントの treemacs 言及のみ)

- [ ] **Step 1: init.el から 4 つの leaf を削除する**

`(leaf treemacs …)`、`(leaf treemacs-nerd-icons …)`、`(leaf treemacs-magit …)`、`(leaf treemacs-tab-bar …)` の 4 ブロックを丸ごと削除する。

```bash
grep -n "leaf treemacs" ~/.dotfiles/.emacs.d/init.el
```

Expected: 出力なし。

- [ ] **Step 2: hide-mode-line の hook を付け替える**

```elisp
(leaf hide-mode-line
  :doc "モードラインを隠す"
  :ensure t
  :leaf-defer nil
  :hook
  ((vterm-mode-hook) . hide-mode-line-mode)
  ((dired-mode-hook vterm-mode-hook wamei/term-list-mode-hook)
   . (lambda() (display-line-numbers-mode 0))))
```

(sidebar の hide-mode-line は Task 10 の leaf project-sidebar の `:hook` で付いている。)

- [ ] **Step 3: init.el のコメントを直す**

`grep -n treemacs ~/.dotfiles/.emacs.d/init.el` で残った言及を確認し、次の方針で文言だけ直す (挙動は変えない):

- 端末パネル (`wamei/term--back-window` の docstring、`wamei/term-toggle` の docstring、display-buffer-alist のコメント): 「treemacs」→「sidebar」
- claude-code-ide のコメント「treemacs が左」→「sidebar が左」
- tab-bar の `:preface` コメント「タブ名の決定と固定、treemacs 側のガードは project-tabs.el に分けている。」→「タブ名の決定と固定は project-tabs.el に分けている。」、`:config` の「タブ名 = treemacs のスコープが動く」→「タブ名が動く」
- magit のコメント「(treemacs と claude-code-ide はパッケージが付け、端末は vterm の …)」→「(claude-code-ide はパッケージが付け、sidebar と端末は display-buffer-alist で付けている)」
- desktop のコメントはTask 13 で直っている

```bash
grep -n treemacs ~/.dotfiles/.emacs.d/init.el
```

Expected: 出力なし。

- [ ] **Step 4: project-tabs.el を整理する**

- Commentary の「2.」から treemacs-tab-bar の説明 (「treemacs-tab-bar はタブ名をそのままスコープ…」から「使い捨て workspace も増え続ける。」まで) を削り、次にする:

```
;; 2. タブ名の固定 (`wamei/project-tabs-pin-name')
;;    tab-bar はタブ名を再描画のたびに再計算するので、1. だけだとカレント
;;    バッファのプロジェクトが変わるたびにタブ名も変わる。タブにプロジェクトの
;;    バッファが初めて出た時点で tab-rename して explicit-name を立て、
;;    以後は名前を動かさない。
```

- Commentary の「3. treemacs のガード」の段落と「このファイルはそのための 3 つの部品を持つ」の「3 つ」→「2 つ」
- `declare-function` 3 行を削除
- `;;; treemacs のガード` セクションと `wamei/treemacs--find-file-node-guard` を削除
- `wamei/project-tabs--pin-name-soon` の docstring を「tab-rename は window 構成を変える処理を呼ぶことがあるので、再表示中に走る window change 関数の中では直接呼ばない。」に変える

- [ ] **Step 5: project-tabs-test.el を整理する**

- `(require 'treemacs)` を削除
- `;;; treemacs のガード` セクション (フィクスチャ `wamei/project-tabs-test--project` と `ert-deftest wamei/project-tabs-treemacs-guard-*` 3 本) を削除

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l project-tabs-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

Expected: 8 tests pass (元 7 + Task 9 の 1)。

- [ ] **Step 6: desktop-side-windows.el のコメント**

`grep -n treemacs desktop-side-windows.el` の 4 箇所は説明文なので、「treemacs」→「sidebar」に置き換える。`desktop-side-windows-test.el` の `" *Treemacs-Scoped-Buffer-a*"` と `treemacs-restorer` はテスト用の仮名なので、`" *sidebar: a*"` / `sidebar-restorer` に改名し、正規表現 `"\\` \\*Treemacs-"` は `"\\` \\*sidebar: "` にする。

```bash
cd ~/.dotfiles/.emacs.d && emacs -Q --batch -l desktop-side-windows-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -3
```

Expected: 全部 pass。

- [ ] **Step 7: init.el 全体の確認**

```bash
emacs --batch -l ~/.emacs.d/init.el --eval '(kill-emacs 0)' 2>&1 | grep -iE "Warning \(leaf\)|Error \(leaf\)|error|backtrace"
grep -rn treemacs ~/.dotfiles/.emacs.d/*.el
```

Expected: どちらも出力なし (treemacs-tab-bar のバックトレースも消える)。

- [ ] **Step 8: コミット**

```bash
cd ~/.dotfiles && git add .emacs.d/init.el .emacs.d/project-tabs.el .emacs.d/project-tabs-test.el .emacs.d/desktop-side-windows.el .emacs.d/desktop-side-windows-test.el && git commit -m "$(cat <<'EOF'
treemacs を削除し、project-tabs の treemacs ガードと desktop の復元を整理する

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```

---

### Task 15: elpa の掃除と最終確認

**Files:** なし (elpa ディレクトリの削除と検証のみ)

- [ ] **Step 1: 全テストを回す**

```bash
cd ~/.dotfiles/.emacs.d && for t in dired-tree dired-git-status project-sidebar project-tabs desktop-side-windows; do echo "== $t"; emacs -Q --batch -l $t-test.el -f ert-run-tests-batch-and-exit 2>&1 | tail -1; done
```

Expected: 全部 `Ran N tests, N results as expected`。

- [ ] **Step 2: treemacs の elpa ディレクトリを削除する**

`:ensure` で再生成できるパッケージなので内容の dump は不要。他パッケージの依存が無いことを先に確かめる。

```bash
grep -l "treemacs" ~/.emacs.d/elpa/*/*-pkg.el | grep -v "/treemacs"
```

Expected: 出力なし。その後:

```bash
rm -rf ~/.emacs.d/elpa/treemacs-* ~/.emacs.d/elpa/treemacs-magit-* ~/.emacs.d/elpa/treemacs-nerd-icons-* ~/.emacs.d/elpa/treemacs-tab-bar-*
ls ~/.emacs.d/elpa | grep treemacs
```

Expected: 出力なし。

- [ ] **Step 3: Emacs を再起動して確認する**

ユーザーに Emacs を再起動してもらい、次を確認する:

- 起動時に `*Warnings*` が出ない (`emacsclient --eval '(get-buffer "*Warnings*")'` が nil)
- desktop 復元で各タブの左に sidebar が戻り、幅が保たれている
- `C-x C-n` の 4 態、follow、クリック / ダブルクリック、`TAB` 展開と展開記憶、git 色、右クリック、D&D
- 通常の dired (`C-x C-j`) でも `TAB` 展開・色・右クリック・D&D が効く
- magit でコミットした直後に sidebar の色が更新される

- [ ] **Step 4: 仕様書を実装に合わせる**

実装中に spec と違えた点 (drop の既定を move にした、`private` の扱い、名前空間 `wamei/` など) を `docs/superpowers/specs/2026-09-05-dired-sidebar-design.md` に反映する。

```bash
cd ~/.dotfiles && git add docs/superpowers/specs/2026-09-05-dired-sidebar-design.md && git commit -m "$(cat <<'EOF'
dired サイドバーの仕様書を実装に合わせる

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
EOF
)"
```
