# vterm から ghostel への移行 実装計画

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Emacs の端末を emacs-libvterm から ghostel (libghostty-vt) に完全移行し、libvterm の不足を埋めるために書いた自前レイヤー (faint 変換・既定色の抑止・pty サイズ同期・ホイール転送・送信のブロック回避・OSC 51;A) を全部落とす。

**Architecture:** 端末バッファ名 `*term: <project>[ N]*` と `display-buffer-alist` は据え置き、その下の端末実装だけ差し替える。キー入力は `ghostel-keymap-exceptions` で「Emacs 側に残すキー」を宣言する方式に寄せ、`ghostel-mode-map` は作り込まない。desktop 復元は `ghostel-desktop.el` を入れ物にし、スクロールバックの書き出しと `ghostel-pre-spawn-hook` での環境変数注入だけ自前で持つ。

**Tech Stack:** Emacs 31.1 (macOS NS ビルド) Lisp、ERT (batch)、ghostel 20260902.1753 + ネイティブモジュール 0.53.0 (MELPA / プリビルド)、leaf (init.el)、zsh

**Spec:** `docs/superpowers/specs/2026-09-08-vterm-to-ghostel-design.md`

## Global Constraints

- 作業ディレクトリは `~/.dotfiles/.emacs.d`。テストは `emacs -Q --batch -l <file>-test.el -f ert-run-tests-batch-and-exit`。
- `init.el` は `~/.emacs.d/init.el` からの symlink。sibling の `.el` は `(file-name-directory (file-truename user-init-file))` から読む。
- 端末バッファ名は `*term: <project>*` / `*term: <project> N*` (N は 2 以上)。一覧は `*terminals: <project>*`。**この命名は変更しない。**
- `ghostel-keymap-exceptions` の文字列は `key-description` 形式 (`"C-<tab>"`、`"C-S-<tab>"`)。`define-key` に渡すキー記述 (`(kbd "<C-tab>")`) とは別物なので混同しない。
- ghostel の公開 API だけを使う (`ghostel-create` / `ghostel-exec` / `ghostel-send-key` / `ghostel-send-string` / `ghostel-yank` / `ghostel-title` / `ghostel-shell` / `ghostel-max-scrollback` / `ghostel-mode-hook` / `ghostel-pre-spawn-hook` / `ghostel-buffer-name-function` / `ghostel-prompt` プロパティ / `face` プロパティ)。`ghostel--` で始まるものへの advice は入れない。
- 端末の face は `face` プロパティに plist で載る (vterm の `font-lock-face` ではない)。装飾のないセルには face が付かない。
- コミットは日本語 1 行サマリ + 本文。既存の履歴と同じ書き方にする。

---

### Task 1: term-input.el を ghostel の送信 API に移す

ホイール転送 (`wamei/term-input-mouse-mode`) は ghostel 本体がマウスを SGR で子プロセスへ転送するので不要になる。kill-ring 連携 (C-k) とクリップボード画像の受け渡し (Cmd+V) は ghostel にも無いので残す。`wamei/term-input-paste-mode` は minor mode をやめ、キーは Task 4 で claude-panel のバッファローカルキーマップに移すため、ここではコマンドだけ残す。

**Files:**
- Modify: `term-input.el`
- Test: `term-input-test.el`

**Interfaces:**
- Consumes: なし
- Produces:
  - `(wamei/term-input-kill-line)` — interactive。point から行末までを kill-ring に入れて C-k を端末へ送る
  - `(wamei/term-input-paste)` — interactive。クリップボードに画像があれば C-v を端末へ送り、無ければ `ghostel-yank`
  - `(wamei/term-input--clipboard-image-p)` — クリップボードに画像があれば非 nil

- [ ] **Step 1: テストのスタブを ghostel に差し替え、消える機能のテストを落とす**

`term-input-test.el` の冒頭のスタブ定義 (14 行目から `wamei/term-input-test--call-kill-line` の直前まで) を次で置き換える:

```elisp
;;; テスト用の ghostel スタブ

(defvar wamei/term-input-test--sent nil
  "スタブが受け取った送信内容。(KEY-NAME MODS) か `yank'。")

(defmacro wamei/term-input-test--with-ghostel (&rest body)
  "ghostel の送信関数をスタブに差し替えて BODY を実行する。"
  (declare (indent 0))
  `(let ((wamei/term-input-test--sent nil)
         (kill-ring nil)
         (kill-ring-yank-pointer nil)
         (last-command nil)
         (this-command nil))
     (cl-letf (((symbol-function 'ghostel-send-key)
                (lambda (key-name &optional mods)
                  (push (list key-name mods) wamei/term-input-test--sent)))
               ((symbol-function 'ghostel-yank)
                (lambda (&rest _) (push 'yank wamei/term-input-test--sent))))
       ,@body)))
```

続けて、残すテストの中の `wamei/term-input-test--with-vterm` を `wamei/term-input-test--with-ghostel` に置換し、送信の期待値を書き換える:

- `wamei/term-input-kill-line-saves-rest-of-line`、`-at-eol-leaves-kill-ring`: `'(("k" nil nil t))` → `'(("k" "ctrl"))`
- `wamei/term-input-paste-sends-ctrl-v-for-image`: 期待値 `'(("v" nil nil t))` → `'(("v" "ctrl"))`。`cl-letf` で `vterm-yank` を差し替えている 2 行は不要 (スタブ側に `ghostel-yank` があるので) なので消し、`(wamei/term-input-test--with-clipboard [TARGETS image/png] (call-interactively #'wamei/term-input-paste))` を直接書く
- `wamei/term-input-paste-yanks-for-text`: 同様に `cl-letf` を外す。期待値 `'(yank)` は変わらない

次のテストと補助定義を**削除**する (機能ごと消えるため):

- `;;; マウスホイールの SGR 列` 節の 4 テスト (`-sgr-wheel-up`、`-sgr-wheel-down-position`、`-wheel-button-from-event`、`-wheel-button-rejects-other-events`)
- `;;; ホイール転送コマンド` 節すべて (`--with-window-buffer`、`--wheel-event`、`-forward-wheel-sends-sgr`、`-forward-wheel-does-not-wait-for-output`、`-forward-wheel-uses-event-window-buffer`、`-forward-wheel-in-copy-mode-scrolls-emacs`)
- `;;; minor mode` 節の 2 テスト (`-mouse-mode-binds-wheel-events`、`-mouse-mode-off-restores-bindings`)
- `wamei/term-input-paste-mode-binds-super-v`
- `(defvar vterm-timer-delay 0.1 ...)` の宣言、`wamei/term-input-test--delays`、`wamei/term-input-test--buffers`

- [ ] **Step 2: テストを走らせて落ちることを確認**

Run: `emacs -Q --batch -l term-input-test.el -f ert-run-tests-batch-and-exit`
Expected: FAIL。`ghostel-send-key` が未定義のまま `vterm-send-key` を呼ぶので、kill-line と paste のテストが `void-function vterm-send-key` で落ちる。

- [ ] **Step 3: term-input.el を書き換える**

冒頭 (Commentary と require、declare-function) を次にする:

```elisp
;;; term-input.el --- ghostel への入力を Emacs 側の操作と結びつける -*- lexical-binding: t; -*-

;;; Commentary:

;; ghostel は semi-char モードでほとんどのキーを端末へ流すので、Emacs 側の
;; kill-ring やクリップボードとは結びつかない。ここでは次の 2 つを補う。
;;
;; - `wamei/term-input-kill-line'
;;   C-k を送る前に point から行末までを kill-ring に入れる。行の削除自体は
;;   従来どおりシェル (zsh の ZLE) が行うので、シェル側の挙動は変わらない。
;;   画面幅を超えて折り返した入力行は、見えている行末までしか拾えない。
;;
;; - `wamei/term-input-paste'
;;   Cmd+V でクリップボードの画像を端末のプログラムに渡す。Cmd+V を
;;   `ghostel-yank' に割り当てるとテキストしか送れず、画像をコピーしても何も
;;   起きない。Claude Code は C-v を受けると自分で osascript を叩いて macOS の
;;   クリップボードから画像を取り出すので、Emacs 側で画像を運ぶ必要はなく、
;;   キーだけ端末へ流せばよい。シェルでは C-v が quoted-insert になって固まるので、
;;   キーの束縛は Claude のバッファだけで行う (claude-panel.el)。
;;
;; マウスホイールは ghostel 本体が SGR のマウス報告として子プロセスへ転送する
;; ので、ここでは何もしない。

;;; Code:

(require 'seq)

(declare-function ghostel-send-key "ghostel")
(declare-function ghostel-yank "ghostel")
```

`wamei/term-input-kill-line` の最終行を差し替える:

```elisp
  (ghostel-send-key "k" "ctrl"))
```

`wamei/term-input--line-rest` の docstring の「vterm が埋める」を「端末が埋める」にする。

`;;; マウスホイール転送` 節を丸ごと削除する (`--sgr-mouse`、`--wheel-button`、`wamei/term-input-forward-wheel`、`wamei/term-input-mouse-mode-map`、`wamei/term-input-mouse-mode`)。

`wamei/term-input-paste` を次にする:

```elisp
(defun wamei/term-input-paste ()
  "クリップボードに画像があれば C-v を端末へ送り、無ければ通常の貼り付け。
Claude Code は C-v を受けると osascript で macOS のクリップボードを
«class PNGf» として読み、ファイルに書き出して添付する。画像そのものは
Emacs を経由しないので、ここで送るのはキーだけでよい。"
  (interactive)
  (if (wamei/term-input--clipboard-image-p)
      (ghostel-send-key "v" "ctrl")
    (ghostel-yank)))
```

`wamei/term-input-paste-mode-map` と `wamei/term-input-paste-mode` を削除する。

- [ ] **Step 4: テストが通ることを確認**

Run: `emacs -Q --batch -l term-input-test.el -f ert-run-tests-batch-and-exit`
Expected: PASS (9 tests: kill-line 4、clipboard-image-p 3、paste 2)

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles
git add .emacs.d/term-input.el .emacs.d/term-input-test.el
git commit -m "$(cat <<'EOF'
term-input を ghostel の送信 API に移す

ホイール転送は ghostel 本体がマウスを SGR で子プロセスへ渡すので削除する。
vterm-send-string が accept-process-output で待つ問題への対策
(vterm-timer-delay を 0 に束縛する) も、ghostel--send-string が待たないので
一緒に落とす。

paste は minor mode をやめてコマンドだけにする。キーの束縛は claude-panel
側のバッファローカルキーマップに移す。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_014YP2EKYnKbZGZFT9FxXZdD
EOF
)"
```

---

### Task 2: term-panel.el を ghostel で端末を起こすようにする

端末の生成を `ghostel-create` に、一覧のラベルを `ghostel-title` に移す。pty サイズ同期 (`vterm--filter` の advice 本体) は ghostel が window 幅に即追従するので削除する。タイトル変更で一覧を描き直す契機は、advice ではなく公開フック `ghostel-buffer-name-function` に移す (この変数はタイトル変更と cd のたびに TITLE を引数に呼ばれ、nil を返すとバッファ名は変更されない — `ghostel.el:383-393, 3431-3439`)。

**Files:**
- Modify: `term-panel.el`
- Test: `term-panel-test.el`

**Interfaces:**
- Consumes: なし
- Produces:
  - `(wamei/term--create INDEX)` — INDEX 番目の端末バッファを作って返す。window 構成は変えない
  - `(wamei/term--label BUFFER)` — 一覧に出す表示名 (`ghostel-title`、無ければシェル名)
  - `(wamei/term--on-title-change TITLE)` — `ghostel-buffer-name-function` に設定する。常に nil を返す
  - `(wamei/term--setup-buffer)` — `ghostel-mode-hook` に入れる
  - `(wamei/term-panel-setup)` — `display-buffer-alist` の登録 (現状のまま)

- [ ] **Step 1: テストを ghostel 前提に書き換える**

`term-panel-test.el` の fake を差し替える。`wamei/term-panel-test--fake-vterm` を次で置き換える (`ghostel-create` は表示せずバッファを返すので `switch-to-buffer` をしない):

```elisp
(defun wamei/term-panel-test--fake-ghostel-create (&optional name _display _identity)
  "`ghostel-create' の代わり。NAME のバッファを作って `default-directory' を引き継ぐ。"
  (let ((dir default-directory))
    (with-current-buffer (get-buffer-create name)
      (setq default-directory dir)
      (current-buffer))))
```

`wamei/term-panel-test--with-projects` の `cl-letf` を差し替える:

```elisp
         (cl-letf (((symbol-function 'ghostel-create)
                    #'wamei/term-panel-test--fake-ghostel-create))
```

`wamei/term-panel-record-title-updates-label` を、タイトルの出どころを `ghostel-title` にしたテストへ置き換える:

```elisp
(ert-deftest wamei/term-panel-label-comes-from-ghostel-title ()
  "一覧のラベルは端末が報告したタイトル (`ghostel-title') を出す。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (with-current-buffer (wamei/term--create 2)
        (setq-local ghostel-title "make test"))
      (should (string-match-p "2: make test"
                              (with-current-buffer (wamei/term--list-refresh)
                                (buffer-string)))))))

(ert-deftest wamei/term-panel-title-change-refreshes-list ()
  "`ghostel-buffer-name-function' として呼ばれると一覧を描き直し、nil を返す
\(nil はバッファ名を変えないという意味)。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha
      (wamei/term--create 1)
      (let ((list-buffer (wamei/term--list-buffer)))
        (with-current-buffer (get-buffer "*term: alpha*")
          (setq-local ghostel-title "make test")
          (should-not (wamei/term--on-title-change "make test")))
        (should (string-match-p "make test"
                                (with-current-buffer list-buffer (buffer-string))))))))

(ert-deftest wamei/term-panel-title-change-ignores-other-buffers ()
  "端末以外のバッファでは一覧を触らない。"
  (wamei/term-panel-test--with-projects (alpha)
    (wamei/term-panel-test--in alpha (wamei/term--create 1))
    (with-temp-buffer
      (should-not (wamei/term--on-title-change "x")))))
```

`;;; 作成直後の pty サイズ同期` 節を丸ごと削除する (`--with-process-buffer` と 3 テスト: `-sync-size-once-on-first-output`、`-sync-size-ignores-other-vterm-buffers`、`-sync-size-survives-dead-buffer`)。

テストファイル冒頭の `(require 'cl-lib)` の後に、batch で `ghostel-title` を `setq-local` できるようにする宣言を足す (ghostel 本体は読まない):

```elisp
;; ghostel 本体の buffer-local 変数。ghostel を読まない batch でも
;; setq-local / buffer-local-value できるよう special にしておく。
(defvar-local ghostel-title nil
  "端末が報告したタイトル (テスト用のスタブ定義)。")
```

- [ ] **Step 2: テストを走らせて落ちることを確認**

Run: `emacs -Q --batch -l term-panel-test.el -f ert-run-tests-batch-and-exit`
Expected: FAIL。`wamei/term--create` が `vterm` を呼ぶので `void-function vterm`、`wamei/term--on-title-change` も未定義。

- [ ] **Step 3: term-panel.el を書き換える**

Commentary の 1-11 行目のうち vterm を指す 2 箇所を直す:

```elisp
;; ghostel の端末をプロジェクト (タブ) ごとにまとめ、フレーム下部の side window に
;; 出す。端末が 2 つ以上あるときは右隣に一覧 (`wamei/term-list-mode') を出す。
...
;; ghostel そのものへの結線 (display-buffer-alist、ghostel-mode-hook、
;; ghostel-buffer-name-function) は init.el の ghostel ブロックで行う。
;; テストは term-panel-test.el。
```

`(defvar vterm-shell)` を差し替える:

```elisp
(defvar ghostel-shell)                  ; ghostel.el
(defvar ghostel-title)                  ; ghostel.el (buffer-local)
(declare-function ghostel-create "ghostel" (&optional name display identity))
```

`wamei/term--title` の `defvar-local` (35-37 行目) を削除する。

`wamei/term--setup-buffer` の docstring 1 行目を「端末バッファのパネル向け設定。`ghostel-mode-hook' から呼ぶ。」にする。

`wamei/term--size-synced` の `defvar-local` と `wamei/term--sync-size-on-first-output` を削除する。

`wamei/term--create` を差し替える:

```elisp
(defun wamei/term--create (index)
  "INDEX 番目の端末を作って返す。
`ghostel-create' は DISPLAY を渡さなければ表示しないので、window 構成は変わらない
\(パネルへの表示は `wamei/term--show' が display-buffer で行う)。"
  (let ((default-directory (wamei/term--root)))
    (ghostel-create (wamei/term--buffer-name index))))
```

`wamei/term--record-title` を次で置き換える:

```elisp
(defun wamei/term--on-title-change (_title)
  "端末のタイトルが変わったら一覧を描き直す。バッファ名は変えない。
`ghostel-buffer-name-function' に設定して使う。この変数はタイトル変更 (OSC 0/2)
と cd (OSC 7) のたびに端末バッファで呼ばれる公開フックで、nil を返すと
`ghostel--rename-managed' が no-op になりバッファ名は変わらない。
呼ばれた時点で `ghostel-title' は新しい値になっている。
別タブで見えていない一覧も描き直しておく (戻ったときに古いままにしない)。"
  (when (and (string-prefix-p "*term: " (buffer-name))
             (get-buffer (wamei/term--list-buffer-name)))
    (wamei/term--list-refresh))
  nil)
```

`wamei/term--label` を差し替える:

```elisp
(defun wamei/term--label (buffer)
  "一覧に出す BUFFER の表示名。最後に実行したコマンド、無ければシェル名。
タイトルは .zshrc の preexec が OSC 0 で流し、ghostel が `ghostel-title' に入れる。"
  (or (buffer-local-value 'ghostel-title buffer)
      (file-name-nondirectory (if (boundp 'ghostel-shell) ghostel-shell shell-file-name))))
```

`wamei/term-list-kill` のコメント「vterm はプロセスが生きているため」を「端末はプロセスが生きているため」にする。

- [ ] **Step 4: テストが通ることを確認**

Run: `emacs -Q --batch -l term-panel-test.el -f ert-run-tests-batch-and-exit`
Expected: PASS

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles
git add .emacs.d/term-panel.el .emacs.d/term-panel-test.el
git commit -m "$(cat <<'EOF'
端末パネルを ghostel で起こす

ghostel-create は表示せずバッファを返すので save-window-excursion が不要。
pty サイズは window 幅に即追従し、shell 起動時の stty で上書きされないので
最初の出力でサイズを直す advice も要らない。

一覧のラベルは ghostel-title から取る。描き直しの契機は private な
vterm--set-title の advice ではなく、公開フックの
ghostel-buffer-name-function (nil を返せば改名しない) に移す。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_014YP2EKYnKbZGZFT9FxXZdD
EOF
)"
```

---

### Task 3: term-restore.el を ghostel-desktop 委譲に組み替える

端末バッファの保存と再生成は `ghostel-desktop.el` に任せる (`ghostel-mode` が `desktop-save-buffer` を設定し、ハンドラは ghostel の load 時に `desktop-buffer-mode-handlers` へ登録される)。自前で持つのは (1) スクロールバックの書き出し、(2) `ghostel-pre-spawn-hook` での `WAMEI_TERM_RESTORE` 注入、(3) 復元の仕上げ (`desktop-restore-eager` が 10 なので lazy に回された端末の取りこぼし生成、タイトルの復元、記録のクリア)、の 3 つ。

**記録を消す契機に注意**: 注入時ではなく仕上げで消す。注入時に消すと、ghostel-desktop が `desktop-read` 中に復元した端末の記録が仕上げに届かず、タイトルを戻せない。

**Files:**
- Modify: `term-restore.el`
- Test: `term-restore-test.el`

**Interfaces:**
- Consumes: なし (Task 2 とは独立。`*term: ` の命名規則だけ共有)
- Produces:
  - `wamei/term-restore-saved` — `((:name BUFFER-NAME :directory DIR :title TITLE :scrollback FILE) ...)` の plist リスト。`desktop-globals-to-save` で保存
  - `(wamei/term-restore-save)` — `desktop-save-hook` から呼ぶ
  - `(wamei/term-restore--inject-scrollback)` — `ghostel-pre-spawn-hook` から呼ぶ。引数なし。記録は消さない
  - `(wamei/term-restore-ensure)` — `desktop-after-read-hook` (深さ -10) から呼ぶ。取りこぼしの生成 + タイトル復元 + 記録のクリア
  - `(wamei/term-restore-setup)` — 上記の結線

- [ ] **Step 1: face プロパティ名と既定色のテストを書き換える**

`term-restore-test.el` の `wamei/term-restore-test--colored` を差し替える:

```elisp
(defun wamei/term-restore-test--colored (text &rest face)
  "TEXT に ghostel が付けるのと同じ `face' plist FACE を付けて返す。"
  (propertize text 'face face))
```

`wamei/term-restore-ansi-omits-default-colors` を**削除**し、代わりに次を足す (ghostel は装飾のないセルに face を付けないので、既定色の抑止そのものが不要になる):

```elisp
(ert-deftest wamei/term-restore-ansi-emits-colors-equal-to-default ()
  "ghostel は装飾のないセルに face を付けないので、テーマの既定色と同じ色でも
抑止しない (face が付いている = 端末が明示した色)。"
  (should (equal (wamei/term-restore--ansi
                  (concat "plain"
                          (wamei/term-restore-test--colored
                           "red" :foreground "#ff0000")))
                 "plain\e[38;2;255;0;0mred\e[0m")))
```

`wamei/term-restore-test--insert-marked` を差し替える:

```elisp
(defun wamei/term-restore-test--insert-marked (text)
  "TEXT を ghostel がプロンプトに付ける `ghostel-prompt' プロパティ付きで挿入する。
ghostel は OSC 133 でプロンプトの範囲を受け取り、その文字に印を付ける。"
  (insert (propertize text 'ghostel-prompt t 'rear-nonsticky t)))
```

- [ ] **Step 2: テストを走らせて落ちることを確認**

Run: `emacs -Q --batch -l term-restore-test.el -f ert-run-tests-batch-and-exit`
Expected: FAIL。`--ansi` が `font-lock-face` を見るので色のテストが素通しになり、`--prompt-line-p` が `vterm-prompt` を見るのでプロンプト除外のテストが落ちる。

- [ ] **Step 3: 色とプロンプト判定を ghostel のプロパティに合わせる**

`term-restore.el` の `wamei/term-restore--rgb` の docstring 2 行目を「端末は色を #rrggbb で付けるので自前で読む。」にする。

`wamei/term-restore--sgr-params` を差し替える (既定色の抑止を落とし、`face-foreground`/`face-background` 依存も消える):

```elisp
(defun wamei/term-restore--sgr-params (face)
  "ghostel の `face' plist FACE を SGR の引数 (文字列のリスト) にする。
属性、前景色、背景色の順。ghostel は装飾のないセルには face を付けないので、
ここに来る色は端末が明示したものだけ。"
  (let ((params nil))
    (when (eq (plist-get face :weight) 'bold) (push "1" params))
    (when (eq (plist-get face :slant) 'italic) (push "3" params))
    (when (plist-get face :underline) (push "4" params))
    (when (plist-get face :inverse-video) (push "7" params))
    (when (plist-get face :strike-through) (push "9" params))
    (pcase-dolist (`(,key ,code) '((:foreground "38") (:background "48")))
      (when-let* ((rgb (wamei/term-restore--rgb (plist-get face key))))
        (push (format "%s;2;%d;%d;%d" code (nth 0 rgb) (nth 1 rgb) (nth 2 rgb))
              params)))
    (nreverse params)))
```

`wamei/term-restore--ansi` の `font-lock-face` を `face` にする (docstring と 2 箇所の参照):

```elisp
(defun wamei/term-restore--ansi (text)
  "TEXT の `face' (ghostel が色ごとに付ける plist) を SGR エスケープにして
プロパティなしの文字列で返す。face が同じ区間ごとに開始のエスケープを置き、
区間の終わりで \\e[0m に戻す。"
  (let ((pos 0)
        (parts nil))
    (while (< pos (length text))
      (let* ((next (or (next-single-property-change pos 'face text)
                       (length text)))
             (chunk (substring-no-properties text pos next))
             (params (wamei/term-restore--sgr-params
                      (get-text-property pos 'face text))))
        (push (if params
                  (concat "\e[" (string-join params ";") "m" chunk "\e[0m")
                chunk)
              parts)
        (setq pos next)))
    (apply #'concat (nreverse parts))))
```

`wamei/term-restore--prompt-line-p` を差し替える:

```elisp
(defun wamei/term-restore--prompt-line-p ()
  "現在行がプロンプトの行なら非 nil。
ghostel は OSC 133 のシェル統合 (bash/zsh/fish に自動注入される) でプロンプトの
範囲を受け取り、その文字に `ghostel-prompt' プロパティを付ける。行内 (末尾の
改行を含む) にその印があればプロンプトの行とみなす。"
  (text-property-any (line-beginning-position)
                     (min (point-max) (1+ (line-end-position)))
                     'ghostel-prompt t))
```

`wamei/term-restore--tail` と `--content` の docstring から vterm の名前を外す (「vterm は画面の下端まで空行で埋める」→「端末は画面の下端まで空行で埋める」、「色 (font-lock-face) は残し」→「色 (face) は残し」)。

- [ ] **Step 4: テストが通ることを確認**

Run: `emacs -Q --batch -l term-restore-test.el -f ert-run-tests-batch-and-exit`
Expected: PASS (この時点では保存・復元の構造は旧来のまま)

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles
git add .emacs.d/term-restore.el .emacs.d/term-restore-test.el
git commit -m "$(cat <<'EOF'
スクロールバックの色とプロンプト判定を ghostel のプロパティに合わせる

ghostel は face を font-lock-face ではなく face プロパティに載せ、装飾の
ないセルには何も付けない。既定色のセルにもテーマ色を貼る vterm 向けの
抑止処理が不要になる。プロンプトの印は OSC 133 由来の ghostel-prompt。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_014YP2EKYnKbZGZFT9FxXZdD
EOF
)"
```

- [ ] **Step 6: 保存・注入・補完のテストを書く**

`term-restore-test.el` の `;;; 復元` 以降 (末尾の `(provide 'term-restore-test)` の直前まで) にある旧テストと `wamei/term-restore--with-fake-vterm` を削除し、次で置き換える:

```elisp
;;; 保存

(defmacro wamei/term-restore-test--with-saved-dir (&rest body)
  "スクロールバックの出力先を一時ディレクトリにして BODY を評価する。"
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "term-restore-" t)))
          (wamei/term-restore-directory dir)
          (wamei/term-restore-saved nil))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(ert-deftest wamei/term-restore-save-records-name-directory-and-title ()
  "端末ごとにバッファ名・ディレクトリ・タイトル・スクロールバックのパスを記録する。"
  (wamei/term-restore-test--with-saved-dir
    (let ((buffer (get-buffer-create "*term: foo 2*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq default-directory "/tmp/")
              (setq-local ghostel-title "make test")
              (insert "hello\n"))
            (wamei/term-restore-save)
            (let ((entry (car wamei/term-restore-saved)))
              (should (equal (plist-get entry :name) "*term: foo 2*"))
              (should (equal (plist-get entry :directory) "/tmp/"))
              (should (equal (plist-get entry :title) "make test"))
              (should (file-readable-p (plist-get entry :scrollback)))
              (should (equal (with-temp-buffer
                               (insert-file-contents (plist-get entry :scrollback))
                               (buffer-string))
                             "hello\n"))))
        (kill-buffer buffer)))))

(ert-deftest wamei/term-restore-save-prunes-unreferenced-files ()
  "記録されていないスクロールバックのファイルは消す。"
  (wamei/term-restore-test--with-saved-dir
    (let ((stale (expand-file-name "gone-1.txt" wamei/term-restore-directory)))
      (write-region "old\n" nil stale nil 'silent)
      (wamei/term-restore-save)
      (should-not (file-exists-p stale)))))

;;; スクロールバックの注入

(ert-deftest wamei/term-restore-inject-sets-env-for-saved-buffer ()
  "記録のあるバッファ名で端末が起動するとき WAMEI_TERM_RESTORE を渡す。"
  (wamei/term-restore-test--with-saved-dir
    (let ((file (expand-file-name "foo-1.txt" wamei/term-restore-directory)))
      (write-region "old output\n" nil file nil 'silent)
      (setq wamei/term-restore-saved
            (list (list :name "*term: foo*" :directory "/tmp/"
                        :title nil :scrollback file)))
      (with-current-buffer (get-buffer-create "*term: foo*")
        (unwind-protect
            ;; ghostel-pre-spawn-hook は process-environment を動的束縛した
            ;; 状態で呼ぶので、それを模す
            (let ((process-environment (copy-sequence process-environment)))
              (wamei/term-restore--inject-scrollback)
              (should (equal (getenv "WAMEI_TERM_RESTORE") file)))
          (kill-buffer (current-buffer)))))))

(ert-deftest wamei/term-restore-inject-ignores-missing-file ()
  "記録はあるがファイルが読めないときは環境変数を立てない。"
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: foo*" :directory "/tmp/"
                      :title nil
                      :scrollback (expand-file-name "gone.txt"
                                                    wamei/term-restore-directory))))
    (with-current-buffer (get-buffer-create "*term: foo*")
      (unwind-protect
          (let ((process-environment (copy-sequence process-environment)))
            (wamei/term-restore--inject-scrollback)
            (should-not (getenv "WAMEI_TERM_RESTORE")))
        (kill-buffer (current-buffer))))))

(ert-deftest wamei/term-restore-inject-ignores-unknown-buffer ()
  "記録の無いバッファでは何もしない。"
  (wamei/term-restore-test--with-saved-dir
    (with-current-buffer (get-buffer-create "*term: other*")
      (unwind-protect
          (let ((process-environment (copy-sequence process-environment)))
            (wamei/term-restore--inject-scrollback)
            (should-not (getenv "WAMEI_TERM_RESTORE")))
        (kill-buffer (current-buffer))))))

;;; 復元の仕上げ

(defmacro wamei/term-restore-test--with-fake-create (&rest body)
  "`ghostel-create' をバッファを作るだけの偽物にして BODY を評価する。
`calls' に (NAME . DIRECTORY) が積まれる。"
  (declare (indent 0))
  `(let ((calls nil))
     (cl-letf (((symbol-function 'ghostel-create)
                (lambda (&optional name &rest _)
                  (push (cons name default-directory) calls)
                  (get-buffer-create name))))
       ,@body)))

(ert-deftest wamei/term-restore-ensure-creates-missing-terminals ()
  "ghostel-desktop が復元しなかった端末だけを作り、タイトルを戻す。"
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: foo*" :directory "/tmp/"
                      :title "make test" :scrollback nil)))
    (wamei/term-restore-test--with-fake-create
      (unwind-protect
          (progn
            (wamei/term-restore-ensure)
            (should (equal calls '(("*term: foo*" . "/tmp/"))))
            (should (equal (buffer-local-value 'ghostel-title (get-buffer "*term: foo*"))
                           "make test")))
        (kill-buffer "*term: foo*")))))

(ert-deftest wamei/term-restore-ensure-leaves-live-terminals-alone ()
  "既にあるバッファは作り直さないが、タイトルは戻す
\(ghostel-desktop が desktop-read 中に復元した端末がこの経路に来る)。"
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: foo*" :directory "/tmp/"
                      :title "make test" :scrollback nil)))
    (let ((buffer (get-buffer-create "*term: foo*")))
      (unwind-protect
          (wamei/term-restore-test--with-fake-create
            (wamei/term-restore-ensure)
            (should-not calls)
            (should (equal (buffer-local-value 'ghostel-title buffer) "make test")))
        (kill-buffer buffer)))))

(ert-deftest wamei/term-restore-ensure-keeps-reported-title ()
  "端末が既にタイトルを報告していれば上書きしない。"
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: foo*" :directory "/tmp/"
                      :title "古いコマンド" :scrollback nil)))
    (let ((buffer (get-buffer-create "*term: foo*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer (setq-local ghostel-title "新しいコマンド"))
            (wamei/term-restore-test--with-fake-create
              (wamei/term-restore-ensure)
              (should (equal (buffer-local-value 'ghostel-title buffer) "新しいコマンド"))))
        (kill-buffer buffer)))))

(ert-deftest wamei/term-restore-ensure-clears-records ()
  "仕上げで記録を空にする。同じ名前で開き直した端末に前回の出力を再生しない。"
  (wamei/term-restore-test--with-saved-dir
    (let ((file (expand-file-name "foo-1.txt" wamei/term-restore-directory)))
      (write-region "old output\n" nil file nil 'silent)
      (setq wamei/term-restore-saved
            (list (list :name "*term: foo*" :directory "/tmp/"
                        :title nil :scrollback file)))
      (let ((buffer (get-buffer-create "*term: foo*")))
        (unwind-protect
            (wamei/term-restore-test--with-fake-create
              (wamei/term-restore-ensure)
              (should-not wamei/term-restore-saved)
              (with-current-buffer buffer
                (let ((process-environment (copy-sequence process-environment)))
                  (wamei/term-restore--inject-scrollback)
                  (should-not (getenv "WAMEI_TERM_RESTORE")))))
          (kill-buffer buffer))))))

(ert-deftest wamei/term-restore-ensure-falls-back-to-home ()
  "記録のディレクトリが無くなっていればホームで作る。"
  (wamei/term-restore-test--with-saved-dir
    (setq wamei/term-restore-saved
          (list (list :name "*term: foo*" :directory "/nonexistent-dir-xyz/"
                      :title nil :scrollback nil)))
    (wamei/term-restore-test--with-fake-create
      (unwind-protect
          (progn
            (wamei/term-restore-ensure)
            (should (equal (cdar calls) (expand-file-name "~/"))))
        (kill-buffer "*term: foo*")))))
```

テストファイル冒頭の `(require 'cl-lib)` の後に、Task 2 と同じ `ghostel-title` の宣言を足す:

```elisp
;; ghostel 本体の buffer-local 変数。ghostel を読まない batch でも
;; setq-local / buffer-local-value できるよう special にしておく。
(defvar-local ghostel-title nil
  "端末が報告したタイトル (テスト用のスタブ定義)。")
```

- [ ] **Step 7: テストを走らせて落ちることを確認**

Run: `emacs -Q --batch -l term-restore-test.el -f ert-run-tests-batch-and-exit`
Expected: FAIL。`wamei/term-restore--inject-scrollback` と `wamei/term-restore-ensure` が未定義、`wamei/term-restore-save` の記録が `:project`/`:index` の plist のまま。

- [ ] **Step 8: 保存・注入・補完を実装する**

`term-restore.el` の Commentary を差し替える:

```elisp
;;; term-restore.el --- desktop で端末 (ghostel) のスクロールバックを復元する -*- lexical-binding: t; -*-

;;; Commentary:

;; 端末バッファそのものの保存・復元は ghostel-desktop.el が受け持つ
;; (`ghostel-mode' が `desktop-save-buffer' を設定し、復元ハンドラは ghostel の
;; load 時に `desktop-buffer-mode-handlers' へ登録される)。ただし復元されるのは
;; ディレクトリと identity だけで、スクロールバックは戻らない。
;;
;; ここでは端末ごとに
;;   バッファ名 / 作業ディレクトリ / タイトル (最後のコマンド) /
;;   スクロールバックの末尾 N 行
;; を `wamei/term-restore-saved' に記録し、desktop のグローバル変数として
;; 一緒に保存する。
;;
;; スクロールバックは desktop ファイルを太らせないよう別ファイル
;; (`wamei/term-restore-directory' 配下) に置き、内容が変わったときだけ書く。
;; 復元時は `ghostel-pre-spawn-hook' でそのパスを WAMEI_TERM_RESTORE に載せ、
;; .zshrc が起動時に cat する。色は ghostel が付けた face を SGR エスケープに
;; 写して書いておき、cat したときに端末が解釈する。記録は復元の仕上げ
;; (`wamei/term-restore-ensure') で空にするので、同じ名前で開き直した端末に
;; 古い出力は出ない。
;;
;; `desktop-restore-eager' (init.el では 10) を超えた端末は desktop が idle 復元に
;; 回すため、side window の復元 (desktop-side-windows) に間に合わないことがある。
;; `wamei/term-restore-ensure' が `desktop-after-read-hook' で取りこぼしを作り、
;; タイトルを戻し、記録を空にする。
```

`wamei/term-restore-saved` の docstring を差し替える:

```elisp
(defvar wamei/term-restore-saved nil
  "前回保存した端末の記録。plist のリストで、各要素は
:name (バッファ名) :directory (作業ディレクトリ)
:title (最後に報告されたタイトル、無ければ nil) :scrollback (書き出したファイル)
を持つ。`desktop-globals-to-save' 経由で desktop ファイルに書かれる。")
```

`(defvar wamei/term--title)` の宣言を削除し、代わりに ghostel の宣言を置く:

```elisp
(defvar ghostel-title)                  ; ghostel.el (buffer-local)
(defvar ghostel-pre-spawn-hook)         ; ghostel.el
(declare-function ghostel-create "ghostel" (&optional name display identity))
```

`wamei/term-restore--entry` を差し替える:

```elisp
(defun wamei/term-restore--entry (project index buffer)
  "PROJECT の INDEX 番目の端末 BUFFER の記録を作り、スクロールバックを書き出す。
PROJECT と INDEX はスクロールバックのファイル名にだけ使う。"
  (let ((file (wamei/term-restore--scrollback-file project index)))
    (with-current-buffer buffer
      (wamei/term-restore--write-scrollback
       file
       (wamei/term-restore--ansi
        (wamei/term-restore--tail (wamei/term-restore--content)
                                  wamei/term-restore-scrollback-lines)))
      (list :name (buffer-name buffer)
            :directory default-directory
            :title (buffer-local-value 'ghostel-title buffer)
            :scrollback file))))
```

`;;; 復元` 節の `(declare-function vterm "vterm")`、`wamei/term-restore--buffer-name`、`wamei/term-restore--create`、`wamei/term-restore-all` を次で置き換える:

```elisp
;;; 復元

(defun wamei/term-restore--entry-for (name)
  "バッファ名 NAME の記録。無ければ nil。"
  (seq-find (lambda (entry) (equal (plist-get entry :name) name))
            wamei/term-restore-saved))

(defun wamei/term-restore--inject-scrollback ()
  "この端末に記録があれば WAMEI_TERM_RESTORE に載せる。
`ghostel-pre-spawn-hook' から呼ぶ。このフックは端末バッファで
`process-environment' を動的束縛した状態で呼ばれるので、`setenv' がそのまま
子プロセスに届く。記録を消すのはここではなく `wamei/term-restore-ensure'
\(desktop の復元が終わったとき)。ここで消すと、ghostel-desktop が
`desktop-read' 中に復元した端末の記録が仕上げに届かず、タイトルを戻せない。"
  (when-let* ((entry (wamei/term-restore--entry-for (buffer-name)))
              (file (plist-get entry :scrollback)))
    (when (file-readable-p file)
      (setenv "WAMEI_TERM_RESTORE" file))))

(defun wamei/term-restore--entry-directory (entry)
  "記録 ENTRY の作業ディレクトリ。無くなっていればホーム。
変数 `wamei/term-restore-directory' (スクロールバックの置き場) とは別物。"
  (let ((directory (plist-get entry :directory)))
    (if (and directory (file-directory-p directory))
        (file-name-as-directory directory)
      (expand-file-name "~/"))))

(defun wamei/term-restore-ensure ()
  "desktop の復元の仕上げ。取りこぼした端末を作り、タイトルを戻し、記録を空にする。
`desktop-after-read-hook' から (深さ -10 で) 呼ぶ。side window の開き直し
\(desktop-side-windows) より先に走らせ、パネルに出すバッファを用意しておく。

端末の生成は `desktop-restore-eager' (init.el では 10) を超えて idle 復元に
回されたものの取りこぼし。ghostel-desktop が `desktop-read' 中に復元していれば
生成は起きず、タイトルの復元だけが効く。端末が既にタイトルを報告していれば
そちらを残す。記録を最後に空にするのは、同じ名前で開き直した端末に前回の
出力を再生しないため。"
  (dolist (entry wamei/term-restore-saved)
    (let ((name (plist-get entry :name)))
      (condition-case err
          (let ((buffer (or (get-buffer name)
                            (let ((default-directory
                                   (wamei/term-restore--entry-directory entry)))
                              (ghostel-create name)))))
            (when-let* ((title (plist-get entry :title)))
              (with-current-buffer buffer
                (unless ghostel-title (setq-local ghostel-title title)))))
        (error (message "term-restore: %s を復元できません: %s"
                        name (error-message-string err))))))
  (setq wamei/term-restore-saved nil))
```

`wamei/term-restore-setup` を差し替える:

```elisp
(defun wamei/term-restore-setup ()
  "desktop の保存・読み込みに組み込む。"
  (add-to-list 'desktop-globals-to-save 'wamei/term-restore-saved)
  (add-hook 'desktop-save-hook #'wamei/term-restore-save)
  ;; 端末の起動時にスクロールバックを環境変数で渡す
  ;; (ghostel-desktop の復元経路でも wamei/term-restore-ensure でも通る)
  (add-hook 'ghostel-pre-spawn-hook #'wamei/term-restore--inject-scrollback)
  ;; desktop-side-windows の開き直しより先に、取りこぼした端末を用意して
  ;; タイトルを戻し、記録を空にする
  (add-hook 'desktop-after-read-hook #'wamei/term-restore-ensure -10))
```

`wamei/term-restore--prune` の docstring はそのままで動く (`:scrollback` キーは変わっていない)。

- [ ] **Step 9: テストが通ることを確認**

Run: `emacs -Q --batch -l term-restore-test.el -f ert-run-tests-batch-and-exit`
Expected: PASS

- [ ] **Step 10: コミット**

```bash
cd ~/.dotfiles
git add .emacs.d/term-restore.el .emacs.d/term-restore-test.el
git commit -m "$(cat <<'EOF'
端末の desktop 復元を ghostel-desktop に委譲する

バッファの保存・再生成は ghostel-desktop.el が持っているので、自前の
生成経路を捨てる。残すのはスクロールバックの書き出しと、
ghostel-pre-spawn-hook での WAMEI_TERM_RESTORE 注入。フックは
process-environment を動的束縛した状態で呼ばれるので setenv で足りる。

記録は desktop の復元が終わった時点で空にする。同じ名前で開き直した端末に
前回の出力が出るのを防ぐ。注入時に消さないのは、ghostel-desktop が復元した
端末の記録が仕上げに届かずタイトルを戻せなくなるため。desktop-restore-eager
が 10 なので、lazy に回された端末は desktop-after-read-hook で作り直す。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_014YP2EKYnKbZGZFT9FxXZdD
EOF
)"
```

---

### Task 4: claude-panel.el のタイトルを ghostel-title の遅延参照にする

`vterm--set-title` の advice で会話名を覚えていたのをやめ、`ghostel-title` を読む。tab-line は `tab-line-cache-key-function` が変化を検知して描き直すので、`force-mode-line-update` を呼ぶ必要はなくなり buffer-local 状態が 1 つ減る。Cmd+V の束縛も、init.el の `claude-code-ide--configure-vterm-buffer` advice (vterm ブランチ専用なので使えない) からこのファイルの合成キーマップへ移す。

**Files:**
- Modify: `claude-panel.el`
- Test: `claude-panel-test.el`

**Interfaces:**
- Consumes: `(wamei/term-input-paste)` (Task 1)
- Produces:
  - `(wamei/claude-panel--tab-name BUFFER &optional _BUFFERS)` — タブ名
  - `(wamei/claude-panel--cache-key TABS)` — tab-line のキャッシュキー
  - `wamei/claude-panel-map` — `C-tab` / `C-S-tab` / `C-S-iso-lefttab` / `s-v`

- [ ] **Step 1: テストを ghostel-title 前提に書き換える**

`claude-panel-test.el` の `;;; タブ名: Claude が端末タイトルに出すセッション名` 節にある 3 つのテストを置き換える。

- `wamei/claude-panel-tab-name-prefers-terminal-title` (140 行目付近)
- `wamei/claude-panel-record-title-ignores-non-claude-buffers` (149 行目付近) — **削除**。記録という段がなくなり、`--tab-name` は tab-line が Claude のバッファに対してだけ呼ぶので、セッション外のバッファを弾く必要がない
- `wamei/claude-panel-record-title-keeps-fallback-on-blank-title` (155 行目付近)

前 2 つを次で置き換える (3 つ目は下の `-keeps-fallback-on-blank-title` が引き継ぐ):

```elisp
(ert-deftest wamei/claude-panel-tab-name-comes-from-ghostel-title ()
  "タブ名は端末が報告したタイトルから状態表示を外したもの。"
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/" "a"))
           (buffer (claude-code-ide-mcp-session-buffer a)))
      (with-current-buffer buffer
        (setq-local ghostel-title "✳ 会話の名前"))
      (should (equal (wamei/claude-panel--tab-name buffer) "会話の名前")))))

(ert-deftest wamei/claude-panel-tab-name-keeps-fallback-on-blank-title ()
  "状態表示だけのタイトルや空のタイトルではセッション名に落とす。"
  (wamei/claude-panel-test--with-env
    (let* ((a (wamei/claude-panel-test--session "/tmp/proj/" "a"))
           (buffer (claude-code-ide-mcp-session-buffer a)))
      (with-current-buffer buffer
        (setq-local ghostel-title "✳ "))
      (should (equal (wamei/claude-panel--tab-name buffer) "proj:a"))
      (with-current-buffer buffer
        (setq-local ghostel-title nil))
      (should (equal (wamei/claude-panel--tab-name buffer) "proj:a")))))
```

`wamei/claude-panel-cache-key-changes-with-title` の `(wamei/claude-panel--record-title "✳ 新しい名前")` を次にする:

```elisp
          (with-current-buffer buffer (setq-local ghostel-title "✳ 新しい名前"))
```

`wamei/claude-panel-enable-hooks-vterm-title` を**削除**し、代わりに次を足す:

```elisp
(ert-deftest wamei/claude-panel-map-binds-super-v-to-paste ()
  "Claude のバッファでは Cmd+V がクリップボードの画像を端末へ渡す。"
  (should (eq (lookup-key wamei/claude-panel-map (kbd "s-v"))
              #'wamei/term-input-paste)))
```

テストファイル冒頭の require の後に、`ghostel-title` の宣言と term-input.el の読み込みを足す:

```elisp
;; ghostel 本体の buffer-local 変数 (テスト用のスタブ定義)。
(defvar-local ghostel-title nil
  "端末が報告したタイトル。")

;; wamei/claude-panel-map が s-v に束縛するコマンドの実体。
(load (expand-file-name "term-input.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
```

- [ ] **Step 2: テストを走らせて落ちることを確認**

Run: `emacs -Q --batch -l claude-panel-test.el -f ert-run-tests-batch-and-exit`
Expected: FAIL。`--tab-name` が `wamei/claude-panel--title` を読むのでタイトルが反映されず、`s-v` の束縛も無い。

- [ ] **Step 3: claude-panel.el を書き換える**

`wamei/claude-panel--title` の `defvar-local` (77 行目付近) と `wamei/claude-panel--record-title` (90-96 行目) を削除し、代わりに次を置く:

```elisp
(defvar ghostel-title)                  ; ghostel.el (buffer-local)
(declare-function wamei/term-input-paste "term-input")

(defun wamei/claude-panel--title (buffer)
  "BUFFER の会話名。端末が報告したタイトルから状態表示を外したもの。
ghostel は OSC 0/2 のタイトルを `ghostel-title' に入れる。claude-code-ide は
Claude のバッファでバッファ名の自動リネームを切るが、`ghostel-title' 自体は
設定されるので値は読める。"
  (when-let* ((title (buffer-local-value 'ghostel-title buffer)))
    (wamei/claude-panel--clean-title title)))
```

`wamei/claude-panel--tab-name` の 1 行目を差し替える:

```elisp
  (or (wamei/claude-panel--title buffer)
```

`wamei/claude-panel--cache-key` の `mapcar` を差し替える:

```elisp
          (mapcar #'wamei/claude-panel--title tabs)))
```

`wamei/claude-panel-map` に Cmd+V を足し、docstring を更新する:

```elisp
(defvar wamei/claude-panel-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-tab>") #'wamei/claude-panel-next)
    (define-key map (kbd "<C-S-tab>") #'wamei/claude-panel-previous)
    ;; 端末によっては Shift-Tab が iso-lefttab として報告される
    (define-key map (kbd "<C-S-iso-lefttab>") #'wamei/claude-panel-previous)
    ;; Cmd+V でクリップボードの画像を Claude に渡す (term-input.el)。
    ;; シェルでは C-v が quoted-insert になるので端末パネル全体には掛けない。
    (define-key map (kbd "s-v") #'wamei/term-input-paste)
    map)
  "Claude バッファでセッションを巡回し、Cmd+V を横取りするキーマップ。
グローバルの C-tab (端末パネルの巡回) と Cmd+V をバッファ内だけ上書きする。")
```

`wamei/claude-panel--setup` のコメント「vterm-mode-map は全端末で共有なので」を「ghostel のキーマップは全端末で共有なので」にする。

`wamei/claude-panel-enable` から次の 2 行を削除する:

```elisp
  (with-eval-after-load 'vterm
    (advice-add 'vterm--set-title :before #'wamei/claude-panel--record-title))
```

- [ ] **Step 4: テストが通ることを確認**

Run: `emacs -Q --batch -l claude-panel-test.el -f ert-run-tests-batch-and-exit`
Expected: PASS

- [ ] **Step 5: コミット**

```bash
cd ~/.dotfiles
git add .emacs.d/claude-panel.el .emacs.d/claude-panel-test.el
git commit -m "$(cat <<'EOF'
Claude パネルの会話名を ghostel-title から読む

private な vterm--set-title への advice で覚えていたのをやめ、
ghostel-title を必要なときに読む。tab-line-cache-key-function が変化を
見ているので描き直しは起きる。buffer-local 状態が 1 つ減る。

Cmd+V の束縛は、claude-code-ide--configure-vterm-buffer (vterm ブランチ
専用で ghostel では呼ばれない) への advice から、このファイルが既に
持っている合成キーマップへ移す。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_014YP2EKYnKbZGZFT9FxXZdD
EOF
)"
```

---

### Task 5: init.el を leaf ghostel に差し替える

Task 1-4 の結線を入れ替え、libvterm 向けの advice 3 本と `vterm-mode-map` への `define-key` 6 本を落とす。

**Files:**
- Modify: `.emacs.d/init.el`

**Interfaces:**
- Consumes: Task 1-4 が produce したものすべて
- Produces: なし (設定の最終結線)

- [ ] **Step 1: leaf vterm を leaf ghostel に置き換える**

`init.el` の `(leaf vterm ...)` ブロック (312-427 行目) を次で置き換える:

```elisp
(leaf ghostel
  :doc "フレーム下部に固定する端末パネル"
  :ensure t
  :bind (("C-z" . wamei/term-toggle)
         ("C-S-z" . wamei/term-new)
         ("C-q t c" . wamei/term-new)
         ;; グローバルに置くことで tab-bar-mode の再有効化に上書きされない
         ("<C-tab>" . wamei/term-next)
         ("C-q t n" . wamei/term-next)
         ("<C-S-tab>" . wamei/term-previous)
         ("C-q t p" . wamei/term-previous)
         ;; 端末によっては Shift-Tab が iso-lefttab として報告される
         ("<C-S-iso-lefttab>" . wamei/term-previous))
  ;; ghostel-keymap-exceptions は「端末へ送らず Emacs 側で処理するキー」。
  ;; 既定 ("C-c" "C-x" "C-u" "C-h" "M-x" "M-:" "C-\\") に、パネルの操作と
  ;; Emacs 側で使いたいキーを足す。文字列は key-description 形式なので
  ;; "<C-tab>" ではなく "C-<tab>" と書く。C-S-z と C-S-<iso-lefttab> は
  ;; ghostel が束縛しないので挙げる必要がない。
  ;; :set でキーマップを作り直す defcustom なので customize 経由で設定する。
  ;;
  ;; C-c は既定どおり例外のままにする (端末へは C-c C-c で SIGINT が届く)。
  ;; その代わり C-c 配下の ghostel のコマンド (C-c C-t copy mode /
  ;; C-c C-l line mode / C-c C-p ハイパーリンク) が使える。
  ;;
  ;; ghostel-module-directory は、既定のパッケージディレクトリだと package の
  ;; 更新でロード中のモジュールが消えるので elpa の外に置く。
  :custom `((ghostel-keymap-exceptions
             . '("C-c" "C-x" "C-u" "C-h" "M-x" "M-:" "C-\\"
                 "C-g" "C-l" "M-o" "M-w"
                 "C-q" "C-z"
                 "C-<tab>" "C-S-<tab>"))
            ;; ghostel-max-scrollback は行数ではなく「バイト数」(既定 5MB)。
            ;; 移行元の vterm-max-scrollback は行数 (10000 行) だったので、
            ;; docstring の「5MB ≒ 5,000 行」の比率から 10MB にして
            ;; 10000 行相当を確保する。行数だと思って 10000 を入れると
            ;; 約 10KB = 十数行しか残らない。
            (ghostel-max-scrollback . ,(* 10 1024 1024))
            (ghostel-module-directory . ,(locate-user-emacs-file "ghostel/"))
            ;; タイトルが変わったら端末一覧を描き直す (term-panel.el)。
            ;; nil を返す関数なのでバッファ名は変わらない。
            (ghostel-buffer-name-function . #'wamei/term--on-title-change))
  :preface
  ;; kill-ring 連携とクリップボードの画像渡し。実体は term-input.el
  ;; (init.el は symlink なので実体の隣から読む)。
  (load (expand-file-name "term-input"
                          (file-name-directory (file-truename user-init-file)))
        nil t)

  ;; 端末パネル (下部 side window) と端末一覧の管理は term-panel.el。
  ;; 端末・一覧ともプロジェクト (タブ) ごとにバッファを分ける。
  (load (expand-file-name "term-panel"
                          (file-name-directory (file-truename user-init-file)))
        nil t)

  (defconst wamei/term-glyph-substitutions
    '((?⏺ . ?●)    ; claude の応答・ツール呼び出しの行頭
      (?⏵ . ?▶)    ; claude の "⏵⏵ auto mode on"
      (?⧉ . ?❐))   ; claude の "⧉ In file" (❐ は Menlo が持つ)
    "端末バッファで表示だけ置き換える文字の alist (元の文字 . 表示する文字)。
これらは手元のどのフォントでも行高が既定フォント (20px) に収まらず
(STIX Two Math は descent 9px)、含む行だけ伸びて TUI の画面が上下に揺れる。
バッファの内容は変えず display table で同形の記号を描く。")

  (defun wamei/term--substitute-tall-glyphs ()
    "`wamei/term-glyph-substitutions' を現在のバッファの display table に登録する。
ghostel-mode は `buffer-display-table' を使わないので、無ければ自分で作る。
face を付けないので元の文字の色はそのまま引き継がれる。"
    (when (display-graphic-p)
      (let ((table (or buffer-display-table (make-display-table))))
        (pcase-dolist (`(,from . ,to) wamei/term-glyph-substitutions)
          (aset table from (vector (make-glyph-code to))))
        (setq buffer-display-table table))))
  :init
  ;; 端末は下部 side window の slot 0、一覧は同じ side の slot 1 (右隣) へ。
  ;; :config だと ghostel がロードされるまで登録されないので :init で行う。
  (wamei/term-panel-setup)
  :config
  ;; C-k はそのまま端末へ送ると zsh の CUTBUFFER にしか残らないので、
  ;; 送る前に point から行末までを kill-ring に入れる (term-input.el)。
  (define-key ghostel-semi-char-mode-map (kbd "C-k") #'wamei/term-input-kill-line)
  ;; Cmd+V は kill-ring から端末へ貼る (ghostel の既定では未束縛でグローバルの
  ;; yank が効いてしまい、バッファに挿入されるだけで端末には届かない)。
  ;; C-y / M-y は ghostel が ghostel-yank / ghostel-yank-pop を持っている。
  (define-key ghostel-semi-char-mode-map (kbd "s-v") #'ghostel-yank)

  (add-hook 'ghostel-mode-hook #'wamei/term--substitute-tall-glyphs)
  ;; 高さの記憶、kill 時の後始末、非アクティブ時のカーソル非表示 (term-panel.el)
  (add-hook 'ghostel-mode-hook #'wamei/term--setup-buffer))
```

- [ ] **Step 2: hide-mode-line の hook 名を差し替える**

296-299 行目の `vterm-mode-hook` を `ghostel-mode-hook` にする:

```elisp
  :hook
  ((ghostel-mode-hook) . hide-mode-line-mode)
  ((dired-mode-hook ghostel-mode-hook wamei/term-list-mode-hook)
   . (lambda() (display-line-numbers-mode 0)))
```

`display-line-numbers-mode 0` は見た目だけの設定ではない。ghostel は
`window-max-chars-per-line` で pty の桁数を決めるので、行番号が出ていると
pty が window より狭くなる (Phase 0 実測で 40 桁の window に対し 35 桁)。

- [ ] **Step 3: claude-code-ide と docker のバックエンドを差し替える**

`leaf claude-code-ide` の `:custom` を変更する:

- `(claude-code-ide-terminal-backend . 'vterm)` → `(claude-code-ide-terminal-backend . 'ghostel)`
- `(claude-code-ide-prevent-reflow-glitch . nil)` の行とその上のコメント (538-546 行目) を**削除** (vterm 専用の workaround で、ghostel ブランチでは参照されない)

`leaf claude-code-ide` の `:config` から次の 2 本の advice とそのコメント (551-558 行目) を**削除**する:

```elisp
  (advice-add 'claude-code-ide--configure-vterm-buffer :after #'...term-input-mouse-mode)
  (advice-add 'claude-code-ide--configure-vterm-buffer :after #'...term-input-paste-mode)
```

`leaf docker` の `:custom` を変更する:

```elisp
   ;; 対話が要るコマンド (exec / attach / image run) を出す端末。既定の auto は
   ;; eat > ghostel > vterm > shell の順に見つけたものを使うので、後で eat を
   ;; 入れたときに黙って切り替わる。ghostel に固定する。
   ;; ここで開く端末のバッファ名は "* docker ... *" で、端末パネルの
   ;; display-buffer-alist ("\\`\\*term: ") には当たらないのでパネルとは独立に出る。
   (docker-terminal-backend . 'ghostel)))
```

- [ ] **Step 4: desktop 周りのコメントを実態に合わせる**

`init.el:1035-1036` のコメントを差し替える:

```elisp
  ;; 端末バッファの保存・復元は ghostel-desktop.el が受け持つ。term-restore.el は
  ;; スクロールバックの書き出しと、起動時に WAMEI_TERM_RESTORE で渡す部分だけを持つ。
```

`wamei/desktop--restore-term` の docstring 2-3 行目を差し替える (端末を作るのが term-restore ではなくなったため):

```elisp
端末バッファは ghostel-desktop が desktop-read 中に、取りこぼしは term-restore
\(desktop-after-read-hook の先頭) が復元しているので、パネルに出ていたもの
\(SPEC の :buffer) をそのまま出す。
```

- [ ] **Step 5: init.el がエラーなくロードできることを確認**

Run:
```bash
S=/tmp/ghostel-init-check
rm -rf "$S" && mkdir -p "$S/elpa"
ln -s ~/.dotfiles/.emacs.d/init.el "$S/init.el"
ln -s ~/.dotfiles/.emacs.d/early-init.el "$S/early-init.el"
for d in eln-cache tree-sitter .cache; do ln -s ~/.emacs.d/$d "$S/$d"; done
for p in ~/.emacs.d/elpa/*; do ln -s "$p" "$S/elpa/$(basename "$p")"; done
emacs --daemon=ghostel-init-check --init-directory="$S" 2>&1 | tail -20
emacsclient -s ghostel-init-check --eval '(list (featurep (quote ghostel)) ghostel-keymap-exceptions claude-code-ide-terminal-backend docker-terminal-backend)'
emacsclient -s ghostel-init-check --eval '(kill-emacs)'
```

Expected: daemon が起動し、`(t ("C-c" ...) ghostel ghostel)` が返る。`Error` / `void-function` / `void-variable` が出ないこと。

`ghostel` が MELPA から入っていない環境では `:ensure t` が取得する。ネイティブモジュールは初回の `M-x ghostel` で `ghostel-module-auto-install` (既定 `ask`) が尋ねる。

- [ ] **Step 6: コミット**

```bash
cd ~/.dotfiles
git add .emacs.d/init.el
git commit -m "$(cat <<'EOF'
端末を vterm から ghostel に差し替える

libvterm の不足を埋めていた 3 本の advice を落とす:
vterm--get-color (既定色セルに色が貼られて auto-dim が透けない)、
vterm--filter (shell 起動時の stty が pty サイズを上書きする)、
vterm--set-title (タイトルを拾う口が無い)。ghostel はいずれも本体で
面倒を見る。

キーは ghostel-keymap-exceptions で「Emacs 側に残すキー」を宣言する方式に
寄せ、vterm-mode-map への define-key 6 本をやめる。C-c は prefix のままに
するので SIGINT は C-c C-c、代わりに C-c C-t / C-c C-l が使える。
例外の文字列は key-description 形式 ("C-<tab>")。

display-line-numbers-mode 0 は ghostel では見た目の設定ではない。
window-max-chars-per-line で pty の桁数が決まるため、行番号が出ていると
pty が window より狭くなる。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_014YP2EKYnKbZGZFT9FxXZdD
EOF
)"
```

---

### Task 6: .zshrc のプロンプト印を落とす

ghostel は OSC 133 のシェル統合を zsh に自動注入するので、vterm 独自の OSC 51;A を出す必要がない。

**Files:**
- Modify: `.zshrc`

**Interfaces:**
- Consumes: `wamei/term-restore--inject-scrollback` が渡す `WAMEI_TERM_RESTORE` (Task 3)
- Produces: なし

- [ ] **Step 1: PROMPT から印の呼び出しを外す**

`.zshrc:136` のコメントを削除し、139 / 142 / 144 行目の `$(_wamei_vterm_prompt_mark)` を含む `%{...%}` を取る:

- `PROMPT="%B%F{white}%(?..%K{red}            status code -%?-            %{%k%}%{\$(_wamei_vterm_prompt_mark)%}` → 末尾の `%{\$(_wamei_vterm_prompt_mark)%}` を削除
- `PROMPT+='$(show_env)%{$(_wamei_vterm_prompt_mark)%}'` → `PROMPT+='$(show_env)'`
- `$ %{\$(_wamei_vterm_prompt_mark)%}"` → `$ "`

- [ ] **Step 2: 復元ブロックを ghostel 判定にする**

240-256 行目を次で置き換える:

```sh
# Emacs (ghostel) 内でだけ効くセッション復元の連携 (term-restore.el)。
# プロンプトの位置は ghostel が OSC 133 のシェル統合を自動注入して拾うので、
# ここでは何も出さない (vterm 時代は OSC 51;A を自前で出していた)。
if [[ $INSIDE_EMACS == *ghostel* ]]; then
  # 復元された端末では前回の出力の末尾が WAMEI_TERM_RESTORE のファイルに入っている
  # (色は term-restore.el が SGR エスケープにして書いてある)。最初のプロンプトの前に
  # そのまま出し、子プロセスに引き継がないよう unset する。
  if [[ -n $WAMEI_TERM_RESTORE && -r $WAMEI_TERM_RESTORE ]]; then
    cat -- "$WAMEI_TERM_RESTORE"
  fi
  unset WAMEI_TERM_RESTORE
fi
```

`_wamei_set_terminal_title` (preexec で OSC 0 を出す) と `add-zsh-hook` はそのまま残す。これが `ghostel-title` の出どころになる。

- [ ] **Step 3: zsh が構文エラーなく読めることを確認**

Run: `zsh -n ~/.dotfiles/.zshrc && echo OK`
Expected: `OK`

Run: `grep -n "_wamei_vterm_prompt_mark\|INSIDE_EMACS" ~/.dotfiles/.zshrc`
Expected: `_wamei_vterm_prompt_mark` が 1 件も出ず、`INSIDE_EMACS` は `*ghostel*` の 1 行だけ。

- [ ] **Step 4: コミット**

```bash
cd ~/.dotfiles
git add .zshrc
git commit -m "$(cat <<'EOF'
プロンプトの位置を ghostel のシェル統合に任せる

ghostel は OSC 133 のシェル統合を zsh に自動注入するので、PROMPT の各行末
から vterm 独自の OSC 51;A を出す必要がない。スクロールバックの再生
(WAMEI_TERM_RESTORE の cat) は INSIDE_EMACS=ghostel で判定する。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_014YP2EKYnKbZGZFT9FxXZdD
EOF
)"
```

---

### Task 7: term-faint.el と vterm を消す

libvterm に SGR 2 (faint) が無いための入力ストリーム書き換えは、ghostel が faint を実装している (fg を bg 方向にブレンドする) ので不要。

**Files:**
- Delete: `.emacs.d/term-faint.el`, `.emacs.d/term-faint-test.el`

**Interfaces:**
- Consumes: なし (Task 5 で `term-faint.el` の load と `wamei/term-faint-enable` は既に消えている)
- Produces: なし

- [ ] **Step 1: どこからも参照されていないことを確認**

Run: `grep -rn "term-faint\|wamei/term-faint" ~/.dotfiles --include="*.el" --include="*.zshrc" --include="*.md" | grep -v docs/superpowers`
Expected: 出力なし。何か出たら Task 5 の差し替え漏れなので先に直す。

- [ ] **Step 2: ファイルを削除する**

```bash
cd ~/.dotfiles
git rm .emacs.d/term-faint.el .emacs.d/term-faint-test.el
```

- [ ] **Step 3: 残ったテストが全部通ることを確認**

Run:
```bash
cd ~/.dotfiles/.emacs.d
for f in term-input term-panel term-restore claude-panel claude-grid claude-usage early-init; do
  echo "== $f =="
  emacs -Q --batch -l "$f-test.el" -f ert-run-tests-batch-and-exit || echo "FAILED: $f"
done
```
Expected: すべて PASS、`FAILED:` が出ない。

- [ ] **Step 4: vterm をアンインストールする**

Run:
```bash
emacsclient -s ghostel-init-check --eval '(kill-emacs)' 2>/dev/null
emacs -Q --batch --eval '(progn (setq package-user-dir (expand-file-name "~/.emacs.d/elpa")) (package-initialize) (if (package-installed-p (quote vterm)) (package-delete (car (alist-get (quote vterm) package-alist)) t) (message "vterm not installed")))'
ls ~/.emacs.d/elpa | grep -c vterm
```
Expected: 最後の `grep -c` が `0`。

起動中の Emacs が vterm をロードしている場合、`package-delete` はファイルを消すだけでロード済みのシンボルは残る。ロード済みの Emacs を再起動するまで vterm のバッファは動き続けるので、先に端末を閉じておく。

- [ ] **Step 5: 参照が残っていないことを確認**

Run: `grep -rn "vterm" ~/.dotfiles --include="*.el" --include="*.zshrc" | grep -v docs/superpowers`
Expected: 出力なし。

- [ ] **Step 6: コミット**

```bash
cd ~/.dotfiles
git commit -m "$(cat <<'EOF'
term-faint.el を消す

libvterm は SGR 2 (faint) を実装しておらず、薄字の指定がセルへ届く前に
落ちるので、vterm--filter の around advice で入力ストリームの SGR 2 を
色指定に書き換えていた。ghostel は faint を実装している (前景色を背景色
方向にブレンドする) ので、この層は要らない。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_014YP2EKYnKbZGZFT9FxXZdD
EOF
)"
```

---

### Task 8: 隔離 daemon と GUI で目視確認する

batch テストは純関数と結線しか見ない。実際の描画・pty・desktop 復元は動かして確かめる。

**Files:** なし (確認のみ)

**Interfaces:**
- Consumes: Task 1-7 のすべて
- Produces: なし

- [ ] **Step 1: 隔離 daemon を立てて tty frame を付ける**

Run:
```bash
S=/tmp/ghostel-verify
rm -rf "$S" && mkdir -p "$S/elpa"
ln -s ~/.dotfiles/.emacs.d/init.el "$S/init.el"
ln -s ~/.dotfiles/.emacs.d/early-init.el "$S/early-init.el"
for d in eln-cache tree-sitter .cache; do ln -s ~/.emacs.d/$d "$S/$d"; done
for p in ~/.emacs.d/elpa/*; do ln -s "$p" "$S/elpa/$(basename "$p")"; done
emacs --daemon=ghostel-verify --init-directory="$S" 2>&1 | tail -5
emacsclient -s ghostel-verify --eval '(progn (setq ghostel-module-auto-install (quote download)) (ghostel-download-module) (ghostel--module-version))'
tmux kill-session -t ghostel-verify 2>/dev/null
tmux new-session -d -s ghostel-verify -x 120 -y 40 "emacsclient -s ghostel-verify -t"
```

Expected: モジュールのバージョン文字列 (`"0.53.0"` 以上) が返る。

**注意:** frame が無いと ghostel は描画しない (`ghostel--redraw-now` は render window が無いとスキップする)。以降の確認は必ず tty frame を付けた状態で行う。

- [ ] **Step 2: 端末パネルと pty サイズを確認**

Run:
```bash
emacsclient -s ghostel-verify --eval '(progn (wamei/term-toggle) (list (buffer-name (window-buffer (wamei/term--window))) (window-body-width (wamei/term--window))))'
```
Expected: `("*term: <project>*" N)` が返る。

Run:
```bash
emacsclient -s ghostel-verify --eval '(with-current-buffer (window-buffer (wamei/term--window)) (list (bound-and-true-p display-line-numbers) (bound-and-true-p hide-mode-line-mode)))'
```
Expected: `(nil t)` — 行番号が消えていること (出ていると pty が window より狭くなる)。

Run:
```bash
emacsclient -s ghostel-verify --eval '(with-current-buffer (window-buffer (wamei/term--window)) (ghostel-send-string "stty size\n"))'
sleep 1
emacsclient -s ghostel-verify --eval '(with-current-buffer (window-buffer (wamei/term--window)) (list (window-max-chars-per-line (get-buffer-window (current-buffer))) (window-body-height (get-buffer-window (current-buffer))) (buffer-substring-no-properties (max (point-min) (- (point) 120)) (point))))'
```
Expected: `stty size` の出力の行数・桁数が `window-body-height` / `window-max-chars-per-line` と一致する。

- [ ] **Step 3: 2 つ目の端末と一覧、タイトル追従を確認**

Run:
```bash
emacsclient -s ghostel-verify --eval '(wamei/term-new)'
sleep 1
emacsclient -s ghostel-verify --eval '(with-current-buffer (window-buffer (wamei/term--window)) (ghostel-send-string "sleep 3\n"))'
sleep 1
emacsclient -s ghostel-verify --eval '(with-current-buffer (wamei/term--list-buffer) (buffer-string))'
```
Expected: 一覧に 2 行出て、実行中の端末のラベルが `sleep` になっている (`.zshrc` の preexec が OSC 0 でタイトルを流し、`ghostel-buffer-name-function` に入れた `wamei/term--on-title-change` が一覧を描き直す)。

- [ ] **Step 4: kill-ring 連携と C-c を確認**

Run:
```bash
emacsclient -s ghostel-verify --eval '(with-current-buffer (window-buffer (wamei/term--window)) (list (key-binding (kbd "C-k")) (key-binding (kbd "s-v")) (key-binding (kbd "C-c C-c")) (key-binding (kbd "C-z")) (key-binding (kbd "C-<tab>"))))'
```
Expected: `(wamei/term-input-kill-line ghostel-yank ghostel-send-C-c wamei/term-toggle wamei/term-next)`

- [ ] **Step 5: desktop の保存と復元を確認**

Run:
```bash
emacsclient -s ghostel-verify --eval '(progn (desktop-save (car desktop-path) t) (list (length wamei/term-restore-saved) (mapcar (lambda (e) (plist-get e :name)) wamei/term-restore-saved)))'
emacsclient -s ghostel-verify --eval '(kill-emacs)'
tmux kill-session -t ghostel-verify
```
Expected: 端末の数だけ記録があり、`:name` が `*term: ...*` になっている。

Run:
```bash
emacs --daemon=ghostel-verify --init-directory=/tmp/ghostel-verify 2>&1 | tail -5
tmux new-session -d -s ghostel-verify -x 120 -y 40 "emacsclient -s ghostel-verify -t"
sleep 3
emacsclient -s ghostel-verify --eval '(mapcar #'"'"'buffer-name (seq-filter (lambda (b) (string-prefix-p "*term: " (buffer-name b))) (buffer-list)))'
emacsclient -s ghostel-verify --eval '(with-current-buffer (window-buffer (wamei/term--window)) (buffer-substring-no-properties (point-min) (min (point-max) 400)))'
```
Expected: 端末バッファが本数どおり戻り、パネルの中身に前回のスクロールバック (色付き) が出ている。

- [ ] **Step 6: Claude Code のパネルを確認**

Run:
```bash
emacsclient -s ghostel-verify --eval '(claude-code-ide)'
sleep 5
emacsclient -s ghostel-verify --eval '(let ((b (seq-find (lambda (b) (string-prefix-p "*claude-code" (buffer-name b))) (buffer-list)))) (with-current-buffer b (list major-mode (bound-and-true-p tab-line-mode) (key-binding (kbd "s-v")) ghostel-title)))'
```
Expected: `(ghostel-mode t wamei/term-input-paste ...)`。

- [ ] **Step 7: GUI frame で faint と背の高いグリフを目視する**

Run: `emacsclient -s ghostel-verify -c`

GUI frame で確認する:
1. `C-z` でパネルを開き、Claude Code を起動して入力欄の推奨プロンプトが**薄字**で出る (faint が効いている)
2. `⏺` `⏵` `⧉` を含む行が並んでも画面が上下に揺れない (display table の置換が効いている)
3. TUI にマウスを乗せてホイールを回すと TUI 内部がスクロールする (Emacs のバッファスクロールではない)
4. 画像をコピーして Claude のパネルで Cmd+V を押すと画像が添付される
5. 非選択の端末 window が `auto-dim-other-buffers` で暗くなる (既定背景に色が貼られていない)

- [ ] **Step 8: 後片付け**

Run:
```bash
emacsclient -s ghostel-verify --eval '(kill-emacs)'
tmux kill-session -t ghostel-verify 2>/dev/null
rm -rf /tmp/ghostel-verify /tmp/ghostel-init-check
```

- [ ] **Step 9: 目視結果を spec に追記してコミット**

`docs/superpowers/specs/2026-09-08-vterm-to-ghostel-design.md` の `## 7. テストと検証` の「隔離 daemon での目視」の各項目に、確認できた日付と結果を 1 行ずつ書く。落ちた項目があればそれも書き、原因が分かるまでこのタスクを完了にしない。

```bash
cd ~/.dotfiles
git add docs/superpowers/specs/2026-09-08-vterm-to-ghostel-design.md
git commit -m "$(cat <<'EOF'
ghostel 移行の目視確認の結果を書き残す

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_014YP2EKYnKbZGZFT9FxXZdD
EOF
)"
```

---

## 実装順と並列化

Task 1-4 は互いに独立で並列に進められる (Task 4 は Task 1 の `wamei/term-input-paste` を参照するが、名前と引数は上の Interfaces で確定しているので実装は並行できる)。Task 5 は 1-4 の後。Task 6 は Task 3 の後 (どちらも `WAMEI_TERM_RESTORE` の受け渡しに関わる)。Task 7 は Task 5 の後。Task 8 は最後。

```
Task 1 ─┐
Task 2 ─┤
Task 3 ─┼─→ Task 5 ─→ Task 7 ─→ Task 8
Task 4 ─┘        ↑
Task 6 ──────────┘
```
