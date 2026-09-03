# claude-complete: claude CLI によるゴーストテキスト補完

日付: 2026-09-03

## 目的

Emacs のコードバッファで、入力が止まったときにカーソル位置の続きを Claude が提案し、
薄い色のゴーストテキストとして表示、TAB で受け入れられるようにする。
バックエンドは Claude Code の契約で使える `claude -p` に限定し、API キーや他社サービスは使わない。
eglot 管理下のバッファでは LSP の補完候補（その位置で有効な識別子名）をプロンプトに混ぜ、
識別子の捏造を抑える。

## 前提と制約

- `claude -p --model haiku` は思考 0 で応答 2.1〜2.4 秒（実測、既存 claude-cli.el と同じフラグ）。
  自動トリガーはこの遅延を前提にし、打鍵で即キャンセルする。
- 自動トリガー 1 回ごとに Claude Code の利用枠を 1 リクエスト消費する。idle 遅延は変数で調整できるようにする。
- 既存の `wamei/claude-cli-run`（claude-cli.el）をプロセス基盤として再利用する。
- Emacs 31、corfu（auto, delay 0）と eglot が有効な環境。

## 全体構成

```
init.el ─ (leaf claude-complete) ─ prog-mode-hook で wamei/claude-complete-mode を有効化
                │
claude-complete.el ─ minor mode / overlay / idle timer / プロンプト生成 / 出力整形 / eglot 文脈
                │
claude-cli.el ─ wamei/claude-cli-run（非同期 claude -p 実行）
```

ファイル:

| ファイル | 変更 |
|---|---|
| `.emacs.d/claude-complete.el` | 新規 |
| `.emacs.d/claude-complete-test.el` | 新規（ERT、batch 実行） |
| `.emacs.d/claude-cli.el` | キャンセル済みプロセスの失敗を message しない変更 |
| `.emacs.d/init.el` | `leaf claude-complete` を追加 |

## モジュール分割

### 純粋関数層（バッファや外部プロセスに依存しない、またはバッファ読み取りのみ）

- `wamei/claude-complete--context ()`
  現在バッファから plist を返す。
  `:prefix` 点の手前 `wamei/claude-complete-prefix-chars`（既定 3000）文字、
  `:suffix` 点の後ろ `wamei/claude-complete-suffix-chars`（既定 1000）文字、
  `:path` バッファのファイル名（プロジェクト相対、無ければバッファ名）、
  `:language` major-mode 名から `-ts-mode` / `-mode` を除いた文字列。
- `wamei/claude-complete--prompt (context identifiers)`
  stdin に流す本文を返す。形式:

  ```
  <file path="src/util.ts" language="typescript">
  ...prefix...<CURSOR>...suffix...
  </file>
  <identifiers>
  clamp, Math, value, ...
  </identifiers>
  ```

  `identifiers` が空なら `<identifiers>` ブロックを出さない。
- `wamei/claude-complete-system-prompt`（変数）
  「補完エンジンとして `<CURSOR>` に挿入すべきコードだけを返す。フェンス・説明・既存コードの繰り返しは禁止。
  `<identifiers>` にある名前を優先して使う」という趣旨の英文。
- `wamei/claude-complete--clean (text context)`
  出力整形。順に:
  1. 先頭と末尾のコードフェンス（```lang ... ```）を剥がす
  2. 出力先頭が「カーソルのある行の点より手前の部分」と重なっていれば、その分を落とす
     （モデルが行全体を返してくる癖への対処）
  3. 出力末尾が `:suffix` の先頭と重なっていれば、その分を落とす
  4. 末尾の改行を 1 つに正規化し、空白だけなら nil を返す

### 状態層（buffer-local 変数）

| 変数 | 役割 |
|---|---|
| `wamei/claude-complete--overlay` | 表示中のゴーストテキスト overlay |
| `wamei/claude-complete--process` | 走行中の claude プロセス |
| `wamei/claude-complete--timer` | idle timer |
| `wamei/claude-complete--request` | 要求時点の `(tick . point)`。応答の鮮度判定に使う |

### 副作用層

- eglot 文脈 `wamei/claude-complete--eglot-identifiers (callback)`
  `(eglot-managed-p)` かつ `(eglot-server-capable :completionProvider)` のとき、
  `jsonrpc-async-request` で `:textDocument/completion` を投げ、`:items`（または配列そのまま）の
  `:label` を最大 `wamei/claude-complete-max-identifiers`（既定 50）件集めて `callback` に渡す。
  非管理・非対応・エラー・タイムアウト（`:timeout` 0.3 秒）のときは空リストで `callback` を呼ぶ。
  常に `callback` は 1 回だけ呼ばれる。
- 表示 `wamei/claude-complete--show (text)`
  点に長さ 0 の overlay を作り、`after-string` に `shadow` 顔を付けた `text` を入れる。
  複数行はそのまま `after-string` に改行を含めて表示する。
  TAB の束縛は mode map に `:filter` 付き `menu-item` で置き、overlay 表示中だけ
  `wamei/claude-complete-accept` を返す（長さ 0 の overlay の `keymap` プロパティは点の位置で拾われないため）。
  非表示時は TAB は既定の束縛（インデント等）に落ちる。
- 確定 `wamei/claude-complete-accept`
  `after-string` の文字列を点に挿入し、overlay を消す。
- 破棄 `wamei/claude-complete-dismiss`
  overlay を消し、走行中プロセスがあればキャンセルする。
  `pre-command-hook` から、実行されるコマンドが `wamei/claude-complete-accept` 以外なら呼ぶ。
- プロセスキャンセル `wamei/claude-complete--cancel`
  `(process-put proc 'wamei/claude-cli-cancelled t)` を付けてから `delete-process` する。
  claude-cli.el の sentinel はこのプロパティを見て message を出さない。

## データフロー

1. トリガー
   - 手動: `wamei/claude-complete` コマンド（`C-c C-.`、mode map）
   - 自動: `post-command-hook` で timer を張り直し、`wamei/claude-complete-idle-delay`（既定 1.0 秒）後に発火。
     `wamei/claude-complete-auto`（既定 t）が nil なら自動は張らない。
2. ガード（どれかに当たれば何もしない）
   - `completion-in-region-mode` が非 nil（corfu のポップアップ表示中）
   - バッファが読み取り専用
   - minibuffer
3. 走行中プロセスがあればキャンセルし、`--request` に `(buffer-chars-modified-tick . point)` を保存
4. `--eglot-identifiers` を非同期で取り、コールバックで `--context` と `--prompt` を作って
   `wamei/claude-cli-run` を `wamei/claude-complete-model`（既定 "haiku"）で起動
5. 応答コールバックで `--request` と現在の `(tick . point)` を照合。違えば捨てる
6. `--clean` して nil でなければ `--show`

## エラー処理

- claude の失敗は `wamei/claude-cli-run` の既存挙動どおり message に出す。キャンセル（プロパティ付き）は無音
- eglot 側のエラー・タイムアウトは無視し、識別子なしで続行
- 応答が空、または整形後に nil なら表示しない
- `wamei/claude-complete-mode` 無効化時に timer・process・overlay をすべて片付け、hook を外す
- バッファ kill 時も同様に片付ける（`kill-buffer-hook`）

## テスト

`claude-complete-test.el`。実行: `emacs -Q --batch -l claude-complete-test.el -f ert-run-tests-batch-and-exit`。
claude-cli-test.el のスタブ（実行可能シェルスクリプトを `wamei/claude-cli-program` に差す）を再利用する。

| 対象 | 観点 |
|---|---|
| `--context` | prefix/suffix の切り出し長、path・language の導出 |
| `--prompt` | `<CURSOR>` の位置、identifiers 有無での `<identifiers>` ブロックの有無 |
| `--clean` | フェンス除去、行頭重複除去、suffix 重複除去、空なら nil |
| `--show` / accept / dismiss | temp buffer で overlay の生成、TAB 挿入、他コマンドで消える |
| 鮮度判定 | 応答前にバッファを変えると表示されない |
| ガード | `completion-in-region-mode` 中は起動しない |
| キャンセル | キャンセル済みプロセスの失敗で message が出ない（`message` を advice して捕捉） |
| eglot | `eglot-managed-p` を nil に stub すると識別子なしで動く。t の場合は `jsonrpc-async-request` を stub して label が prompt に載る |
| mode 無効化 | timer・process・overlay が残らない |

## 実装順序

1. 手動トリガー + overlay 表示 + TAB 確定 / 他コマンドで破棄（claude-cli.el のキャンセル無音化を含む）
2. idle 自動トリガー（遅延変数、打鍵で即キャンセル、同時実行 1 本、corfu 中の抑止）
3. eglot 文脈注入（補完候補の label）

## 対象外（YAGNI）

- 複数候補の切替、単語・行単位の部分受け入れ
- documentSymbol によるファイル概要や flymake 診断のプロンプト注入（必要になれば追加）
- 他バッファの内容を文脈に含めること
- ストリーミング表示
