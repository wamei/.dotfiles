# org メモと iOS のメモ.app を双方向に同期する

日付: 2026-10-13

前提となる設計: `docs/superpowers/specs/2026-09-09-project-memo-design.md`
(`~/org/` のメモと `C-x C-m` / `C-x m`)

## 目的

普段 iPhone でメモに使っているのはメモ.app (Apple Notes) なので、`~/org/` の org メモを
メモ.app と双方向に同期し、iPhone からも同じメモを読み書きできるようにする。

## 決定事項 (ブレインストーミングで合意済み)

| 論点 | 決定 |
|---|---|
| 方向 | 双方向。Mac と iPhone のどちらで書いても揃う |
| 対象 | opt-in。org ファイルに `#+notes_sync: yes` があるものだけ |
| iPhone で作ったメモ | メモ.app の `org` フォルダ (iCloud アカウント) に置いたものだけ取り込む。org 側に無ければ `~/org/<タイトル>.org` を作る |
| 実行タイミング | launchd で 5 分おき + `M-x org-notes-sync` + Emacs でメモを保存したとき非同期で |
| 衝突 | 3-way マージ。衝突箇所は org に衝突マーカーを入れ、メモ.app には書かない |
| 削除 | 伝える。メモ側の削除 / フォルダ外への移動 → org を `~/org/.notes-trash/` へ。org 側の削除 / opt-in 解除 → メモを削除 (メモ.app の「最近削除した項目」へ) |
| 書式 | メモ.app で表現できるものはできる限り中間表現に取り込む。将来 markdown に乗り換えられるよう、ローカル書式は差し替え可能にする |
| 画像 | Emacs (GUI) でインライン表示する。tty での表示は後日 |

## 調査で分かったこと (spike, 2026-10-12〜13)

メモ.app には公開 API が無い。読み書きの経路を実機で比較した。

- **AppleScript (`body` の HTML)**: 読みでは見出し・書式は取れるが、チェックリストの状態、引用、
  リンク先、インデントが落ち、隣接するリストの種類も潰れる。書きでは見出しが
  「本文 + 太字 + 文字サイズ」になり、チェックリストと引用は作れない。
- **NoteStore.sqlite の protobuf を直接書く**: 段落属性だけを書き換えても iPhone には届かない。
  本文は CRDT (topotext) で、区間の更新時刻と端末ごとの時計を正しく進めれば iPhone に届き、
  その後の iPhone 側の編集とも正しく合流した。ただしメモ.app 起動中は書き込みに気づかない。
- **NotesShared.framework を読み込んで Core Data 経由で書く (採用)**:
  `+[ICPaths setApplicationDocumentsURL:]` で DB の場所を差し替えられるので、コピーで安全に検証できた。
  `ICNote.mergeableString` (`ICTTMergeableAttributedString`) の
  `replaceCharactersInRange:withString:` / `setAttributes:range:` で、Apple 自身のコードが
  端末 ID の登録・区間の挿入・時計の更新を行う。スキーマ移行は起きず、変更記録
  (`ATRANSACTION` / `ACHANGE`) も正しく追加される。**メモ.app 起動中でも表示には即座に反映される。**
  ただし iCloud への送信は、メモ.app を起動し直したとき (または未起動から起動したとき) にしか起きない。
  `open -g`、AppleScript でのフォルダやメモの作成、Darwin 通知の候補では送信されなかった。
  自プロセスからの送信 (`ICCloudContext`) は CloudKit のエンタイトルメントが要るので不可。

読み取りの段落属性 (`TTStyle` = `ICTTParagraphStyle`):
`style` 0 タイトル / 1 見出し / 2 小見出し / 3 本文 / 4 等幅 / 100 箇条書き / 101 ダッシュ /
102 番号 / 103 チェックリスト (`todo` = `ICTTTodo`, `done`)、`indent`、`blockQuoteLevel`、`uuid`。

## 構成

```
tools/org-notes-sync/
  bridge/notes-bridge.m   NotesShared を読み込む小さなネイティブ CLI (ObjC, clang でビルド)
  bridge/build.sh         ビルド
  src/doc.ts              中間表現 (契約)
  src/formats/format.ts   ローカル書式の契約 (LocalFormat)
  src/formats/org.ts      org 実装
  src/merge.ts            3-way マージ (純粋関数)
  src/reconcile.ts        両側の現状 + state → 実行計画 (純粋関数)
  src/state.ts            ペア表と base の読み書き
  src/orgfs.ts            ~/org アダプタ
  src/notes.ts            メモ側アダプタ (bridge / AppleScript / flush)
  src/cli.ts              入口。ロック、1 周の同期、ログ
```

判断はすべて純粋関数 (`reconcile` / `merge` / 書式変換) で行い、外界に触れるのはアダプタだけにする。

### 中間表現 (`src/doc.ts`)

```ts
type Mark = "bold" | "italic" | "underline" | "strike"
type Inline =
  | { kind: "text"; text: string; marks: Mark[]; link?: string }
  | { kind: "attachment"; id: string; type: string }   // U+FFFC 1 文字に対応
type ParaStyle =
  | { kind: "title" } | { kind: "heading" } | { kind: "subheading" } | { kind: "body" }
  | { kind: "mono" }
  | { kind: "list"; list: "bullet" | "dash" | "number" }
  | { kind: "check"; done: boolean }
type Paragraph = { style: ParaStyle; indent: number; quote: boolean; inlines: Inline[] }
type Doc = { paragraphs: Paragraph[] }   // 1 段落目がタイトル
```

メモ.app で表現できてもここに持たないもの: ハイライト色・文字色・フォント・配置。
書き込みは差分適用なので、変更しない段落ではこれらも保たれる。

### ローカル書式の契約 (`src/formats/format.ts`)

```ts
interface LocalFormat {
  ext: string
  parse(text: string): { doc: Doc; optIn: boolean; preserved: string[] }
  render(doc: Doc, preserved: string[]): string   // preserved = 制御行 (#+notes_sync など)
  hasConflictMarkers(text: string): boolean
}
```

markdown に乗り換えるときは `formats/markdown.ts` を足し、設定の `format` を切り替え、
`~/org` を変換する移行コマンドを 1 回流す。state (base は Doc の JSON) とメモ.app 側はそのまま使える。

### org の対応 (`src/formats/org.ts`)

| 中間表現 | org |
|---|---|
| 1 段落目 (title) | `#+title: X` (無ければファイル名) |
| heading / subheading | `* ` / `** ` |
| body | 普通の行。`***` 以下の見出しは「行全体が太字の body」として読み、書くときも `*** ` に戻す |
| mono (連続段落) | `#+begin_example` 〜 `#+end_example` |
| list bullet / dash / number | `- ` / `+ ` / `1. ` (番号は振り直す)。深さは 2 空白 × indent |
| check | `- [ ] ` / `- [X] ` |
| quote (連続段落) | `#+begin_quote` 〜 `#+end_quote` |
| indent (リスト以外) | 行頭の空白 2 つで 1 段 |
| bold / italic / underline / strike | `*b*` / `/i/` / `_u_` / `+s+`。日本語の中でも認識するよう org より緩く読む |
| link | `[[url][文字]]`。文字が URL と同じなら `[[url]]` ではなく URL のまま |
| 画像の添付 | `[[file:.notes-media/<添付ID>.jpg]]` の 1 行 |
| その他の添付 (表など) | `#+notes_attachment: <添付ID> <種類>` の 1 行 |
| 制御行 | `#+notes_sync: yes` などは preserved として本文に混ぜずに保つ |

不変条件 (テストで固定する): 対応範囲内の org テキストは `render(parse(t)) == t`、
任意の Doc で `parse(render(d)).doc == d`。

### bridge (`bridge/notes-bridge.m`)

`/System/Library/PrivateFrameworks/NotesShared.framework` を `dlopen` し、ObjC ランタイム経由で呼ぶ。
入出力は JSON (stdin / stdout)。

| サブコマンド | 内容 |
|---|---|
| `selfcheck` | 必要なクラスとセレクタの存在を確認。欠けていれば名前を列挙して終了コード 3 |
| `list --folder org` | iCloud アカウントの `org` フォルダ内 (削除済みを除く) の `{id, title, modified, doc}` を返す。フォルダが無ければ `folderMissing: true` |
| `read --id <id>` | 1 件の Doc |
| `write --id <id>` (stdin: Doc) | 現在の内容との差分を文字列操作と属性設定に落として適用し、`saveNoteData` → `updateChangeCountWithReason:` → 保存。適用後に読み戻した Doc を返す |
| `export-media --id <att> --to <path>` | 画像添付の実体ファイルを書き出す (HEIC は `sips` で JPEG 化) |
| `--store <dir>` (共通) | `ICPaths setApplicationDocumentsURL:` で DB の場所を差し替える (テスト用) |

- id は `ICNote.identifier` (ZIDENTIFIER の UUID)。端末をまたいで変わらない。
- 差分適用: 段落単位で LCS を取り、変わった段落だけ文字を置き換え、変わった段落の属性だけ設定する。
  既存の属性辞書に上書きする形で、扱わないキーは残す。
- 太字などの文字属性・リンク・添付のキー名は実装の最初に実物から確かめる
  (`attributedString` を列挙して記録する)。

### メモ側アダプタ (`src/notes.ts`)

- 読み・書きは bridge。
- 作成は AppleScript (`make new note at folder "org"`、本文はタイトル 1 行) → bridge `write`。
- 削除は AppleScript (`delete note id …`)。メモ.app の「最近削除した項目」に入る。
- `org` フォルダが無ければ AppleScript で作る。
- **flush (送信)**: 1 周で 1 件でも書いたら最後に 1 回。
  - メモ.app が未起動 → `open -g -a Notes`、書いたメモの版番号が揃う
    (`ZCURRENTLOCALVERSION == ZLATESTVERSIONSYNCEDTOCLOUD`) のを待ってから終了させる
  - 起動中で手前に無い → 終了して `open -g -a Notes`
  - 手前にある → 保留 (state に `flushPending`)。次の周で再挑戦
  - 60 秒で揃わなければ `flushPending` を残して警告

### 同期アルゴリズム (`src/reconcile.ts`)

比較は Doc 同士で行う。base はペアごとに前回揃えた Doc。

| org | メモ | 動作 |
|---|---|---|
| 変化なし | 変化なし | 何もしない |
| 変更あり | 変化なし | メモへ書く |
| 変化なし | 変更あり | org へ書く (preserved は保つ) |
| 変更あり | 変更あり | 3-way マージ (base / org / メモをローカル書式で描画した行単位)。きれいなら両側へ |
| 衝突 | | org に衝突マーカー入りで書き、base をメモ側の現状に進める。メモには書かない |
| 衝突マーカーが残っている | | そのペアの「org → メモ」を止める |
| 削除 / opt-in 解除 | あり | メモを削除 |
| あり | 削除 / フォルダ外 | org を `~/org/.notes-trash/` へ移す |
| opt-in でペア無し | | メモを作ってペアに |
| | `org` フォルダでペア無し | `~/org/<タイトル>.org` を作る (`#+title` と `#+notes_sync: yes` 付き。ファイル名に使えない文字は `-`、重複は `-2` …) |
| ペアの無い org とメモが同じタイトル | | base を空としてペアにしてマージ (state を失ったときの重複防止) |

書き込みは「書く → 読み戻す → base を読み戻した Doc にする」。

### 画像

- 画像の添付は `~/org/.notes-media/<添付ID>.jpg` に書き出す (既にあれば飛ばす)。
- 同期の最後に、どの org からも参照されないファイルを `.notes-media` から消す。
- Emacs から新しい画像を足してメモへ送る機能は今回入れない (`.notes-media` 以外を指すリンクは
  メモ側ではリンク文字列になる)。

### 状態

`~/.local/state/org-notes-sync/`
- `state.json`: `{ version, format, pairs: [{ file, noteId, base: Doc }], flushPending }`。
  ペアを 1 件処理するごとに一時ファイル + rename で保存する。
- `lock`: 同時実行防止。取れなければ `pending` 印を残して即終了し、実行中のプロセスが
  終わり際にもう 1 周する。
- `log`: 1 MB を超えたら先頭を切り詰める。

## 連携

### launchd

`~/Library/LaunchAgents/jp.wamei.org-notes-sync.plist` (`StartInterval` 300)。
mise の bootstrap で配置する。実行ファイルは `bun build --compile` で作る
`~/.local/bin/org-notes-sync` と、`~/.local/bin/notes-bridge`。フルディスクアクセスは
`org-notes-sync` に 1 回与える (作り直したら与え直す)。

### Emacs (init.el の `leaf project-memo` に直接書く)

- `after-save-hook`: `org-directory` 配下で `#+notes_sync: yes` のバッファなら
  `make-process` で `org-notes-sync` を非同期に実行する。終了時、警告があれば `message`。
- 同期対象のメモでは `auto-revert-mode` を有効にする。
- GUI では `org-display-inline-images` で画像を表示し、`org-image-actual-width` で幅を抑える。
  auto-revert の後にも表示し直す。tty は後日。
- `M-x org-notes-sync` で手動同期。

## エラー処理

全体を止める (何も書かず、終了コード 2、警告) 条件:
- bridge の `selfcheck` 失敗
- DB を読めない (フルディスクアクセスが無い)。警告に許可の手順を含める
- `org` フォルダが無い / 中身が 0 件なのにペアが 1 件以上ある
- 1 周で削除が 3 件以上かつペアの半数以上 (`--force` で解除)

ペア単位の失敗はそのペアだけ飛ばし、base を更新しない。org ファイルは一時ファイル + rename で書き、
読んだ後に mtime が変わっていたら書かずに次の周へ回す。

## テスト

- 単体 (bun test): org ⇔ Doc の往復 (書式ごとの表)、merge、reconcile (上の表の各行)、
  ファイル名、全体停止の条件、state の読み書き。
- シナリオ (bun test): メモ側・org 側を偽のアダプタにして複数周を回す。iPhone 編集の反映、
  衝突 → 解消 → 反映、削除、opt-in 解除、画像の追加・削除、state 喪失時の重複防止。
- bridge の結合テスト: 本物の DB を一時ディレクトリにコピーし `--store` で差し向けて
  read / write / 差分適用を確かめる。本物の DB には触れない。`ORG_NOTES_SYNC_IT=1` のときだけ。
- Emacs (ert, batch): 保存フックが対象のメモでだけ同期を蹴ること、画像表示の有効化。
  init.el から defun フォームだけ読んで評価する。テストは scratchpad に置き repo に残さない。
- 実機: テスト用フォルダで iPhone と往復する手順 (`tools/org-notes-sync/README.md`)。

## リスク

- NotesShared は非公開 API。OS の更新で変わり得るので selfcheck で止める。
- メモ.app の再起動による送信は、Mac でメモ.app を手前で使っている間は保留される。
- iCloud 上のデータに書くので、実機確認はテスト用フォルダから始める。
