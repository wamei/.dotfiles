# notes-sync

Markdown のメモファイル (`~/notes/*.md`) と Apple メモを双方向に同期する CLI。launchd が 30 分ごとと、メモの DB が書き換わったとき (iPhone の編集がメモ.app に届いたときなど。間隔は最低 15 秒) に走らせる。
設計は [docs/superpowers/specs/2026-10-13-org-notes-sync-design.md](../../docs/superpowers/specs/2026-10-13-org-notes-sync-design.md) を参照 (当時は org 書式で `org-notes-sync` という名前だった)。

## インストール

```sh
mise run notes-sync:install   # ビルドして置くだけ。launchd には読み込まない
mise run notes-sync:enable    # 確認が済んだら launchd に読み込む (install.sh --load)
```

前提: git は Xcode Command Line Tools のもの (bridge のビルドに要る clang も同じ)。

`install` は次を行う。

- 読み込み済みのエージェントがあれば止める (古いバイナリを動かし続けないため)
- `bridge/build.sh` で `~/.local/bin/notes-bridge` を作る
- `bun build --compile` で `~/.local/bin/notes-sync` を作る
- `~/Library/LaunchAgents/jp.wamei.notes-sync.plist` を置く

`enable` は同じことをした上で `launchctl bootstrap gui/$(id -u)` で読み込む。

### 権限 (2 つ)

効かせる対象は `~/.local/bin/notes-sync` (bridge はその子プロセスとして走る)。install はキーチェーンの「Apple Development」証明書 (環境変数 `NOTES_SYNC_SIGN_IDENTITY` で別の証明書を指定できる) でバイナリに署名し直すので、作り直しても許可は残る。証明書が無いと ad-hoc 署名のままになり、作り直すたびに与え直しになる。

1. フルディスクアクセス (メモの DB を読むため)
   システム設定 → プライバシーとセキュリティ → フルディスクアクセス → `+` で `~/.local/bin/notes-sync` を追加してオンにする。
2. オートメーション (Apple Events。メモ.app を操作するため)
   初回の実行時に「notes-sync が "メモ" を制御しようとしています」と聞かれる (osascript の JXA と `open` 経由)。許可する。
   後から変えるには システム設定 → プライバシーとセキュリティ → オートメーション。

Emacs で同期対象のメモを保存したときの同期 (保存フック、`.emacs.d/notes-sync.el`) は CLI を直接動かさず、launchd のジョブを `launchctl kickstart` する。
子プロセスとして動かすと権限の判定を受けるのは Emacs.app になり、Emacs.app を更新するたびに許可が外れるため。
Emacs.app に権限は要らない。その代わり保存フックからの同期は launchd にジョブが読み込まれている (`mise run notes-sync:enable` 済み) ことが前提になる。
走っている最中に保存したときは state ディレクトリに `pending` を置き、走っている側にもう 1 周させる。
ターミナルから手で走らせるときは、権限はそのターミナルのものになる。

## 使い方

- Markdown ファイルの先頭の front matter に `notes_sync: yes` を書くと同期の対象になる (大文字小文字は区別しない)。

  ```markdown
  ---
  notes_sync: yes
  ---
  # 買い物
  - [ ] 卵
  ```

- 対象は `~/notes` の直下にある `.md` ファイルだけ。サブディレクトリの中のファイルは同期しない。
- 同期済みのファイルをリネームしても、メモはそのまま新しい名前のファイルと組み直される (内容かタイトルで見分ける)。
- メモ.app 側は iCloud アカウントの `project-notes` フォルダ (既定の「メモ」フォルダ `Notes` と紛れない名前にする)に入る。
- 画像は `~/notes/.notes-media/<添付ID>.jpg` に落ち、本文には `![](.notes-media/<添付ID>.jpg)` が入る。画像以外の添付は `[attachment](notes-attachment:<添付ID>)`。
- パスワードで保護されたメモは同期しない。
- メモ.app で消すと、ファイルは `~/notes/.notes-trash/` に移る。ファイルを消す、または `notes_sync: yes` を外すと、メモは「最近削除した項目」に移る。
- Mac はメモ.app が動いている間しか iCloud の変更を受け取らないので、同期のたびにメモ.app が止まっていればバックグラウンドで起動し、起動したままにする。
- アップロードのため、メモ.app はバックグラウンドで再起動される。メモ.app が最前面にある間は延期され (警告が出る)、離れたら反映される。
- 送信が 60 秒で終わらなかったとき (オフラインなど) は、15 分経つまでメモ.app を起動し直さない。3 回続けて終わらなければ警告する。

### 書式

1 行が 1 段落 (メモ.app の 1 行) になる。Markdown の段落の結合 (改行だけの行の連結) はしない。

| メモ.app | Markdown |
| --- | --- |
| タイトル (1 行目) | `# タイトル` (front matter の直後。無ければファイル名) |
| 見出し / 小見出し | `## ` / `### ` (`#` も見出しとして読む) |
| 全体が太字の行 | `#### ` (`#####` 以上も太字の行として読む) |
| 箇条書き / ダッシュ / 番号 | `- ` / `+ ` / `1. ` (`1) ` も読む) |
| チェックリスト | `- [ ] ` / `- [x] ` |
| 等幅 | `` ``` `` で囲む (言語指定は読み捨てる) |
| 引用 | 行頭の `> ` (空行は `>`) |
| インデント | 行頭の空白 2 つで 1 段 |
| 太字 / 斜体 / 下線 / 取り消し線 | `**x**` / `*x*` (`_x_` も読む。太字と重なると `**_x_**`) / `<u>x</u>` / `~~x~~` |
| リンク | `[ラベル](URL)`、裸の `https://...` |

パスや snake_case、算術式の `*` を装飾と取り違えないよう、`*` `_` `~~` は前後が空白・句読点・日本語のときだけ装飾として読む。

### 衝突の解消

同じ行を両側で変えると、ファイルに次のマーカーが入る。

```
<<<<<<< local
(ファイル側の内容)
=======
(メモ側の内容)
>>>>>>> notes
```

どちらかに直してマーカーを消し、保存すれば次の同期でメモに反映される。

### 手で走らせる

```sh
~/.local/bin/notes-sync [--force] [--dir DIR] [--state-dir DIR] [--folder NAME] [--bridge PATH] [--store PATH]
```

| フラグ | 内容 |
| --- | --- |
| `--force` | 安全のための中断 (大量削除や読み取り異常の検知) を無視して進める |
| `--dir` | メモのディレクトリ (既定 `~/notes`) |
| `--state-dir` | 状態ディレクトリ (既定 `~/.local/state/notes-sync`) |
| `--folder` | メモ.app のフォルダ名 (既定 `project-notes`) |
| `--bridge` | bridge のパス (既定 `~/.local/bin/notes-bridge`。環境変数 `NOTES_SYNC_BRIDGE` が優先) |
| `--store` | メモのストアの **コピー** に対して読み書きする (テスト用)。本物のメモ.app には触らないので、メモの作成・削除と iCloud への送信はしない (作成・削除の動作は警告して飛ばす) |

終了コードは 0 が正常、2 が中断、1 が想定外のエラー。bridge の終了コードは 1 その他、3 selfcheck 失敗 (macOS の API が変わった)、4 DB が読めない、5 添付の拒否、6 書き込みの検証不一致 (リンクの URL がメモに正規化されたときはそう出る)、7 読んだ後にメモが更新された (書かずに次の同期に回す)。

## ログ

- 警告は stderr に `notes-sync: ` を付けて出て、`~/.local/state/notes-sync/log` にも残る。保存フックから蹴った同期の警告は、Emacs がジョブの終了後に `launchd.err` から拾ってエコーエリアに出す。
- launchd の出力は `~/.local/state/notes-sync/launchd.out` / `launchd.err`。

## テスト

```sh
bun test                                  # ユニットテスト
NOTES_SYNC_IT=1 bun test             # bridge の結合テスト (メモのストアのコピーに対して走る)
NOTES_SYNC_IT=1 NOTES_SYNC_IT_FOLDER=<folder> bun test
```

結合テストはターミナルにフルディスクアクセスが要る。

## アンインストール

```sh
launchctl bootout "gui/$(id -u)/jp.wamei.notes-sync" || true
rm -f ~/Library/LaunchAgents/jp.wamei.notes-sync.plist
rm ~/.local/bin/notes-sync ~/.local/bin/notes-bridge
```

状態 (`~/.local/state/notes-sync/`) は必要なら手で消す。

## 実機確認の手順

本物のメモと iPhone を使う確認。順序は install → 権限 → 手で確認 → enable。

1. ビルドして入れる (`mise run notes-sync:install`)。launchd には読み込まれない。
2. フルディスクアクセスを `~/.local/bin/notes-sync` に与える (「権限」を参照)。
3. 念のためエージェントが止まっていることを確かめる: `launchctl bootout "gui/$(id -u)/jp.wamei.notes-sync" || true`。
4. 一時のディレクトリとテスト用のメモのフォルダを作り、手で走らせる。最初の実行でオートメーション (メモ) の許可を求められるので許可する。
   `~/.local/bin/notes-sync --dir /tmp/notes-test --state-dir /tmp/notes-test-state --folder notes-test`
5. 次の点を確かめる。
   1. `notes_sync: yes` を付けた .md がメモに現れ、iPhone で見える。
   2. iPhone で編集・チェック・見出しを変えるとファイルに戻る。
   3. iPhone で写真を貼ると、ファイルに画像リンクが入り、`.notes-media/` に jpg が落ちる。
   4. 両側で同じ行を編集するとマーカーが入る。解消して保存すると iPhone に反映される。
   5. iPhone でメモを消すと、ファイルが `.notes-trash/` に移る。
   6. メモ.app が最前面のときは反映が延期され、離れると届く。
   7. 同期済みのファイルをリネームしても、メモが消えたり増えたりせず、iPhone の編集がリネーム後のファイルに戻る。
   8. Mac をオフラインにしてファイルを編集し保存する。送信が時間切れになった後、15 分経つまでメモ.app が起動し直されないこと、オンラインに戻すと届くことを確かめる。
6. 問題なければ launchd を有効にする: `mise run notes-sync:enable`。
   Emacs で同期対象のメモを保存すると同期が走ることを確かめる (`~/.local/state/notes-sync/log` に行が足され、エコーエリアに権限のエラーが出ない)。
7. 実際のメモ 1 つに `notes_sync: yes` を付けて様子を見る。

## org-notes-sync からの移行

以前は `~/org/*.org` を `org-notes-sync` で同期していた。名前・置き場・書式が変わったので、次の手順で移す。
state は引き継がず、メモ.app の新しい `project-notes` フォルダで組み直す。

1. 古いエージェントを止めて消す。
   `launchctl bootout "gui/$(id -u)/jp.wamei.org-notes-sync" || true`、`rm ~/Library/LaunchAgents/jp.wamei.org-notes-sync.plist ~/.local/bin/org-notes-sync`
2. org を Markdown に変換する。まず `--dry-run` で確かめる。
   `bun tools/notes-sync/cli/migrate-org-to-md.ts --dry-run` (`--org-dir` 既定 `~/org`、`--dir` 既定 `~/notes`)
   - 直下の `*.org` (ドットで始まるものも) を `<dir>/<名前>.md` に書き、`.notes-media/` と `.notes-trash/` を移す。`.notes-trash/` の中身は org のまま。
   - `#+key: value` の制御行は front matter の `key: value` になる (`#+notes_sync: yes` → `notes_sync: yes`)。
   - 書いた Markdown を読み戻して org と同じ内容・同じ opt-in になるかを全件確かめ、1 件でも違えば (Markdown で別の意味になる行があるなど) 何も書かずに止まる。移す先に同じ名前があっても止まる。
   - 元の `~/org` は消さない。確認が済んだら手で退避する。
3. `mise run notes-sync:install` で入れ直し、「権限」の 2 つを新しいバイナリ `~/.local/bin/notes-sync` に与え直す (名前と署名の identifier が変わったので、以前の許可は効かない)。
4. 手で 1 回走らせて確かめてから `mise run notes-sync:enable`。opt-in したメモがメモ.app の `project-notes` フォルダに作られる。
5. メモ.app の古い `org` フォルダは手で消す (同期はもうしない)。古い state (`~/.local/state/org-notes-sync/`) も不要になる。
