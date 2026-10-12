# org-notes-sync

org のメモファイルと Apple メモを双方向に同期する CLI。launchd が 5 分ごとに 1 回ずつ走らせる。
設計は [docs/superpowers/specs/2026-10-13-org-notes-sync-design.md](../../docs/superpowers/specs/2026-10-13-org-notes-sync-design.md) を参照。

## インストール

```sh
mise run org-notes-sync:install
```

次を行う。

- `bridge/build.sh` で `~/.local/bin/notes-bridge` を作る
- `bun build --compile` で `~/.local/bin/org-notes-sync` を作る
- `~/Library/LaunchAgents/jp.wamei.org-notes-sync.plist` を置き、`launchctl bootout` → `launchctl bootstrap gui/$(id -u)` で登録する

### フルディスクアクセス

メモの DB を読むため、フルディスクアクセスが要る。

1. システム設定 → プライバシーとセキュリティ → フルディスクアクセス
2. `+` で `~/.local/bin/org-notes-sync` (と `~/.local/bin/notes-bridge`) を追加してオンにする

バイナリを作り直すと署名が変わるので、インストールのたびに与え直す。

## 使い方

- org ファイルの先頭付近に `#+notes_sync: yes` を書くと同期の対象になる (大文字小文字は区別しない)。
- メモ.app 側は iCloud アカウントの `org` フォルダに入る。
- 画像は `~/org/.notes-media/<添付ID>.jpg` に落ち、org には画像リンクが入る。
- パスワードで保護されたメモは同期しない。
- メモ.app で消すと、org ファイルは `~/org/.notes-trash/` に移る。org ファイルを消す、または `#+notes_sync: yes` を外すと、メモは「最近削除した項目」に移る。
- アップロードのため、メモ.app はバックグラウンドで再起動される。メモ.app が最前面にある間は延期され、離れたら反映される。

### 衝突の解消

同じ行を両側で変えると、org ファイルに次のマーカーが入る。

```
<<<<<<< org
(org 側の内容)
=======
(メモ側の内容)
>>>>>>> notes
```

どちらかに直してマーカーを消し、保存すれば次の同期でメモに反映される。

### 手で走らせる

```sh
~/.local/bin/org-notes-sync [--force] [--org-dir DIR] [--state-dir DIR] [--folder NAME] [--bridge PATH] [--store PATH]
```

| フラグ | 内容 |
| --- | --- |
| `--force` | 安全のための中断 (大量削除や読み取り異常の検知) を無視して進める |
| `--org-dir` | org ディレクトリ (既定 `~/org`) |
| `--state-dir` | 状態ディレクトリ (既定 `~/.local/state/org-notes-sync`) |
| `--folder` | メモのフォルダ名 (既定 `org`) |
| `--bridge` | bridge のパス (既定 `~/.local/bin/notes-bridge`。環境変数 `ORG_NOTES_SYNC_BRIDGE` が優先) |
| `--store` | bridge にメモのストアの **コピー** を読ませる (テスト用) |

終了コードは 0 が正常、2 が中断、1 が想定外のエラー。bridge の終了コードは 1 その他、3 selfcheck 失敗 (macOS の API が変わった)、4 DB が読めない、5 添付の拒否、6 書き込みの検証不一致。

## ログ

- 警告は stderr に `org-notes-sync: ` を付けて出て、`~/.local/state/org-notes-sync/log` にも残る。
- launchd の出力は `~/.local/state/org-notes-sync/launchd.out` / `launchd.err`。

## テスト

```sh
bun test                                  # ユニットテスト
ORG_NOTES_SYNC_IT=1 bun test             # bridge の結合テスト (メモのストアのコピーに対して走る)
ORG_NOTES_SYNC_IT=1 ORG_NOTES_SYNC_IT_FOLDER=<folder> bun test
```

結合テストはターミナルにフルディスクアクセスが要る。

## アンインストール

```sh
launchctl bootout "gui/$(id -u)/jp.wamei.org-notes-sync"
rm ~/Library/LaunchAgents/jp.wamei.org-notes-sync.plist
rm ~/.local/bin/org-notes-sync ~/.local/bin/notes-bridge
```

状態 (`~/.local/state/org-notes-sync/`) は必要なら手で消す。

## 実機確認の手順

本物のメモと iPhone を使う確認。launchd を有効にする前に行う。

1. ビルドして入れる (`mise run org-notes-sync:install`)。
2. フルディスクアクセスを与える (上記)。
3. 手で試す間は競合しないよう、エージェントを止める: `launchctl bootout "gui/$(id -u)/jp.wamei.org-notes-sync"`。
4. 一時の org ディレクトリとテスト用のメモのフォルダを作り、手で走らせる。
   `~/.local/bin/org-notes-sync --org-dir /tmp/org-test --state-dir /tmp/org-test-state --folder org-test`
5. 次の 6 点を確かめる。
   1. `#+notes_sync: yes` を付けた org がメモに現れ、iPhone で見える。
   2. iPhone で編集・チェック・見出しを変えると org に戻る。
   3. iPhone で写真を貼ると、org に画像リンクが入り、`.notes-media/` に jpg が落ちる。
   4. 両側で同じ行を編集するとマーカーが入る。解消して保存すると iPhone に反映される。
   5. iPhone でメモを消すと、org ファイルが `.notes-trash/` に移る。
   6. メモ.app が最前面のときは反映が延期され、離れると届く。
6. 問題なければ launchd を有効にする: `launchctl bootstrap "gui/$(id -u)" ~/Library/LaunchAgents/jp.wamei.org-notes-sync.plist`。
7. 実際のメモ 1 つに `#+notes_sync: yes` を付けて様子を見る。
