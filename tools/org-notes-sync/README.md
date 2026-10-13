# org-notes-sync

org のメモファイルと Apple メモを双方向に同期する CLI。launchd が 5 分ごとに 1 回ずつ走らせる。
設計は [docs/superpowers/specs/2026-10-13-org-notes-sync-design.md](../../docs/superpowers/specs/2026-10-13-org-notes-sync-design.md) を参照。

## インストール

```sh
mise run org-notes-sync:install   # ビルドして置くだけ。launchd には読み込まない
mise run org-notes-sync:enable    # 確認が済んだら launchd に読み込む (install.sh --load)
```

前提: git は Xcode Command Line Tools のもの (bridge のビルドに要る clang も同じ)。

`install` は次を行う。

- 読み込み済みのエージェントがあれば止める (古いバイナリを動かし続けないため)
- `bridge/build.sh` で `~/.local/bin/notes-bridge` を作る
- `bun build --compile` で `~/.local/bin/org-notes-sync` を作る
- `~/Library/LaunchAgents/jp.wamei.org-notes-sync.plist` を置く

`enable` は同じことをした上で `launchctl bootstrap gui/$(id -u)` で読み込む。

### 権限 (2 つ)

効かせる対象は `~/.local/bin/org-notes-sync` (bridge はその子プロセスとして走る)。どちらもバイナリを作り直すと署名が変わるので与え直す。

1. フルディスクアクセス (メモの DB を読むため)
   システム設定 → プライバシーとセキュリティ → フルディスクアクセス → `+` で `~/.local/bin/org-notes-sync` を追加してオンにする。
2. オートメーション (Apple Events。メモ.app を操作するため)
   初回の実行時に「org-notes-sync が "メモ" を制御しようとしています」と聞かれる (osascript の JXA と `open` 経由)。許可する。
   後から変えるには システム設定 → プライバシーとセキュリティ → オートメーション。

Emacs で同期対象のメモを保存したときの同期 (保存フック) は Emacs の子プロセスとして走るので、**Emacs.app の権限** で動く。
保存フックからも同期させるなら、Emacs.app にもフルディスクアクセスとオートメーション (メモ) を与える。
与えたくなければ保存フックからの同期は失敗するに任せ、launchd の 5 分ごとの同期に頼る。
ターミナルから手で走らせるときも同じで、権限はそのターミナルのものになる。

## 使い方

- org ファイルの先頭付近に `#+notes_sync: yes` を書くと同期の対象になる (大文字小文字は区別しない)。
- 対象は `~/org` の直下にある `.org` ファイルだけ。サブディレクトリの中のファイルは同期しない。
- 同期済みの org ファイルをリネームしても、メモはそのまま新しい名前のファイルと組み直される (内容かタイトルで見分ける)。
- メモ.app 側は iCloud アカウントの `org` フォルダに入る。
- 画像は `~/org/.notes-media/<添付ID>.jpg` に落ち、org には画像リンクが入る。
- パスワードで保護されたメモは同期しない。
- メモ.app で消すと、org ファイルは `~/org/.notes-trash/` に移る。org ファイルを消す、または `#+notes_sync: yes` を外すと、メモは「最近削除した項目」に移る。
- Mac はメモ.app が動いている間しか iCloud の変更を受け取らないので、同期のたびにメモ.app が止まっていればバックグラウンドで起動し、起動したままにする。
- アップロードのため、メモ.app はバックグラウンドで再起動される。メモ.app が最前面にある間は延期され (警告が出る)、離れたら反映される。
- 送信が 60 秒で終わらなかったとき (オフラインなど) は、15 分経つまでメモ.app を起動し直さない。3 回続けて終わらなければ警告する。

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
| `--store` | メモのストアの **コピー** に対して読み書きする (テスト用)。本物のメモ.app には触らないので、メモの作成・削除と iCloud への送信はしない (作成・削除の動作は警告して飛ばす) |

終了コードは 0 が正常、2 が中断、1 が想定外のエラー。bridge の終了コードは 1 その他、3 selfcheck 失敗 (macOS の API が変わった)、4 DB が読めない、5 添付の拒否、6 書き込みの検証不一致 (リンクの URL がメモに正規化されたときはそう出る)、7 読んだ後にメモが更新された (書かずに次の同期に回す)。

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
launchctl bootout "gui/$(id -u)/jp.wamei.org-notes-sync" || true
rm -f ~/Library/LaunchAgents/jp.wamei.org-notes-sync.plist
rm ~/.local/bin/org-notes-sync ~/.local/bin/notes-bridge
```

状態 (`~/.local/state/org-notes-sync/`) は必要なら手で消す。

## 実機確認の手順

本物のメモと iPhone を使う確認。順序は install → 権限 → 手で確認 → enable。

1. ビルドして入れる (`mise run org-notes-sync:install`)。launchd には読み込まれない。
2. フルディスクアクセスを `~/.local/bin/org-notes-sync` に与える。保存フックからも同期させるなら Emacs.app にも与える (「権限」を参照)。
3. 念のためエージェントが止まっていることを確かめる: `launchctl bootout "gui/$(id -u)/jp.wamei.org-notes-sync" || true`。
4. 一時の org ディレクトリとテスト用のメモのフォルダを作り、手で走らせる。最初の実行でオートメーション (メモ) の許可を求められるので許可する。
   `~/.local/bin/org-notes-sync --org-dir /tmp/org-test --state-dir /tmp/org-test-state --folder org-test`
5. 次の点を確かめる。
   1. `#+notes_sync: yes` を付けた org がメモに現れ、iPhone で見える。
   2. iPhone で編集・チェック・見出しを変えると org に戻る。
   3. iPhone で写真を貼ると、org に画像リンクが入り、`.notes-media/` に jpg が落ちる。
   4. 両側で同じ行を編集するとマーカーが入る。解消して保存すると iPhone に反映される。
   5. iPhone でメモを消すと、org ファイルが `.notes-trash/` に移る。
   6. メモ.app が最前面のときは反映が延期され、離れると届く。
   7. 同期済みの org ファイルをリネームしても、メモが消えたり増えたりせず、iPhone の編集がリネーム後のファイルに戻る。
   8. Mac をオフラインにして org を編集し保存する。送信が時間切れになった後、15 分経つまでメモ.app が起動し直されないこと、オンラインに戻すと届くことを確かめる。
   9. Emacs で同期対象のメモを保存すると同期が走る (`*org-notes-sync-stderr*` に権限のエラーが出ない)。出るなら Emacs.app に権限を与えるか、launchd に任せる。
6. 問題なければ launchd を有効にする: `mise run org-notes-sync:enable`。
7. 実際のメモ 1 つに `#+notes_sync: yes` を付けて様子を見る。
