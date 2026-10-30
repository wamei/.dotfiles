# chrome-profiles

Chrome のプロファイルごとに、Dock と Cmd+Tab で別アプリになる Chrome の複製 (`~/Applications/Chrome <名前>.app`) を作る。
アイコンは Chrome のアイコンの右上に、プロファイルの Google アカウントの写真を重ねたもの。元の `/Applications/Google Chrome.app` とそのデータフォルダはそのまま残る。
設計は [docs/superpowers/specs/2026-10-30-chrome-profile-apps-design.md](../../docs/superpowers/specs/2026-10-30-chrome-profile-apps-design.md) を参照。

## しくみ

- 同じデータフォルダを使う Chrome は 1 プロセスにまとまるので、複製ごとにデータフォルダ (`~/Library/Application Support/Chrome Profiles/<id>/`) を分ける。
- データフォルダは起動引数 `--user-data-dir` でしか渡せない。Dock のクリックなど引数なしで起動された複製は、常駐の `chrome-profiles-watch` が止めて、引数付きで起動し直す。
- 複製の識別子は元と同じ `com.google.Chrome` なので、PWA の起動用アプリ (`~/Applications/Chrome Apps.localized`) などが元の Chrome のつもりで複製を起動することがある (元のデータフォルダと `--no-startup-window` が付いてくる)。元のデータフォルダを指定した起動は、監視が止めて同じ引数で元の Chrome に回す。
- 複製は `cp -c` (APFS のクローン) なので、ディスクはほぼ食わない。Chrome が更新されたら (`Info.plist` の変化を launchd が拾う) 作り直す。動いている複製は終了したときに作り直す。
- 作り直しは `.app` の中の `Contents` だけを入れ替えるので、Dock の固定は外れない。

## インストール

```sh
mise run chrome-profiles:install   # ~/.local/bin に入れて plist を置くだけ
chrome-profiles init               # 元の Local State から ~/.config/chrome-profiles/config.json を作る (要らないプロファイルは消す)
chrome-profiles build              # 複製を作る
# Chrome を終了してから
chrome-profiles seed               # Local State とプロファイルを複写する (ログインを引き継ぐ。id を並べれば一部だけ)
mise run chrome-profiles:enable    # 監視と作り直しを launchd に読み込む
```

最後に `~/Applications` の `Chrome <名前>.app` を Dock にドラッグして固定する。

## コマンド

| コマンド | 内容 |
|---|---|
| `init [--force]` | 設定を作る。id は名前から (日本語だけの名前ならディレクトリ名から) |
| `seed [id...]` | 元のデータフォルダから `Local State` と担当のプロファイルだけを複写する。Chrome が動いていると断る。複写済みなら飛ばす |
| `build [--quiet] [--settle 秒]` | 複製を作る/作り直す/アイコンを付け直す。記録 (`~/.local/state/chrome-profiles/build.json`) と同じなら何もしない |
| `launch <id> [url...]` | 複製をデータフォルダ付きで起動する。動いていれば URL をそこで開く |
| `list` | 設定と、複写・複製の有無 |
| `classify` / `redirect` | 監視から呼ばれる。起動の引数から、そのまま動かす・その複製として起動し直す・元の Chrome に回す、を決めて実行する |

ログは `~/.local/state/chrome-profiles/{watch,build}.log`。

## 注意

- 複写したデータフォルダは、その時点の元のものの写し。以後は元の Chrome と別々に育つ。
- 同じセッションを元の Chrome と複製の両方で使うと、サイトによっては片方がログアウトされる。複写したプロファイルは複製の側でだけ使う。
- 複製の識別子は元と同じ `com.google.Chrome`。AppleScript や `open -a "Google Chrome"` のように識別子・名前で Chrome を呼ぶものからは区別できない。
- `chrome-profiles-watch` が止まっているときに Dock から複製を起動すると、元の Chrome が前面に出るだけで複製は終わる。
