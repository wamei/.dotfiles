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
| `finicky` | Finicky の設定から import する `~/.config/chrome-profiles/finicky.js` を書き出す (`build` も毎回書き出す) |
| `list` | 設定と、複写・複製の有無 |
| `classify` / `redirect` | 監視から呼ばれる。起動の引数から、そのまま動かす・その複製として起動し直す・元の Chrome に回す、を決めて実行する |

ログは `~/.local/state/chrome-profiles/{watch,build}.log`。

## Finicky でリンクを振り分ける

Finicky (`brew-cask:finicky`) を既定のブラウザにして、外部アプリから開いたリンクを複製に振り分ける。
`chrome-profiles` が書き出す `~/.config/chrome-profiles/finicky.js` には、プロファイルごとに「URL を受けて Finicky の browser を返す関数」が入る。
Finicky の設定 (`~/.config/finicky/finicky.js`、取引先のドメインなどが入るので dotfiles には入れない) からこれを import する。

```js
// Finicky は変換した設定を ~/Library/Caches に置いてからまとめるので、相対パスでは解決できない。絶対パスで書く
import { chromeProfiles as cp } from "/Users/<you>/.config/chrome-profiles/finicky.js"

export default {
  options: { logRequests: false }, // 既定では開いた URL をログに書く
  defaultBrowser: cp.rit,
  handlers: [
    { match: (url, { opener }) => opener?.bundleId === "com.tinyspeck.slackmacgap", browser: cp.linka },
    { match: ["*.example.com/*"], browser: cp.wamei },
  ],
}
```

- Finicky は `open -a <複製> <URL> --args --user-data-dir=… --profile-directory=…` を実行する (`args` に `--args` があると Finicky は自分では付け足さず、URL も足さないので、URL を `--args` の前に置く)。複製が動いていれば URL だけがそのアプリに届き、新しいプロセスは起動しない。止まっていれば引数付きで起動して URL を開く。`-n` を付けると、リンクのたびに複製のプロセスがもう 1 つ起動して Dock にちらつく。
- `logRequests: false` にしても、それまで開いていたログファイルには書き続ける。設定を変えたら Finicky を再起動する。
- Finicky のキャッシュは設定ファイル自体の更新時刻しか見ない。`chrome-profiles` は書き出した内容が変わったら Finicky の設定ファイルを同じ中身で書き直して読み直させる。

## アイコンの画像

既定ではプロファイルの Google アカウントの写真 (`<プロファイル>/Google Profile Picture.png`、複写先のものを優先) を重ねる。
Chrome は既存のプロファイルの写真をなかなか取り直さない (アカウント側で変えても古い写真のまま) ので、変えたいときは設定の `picture` に画像を指定して `chrome-profiles build` する。

```json
{ "id": "rit", "name": "RIT", "profileDir": "Default", "picture": "~/.config/chrome-profiles/pictures/rit.png" }
```

アイコンを直したくて複製の Chrome でプロファイルを消して作り直すと、データフォルダの中のプロファイルが別のもの (`Profile 5` など) になり、設定と食い違って起動が壊れる。やり直すときは、壊れたデータフォルダを退けて `chrome-profiles seed <id>` で複写し直す。

## 注意

- 複写したデータフォルダは、その時点の元のものの写し。以後は元の Chrome と別々に育つ。
- 同じセッションを元の Chrome と複製の両方で使うと、サイトによっては片方がログアウトされる。複写したプロファイルは複製の側でだけ使う。
- 複製の識別子は元と同じ `com.google.Chrome`。AppleScript や `open -a "Google Chrome"` のように識別子・名前で Chrome を呼ぶものからは区別できない。
- `chrome-profiles-watch` が止まっているときに Dock から複製を起動すると、元の Chrome が前面に出るだけで複製は終わる。
