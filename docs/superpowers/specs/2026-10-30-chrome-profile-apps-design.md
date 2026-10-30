# Chrome のプロファイルごとの Dock アプリ (chrome-profiles)

## 目的

Chrome のプロファイルごとに、Dock と Cmd+Tab で別のアプリとして扱える Chrome を用意する。
アイコンは Chrome のアイコンの右上にプロファイル写真を重ねたもの。元の `/Applications/Google Chrome.app` と、そのデータフォルダはそのまま残す。

## 前提 (2026-10-30 に実機で確認)

- 同じデータフォルダ (user-data-dir) を使う Chrome は 1 プロセスにまとまる。`--profile-directory` を変えても Dock は分かれない。分けるにはデータフォルダを分ける。
- データフォルダは起動引数 `--user-data-dir` でしか変えられない (環境変数は Linux だけ。Info.plist の `CrProductDirName` を書き換えると署名が壊れる)。
- `cp -c` (APFS のクローン) で複製した Chrome.app に Finder のカスタムアイコンを付けても、Gatekeeper の判定は Google の Developer ID で accepted のまま。動いているプロセスのアイコンもカスタムアイコンになる。識別子は `com.google.Chrome` のまま。
- 複製した Chrome は、元のデータフォルダを複写したものの Cookie を復号できる (キーチェーンの「Chrome Safe Storage」を同じ署名で読める)。ログインを引き継げる。
- 複製を引数なしで起動すると、元のデータフォルダのロックを見て元の Chrome に処理を渡して終わる (元の Chrome が前面に出る)。`NSWorkspace.didLaunchApplicationNotification` を受けて SIGKILL すれば、渡す前に止められる (0.3〜0.6 秒で届き、元の Chrome は前面に出なかった)。

## 構成

`tools/chrome-profiles/` に置く。notes-sync と同じく bun の CLI と、AppKit が要る部分だけの小さな Swift のバイナリ。

| 部品 | 役割 |
|---|---|
| 設定 `~/.config/chrome-profiles/config.json` | プロファイルごとの `id` (slug)・表示名・元のプロファイルディレクトリ名。マシンごとに違うのでリポジトリには入れない。`init` が元の `Local State` から作る |
| CLI `chrome-profiles seed <id>...` | 元のデータフォルダから `Local State`・`First Run`・担当のプロファイルのディレクトリだけを `~/Library/Application Support/Chrome Profiles/<id>/` に `cp -c` で複写し、`Local State` の `profile.info_cache` / `profiles_order` / `last_used` / `last_active_profiles` を担当のものだけにする (丸ごとだと部品のファイルが多く 4 分以上かかった)。写す前後で Chrome が動いていたら取りやめる。複写済みなら飛ばす |
| CLI `chrome-profiles build` | `~/Applications/Chrome <表示名>.app` を作る/作り直す。`/Applications/Google Chrome.app` の版 (`CFBundleShortVersionString`) かアイコンの元 (プロファイル写真) が変わったときだけ作り直す。複製が動いている間は作り直さない (次の機会に回す) |
| CLI `chrome-profiles launch <id> [url...]` | `open -na <複製> --args --user-data-dir=… --profile-directory=… [url...]`。監視と、後で入れる Finicky が使う |
| Swift `chrome-profiles-icon` | Chrome のアイコンに、円形に切り抜いたプロファイル写真を右上に重ね、`NSWorkspace.setIcon` で複製に付ける |
| Swift `chrome-profiles-watch` (LaunchAgent, 常駐) | 複製の起動を見張り、引数に `--user-data-dir` が無ければ SIGKILL して `chrome-profiles launch <id>` を呼ぶ。元の Chrome のデータフォルダを指定した起動 (PWA の起動用アプリが識別子で Chrome を探して複製を掴んだときに実際に起きた) は SIGKILL して、同じ引数で元の Chrome を起動する (`chrome-profiles redirect`)。複製が終了したときに `build` を呼ぶ (動いていて作り直せなかった分を拾う) |
| LaunchAgent (WatchPaths) | `/Applications/Google Chrome.app/Contents/Info.plist` が変わったら `build` を走らせる |

複製の作り直しは、新しいクローンを横に作ってアイコンを付けてから `rename` で差し替える。Dock に固定した項目はパスで覚えているので、差し替えても外れない (要確認)。

## 状態と判定の分離

純粋な関数 (テスト対象) と、ファイル・プロセスを触る層を分ける。

- `Local State` の剪定: JSON を受け取り、残すプロファイルだけにした JSON を返す
- 作り直しの判定: 記録した版・写真のハッシュと、今の版・写真のハッシュから「作る / 作り直す / 何もしない / 動いているので後回し」を返す
- 起動引数の組み立て: 設定とURLから `open` の引数列を返す
- 監視の判定: プロセスの引数列とバンドルのパスから「放っておく / 止めて launch <id>」を返す (Swift 側は引数列を取って CLI の `classify` に聞く形にして、判定を TS に寄せる)

複製に付けた版とハッシュは `~/.local/state/chrome-profiles/build.json` に残す。

## 確認が要る点

- Dock に固定した複製を作り直しても固定が外れないか
- Google Software Update (Keystone) が複製を自分の管理対象として登録・更新しようとしないか
- 同じ Google アカウントのセッションが、元のデータフォルダと複写先の両方で使われたときにログアウトされないか (元の Chrome で胡実のプロファイルを使い続ける場合)

## Finicky (2026-10-30 追加)

- 既定のブラウザを Finicky にする。Finicky の browser は `{ name: <複製のパス>, appType: "path", args: [url, "--args", "--user-data-dir=…", "--profile-directory=…"] }`。Finicky 4.2.2 は `args` に `--args` が入っていると自分では付けず URL も足さないので、`open -a <複製> <URL> --args …` になる。動いている複製には URL だけが届き、止まっていれば引数付きで起動する。最初は `-n` を付けていたが、リンクのたびに複製のプロセスが増えて Dock にちらつくのでやめた。Finicky の `isAppRunning` は複製を見分けられない (`localizedName` がどれも Google Chrome) ので、動いているかで分岐させる方法は取れない。
- この関数は `chrome-profiles` が `~/.config/chrome-profiles/finicky.js` に生成し (`launchArgs` から組み立てて起動の指定を 1 か所に保つ)、マシンごとの Finicky の設定 (`~/.config/finicky/finicky.js`) が絶対パスで import する (Finicky は設定を Caches に変換してからまとめるので相対パスは解決できない)。
- Finicky は設定ファイル自体の更新時刻と書き込みしか見ないので、生成ファイルが変わったら設定ファイルを同じ中身で書き直す。
