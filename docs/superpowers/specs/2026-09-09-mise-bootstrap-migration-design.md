# dotfiles を mise bootstrap による宣言的セットアップへ移行する

- 日付: 2026-09-09
- 対象: `~/.dotfiles` (remote: `git@github.com:wamei/.dotfiles.git`)

## 背景

現状のセットアップは 2 か所に分かれている。

- `init.sh` — `ln -sf` を 9 本手書きし、`git config --global` を 4 行叩く手続き的スクリプト
- `README.md` — `brew install` の羅列。実行は人間が手でコピーする手順書

この形には次の問題がある。

1. **README のリストが実態から乖離している。** 実機には formula が 40 本 (`brew leaves`)、cask が 40 本入っているが、README に載っているのはその一部。新規マシンを README 通りに作っても現環境は再現できない。
2. **`init.sh` が取りこぼしている対象がある。** `bin/rpbcopy` は symlink ではなく `cp` なのでリポジトリを編集しても反映されない。`.config/karabiner/assets/complex_modifications/ja.json` はどこにもリンクされていない。`.gitconfig` は `ln -sf` がコメントアウトされたまま、代わりに `git config --global` が 4 行走っており、リポジトリにある alias 群と `push.default = nothing` が一度も有効になっていない。
3. **冪等性と差分確認の手段がない。** `init.sh` は「今どこまで適用済みか」を答えられない。

mise 2026.9.1 の `mise bootstrap` は、パッケージ・symlink・macOS 設定・ログインシェル・ランタイムを宣言から収束させる。すでに `[tools]` を mise で管理しているので、セットアップ全体を同じ道具に寄せる。

## 方針

- **`[bootstrap.packages]` は cask を中心とし、formula は可能な限り mise の `[tools]` へ移す。**
- **リポジトリの置き場を ghq レイアウトに合わせる。** `ghq.root = ~/projects`、dotfiles 自身は `~/projects/github.com/wamei/.dotfiles`。
- **`~/projects` 配下にある既存 64 ディレクトリの ghq 再配置は本 spec の対象外**とし、別 spec を切る (「対象外」節を参照)。

## 前提: mise 2026.9.1 の制約

公開ドキュメント (mise.jdx.dev) は手元のバイナリより新しい版を記述している。実装は **手元の 2026.9.1 の挙動**に合わせる。確認済みの差分:

| ドキュメントの記述 | 2026.9.1 の実際 |
|---|---|
| `[dotfiles]` の `mode = "track"` | 拒否される (`unknown mode 'track', ignoring entry`) |
| `[bootstrap.macos.defaults]` の配列 / テーブル値 | 拒否される (`unsupported value type ... expected bool, integer, float, or string`) |
| `mise bootstrap --adopt <url>` | 存在しない (`--from` / `--from-dir` はある) |

さらに **`[dotfiles]` のエントリ内の未知キーは警告なしに無視される**。目視レビューでは誤りを検出できないので、検証は必ず `mise bootstrap -n` と `mise bootstrap status` で行う。

## 構成

```
~/projects/github.com/wamei/.dotfiles/
├── mise.toml                  ← 新規。[dotfiles] [bootstrap.*] [tasks.*]
├── .config/mise/config.toml   ← 既存。[tools] [settings] (移行する formula をここに足す)
├── .gitconfig                 ← ~/.gitconfig とマージして symlink 対象にする
├── .gitignore_global          ← 新規。global な core.excludesfile の実体
└── init.sh                    ← 新規マシン用の seed スクリプトへ縮小
```

`mise bootstrap` はリポジトリ直下で実行する。mise は cwd の設定階層全体 (グローバル `~/.config/mise/config.toml` + リポジトリの `mise.toml`) をマージするので、`[tools]` を移さなくても step 14 の `mise install` は解決する。リポジトリ外から実行すると `[bootstrap.*]` が見えないため、**必ずリポジトリ直下で実行する**。初回は `mise trust` が必要。

`[tools]` を `.config/mise/config.toml` に残す理由は 2 つ。グローバル config として `~/.config/mise/config.toml` へ symlink されているのでどのディレクトリでもランタイムが解決すること、そして `.zshrc` の `show_env_mise` がこのパスの realpath と `mise ls --current` の source を突き合わせて「global 由来か」を判定しており、移すとすべてのツールが global 以外として赤く表示されるためである。

### init.sh の役割

`mise bootstrap` を動かすには brew と mise が先に要るので、この一点だけ手続き的に残す。symlink を張る責務は `[dotfiles]` に移し、init.sh からは削除する。

```sh
xcode-select --install         # git が入る
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
eval "$(/opt/homebrew/bin/brew shellenv)"
brew install mise ghq
GHQ_ROOT="$HOME/projects" ghq get git@github.com:wamei/.dotfiles.git
cd "$HOME/projects/github.com/wamei/.dotfiles"
mise trust && mise bootstrap
```

`ghq` は `[tools]` にも宣言するが、clone の時点ではまだ mise が動いていないので seed では brew から入れる。以後は mise 版が PATH 上の正となる。

`ghq.root` をここで `git config --global` で書かず環境変数で渡すのは、`~/.gitconfig` を `[dotfiles]` が symlink で管理するため。seed の時点で `git config --global` を叩くと `~/.gitconfig` が実ファイルとして作られ、直後の `mise bootstrap` が symlink を張れずに衝突する (`--force-dotfiles` が必要になる)。`ghq.root` の恒久的な設定はリポジトリの `.gitconfig` が持つ。

## `[dotfiles]` — symlink

`source` はこの `mise.toml` があるディレクトリ (リポジトリルート) からの相対。

| target | source | 現状 |
|---|---|---|
| `~/.inputrc` | `.inputrc` | symlink 済み |
| `~/.zshenv` | `.zshenv` | symlink 済み |
| `~/.zshrc` | `.zshrc` | symlink 済み |
| `~/.tmux.conf` | `.tmux.conf` | symlink 済み |
| `~/.config/mise/config.toml` | `.config/mise/config.toml` | symlink 済み |
| `~/.config/yamllint/config` | `.config/yamllint/config` | symlink 済み |
| `~/.aws/update-mfa-profile` | `.aws/update-mfa-profile` | symlink 済み |
| `~/.emacs.d/init.el` | `.emacs.d/init.el` | symlink 済み |
| `~/.emacs.d/early-init.el` | `.emacs.d/early-init.el` | symlink 済み |
| `~/bin/rpbcopy` | `bin/rpbcopy` | **`cp -f` によるコピー** → symlink 化 |
| `~/.config/karabiner/assets/complex_modifications/ja.json` | 同パス | **未リンク** → 新規 |
| `~/.gitconfig` | `.gitconfig` | **実ファイル** → マージして symlink |
| `~/.gitignore` | `.gitignore_global` | **リポジトリ外の実ファイル** → 取り込んで symlink |

`~/.gitignore` の現在の中身は `**/.claude/settings.local.json` の 1 行。リポジトリの `.gitignore` はリポジトリ自身の除外 (`.DS_Store`) なので別物であり、`.gitignore_global` という別ファイルとして取り込む。

`bin/pbcopy.plist` (リモート pbcopy 用の launchd agent) は `inetdCompatibility` と `Sockets` を使う。mise の `[bootstrap.macos.launchd.agents]` は `program` / `args` / `start_interval` 系しか表現できないため**対象外**とする。

## `[tools]` — brew から移す formula

`.config/mise/config.toml` の `[tools]` に追加する。

| brew formula | mise の指定 | 備考 |
|---|---|---|
| coreutils | `"aqua:uutils/coreutils"` | GNU ではなく Rust 実装。**単一の multicall バイナリ `coreutils` だけが入る** (下記) |
| direnv | `"aqua:direnv/direnv"` | |
| gh | `"aqua:cli/cli"` | |
| git-lfs | `"aqua:git-lfs/git-lfs"` | `~/.gitconfig` の filter が参照 |
| jq | `"aqua:jqlang/jq"` | |
| ripgrep | `"aqua:BurntSushi/ripgrep"` | |
| tmux | `"aqua:tmux/tmux-builds"` | 静的ビルド配布 |
| pipx | `"aqua:pypa/pipx"` | `[settings]` の `pipx:yamllint` が実体として使う |
| awscli | `"aqua:aws/aws-cli"` | |
| copilot | `"aqua:aws/copilot-cli"` | **brew の `copilot` は AWS Copilot CLI**。mise registry の `copilot` は GitHub Copilot CLI で別物 |
| openjdk@17 | `"core:java" = "temurin-17"` | 下記 |
| — | `"aqua:x-motemen/ghq"` | 新規導入 |
| azure-cli | `"pipx:azure-cli"` | |
| cloudflare-wrangler | `"npm:wrangler"` | mise 側の名前が異なる。実体の取得は `[settings]` の `npm.package_manager = "bun"` |

### coreutils の実体 (実機で確認)

`aqua:uutils/coreutils` が入れるのは **`coreutils` という multicall バイナリ 1 本だけ**で、`ls` や `cp` は PATH に出ない。

- **良い面**: システムの `ls` / `cp` / `rm` を上書きする危険がない
- **悪い面**: brew の GNU coreutils が入れる `gls` `gdate` `gsed` などの `g` プレフィックス付きコマンドは**一切入らない**。使い方が `coreutils ls` `coreutils date` という形に変わる

リポジトリ内に `g` プレフィックス付きコマンドの利用箇所は見つからなかったが、対話シェルで `gdate` などを使っていた場合はここで失われる。Task 3 の検証でこの点を明示的に確認する。

### java のバージョン指定 (実機で確認)

`java = "17"` は **2022 年で更新が止まった OpenJDK GA ビルドの 17.0.2** に解決される (`mise ls-remote core:java` の `17.x` 系は 17.0.0 / 17.0.1 / 17.0.2 で打ち止め)。brew の `openjdk@17` は 17.0.20.1 なので、そのまま移すと 3 年分のパッチが失われる。

`"core:java" = "temurin-17"` を使う。実測で `temurin-17.0.20+101` に解決され、brew 版と同じ世代になる。

`core:` を付けてバックエンド名込みで書くのは、`.zshrc` の `show_env_mise` が `<backend>:<pkg>` 形式のツールを「言語のバージョンではない」としてプロンプトから除外するため。`java` と裸で書くと全プロンプトに `java:temurin-17` が出る。プロンプトに出したければ裸の `java` に変えればよい。

### 付随して必要な `.zshrc` の変更

`java` を mise 管理にすると `JAVA_HOME` は mise が activate 時に設定する。以下 2 行を削除する。

```
export PATH="/opt/homebrew/opt/openjdk@17/bin:$PATH"
export JAVA_HOME="/opt/homebrew/opt/openjdk@17"
```

`libpq` は brew に残すので `PATH="/opt/homebrew/opt/libpq/bin:$PATH"` の行は据え置く。zsh プラグインの `source $HOMEBREW_PREFIX/share/...` も据え置く。

### brew 版の後始末

mise 版と brew 版が両方入っている間は、`.zshrc` の PATH 構築順により mise 版が勝つ。移行を適用し mise 版が実際に動くことを確認したのち、**手で** `brew uninstall` する。bootstrap タスクに `brew uninstall` は仕込まない (bootstrap が破壊的操作を含むことになるため)。コマンドは提示するが実行は利用者に委ねる。

## `[bootstrap.packages]`

### cask (22 本)

```
emacs-plus-app  ghostty  karabiner-elements  google-japanese-ime
1password  claude  visual-studio-code
google-chrome  google-chrome@canary
slack  microsoft-teams  chatwork
orbstack  postman  mysqlworkbench  nordlayer
font-blex-mono-nerd-font  font-hackgen  font-hackgen-nerd
font-source-han-code-jp  font-udev-gothic  font-udev-gothic-nf
```

`google-chrome-canary` は cask が既に存在しない (`google-chrome@canary` に改名済みで、実機には両方が入っている) ため宣言しない。

**`[bootstrap.brew] adopt = true` を必ず入れる。** mise が cask を導入し直すと `/Applications/*.app` が置き換わり、macOS の TCC 権限 (アクセシビリティ・画面収録・フルディスクアクセス) がリセットされる。karabiner-elements・ghostty・1password が直撃する。`adopt` は既存バンドルを差し替えずに所有権だけ記録する。

`emacs-plus-app` は `d12frosted/emacs-plus` tap。mise の brew backend は GitHub tap のみ対応し、API メタデータが無ければ Ruby 定義を自前の DSL シムで評価してソースからビルドする。ビルドに落ちる場合は下記の退避手順に回す。

**artifact 種別による失敗が見込まれる。** mise の brew-cask は app バンドル・binary・command wrapper・フォント・単純な pkg・スクリプトインストーラ・シェル補完しか扱わない。宣言対象には `postflight_steps` (emacs-plus-app, orbstack)、`command_wrapper`、`pkg` (google-japanese-ime, karabiner-elements, microsoft-teams, nordlayer) が含まれる。**どれが通るかは実際に流さないと分からない**ので、次の手順を取る。

1. 全 22 本を `[bootstrap.packages]` に宣言する
2. `mise bootstrap -n --only packages` で dry-run
3. 落ちたものを宣言から外し、`[tasks.bootstrap]` の `brew install --cask ...` へ退避する。理由をコメントに残す

### formula (10 本)

mise に移せなかったもの。

| formula | 残す理由 |
|---|---|
| `mise` | 自己参照 |
| `zsh-autosuggestions` | バイナリでなく zsh プラグイン。`.zshrc:221` が `$HOMEBREW_PREFIX/share/...` を source |
| `zsh-autocomplete` | 同上 (`.zshrc:222`) |
| `zsh-completions` | 同上 |
| `libpq` | registry になし。`.zshrc:210` が PATH に入れ Emacs の sql-mode が `psql` を引く |
| `macism` | `laishulu/homebrew` tap の Swift ビルド |
| `timg` | registry になし。ghostel の kitty graphics で画像を出す |
| `wget` | registry になし |
| `nkf` | registry になし |
| `screen` | registry になし |

`macism` は third-party tap なので `[bootstrap.brew.taps]` に tap URL を宣言する。ソースビルドに落ちる場合は cask と同様に `[tasks.bootstrap]` へ退避する。

### 宣言しないもの

現在インストールされているが、意図的に宣言から外すもの。今のマシンには残るが新規マシンには入らない。

- formula: `ask-cli` `cocoapods` `jmeter` `k6` `maven` `minio-mc` `mysql` `swiftformat` `swiftlint` `xcodegen` `cmake` `libfido2` `libiconv` `python-setuptools` `python@3.11` `python@3.13` `openjdk@21`
- cask: `arc` `figma` `intellij-idea` `pgadmin4` `medis` `cyberduck` `ngrok` `session-manager-plugin` `microsoft-azure-storage-explorer` `adobe-acrobat-reader` `libreoffice` `libreoffice-language-pack` `microsoft-auto-update` `corretto@21` `zoom` `wave` `steam` `google-chrome-canary`

## `[bootstrap.macos.defaults]`

実機から読み出した、明示的に設定されている値のみを宣言する。2026.9.1 はスカラー値しか受け付けないので、Dock の `persistent-apps` のような構造値は表現できない (必要になったら `[tasks.bootstrap]` で `defaults write` する)。

| domain | key | 値 | 意図 |
|---|---|---|---|
| NSGlobalDomain | `KeyRepeat` | 2 | キーリピート最速 |
| NSGlobalDomain | `InitialKeyRepeat` | 30 | |
| NSGlobalDomain | `ApplePressAndHoldEnabled` | false | 長押しでアクセント候補でなくリピートさせる (Emacs に必須) |
| NSGlobalDomain | `NSAutomaticCapitalizationEnabled` | false | |
| NSGlobalDomain | `com.apple.keyboard.fnState` | true | F1–F12 をファンクションキーとして扱う |
| com.apple.dock | `autohide` | true | |
| com.apple.dock | `tilesize` | 64 | |
| com.apple.finder | `AppleShowAllFiles` | true | |
| com.apple.finder | `FXPreferredViewStyle` | `"Nlsv"` | リスト表示 |

`AppleShowAllFiles` は現在文字列 `"True"` として書かれている (README の手順が `defaults write ... True` だったため)。mise は bool として `-bool true` を書くので初回適用時に差分として報告される。Finder はどちらも受け付けるので実害はない。

反映には `killall` が要るので `[bootstrap.hooks.post-defaults]` に `killall Dock Finder || true` を置く。

## `[bootstrap.user]`

```toml
[bootstrap.user]
login_shell = "/bin/zsh"
```

## git 設定

`init.sh` の `git config --global` 4 行を廃止し、`.gitconfig` の symlink に一本化する。現在の `~/.gitconfig` にはリポジトリ側に無い設定があるので、**マージしてから**張り替える。

マージ後の `.gitconfig` に含めるもの:

- リポジトリ側にある: `[color] ui`、alias 群 (`st` `co` `mylog` `graph` `today`)、`[push] default = nothing`、`[core] excludesfile`
- `~/.gitconfig` 側にある: `[user] name` / `email`、`[filter "lfs"]`
- 新規: `[ghq] root = ~/projects`

`[filter "lfs"]` は `git lfs install` が生成するもので、git-lfs を mise 管理に移しても内容は変わらない (`git-lfs` を PATH から引くだけ)。

## `[tasks.*]`

| task | 用途 |
|---|---|
| `bootstrap` | 宣言で表せない補助。mise の brew backend で扱えなかった cask / formula の `brew install`、tap 追加。step 15 で自動実行される。冪等に保つ |
| `update` | `brew update && brew upgrade` → `mise upgrade` → `mise run bootstrap` |
| `verify` | `mise bootstrap -n` による非破壊検証 |

`update` を 3 段に分ける理由は、更新経路が 3 系統に分かれているため。`mise bootstrap` は導入済みならスキップするだけでアップグレードしないので、`[bootstrap.packages]` 由来のものを上げられるのは brew だけである。

## リポジトリの移動

移行の**最終工程**として実施する。既存の symlink はすべて `/Users/wamei/.dotfiles/...` を指しているため、移動と張り直しはセットで行う。

```sh
mkdir -p ~/projects/github.com/wamei
mv ~/.dotfiles ~/projects/github.com/wamei/.dotfiles
cd ~/projects/github.com/wamei/.dotfiles
mise trust && mise bootstrap --only dotfiles
```

- 実行前に `ls -la` で現行 symlink の一覧を控え、失敗時に戻せるようにする
- 移動中は新しい shell を開かない (`~/.zshrc` が一時的に壊れたリンクになる)
- 稼働中の Emacs は `~/.emacs.d/init.el` 経由で旧パスを掴んでいるため、張り直し後に再起動する

## 実施順序

各段階で `mise bootstrap -n` の差分を確認してから適用する。

1. `mise.toml` を作り `[dotfiles]` だけ宣言する。既存 9 件が no-op、新規 4 件だけが差分に出ることを確認して適用する
2. `.gitconfig` をマージし `.gitignore_global` を切り出す。`~/.gitconfig` は実ファイルなので初回は衝突する — 内容を控えたうえで置き換える
3. `[tools]` に移行対象の formula を足し、`mise install` して各コマンドが動くことを確認する。`.zshrc` の `JAVA_HOME` 2 行を削除する
4. `[bootstrap.packages]` を宣言する。落ちた cask / formula を `[tasks.bootstrap]` へ退避する
5. `[bootstrap.macos.defaults]` と `[bootstrap.user]` を宣言する
6. `[tasks.update]` `[tasks.verify]` を足し、`init.sh` を seed スクリプトへ縮小、`README.md` を書き換える
7. brew 版 formula の uninstall コマンドを提示する (実行は利用者)
8. リポジトリを ghq レイアウトへ移動し、`mise bootstrap --only dotfiles` で全 symlink を張り直す

リポジトリ移動を最後に置くのは、途中の工程で `~/.dotfiles` を指す既存 symlink をそのまま検証に使えるようにするため。

## 検証方法

`[dotfiles]` の未知キーは警告なしに無視されるため、目視レビューは検証にならない。各段階で次を行う。

1. **Red**: 宣言を書く前に `mise bootstrap -n` を流し、何も宣言されていない状態を確認する
2. 宣言を書く
3. **Green**: `mise bootstrap -n --only <part>` で意図した差分だけが出ることを確認する
4. 適用後に `mise bootstrap status` で全件 `current` になることを確認する

**既存の symlink はすべて正しい先を向いているので、`[dotfiles]` の初回適用は (リポジトリ移動前なら) 大半が no-op になるはずである。** no-op にならないエントリがあれば宣言が間違っている、という形でテストが成立する。新規 4 件 (`rpbcopy` / karabiner / `.gitconfig` / `.gitignore`) だけが差分として出るのが期待値。

`--only` に渡せる値: `packages` `repos` `dotfiles` `macos-defaults` `user` `tools` `task` など。`--only` は前提を自動では実行しないので、部分適用を重ねる際は順序に注意する。

## 対象外 (別 spec)

- **`~/projects` 配下の既存 64 ディレクトリの ghq 再配置。** 内訳は github.com 30 / github.com.linka 1 / gitlab.com 6 / bitbucket.org 2 / AWS CodeCommit 4 / remote 無しまたは非 git 21。移動は各プロジェクトの絶対パス参照 (`.env`、docker-compose の volume、Emacs の desktop セッション) を壊しうるので、何が壊れるかの調査を伴う独立した作業とする。本 spec では `ghq.root` の設定と `ghq` の導入までを行い、新しく clone するものが ghq レイアウトに載る状態を作る。
- **`bin/pbcopy.plist` の launchd agent 化。** mise の launchd スキーマでは表現できない。
- **`[bootstrap.repos]` の中身。** dotfiles 自身は init.sh が clone する (鶏と卵)。他に宣言したい別リポジトリが出てきた時点で足す。
