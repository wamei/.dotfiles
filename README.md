# dotfiles

macOS の環境構築を [mise](https://mise.jdx.dev/) の `bootstrap` に寄せてある。

## 新規マシン

```sh
curl -fsSL https://raw.githubusercontent.com/wamei/.dotfiles/master/init.sh | bash
```

Xcode Command Line Tools、Homebrew、mise、ghq を入れ、このリポジトリを ghq
レイアウト (`~/projects/github.com/wamei/.dotfiles`) へ clone して
`mise bootstrap` に渡す。以降は宣言が面倒を見る。

## 既に clone してある場合

リポジトリ直下で実行する。mise は cwd の設定階層をマージするので、
リポジトリ外から実行すると `[bootstrap.*]` が見えない。

```sh
mise trust            # このリポジトリの設定を信頼する (未信頼だと非対話では
                      # エラー、対話では毎回プロンプトされる)
mise bootstrap        # 適用
mise bootstrap -n     # dry-run (適用せず差分だけ表示)
mise bootstrap status # 収束状態の確認
mise run verify       # 設定の妥当性を非破壊で検証
mise run update       # brew と mise の管理下をまとめて最新化
```

`mise bootstrap` が宣言的・冪等に適用するもの:

| 宣言 | 内容 |
|---|---|
| `[bootstrap.packages]` | brew の formula と cask |
| `[dotfiles]` | `$HOME` への symlink |
| `[bootstrap.macos.defaults]` | macOS のシステム設定 |
| `[bootstrap.user]` | ログインシェル |
| `[tools]` | ランタイムと単体 CLI (`.config/mise/config.toml`) |
| `[tasks.bootstrap]` | 宣言で表せない補助 |

## 設定ファイルの置き場

| ファイル | 内容 |
|---|---|
| `mise.toml` | `[dotfiles]` `[bootstrap.*]` `[tasks.*]` |
| `.config/mise/config.toml` | `[tools]` `[settings]`。`~/.config/mise/config.toml` へ symlink されるグローバル設定 |

`[tools]` を `mise.toml` へ移していないのは、グローバル設定としてどのディレクトリ
でもランタイムを解決させるためと、`.zshrc` の `show_env_mise` がそのパスを基準に
「global 由来か」を判定しているため。

## リポジトリの置き場

`ghq.root` は `~/projects`。GitHub のリポジトリは
`~/projects/github.com/<owner>/<repo>` に落ちる。

```sh
ghq get <url>
```

## 注記

- mise は 2026.9.1 を前提にしている。公開ドキュメントはこれより新しい版を記述
  しており、`[dotfiles]` の `mode = "track"` や `[bootstrap.macos.defaults]` の
  配列値はこの版では動かない
- `[dotfiles]` のエントリ内の未知キーは警告なしに無視される。編集したら必ず
  `mise run verify` で確認すること
- cask は `[bootstrap.brew] adopt = true` で扱っている。入れ直すと
  `/Applications/*.app` が置き換わり macOS の TCC 権限がリセットされるため、
  画面収録やアクセシビリティなどの許可をアプリごとに UI から再承認する必要が
  出る。`adopt = true` は既存の `.app` をそのまま brew 管理下に取り込み、
  不要な入れ直しを避けるための設定
