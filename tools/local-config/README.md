# local-config

このマシンでどのディレクトリを誰として扱うか (取引先ごとの git の user・gh のアカウント・ssh の鍵・Chrome のプロファイル) を、private リポジトリ `.dotfiles-local` の `identities.toml` 1 つにまとめて、各ツールの設定を書き出す。
取引先の名前やアカウントは公開の dotfiles に入れない。ここにあるのは書き出す仕組みだけ。

## 使い方

```sh
local-config apply --dry-run   # 変わるファイルを並べるだけ
local-config apply             # 書き出す
```

定義の場所は `$LOCAL_CONFIG_DIR` (既定は `~/projects/github.com/wamei/.dotfiles-local`)。

## identities.toml

```toml
# どの dirs にも当たらないときの既定。git の user は公開側 .gitconfig のまま
[[identity]]
id = "me"
dirs = ["~/projects"]
gh = "me"
ssh = [{ host = "github.com", hostname = "github.com", key = "~/.ssh/id_ed25519" }]

[[identity]]
id = "client"
dirs = ["~/projects/github.com.client"]
git = { name = "client-admin", email = "admin@client.example" }
gh = "client-admin"
ssh = [{ host = "github.com.client", hostname = "github.com", key = "~/.ssh/client" }]
chrome = "client"   # chrome-profiles の id
```

## 書き出すもの

| 書き出し先 | 元 | 読むもの |
|---|---|---|
| `~/.gitconfig.local` | `git` を持つ身元の `dirs` ごとに `includeIf "gitdir:…/"` | 公開側 `.gitconfig` の `[include]` |
| `~/.config/local-config/git/<id>.gitconfig` | `git` の user | 上の includeIf |
| `~/.config/gh/users-by-dir` | `gh` を持つ身元の `dirs` | `tools/gh-account/gh` |
| `~/.config/local-config/ssh/github` | `ssh` の Host ブロックと、GitHub の別名ホストをまとめて 443 番に逃がす `Match` | `~/.ssh/config` の `Include` |
| `~/.config/chrome-profiles/config.json` の `dirs` | `chrome` を指す身元の `dirs` (ほかのキーは触らない) | `chrome-profiles open --dir` (Emacs のリンク) |

- 中身が同じなら書かない。生成したものでないファイルを初めて上書きするときは `<ファイル>.pre-local-config` に退避する。
- git の includeIf は後に読んだものが勝つので、浅いディレクトリから並べる (深いほうが勝つ)。
- `~/.ssh/config` 自体は生成しない。.dotfiles-local の `files/ssh/config` に置き、そこから生成した `ssh/github` を `Include` する。
- `dirs` には `*` と `?` が書ける (`"~/projects/github.com/*/mc*"`)。git の `includeIf "gitdir:"` と同じく `/` をまたがず、git・gh・chrome-profiles で同じ意味になる。`{a,b}` は git が解釈しないので使えない。
- chrome-profiles の `config.json` はプロファイルの対応 (`profileDir`) がマシンの Chrome ごとに違うので、`dirs` だけを差し替える。chrome-profiles が無いマシンでは飛ばす。
