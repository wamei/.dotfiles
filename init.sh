#!/bin/bash
set -euo pipefail

# 新規マシンの seed。symlink や brew パッケージの導入は mise.toml の宣言が持つ
# ので、ここでは `mise bootstrap` を動かすのに最低限必要なものだけを入れる。
#
#   curl -fsSL https://raw.githubusercontent.com/wamei/.dotfiles/master/init.sh | bash
#
# 既にこのリポジトリを clone してあるなら、リポジトリ直下で
# `mise trust && mise bootstrap` を叩くだけでよい。

DOTFILES_ROOT="$HOME/projects/github.com/wamei/.dotfiles"

# Xcode Command Line Tools (git が入る)。導入済みなら何もしない。
xcode-select -p >/dev/null 2>&1 || xcode-select --install

# Homebrew。mise と ghq の入手経路であり、cask の導入にも要る。
if ! command -v brew >/dev/null 2>&1; then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi
# brew の prefix は Apple Silicon (/opt/homebrew) と Intel (/usr/local) で違う。
# 決め打ちすると Intel Mac でコマンドが無く set -e で落ちるため両方を試す。
if [ -x /opt/homebrew/bin/brew ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ -x /usr/local/bin/brew ]; then
  eval "$(/usr/local/bin/brew shellenv)"
fi

# mise と ghq。どちらも [tools] / [bootstrap.packages] でも宣言しているが、
# clone の時点ではまだ mise が動いていないのでここでは brew から入れる。
# 導入済みなら何もしない (brew install は対象が古ければ黙って upgrade する
# ため、無条件に呼ぶと seed を再実行するたびに意図しない upgrade が走る。
# upgrade は mise run update の仕事)。
command -v mise >/dev/null 2>&1 || brew install mise
command -v ghq >/dev/null 2>&1 || brew install ghq

# ghq.root は git config ではなく環境変数で渡す。~/.gitconfig は [dotfiles] が
# symlink で管理するため、ここで git config --global を叩くと実ファイルが作られ、
# 直後の mise bootstrap が symlink を張れずに衝突する。恒久的な設定は
# リポジトリの .gitconfig が持っている。
if [ ! -d "$DOTFILES_ROOT" ]; then
  # この seed が動く時点では新規マシンに SSH 鍵が無く、git@ (SSH) では
  # Permission denied (publickey) で失敗する。リポジトリは public なので
  # HTTPS なら無認証で clone できる。remote は後から好みで SSH に変更してよい。
  GHQ_ROOT="$HOME/projects" ghq get https://github.com/wamei/.dotfiles.git
fi

cd "$DOTFILES_ROOT"
mise trust
mise bootstrap
