#!/bin/sh
# マシンだけにある設定 (private の wamei/.dotfiles-local) を取ってきて置く。
#
# mise bootstrap の [tasks.bootstrap] から毎回呼ばれるので冪等にする。この時点で
# dotfiles の symlink (~/bin/local-config) とツール (ghq / bun) は揃っている。
#   1. 無ければ ghq で clone する (private なので GitHub の ssh 鍵が要る)
#   2. mise trust して、そのリポジトリの [dotfiles] の symlink を張る
#   3. pre-push の secretlint を有効にする
#   4. identities.toml から git / gh / ssh / chrome-profiles の設定を書き出す
# clone できない (鍵がまだ無い) ときは手順を出して抜ける。bootstrap 全体は止めない。
set -eu
repo="${LOCAL_CONFIG_DIR:-$HOME/projects/github.com/wamei/.dotfiles-local}"

if [ ! -d "$repo/.git" ]; then
  if ! ghq get -p wamei/.dotfiles-local; then
    echo "bootstrap-local: wamei/.dotfiles-local を clone できませんでした。GitHub の ssh 鍵を置いてから mise bootstrap をやり直してください" >&2
    exit 0
  fi
fi

cd "$repo"
mise trust -q
# 既にあるファイル (新しいマシンの既定の ~/.ssh/config など) は上書きしない。中身を確かめて退けてからやり直す
if ! mise dot apply; then
  echo "bootstrap-local: 上の既存ファイルを確かめて退けてから mise bootstrap をやり直してください" >&2
fi
bun install --frozen-lockfile --silent
git config core.hooksPath .githooks
"$HOME/bin/local-config" apply
