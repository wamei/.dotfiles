#!/bin/sh
# org-notes-sync を ~/.local/bin に入れ、plist を置く。
# 既定では launchd に読み込まない (--load を付けたときだけ bootstrap する)。
# フルディスクアクセスとオートメーションは手で与える (README 参照)。作り直すと署名が変わるので与え直す。
set -eu
load=0
case "${1:-}" in
  --load) load=1 ;;
  "") ;;
  *) echo "usage: $0 [--load]" >&2; exit 2 ;;
esac
here=$(cd "$(dirname "$0")" && pwd)
bin="$HOME/.local/bin"; state="$HOME/.local/state/org-notes-sync"; agents="$HOME/Library/LaunchAgents"
label=jp.wamei.org-notes-sync
uid=$(id -u)
mkdir -p "$bin" "$state" "$agents"

# 古いバイナリが動き続けないよう、作り直す前に止める。完全に消えるまで待つ
launchctl bootout "gui/$uid/$label" 2>/dev/null || true
i=0
while launchctl print "gui/$uid/$label" >/dev/null 2>&1; do
  i=$((i + 1))
  [ "$i" -le 20 ] || { echo "bootout が終わりません: $label" >&2; exit 1; }
  sleep 0.5
done

sh "$here/bridge/build.sh" "$bin/notes-bridge"
(cd "$here" && bun install --frozen-lockfile && bun build --compile cli/org-notes-sync.ts --outfile "$bin/org-notes-sync")
sed "s|@HOME@|$HOME|g" "$here/launchd/$label.plist.in" > "$agents/$label.plist"

if [ "$load" -eq 1 ]; then
  n=0
  until launchctl bootstrap "gui/$uid" "$agents/$label.plist"; do
    n=$((n + 1))
    [ "$n" -lt 5 ] || { echo "bootstrap に失敗しました" >&2; exit 1; }
    sleep 1
  done
  echo "installed and loaded."
else
  echo "installed (まだ launchd には読み込んでいません)。"
  echo "  1. フルディスクアクセスに $bin/org-notes-sync を追加"
  echo "  2. 手で 1 回走らせ、オートメーション (メモ) の確認を許可"
  echo "  3. 有効にする: mise run org-notes-sync:enable"
fi
