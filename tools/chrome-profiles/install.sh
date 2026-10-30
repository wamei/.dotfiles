#!/bin/sh
# chrome-profiles を ~/.local/bin に入れ、plist を置く。
# 既定では launchd に読み込まない (--load を付けたときだけ bootstrap する)。
set -eu
load=0
case "${1:-}" in
  --load) load=1 ;;
  "") ;;
  *) echo "usage: $0 [--load]" >&2; exit 2 ;;
esac
here=$(cd "$(dirname "$0")" && pwd)
bin="$HOME/.local/bin"; state="$HOME/.local/state/chrome-profiles"; agents="$HOME/Library/LaunchAgents"
uid=$(id -u)
labels="jp.wamei.chrome-profiles.watch jp.wamei.chrome-profiles.build"
mkdir -p "$bin" "$state" "$agents"

# 古いバイナリが動き続けないよう、作り直す前に止める。完全に消えるまで待つ
for label in $labels; do
  launchctl bootout "gui/$uid/$label" 2>/dev/null || true
  i=0
  while launchctl print "gui/$uid/$label" >/dev/null 2>&1; do
    i=$((i + 1))
    [ "$i" -le 20 ] || { echo "bootout が終わりません: $label" >&2; exit 1; }
    sleep 0.5
  done
done

sh "$here/swift/build.sh" "$bin"
(cd "$here" && bun install --frozen-lockfile && bun build --compile cli/chrome-profiles.ts --outfile "$bin/chrome-profiles")
for label in $labels; do
  sed "s|@HOME@|$HOME|g" "$here/launchd/$label.plist.in" > "$agents/$label.plist"
done

if [ "$load" -eq 1 ]; then
  for label in $labels; do
    n=0
    until launchctl bootstrap "gui/$uid" "$agents/$label.plist"; do
      n=$((n + 1))
      [ "$n" -lt 5 ] || { echo "bootstrap に失敗しました: $label" >&2; exit 1; }
      sleep 1
    done
  done
  echo "installed and loaded."
else
  echo "installed (まだ launchd には読み込んでいません)。有効にする: mise run chrome-profiles:enable"
fi
