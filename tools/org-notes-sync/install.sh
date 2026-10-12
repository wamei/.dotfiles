#!/bin/sh
# org-notes-sync を ~/.local/bin に入れ、launchd に登録する。
# フルディスクアクセスは手で 1 回与える (README 参照)。作り直すと署名が変わるので与え直す。
set -eu
here=$(cd "$(dirname "$0")" && pwd)
bin="$HOME/.local/bin"; state="$HOME/.local/state/org-notes-sync"; agents="$HOME/Library/LaunchAgents"
label=jp.wamei.org-notes-sync
mkdir -p "$bin" "$state" "$agents"
sh "$here/bridge/build.sh" "$bin/notes-bridge"
(cd "$here" && bun install --frozen-lockfile && bun build --compile cli/org-notes-sync.ts --outfile "$bin/org-notes-sync")
sed "s|@HOME@|$HOME|g" "$here/launchd/$label.plist.in" > "$agents/$label.plist"
launchctl bootout "gui/$(id -u)/$label" 2>/dev/null || true
launchctl bootstrap "gui/$(id -u)" "$agents/$label.plist"
echo "installed. フルディスクアクセスに $bin/org-notes-sync を追加してください"
