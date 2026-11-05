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
# Teams などから開かれたリンクを受け、前面の窓のタイトルで振り分ける補助アプリ。Finicky は open -a で開くので .app にする
# (以前の名前 chrome-profiles-teams.app は消す)
rm -rf "$HOME/.local/share/chrome-profiles/chrome-profiles-teams.app"
window_app="$HOME/.local/share/chrome-profiles/chrome-profiles-window.app"
mkdir -p "$window_app/Contents/MacOS"
cp "$here/swift/window-Info.plist" "$window_app/Contents/Info.plist"
swiftc -O "$here/swift/window.swift" -o "$window_app/Contents/MacOS/chrome-profiles-window"
(cd "$here" && bun install --frozen-lockfile && bun build --compile cli/chrome-profiles.ts --outfile "$bin/chrome-profiles")
# 複製は Google の署名付きの .app なので、launchd から動く build が中身を差し替えたりアイコンを付けたりするには
# 「アプリの管理」の許可が要る (無いと EPERM)。ad-hoc 署名のままだと許可が cdhash に結び付き、作り直すたびに外れる。
# 証明書で署名すれば許可は identifier と証明書に結び付き、作り直しても残る
identity="${CHROME_PROFILES_SIGN_IDENTITY:-}"
if [ -z "$identity" ] && security find-identity -v -p codesigning 2>/dev/null | grep -q '"Apple Development'; then
  identity="Apple Development"
fi
if [ -n "$identity" ]; then
  codesign -f -s "$identity" --identifier jp.wamei.chrome-profiles "$bin/chrome-profiles"
  for name in icon watch; do
    codesign -f -s "$identity" --identifier "jp.wamei.chrome-profiles-$name" "$bin/chrome-profiles-$name"
  done
  # 窓のタイトルを読む「アクセシビリティ」の許可も、作り直して外れないようにする (identifier は Info.plist から)
  codesign -f -s "$identity" "$window_app"
else
  codesign -f -s - "$window_app"
  echo "警告: コード署名の証明書が無いので ad-hoc 署名のままです。作り直すたびに「アプリの管理」の許可を与え直してください" >&2
fi
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
