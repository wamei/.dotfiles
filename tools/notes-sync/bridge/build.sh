#!/bin/sh
# notes-bridge をビルドする。出力先は第 1 引数 (既定は bridge/notes-bridge)
set -eu
here=$(cd "$(dirname "$0")" && pwd)
out=${1:-"$here/notes-bridge"}
clang -fobjc-arc -O2 -Wall -Wno-arc-performSelector-leaks \
  -framework Foundation -framework CoreData -framework UniformTypeIdentifiers \
  "$here/notes-bridge.m" -o "$out"
codesign -s - -f "$out" >/dev/null 2>&1 || true
