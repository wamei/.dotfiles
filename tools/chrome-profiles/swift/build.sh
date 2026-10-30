#!/bin/sh
# Swift の 2 つのバイナリをビルドする。出力先ディレクトリは第 1 引数 (既定は swift/)
set -eu
here=$(cd "$(dirname "$0")" && pwd)
out=${1:-"$here"}
mkdir -p "$out"
for name in icon watch; do
  swiftc -O "$here/$name.swift" -o "$out/chrome-profiles-$name"
  codesign -s - -f "$out/chrome-profiles-$name" >/dev/null 2>&1 || true
done
