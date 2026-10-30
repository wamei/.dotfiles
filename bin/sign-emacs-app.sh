#!/bin/sh
# Emacs.app を Apple Development 証明書で署名し直す。
#
# emacs-plus-app の cask が入れる Emacs.app は ad-hoc 署名で、TCC (フルディスク
# アクセスなど) の許可がビルドの cdhash に結び付く。brew upgrade で Emacs.app が
# 差し替わるたびに許可が黙って外れる。証明書で署名すれば許可は identifier
# (org.gnu.Emacs) と証明書に結び付き、差し替わっても署名し直せば残る
# (証明書を更新したときだけ与え直しになる)。
#
# 既に証明書で署名されていれば何もしない (--deep の署名は 20 秒ほどかかる)。
# 起動中の Emacs.app を署名し直してもよい (実行ファイルは別の inode に置き換わる
# ので動いているプロセスは落ちない。新しい署名は次の起動から効く)。
set -eu
app="${1:-/Applications/Emacs.app}"
[ -d "$app" ] || { echo "sign-emacs-app: $app がありません" >&2; exit 0; }

identity="${EMACS_SIGN_IDENTITY:-}"
if [ -z "$identity" ] && security find-identity -v -p codesigning 2>/dev/null | grep -q '"Apple Development'; then
  identity="Apple Development"
fi
if [ -z "$identity" ]; then
  echo "sign-emacs-app: コード署名の証明書が無いので ad-hoc 署名のままです。Emacs.app を更新するたびに TCC の許可を与え直してください" >&2
  exit 0
fi

# ad-hoc 署名は TeamIdentifier を持たない。証明書で署名済みで中身も壊れていなければ済み
if codesign -dv "$app" 2>&1 | grep -q '^TeamIdentifier=[A-Z0-9]' \
  && codesign --verify --deep --strict "$app" 2>/dev/null; then
  exit 0
fi

echo "sign-emacs-app: $app を \"$identity\" で署名し直します"
codesign -f --deep -s "$identity" "$app"
codesign --verify --deep --strict "$app"
