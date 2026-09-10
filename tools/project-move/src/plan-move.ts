import { spawnSync } from "node:child_process";
import { basename, join, resolve } from "node:path";

export type MovePlan = { kind: "ghq" | "local"; from: string; to: string };
export type GhqProbeResult = { ok: true; to: string } | { ok: false; reason: string };
export type GhqProbe = (dir: string) => GhqProbeResult;

/**
 * ghq が「remote 無し」または「非 git」として local 行きにしてよいと確認済みの
 * 理由だけを許可リストにする (I7)。
 *
 * `Would migrate` の行が出なければ元の実装は無条件に local 扱いにしていたため、
 * ghq 未インストール・設定エラー・出力書式の変更のいずれでも remote を持つ
 * リポジトリが `local/` に落ちてしまっていた。ここに列挙した 2 つ以外の理由は
 * 「ghq が正しく判定できなかった」可能性を捨てきれないので、local 扱いにせず
 * 呼び出し側 (CLI) を止めて理由を見せる。
 */
const KNOWN_LOCAL_REASONS = [/no remotes found/, /failed to detect VCS backend/];

function isKnownLocalReason(reason: string): boolean {
  return KNOWN_LOCAL_REASONS.some((re) => re.test(reason));
}

/**
 * ghq に移動先を訊く。
 *
 * 移動先を自分で組み立てないのは、host/owner/repo の導出 (SSH 別名ホスト
 * github.com.linka や gitlab の入れ子グループ) を ghq と食い違わせないため。
 * 成功時は `Would migrate <from> to <to>` を 1 行返し、remote 無しは
 * `no remotes found`、非 git は `failed to detect VCS backend` で exit 1。
 */
export function ghqProbe(dir: string): GhqProbeResult {
  const r = spawnSync("ghq", ["migrate", "--dry-run", dir], { encoding: "utf8" });
  const m = (r.stdout ?? "").match(/^Would migrate .+ to (.+)$/m);
  if (m) return { ok: true, to: m[1].trim() };
  return { ok: false, reason: ((r.stderr ?? "") + (r.stdout ?? "")).trim() };
}

/**
 * @throws probe が local 行きの許可リストに無い理由で失敗を返したとき (I7)。
 *   呼び出し側はこれを「1 件飛ばして次へ」ではなく「止めて理由を見せる」で
 *   扱うこと (remote を持つリポジトリが誤って local/ に落ちるのを防ぐため)。
 */
export function planMove(
  dir: string,
  opts: { probe: GhqProbe; localRoot: string },
): MovePlan {
  // 位置引数がそのまま渡ってくる想定なので、ここで絶対パスに正規化する (C1)。
  // 相対パスのまま `basename(from)` や probe に渡すと、"BeecoV2" のような裸の
  // 名前が全ファイルの置換対象になり、~/.claude.json 等を二重に壊れたパスへ
  // 書き換えてしまう。resolve() は末尾スラッシュや "." "/.." も正規化するので、
  // 従来の `dir.replace(/\/+$/, "")` は不要になる。
  const from = resolve(dir);
  const probed = opts.probe(from);
  if (probed.ok) return { kind: "ghq", from, to: probed.to };
  if (!isKnownLocalReason(probed.reason)) {
    throw new Error(`ghq could not determine a destination for ${from}: ${probed.reason}`);
  }
  return { kind: "local", from, to: join(opts.localRoot, basename(from)) };
}
