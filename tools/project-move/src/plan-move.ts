import { spawnSync } from "node:child_process";
import { basename, join } from "node:path";

export type MovePlan = { kind: "ghq" | "local"; from: string; to: string };
export type GhqProbeResult = { ok: true; to: string } | { ok: false; reason: string };
export type GhqProbe = (dir: string) => GhqProbeResult;

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

export function planMove(
  dir: string,
  opts: { probe: GhqProbe; localRoot: string },
): MovePlan {
  const from = dir.replace(/\/+$/, "");
  const probed = opts.probe(from);
  return probed.ok
    ? { kind: "ghq", from, to: probed.to }
    : { kind: "local", from, to: join(opts.localRoot, basename(from)) };
}
