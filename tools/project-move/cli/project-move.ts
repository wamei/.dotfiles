#!/usr/bin/env bun
// tools/project-move/cli/project-move.ts
//
// Claude が動いていても安全な作業だけをする。~/.claude 配下の追随は
// claude-state-move の担当 (~/.claude.json は Claude 起動中ずっと書き戻されるため)。
import { appendFile, mkdir, rename } from "node:fs/promises";
import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { spawnSync } from "node:child_process";
import { ghqProbe, planMove, type MovePlan } from "../src/plan-move.ts";
import { applyFixups, isEmacsRunning } from "../src/project-move.ts";
import { defaultMovesLogPath, formatMoveRow } from "../src/moves.ts";
import { parseArgs } from "../src/args.ts";

// 値を取るオプションはここ project-move では --to だけ。claude-state-move.ts と
// 同じ src/args.ts の parseArgs を使うのは、`args.filter(a => !a.startsWith("--"))`
// のような「-- で始まらないものは全部位置引数」という実装が、値を取るオプション
// の値まで位置引数として拾ってしまうバグを二重に持ち込まないため
// (claude-state-move で実際に踏んだ: <old> <new> が無視されて既定ログ
// (実ホーム側!) にフォールバックする事故があった)。
const VALUE_OPTIONS = new Set(["to"]);

let parsed: ReturnType<typeof parseArgs>;
try {
  parsed = parseArgs(process.argv.slice(2), VALUE_OPTIONS);
} catch (e) {
  console.error((e as Error).message);
  process.exit(2);
}
const { flags, options, positional: dirs } = parsed;

// 未知の `--xxx` は静かに無視せず落とす (dry-run 以外のフラグは想定していない)。
for (const f of flags) {
  if (f !== "dry-run") {
    console.error(`unknown option: --${f}`);
    process.exit(2);
  }
}
const dryRun = flags.has("dry-run");
const explicitTo = options.to;

// --to は 1 つのディレクトリにしか許さない。複数のディレクトリに同じ移動先を
// 指定しても意味を成さず、後から処理したものが先の移動結果を上書きしてしまう
// (mkdir + rename の宛先が全員同じになる)。spec の
// `project-move [--dry-run] [--to <path>] <dir>...` という形自体は複数
// ディレクトリを許すが、それは --to 無しのとき (ghq / localRoot が各々に
// 別の移動先を計算する) の話であって、--to で明示指定したときは 1 対 1 の
// 関係でなければ安全に扱えない。ここで早期に弾き、1 つも移動を始めない。
if (explicitTo !== undefined && dirs.length !== 1) {
  console.error("--to can only be used with exactly one directory");
  process.exit(2);
}
const home = homedir();

const EMACS_STATE_FILES = [
  ".emacs.d/recentf",
  ".emacs.d/places",
  ".emacs.d/.emacs.desktop",
  ".emacs.d/.emacs.desktop-nw",
  ".emacs.d/projects",
  ".emacs.d/.cache/treemacs-persist",
  ".emacs.d/history",
].map((p) => join(home, p));

const emacsRunning = isEmacsRunning();
const log = defaultMovesLogPath(home);

for (const dir of dirs) {
  // --to が与えられたときは ghq / localRoot の自動判定を使わず、指定された
  // path をそのまま移動先にする。種別は "local" 扱いにする (ghq が決めた場所
  // ではなく、ユーザーが明示指定した場所という意味で、以降の existsSync チェック
  // や rename も local と同じ経路を通す)。
  const plan: MovePlan =
    explicitTo !== undefined
      ? { kind: "local", from: dir.replace(/\/+$/, ""), to: explicitTo }
      : planMove(dir, { probe: ghqProbe, localRoot: join(home, "projects", "local") });
  console.log(`${plan.kind === "ghq" ? "ghq  " : "local"} ${plan.from} -> ${plan.to}`);

  // 移動前に、失いうるものを見せる (CLAUDE.md の「破壊的操作」節)
  const status = spawnSync("git", ["-C", plan.from, "status", "--porcelain"], { encoding: "utf8" });
  if ((status.stdout ?? "").trim() !== "") {
    console.log(`  uncommitted:\n${status.stdout.trimEnd()}`);
  }

  if (!dryRun) {
    if (plan.kind === "ghq") {
      // ghq に任せる。移動後の `git worktree repair` まで面倒を見てくれる。
      const r = spawnSync("ghq", ["migrate", "-y", plan.from], { stdio: "inherit" });
      if (r.status !== 0) { console.error(`  ghq migrate failed; skipped`); continue; }
    } else {
      await mkdir(dirname(plan.to), { recursive: true });
      if (existsSync(plan.to)) { console.error(`  ${plan.to} exists; skipped`); continue; }
      await rename(plan.from, plan.to);
    }
  }

  const report = await applyFixups(plan, { emacsStateFiles: EMACS_STATE_FILES, dryRun, emacsRunning });
  for (const f of report.rewrittenFiles) console.log(`  rewrite ${f.path} (${f.changedLines} lines)`);
  for (const s of report.manualSteps) console.log(`  todo    ${s}`);
  for (const w of report.warnings) console.log(`  warn    ${w}`);

  if (!dryRun) {
    await mkdir(dirname(log), { recursive: true });
    await appendFile(log, formatMoveRow({ from: plan.from, to: plan.to }, new Date()));
  }
}

console.log(
  dryRun
    ? "\ndry run. nothing was changed."
    : `\nlogged to ${log}. quit every claude session, then run:\n  claude-state-move --dry-run`,
);
