#!/usr/bin/env bun
// tools/project-move/cli/claude-state-move.ts
//
// Claude を全部落としてから叩く。project-move が残した移動ログを読み、
// ~/.claude 配下 6 箇所を新しいパスに追随させる。
import { homedir } from "node:os";
import { join } from "node:path";
import { readFile } from "node:fs/promises";
import { existsSync } from "node:fs";
import { claudeStateMove, isClaudeRunning } from "../src/claude-state.ts";
import {
  defaultMovesLogPath,
  detectMoveCollisions,
  parseMovesTsv,
  type Move,
} from "../src/moves.ts";

const args = process.argv.slice(2);
const dryRun = args.includes("--dry-run");
const positional = args.filter((a) => !a.startsWith("--"));

function optionValue(name: string, fallback: string): string {
  const i = args.indexOf(`--${name}`);
  return i >= 0 && args[i + 1] ? args[i + 1] : fallback;
}

const home = homedir();
const paths = {
  claudeHome: optionValue("claude-home", join(home, ".claude")),
  claudeJson: optionValue("claude-json", join(home, ".claude.json")),
  historyJsonl: optionValue("history", join(home, ".claude", "history.jsonl")),
};

let moves: Move[];
if (positional.length === 2) {
  moves = [{ from: positional[0], to: positional[1] }];
} else {
  const log = optionValue("from-log", defaultMovesLogPath(home));
  if (!existsSync(log)) {
    console.error(`no moves log at ${log}; pass <old> <new> instead`);
    process.exit(2);
  }
  moves = parseMovesTsv(await readFile(log, "utf8"));
}

if (!dryRun && isClaudeRunning()) {
  console.error(
    "claude is running. ~/.claude.json is rewritten continuously while it runs,\n" +
      "so this would be overwritten. quit every claude session and retry.",
  );
  process.exit(1);
}

// 移動リストの衝突 (移動先の重複・連鎖書き換え・スラッグの衝突) を実適用の前に
// 検査する。dry-run はこの検査結果を確認するために回すものなので、ここでは
// 止めずに report.warnings で見せるだけにする。実適用は claudeStateMove の中で
// ファイルを書き換え始めてしまうと後戻りできないので、呼び出し自体をここで
// 止める。衝突があるまま適用すると 538 MB のセッション履歴が別プロジェクトの
// ものと不可逆に混ざり、バックアップはあっても手作業での復元は現実的ではない。
const collisions = detectMoveCollisions(moves);
if (!dryRun && collisions.length > 0) {
  console.error("refusing to apply: the move list has collisions that could corrupt session history");
  for (const c of collisions) console.error(`  ${c}`);
  process.exit(1);
}

const report = await claudeStateMove(moves, paths, { dryRun });

console.log(dryRun ? "--- dry run ---" : "--- applied ---");
for (const rn of report.renames) {
  console.log(`rename  ${rn.from} -> ${rn.to}${rn.merged ? " (merged into existing)" : ""}`);
}
for (const f of report.rewrittenFiles) {
  console.log(`rewrite ${f.path} (${f.changedLines} lines)`);
}
for (const k of report.claudeJsonKeys) {
  console.log(`key     ${k.from} -> ${k.to}`);
}
for (const w of report.warnings) console.log(`warn    ${w}`);
