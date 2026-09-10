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

// 値を取るオプションはここ 1 箇所にだけ列挙する。散らばると「位置引数の走査が
// オプションの値を拾ってしまう」バグが再発するため (レビュー指摘: 実際に
// `--claude-home $TMP/claude` の `$TMP/claude` を positional として拾い、
// 渡した <old> <new> を無視して既定の moves ログ (実ホーム側!) にフォールバック
// する事故が起きた)。
const VALUE_OPTIONS = new Set(["claude-home", "claude-json", "history", "from-log"]);

/**
 * 引数を先頭から順に 1 パスで走査する。
 *
 * `args.filter(a => !a.startsWith("--"))` のような「-- で始まらないものを
 * 全部位置引数とみなす」実装は、値を取るオプションの値 (`--claude-home` の次の
 * トークンなど) まで位置引数として拾ってしまう。ここでは VALUE_OPTIONS に
 * 載っているオプションに当たったら次のトークンを値として明示的に消費して
 * 読み飛ばし、それ以外の `--` 始まりはフラグとしてそのまま次へ、残りだけを
 * 位置引数として集める。
 */
function parseArgs(argv: string[]): {
  dryRun: boolean;
  options: Record<string, string>;
  positional: string[];
} {
  let dryRun = false;
  const options: Record<string, string> = {};
  const positional: string[] = [];

  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === "--dry-run") {
      dryRun = true;
      continue;
    }
    if (arg.startsWith("--")) {
      const name = arg.slice(2);
      if (!VALUE_OPTIONS.has(name)) {
        console.error(`unknown option: ${arg}`);
        process.exit(2);
      }
      const value = argv[i + 1];
      // 値が無い、または次のトークンが別のオプションに見える場合は、それを
      // うっかり値として採用しない (以前のバグの裏返し: `--dry-run` のような
      // 文字列をそのままパスとして受け取ってしまうのを防ぐ)。
      if (value === undefined || value.startsWith("--")) {
        console.error(`missing value for ${arg}`);
        process.exit(2);
      }
      options[name] = value;
      i++; // 値のトークンを消費
      continue;
    }
    positional.push(arg);
  }

  return { dryRun, options, positional };
}

const { dryRun, options, positional } = parseArgs(process.argv.slice(2));

const home = homedir();
const paths = {
  claudeHome: options["claude-home"] ?? join(home, ".claude"),
  claudeJson: options["claude-json"] ?? join(home, ".claude.json"),
  historyJsonl: options["history"] ?? join(home, ".claude", "history.jsonl"),
};

let moves: Move[];
if (positional.length === 2) {
  moves = [{ from: positional[0], to: positional[1] }];
} else {
  const log = options["from-log"] ?? defaultMovesLogPath(home);
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
