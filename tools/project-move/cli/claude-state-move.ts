#!/usr/bin/env bun
// tools/project-move/cli/claude-state-move.ts
//
// Claude を全部落としてから叩く。project-move が残した移動ログを読み、
// ~/.claude 配下 6 箇所を新しいパスに追随させる。
import { homedir } from "node:os";
import { dirname, isAbsolute, join, resolve } from "node:path";
import { appendFile, mkdir, readFile, unlink } from "node:fs/promises";
import { existsSync } from "node:fs";
import { claudeStateMove, isClaudeRunning } from "../src/claude-state.ts";
import {
  appliedMovesLogPath,
  defaultMovesLogPath,
  detectMoveCollisions,
  parseMovesTsv,
  type Move,
} from "../src/moves.ts";
import { parseArgs } from "../src/args.ts";

// 値を取るオプションはここ 1 箇所にだけ列挙する。散らばると「位置引数の走査が
// オプションの値を拾ってしまう」バグが再発するため (レビュー指摘: 実際に
// `--claude-home $TMP/claude` の `$TMP/claude` を positional として拾い、
// 渡した <old> <new> を無視して既定の moves ログ (実ホーム側!) にフォールバック
// する事故が起きた)。パース自体は project-move.ts と共有する src/args.ts に
// 切り出してある。
const VALUE_OPTIONS = new Set(["claude-home", "claude-json", "history", "from-log"]);

let parsed: ReturnType<typeof parseArgs>;
try {
  parsed = parseArgs(process.argv.slice(2), VALUE_OPTIONS);
} catch (e) {
  console.error((e as Error).message);
  process.exit(2);
}
const { flags, options, positional } = parsed;

// parseArgs は未知の `--xxx` もエラーにせず flags に入れて返す (値オプションか
// どうかの判断しかしない汎用関数のため)。「dry-run 以外のフラグは未知」という
// 判断はこの CLI 固有の関心事なので、ここで検証する。
for (const f of flags) {
  if (f !== "dry-run") {
    console.error(`unknown option: --${f}`);
    process.exit(2);
  }
}
const dryRun = flags.has("dry-run");

const home = homedir();
const claudeHome = options["claude-home"] ?? join(home, ".claude");
const paths = {
  claudeHome,
  claudeJson: options["claude-json"] ?? join(home, ".claude.json"),
  // I3: --history の既定値を --claude-home に追随させる。
  // 従来は join(home, ".claude", "history.jsonl") で固定しており、
  // `--claude-home /tmp/fake` だけを渡して `--history` と `--dry-run` を
  // 両方付け忘れると、本物の ~/.claude/history.jsonl を書き換えてしまっていた。
  historyJsonl: options["history"] ?? join(claudeHome, "history.jsonl"),
};

// C1 + I4: 位置引数は 0 個 (ログから読む) か 2 個 (<old> <new>) のどちらかだけを
// 受け付ける。それ以外 (1 個、または 3 個以上) を黙って「ログ全件フォールバック」
// にしていた元の実装は、打ち間違いで蓄積された moves.tsv の全件を適用しかねない
// ので exit 2 にする。
let moves: Move[];
// I5 用: 実際に読んだログファイルのパス。<old> <new> を直接渡した経路では
// 読んだログが無いので null のまま (退避するものが無い)。
let consumedLogPath: string | null = null;

if (positional.length === 2) {
  // C1: 位置引数を絶対パスに正規化する。project-move 側の from と違い、
  // こちらの from は「移動は既に終わっていて、もう存在しない」のが正常なので
  // 存在確認はできない。代わりに「絶対パスであること」を検査する。相対パスを
  // そのまま resolve() すると、呼び出し時の cwd 次第で moves.tsv に記録された
  // 実際の絶対パスとは無関係な (たまたま解決できてしまう) パスへ静かに
  // 化けてしまうため、相対パスは resolve() で補完せず拒否する。
  for (const p of positional) {
    if (!isAbsolute(p)) {
      console.error(`${p}: must be an absolute path`);
      process.exit(2);
    }
  }
  moves = [{ from: resolve(positional[0]), to: resolve(positional[1]) }];
} else if (positional.length === 0) {
  const log = options["from-log"] ?? defaultMovesLogPath(home);
  if (!existsSync(log)) {
    console.error(`no moves log at ${log}; pass <old> <new> instead`);
    process.exit(2);
  }
  moves = parseMovesTsv(await readFile(log, "utf8"));
  consumedLogPath = log;
} else {
  console.error(`expected 0 or 2 positional arguments (<old> <new>), got ${positional.length}`);
  process.exit(2);
}

if (!dryRun && isClaudeRunning()) {
  console.error(
    "claude is running. ~/.claude.json is rewritten continuously while it runs,\n" +
      "so this would be overwritten. quit every claude session and retry.",
  );
  process.exit(1);
}

// 移動リストの衝突 (移動先の重複・連鎖書き換え・スラッグの衝突・スラッグの接頭辞
// の曖昧さ) を実適用の前に検査する。dry-run はこの検査結果を確認するために
// 回すものなので、ここでは止めずに report.warnings で見せるだけにする。実適用は
// claudeStateMove の中でファイルを書き換え始めてしまうと後戻りできないので、
// 呼び出し自体をここで止める。衝突があるまま適用すると 538 MB のセッション履歴が
// 別プロジェクトのものと不可逆に混ざり、バックアップはあっても手作業での復元は
// 現実的ではない。
const collisions = detectMoveCollisions(moves);
if (!dryRun && collisions.length > 0) {
  console.error("refusing to apply: the move list has collisions that could corrupt session history");
  for (const c of collisions) console.error(`  ${c}`);
  process.exit(1);
}

let report;
try {
  report = await claudeStateMove(moves, paths, { dryRun });
} catch (e) {
  // C2: ~/.claude/projects のバックアップが取れなかった (clonefile 非対応など)
  // ときは claudeStateMove が例外を投げて実行を中止する。ここでスタック
  // トレースをそのまま出さず、理由を短く見せて exit 1 にする。
  console.error((e as Error).message);
  process.exit(1);
}

console.log(dryRun ? "--- dry run ---" : "--- applied ---");
if (report.projectsBackupPath) {
  console.log(`backup  ${report.projectsBackupPath}`);
}
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

// I5: 適用が成功したら、消費したログを moves.applied.tsv (隣に置く) へ退避する。
//
// 最も単純な形にした: ログの中身をまるごと退避先へ追記し、元のログファイルは
// 消す。次回の実行は同じログパスを読みにいくが、消費済みの行はもう無いので
// 「未処理分だけ」が自然に残る。project-move は追記し続けるだけでログを
// 刈り込まないので、これをしないと過去に適用済みの行が溜まり続け、
// detectMoveCollisions の連鎖検出が古い行まで対象にしてしまう
// (例: `projects/foo → local/foo` が残ったまま後日 `local/foo → github.com/o/foo`
// を昇格すると、2 行が「to が別の from と一致する」連鎖とみなされ拒否される)。
// 退避先は上書きせず追記するので、監査目的の履歴は失われない。
if (!dryRun && consumedLogPath) {
  const archivePath = appliedMovesLogPath(consumedLogPath);
  await mkdir(dirname(archivePath), { recursive: true });
  const consumed = await readFile(consumedLogPath, "utf8");
  await appendFile(archivePath, consumed);
  await unlink(consumedLogPath);
}
