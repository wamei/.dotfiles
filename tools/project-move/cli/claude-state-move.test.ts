// tools/project-move/cli/claude-state-move.test.ts
//
// CLI をサブプロセスとして起動する統合テスト。src/claude-state.test.ts は
// claudeStateMove() を直接呼ぶユニットテストで、引数パースやプロセスゲート/
// 衝突ゲートといった CLI 固有のロジックはカバーしていなかった。実際に
// 「値を取るオプションの値を位置引数として拾ってしまう」バグ (positional を
// `args.filter(a => !a.startsWith("--"))` で作っていたための事故) がこの隙間を
// すり抜けてレビューで見つかったため、ここで CLI そのものを起動して確認する。
//
// 本物の ~/.claude / ~/.claude.json には一切触れない。paths は全て
// --claude-home / --claude-json / --history で明示的に tmpdir 配下を指す。
import { afterEach, beforeEach, expect, test } from "bun:test";
import { chmodSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const CLI = join(import.meta.dir, "claude-state-move.ts");

let root: string;
let claudeHome: string;
let claudeJson: string;
let historyJsonl: string;

function baseArgs(): string[] {
  return [
    "--claude-home",
    claudeHome,
    "--claude-json",
    claudeJson,
    "--history",
    historyJsonl,
  ];
}

/**
 * 偽の pgrep を用意し、それを含むディレクトリのパスを返す。
 *
 * isClaudeRunning() は `spawnSync("pgrep", ["-x", "claude"])` を呼ぶだけで、
 * CLI 側にテスト用の分岐を注入する経路は無い。以前は環境変数
 * (CLAUDE_STATE_MOVE_TEST_PGREP_STATUS) でこれを迂回していたが、レビューで
 * 「本番コードに安全ゲートの迂回路を置くと、シェルに export が 1 つ残るだけで
 * 『Claude 起動中は絶対に書き換えない』という保証が消え、しかも壊れるのは
 * ~/.claude.json で被害に気付きにくい」と指摘され、迂回路ごと撤回した。
 *
 * 代わりに、CLI を起動するサブプロセスの PATH の先頭に、実行可能な偽の
 * `pgrep` を置いたディレクトリを差し込む。こうすると本番コードは一切変えず、
 * `isClaudeRunning` 内部の `spawnSync` 呼び出しを含む本物の経路をそのまま
 * 通しつつ、その先の pgrep の結果だけを差し替えられる (環境変数で
 * isClaudeRunning 自体を丸ごと迂回するより強い検証になる)。
 */
function fakePgrepPath(exitCode: number): string {
  const binDir = mkdtempSync(join(tmpdir(), `claude-state-cli-pgrep-${exitCode}-`));
  const script = join(binDir, "pgrep");
  writeFileSync(script, `#!/bin/sh\nexit ${exitCode}\n`);
  chmodSync(script, 0o755);
  return binDir;
}

function runCli(args: string[], opts: { fakePgrepExit?: number } = {}) {
  const env = { ...process.env };
  if (opts.fakePgrepExit !== undefined) {
    const binDir = fakePgrepPath(opts.fakePgrepExit);
    env.PATH = `${binDir}:${process.env.PATH ?? ""}`;
  }
  const proc = Bun.spawnSync(["bun", CLI, ...args], {
    env,
    stdout: "pipe",
    stderr: "pipe",
  });
  return {
    exitCode: proc.exitCode,
    stdout: proc.stdout.toString(),
    stderr: proc.stderr.toString(),
  };
}

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "claude-state-cli-"));
  claudeHome = join(root, ".claude");
  claudeJson = join(root, ".claude.json");
  historyJsonl = join(root, ".claude", "history.jsonl");
  mkdirSync(join(claudeHome, "projects"), { recursive: true });
  writeFileSync(claudeJson, JSON.stringify({ projects: {} }));
  writeFileSync(historyJsonl, "");
});
afterEach(() => rmSync(root, { recursive: true, force: true }));

test("値を取るオプションと併用しても、位置引数 2 つを <old> <new> として正しく拾う (回帰テスト)", () => {
  // --claude-home などの「値」が positional に紛れ込むと、この 2 つの位置引数が
  // 無視されて --from-log 分岐 (しかも実ホーム側の既定パス) に落ちてしまっていた。
  const { exitCode, stdout } = runCli([
    "--dry-run",
    ...baseArgs(),
    "/old/path",
    "/new/path",
  ]);
  expect(exitCode).toBe(0);
  // 対象のディレクトリは存在しないので rename 等は出ないが、
  // 「moves ログが見つからない」エラーには絶対に落ちないことを確認する。
  expect(stdout).toContain("--- dry run ---");
});

test("--from-log がログファイルを読む", () => {
  const log = join(root, "moves.tsv");
  writeFileSync(log, "2026-09-11T00:00:00.000Z\t/old/from-log\t/new/from-log\n");
  const projDir = join(claudeHome, "projects", "-old-from-log");
  mkdirSync(projDir, { recursive: true });
  writeFileSync(join(projDir, "s1.jsonl"), `${JSON.stringify({ cwd: "/old/from-log" })}\n`);

  const { exitCode, stdout } = runCli(["--dry-run", ...baseArgs(), "--from-log", log]);
  expect(exitCode).toBe(0);
  expect(stdout).toContain("-new-from-log");
});

test("プロセスゲート: claude 起動中と判定されたら exit 1 で何も書き換えない", () => {
  const before = readFileSync(claudeJson, "utf8");
  const { exitCode, stderr } = runCli([...baseArgs(), "/old/path", "/new/path"], {
    fakePgrepExit: 0, // pgrep -x claude が見つかった (= 起動中) を模す
  });
  expect(exitCode).toBe(1);
  expect(stderr).toContain("claude is running");
  expect(readFileSync(claudeJson, "utf8")).toBe(before);
});

test("プロセスゲート: claude 停止中なら通過して通常どおり実行できる", () => {
  const { exitCode, stdout } = runCli([...baseArgs(), "/old/path", "/new/path"], {
    fakePgrepExit: 1, // pgrep が該当プロセス無しを模す (= 停止中)
  });
  expect(exitCode).toBe(0);
  expect(stdout).toContain("--- applied ---");
});

test("衝突ゲート: 実適用は衝突があると exit 1 で終わり、ファイルを 1 つも変えない", () => {
  const before = readFileSync(claudeJson, "utf8");
  const log = join(root, "moves.tsv");
  writeFileSync(
    log,
    [
      "2026-09-11T00:00:00.000Z\t/old/a\t/new/dest",
      "2026-09-11T00:00:00.000Z\t/old/b\t/new/dest",
      "",
    ].join("\n"),
  );

  const { exitCode, stderr } = runCli([...baseArgs(), "--from-log", log], {
    fakePgrepExit: 1, // 停止中にしてプロセスゲートを通過させ、衝突ゲートだけを見る
  });
  expect(exitCode).toBe(1);
  expect(stderr).toContain("refusing to apply");
  expect(readFileSync(claudeJson, "utf8")).toBe(before);
});

test("--dry-run は衝突があっても止まらず、警告を出して正常終了する", () => {
  const log = join(root, "moves.tsv");
  writeFileSync(
    log,
    [
      "2026-09-11T00:00:00.000Z\t/old/a\t/new/dest",
      "2026-09-11T00:00:00.000Z\t/old/b\t/new/dest",
      "",
    ].join("\n"),
  );

  const { exitCode, stdout } = runCli(["--dry-run", ...baseArgs(), "--from-log", log]);
  expect(exitCode).toBe(0);
  expect(stdout).toContain("warn");
  expect(stdout).toContain("/new/dest");
});
