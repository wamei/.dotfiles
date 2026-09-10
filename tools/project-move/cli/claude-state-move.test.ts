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
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
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

function runCli(args: string[], env: Record<string, string> = {}) {
  const proc = Bun.spawnSync(["bun", CLI, ...args], {
    env: { ...process.env, ...env },
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
  const { exitCode, stderr } = runCli(
    [...baseArgs(), "/old/path", "/new/path"],
    { CLAUDE_STATE_MOVE_TEST_PGREP_STATUS: "0" },
  );
  expect(exitCode).toBe(1);
  expect(stderr).toContain("claude is running");
  expect(readFileSync(claudeJson, "utf8")).toBe(before);
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

  const { exitCode, stderr } = runCli(
    [...baseArgs(), "--from-log", log],
    { CLAUDE_STATE_MOVE_TEST_PGREP_STATUS: "1" },
  );
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
