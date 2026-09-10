// tools/project-move/src/claude-state.test.ts
import { afterEach, beforeEach, expect, test } from "bun:test";
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync, existsSync, readdirSync } from "node:fs";
import { tmpdir } from "node:os";
import { basename, dirname, join } from "node:path";
import {
  claudeStateMove,
  mergeSessionsIndexes,
  rewriteLines,
  type ClaudeStatePaths,
} from "./claude-state.ts";
import { makeRewriter } from "./rewrite.ts";

let root: string;
let paths: ClaudeStatePaths;
const OLD = "/Users/w/projects/mc-gpt";
const NEW = "/Users/w/projects/github.com/o/mc-gpt";
const OLD_SLUG = "-Users-w-projects-mc-gpt";
const NEW_SLUG = "-Users-w-projects-github-com-o-mc-gpt";

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "claude-state-"));
  paths = {
    claudeHome: join(root, ".claude"),
    claudeJson: join(root, ".claude.json"),
    historyJsonl: join(root, ".claude", "history.jsonl"),
  };
  const projDir = join(paths.claudeHome, "projects", OLD_SLUG);
  mkdirSync(join(projDir, "s1", "subagents"), { recursive: true });
  writeFileSync(join(projDir, "s1.jsonl"), `${JSON.stringify({ cwd: OLD })}\n`);
  writeFileSync(
    join(projDir, "s1", "subagents", "agent-a.jsonl"),
    `${JSON.stringify({ cwd: OLD })}\n${JSON.stringify({ cwd: "/elsewhere" })}\n`,
  );
  writeFileSync(
    join(projDir, "sessions-index.json"),
    JSON.stringify({ version: 1, originalPath: OLD, entries: [] }),
  );
  // worktree の子スラッグ。cwd は親を指すので、cwd からは名前を導けない
  mkdirSync(join(paths.claudeHome, "projects", `${OLD_SLUG}--claude-worktrees-wt`), {
    recursive: true,
  });
  writeFileSync(paths.claudeJson, JSON.stringify({ projects: { [OLD]: { allowedTools: [] } } }));
  writeFileSync(paths.historyJsonl, `${JSON.stringify({ display: "x", project: OLD })}\n`);
});
afterEach(() => rmSync(root, { recursive: true, force: true }));

const moves = [{ from: OLD, to: NEW }];

test("dry-run は何も書かず、やることだけを報告する", async () => {
  const report = await claudeStateMove(moves, paths, { dryRun: true });
  expect(report.renames.map((r) => r.to)).toContain(NEW_SLUG);
  expect(existsSync(join(paths.claudeHome, "projects", OLD_SLUG))).toBe(true);
  expect(existsSync(join(paths.claudeHome, "projects", NEW_SLUG))).toBe(false);
  // 差し替え 2: dry-run でも移動元 (from) を読んで書き換え件数を報告する。
  // to 固定で読むと dry-run では改名していないため to が存在せず、
  // プロジェクトディレクトリ配下の jsonl が常に 0 件報告になってしまう。
  // (historyJsonl は projects 配下と無関係に書き換わるので、それだけでは
  // この不具合を検出できない。ファイル名で名指しして確認する。)
  expect(report.rewrittenFiles.some((f) => f.path.endsWith("s1.jsonl"))).toBe(true);
  expect(report.rewrittenFiles.some((f) => f.path.endsWith("agent-a.jsonl"))).toBe(true);
});

test("スラッグディレクトリを改名する", async () => {
  await claudeStateMove(moves, paths, { dryRun: false });
  expect(existsSync(join(paths.claudeHome, "projects", NEW_SLUG))).toBe(true);
  expect(existsSync(join(paths.claudeHome, "projects", OLD_SLUG))).toBe(false);
});

test("worktree の子スラッグも接頭辞一致で連れていく", async () => {
  await claudeStateMove(moves, paths, { dryRun: false });
  expect(
    existsSync(join(paths.claudeHome, "projects", `${NEW_SLUG}--claude-worktrees-wt`)),
  ).toBe(true);
});

test("subagents 配下の jsonl も書き換える", async () => {
  await claudeStateMove(moves, paths, { dryRun: false });
  const p = join(paths.claudeHome, "projects", NEW_SLUG, "s1", "subagents", "agent-a.jsonl");
  const lines = readFileSync(p, "utf8").trim().split("\n").map((l) => JSON.parse(l));
  expect(lines[0].cwd).toBe(NEW);
  expect(lines[1].cwd).toBe("/elsewhere");
});

test("sessions-index.json / claude.json / history.jsonl を書き換える", async () => {
  await claudeStateMove(moves, paths, { dryRun: false });
  const idx = JSON.parse(
    readFileSync(join(paths.claudeHome, "projects", NEW_SLUG, "sessions-index.json"), "utf8"),
  );
  expect(idx.originalPath).toBe(NEW);
  expect(Object.keys(JSON.parse(readFileSync(paths.claudeJson, "utf8")).projects)).toEqual([NEW]);
  expect(JSON.parse(readFileSync(paths.historyJsonl, "utf8").trim()).project).toBe(NEW);
});

test("移動先のスラッグが既にあればマージし、merged を立てる", async () => {
  const dest = join(paths.claudeHome, "projects", NEW_SLUG);
  mkdirSync(dest, { recursive: true });
  writeFileSync(join(dest, "existing.jsonl"), `${JSON.stringify({ cwd: NEW })}\n`);
  const report = await claudeStateMove(moves, paths, { dryRun: false });
  expect(report.renames.find((r) => r.to === NEW_SLUG)?.merged).toBe(true);
  expect(existsSync(join(dest, "existing.jsonl"))).toBe(true);
  expect(existsSync(join(dest, "s1.jsonl"))).toBe(true);
  // 差し替え 3: マージ後、中身を移し終えた移動元ディレクトリは空になり、削除されているはず。
  // 残すと ~/.claude/projects に実在しないパスの名前のディレクトリが溜まる。
  expect(existsSync(join(paths.claudeHome, "projects", OLD_SLUG))).toBe(false);
});

test("実行前にバックアップを取る", async () => {
  await claudeStateMove(moves, paths, { dryRun: false });
  expect(existsSync(`${paths.claudeJson}.project-move-backup`)).toBe(true);
});

test("2 回目の実行は 1 回目のバックアップを上書きしない", async () => {
  const original = readFileSync(paths.claudeJson, "utf8");
  await claudeStateMove(moves, paths, { dryRun: false });
  const backupPath = `${paths.claudeJson}.project-move-backup`;
  expect(readFileSync(backupPath, "utf8")).toBe(original);

  // 2 回目 (対象になる move が無くても構わない。バックアップの副作用だけを見る)
  await claudeStateMove(moves, paths, { dryRun: false });

  // 1 回目の、まだ何も触られていない原本のバックアップは変わらず残っている
  expect(readFileSync(backupPath, "utf8")).toBe(original);
  // 2 回目の実行でも新しいバックアップが増えている (上書きされて消えたのではない)
  const dir = dirname(paths.claudeJson);
  const base = basename(paths.claudeJson);
  const backups = readdirSync(dir).filter((f) => f.startsWith(`${base}.project-move-backup`));
  expect(backups.length).toBeGreaterThan(1);
});

test("sessions-index.json は移動先にもあれば entries をマージし、重複 sessionId は移動先を残す", async () => {
  writeFileSync(
    join(paths.claudeHome, "projects", OLD_SLUG, "sessions-index.json"),
    JSON.stringify({
      version: 1,
      originalPath: OLD,
      entries: [
        { sessionId: "dup", projectPath: OLD, fullPath: "old-dup-path" },
        { sessionId: "srcOnly", projectPath: OLD, fullPath: "old-src-only-path" },
      ],
    }),
  );
  const destDir = join(paths.claudeHome, "projects", NEW_SLUG);
  mkdirSync(destDir, { recursive: true });
  writeFileSync(
    join(destDir, "sessions-index.json"),
    JSON.stringify({
      version: 2,
      originalPath: NEW,
      entries: [
        { sessionId: "dup", projectPath: NEW, fullPath: "new-dup-path" },
        { sessionId: "destOnly", projectPath: NEW, fullPath: "new-dest-only-path" },
      ],
    }),
  );

  await claudeStateMove(moves, paths, { dryRun: false });

  const idx = JSON.parse(readFileSync(join(destDir, "sessions-index.json"), "utf8"));
  expect(idx.version).toBe(2);
  const bySessionId = Object.fromEntries(idx.entries.map((e: { sessionId: string }) => [e.sessionId, e]));
  expect(Object.keys(bySessionId).sort()).toEqual(["destOnly", "dup", "srcOnly"]);
  // 重複した sessionId は移動先側の内容を残す
  expect(bySessionId.dup.fullPath).toBe("new-dup-path");
  // 移動元にしか無いエントリも連結されている
  expect(bySessionId.srcOnly).toBeDefined();
  // マージし終えた移動元ディレクトリは空になり rmdir される
  expect(existsSync(join(paths.claudeHome, "projects", OLD_SLUG))).toBe(false);
});

test("mergeSessionsIndexes: 重複 sessionId は移動先を残し、version/originalPath も移動先を使う", () => {
  const dest = {
    version: 2,
    originalPath: NEW,
    entries: [{ sessionId: "dup", tag: "dest" }, { sessionId: "destOnly" }],
  };
  const src = {
    version: 1,
    originalPath: OLD,
    entries: [{ sessionId: "dup", tag: "src" }, { sessionId: "srcOnly" }],
  };
  const merged = mergeSessionsIndexes(dest, src) as {
    version: number;
    originalPath: string;
    entries: { sessionId: string; tag?: string }[];
  };
  expect(merged.version).toBe(2);
  expect(merged.originalPath).toBe(NEW);
  expect(merged.entries.map((e) => e.sessionId).sort()).toEqual(["destOnly", "dup", "srcOnly"]);
  expect(merged.entries.find((e) => e.sessionId === "dup")?.tag).toBe("dest");
});

test("mergeSessionsIndexes: 片方に entries が無くても落ちない", () => {
  expect(mergeSessionsIndexes({ version: 1 }, { version: 1, entries: [{ sessionId: "a" }] })).toEqual({
    version: 1,
    entries: [{ sessionId: "a" }],
  });
  expect(mergeSessionsIndexes({ version: 1, entries: [{ sessionId: "a" }] }, { version: 1 })).toEqual({
    version: 1,
    entries: [{ sessionId: "a" }],
  });
});

test("書き換え後に一時ファイルが残らない", async () => {
  await claudeStateMove(moves, paths, { dryRun: false });
  // projects 配下・historyJsonl 双方に *.project-move-tmp が残っていないことを確認する。
  function findTmpFiles(dir: string): string[] {
    if (!existsSync(dir)) return [];
    const found: string[] = [];
    for (const entry of readdirSync(dir, { withFileTypes: true })) {
      const p = join(dir, entry.name);
      if (entry.isDirectory()) found.push(...findTmpFiles(p));
      else if (entry.name.endsWith(".project-move-tmp")) found.push(p);
    }
    return found;
  }
  expect(findTmpFiles(paths.claudeHome)).toEqual([]);
});

test("rewriteLines は例外で抜けても *.project-move-tmp を残さない", async () => {
  const filePath = join(root, "boom.jsonl");
  writeFileSync(filePath, "a\nb\nc\n");
  const throwing = (): string => {
    throw new Error("boom");
  };
  const r = makeRewriter(moves);
  await expect(rewriteLines(filePath, r, throwing, false)).rejects.toThrow("boom");
  expect(existsSync(`${filePath}.project-move-tmp`)).toBe(false);
});
