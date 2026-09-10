import { expect, test } from "bun:test";
import { makeRewriter, rewriteClaudeJson, rewriteJsonlLine, rewriteSessionsIndex } from "./rewrite.ts";

const r = makeRewriter([
  { from: "/Users/w/projects/mc-gpt", to: "/Users/w/projects/github.com/o/mc-gpt" },
]);

test("パスを置換する", () => {
  expect(r.rewriteText('"cwd":"/Users/w/projects/mc-gpt"')).toBe(
    '"cwd":"/Users/w/projects/github.com/o/mc-gpt"',
  );
});

test("配下のパスも置換する", () => {
  expect(r.rewriteText("/Users/w/projects/mc-gpt/src/a.ts")).toBe(
    "/Users/w/projects/github.com/o/mc-gpt/src/a.ts",
  );
});

test("別プロジェクトへの過剰一致をしない", () => {
  expect(r.rewriteText("/Users/w/projects/mc-gpt-old/a")).toBe(
    "/Users/w/projects/mc-gpt-old/a",
  );
  expect(r.rewriteText("/Users/w/projects/mc-gpt.bak")).toBe(
    "/Users/w/projects/mc-gpt.bak",
  );
});

test("スラッグも置換する (persistedOutputPath 用)", () => {
  expect(
    r.rewriteText("/Users/w/.claude/projects/-Users-w-projects-mc-gpt/x.jsonl"),
  ).toBe("/Users/w/.claude/projects/-Users-w-projects-github-com-o-mc-gpt/x.jsonl");
});

test("子スラッグ (worktree) も置換する", () => {
  expect(r.rewriteText("-Users-w-projects-mc-gpt--claude-worktrees-foo")).toBe(
    "-Users-w-projects-github-com-o-mc-gpt--claude-worktrees-foo",
  );
});

test("touches は旧パスも旧スラッグも含まない文字列に false", () => {
  expect(r.touches("no paths here")).toBe(false);
  expect(r.touches("/Users/w/projects/mc-gpt")).toBe(true);
  expect(r.touches("-Users-w-projects-mc-gpt")).toBe(true);
});

test("長いパスを先に処理して部分置換を防ぐ", () => {
  const r2 = makeRewriter([
    { from: "/p/a", to: "/x/a" },
    { from: "/p/a/b", to: "/y/b" },
  ]);
  expect(r2.rewriteText("/p/a/b/c")).toBe("/y/b/c");
});

test("置換先に $& が含まれても壊れない", () => {
  const r3 = makeRewriter([
    { from: "/p/a", to: "/p/$&" },
  ]);
  expect(r3.rewriteText("/p/a/b")).toBe("/p/$&/b");
});

test("置換先に $$ が含まれても壊れない", () => {
  const r4 = makeRewriter([
    { from: "/p/a", to: "/p/$$" },
  ]);
  expect(r4.rewriteText("/p/a/b")).toBe("/p/$$/b");
});

test("jsonl: 対象を含まない行は 1 文字も変えずに返す", () => {
  const line = '{"type":"user","cwd":"/other/place"}';
  expect(rewriteJsonlLine(line, r)).toBe(line);
});

test("jsonl: cwd と persistedOutputPath を書き換える", () => {
  const line = JSON.stringify({
    cwd: "/Users/w/projects/mc-gpt",
    toolUseResult: {
      persistedOutputPath: "/Users/w/.claude/projects/-Users-w-projects-mc-gpt/a.txt",
    },
  });
  const out = JSON.parse(rewriteJsonlLine(line, r));
  expect(out.cwd).toBe("/Users/w/projects/github.com/o/mc-gpt");
  expect(out.toolUseResult.persistedOutputPath).toBe(
    "/Users/w/.claude/projects/-Users-w-projects-github-com-o-mc-gpt/a.txt",
  );
});

test("jsonl: 壊れた行はそのまま素通しする (移行で会話を失わない)", () => {
  expect(rewriteJsonlLine("{not json/Users/w/projects/mc-gpt", r)).toBe(
    "{not json/Users/w/projects/mc-gpt",
  );
});

test("claude.json: projects のキーをリネームする", () => {
  const out = rewriteClaudeJson(
    { projects: { "/Users/w/projects/mc-gpt": { allowedTools: ["a"] } }, other: 1 },
    r,
  ) as any;
  expect(Object.keys(out.projects)).toEqual(["/Users/w/projects/github.com/o/mc-gpt"]);
  expect(out.projects["/Users/w/projects/github.com/o/mc-gpt"].allowedTools).toEqual(["a"]);
  expect(out.other).toBe(1);
});

test("claude.json: 移動先のキーが既にあれば既存値を優先して残す", () => {
  const out = rewriteClaudeJson(
    {
      projects: {
        "/Users/w/projects/mc-gpt": { allowedTools: ["old"], onlyOld: true },
        "/Users/w/projects/github.com/o/mc-gpt": { allowedTools: ["new"] },
      },
    },
    r,
  ) as any;
  const merged = out.projects["/Users/w/projects/github.com/o/mc-gpt"];
  expect(merged.allowedTools).toEqual(["new"]);
  expect(merged.onlyOld).toBe(true);
});

test("claude.json: githubRepoPaths の値を置換し重複を畳む", () => {
  const out = rewriteClaudeJson(
    {
      githubRepoPaths: {
        "o/mc-gpt": [
          "/Users/w/projects/mc-gpt",
          "/Users/w/projects/github.com/o/mc-gpt",
        ],
      },
    },
    r,
  ) as any;
  expect(out.githubRepoPaths["o/mc-gpt"]).toEqual([
    "/Users/w/projects/github.com/o/mc-gpt",
  ]);
});

test("sessions-index: originalPath と entries の 2 つのパスを置換する", () => {
  const out = rewriteSessionsIndex(
    {
      version: 1,
      originalPath: "/Users/w/projects/mc-gpt",
      entries: [
        {
          sessionId: "s1",
          projectPath: "/Users/w/projects/mc-gpt",
          fullPath: "/Users/w/.claude/projects/-Users-w-projects-mc-gpt/s1.jsonl",
        },
      ],
    },
    r,
  ) as any;
  expect(out.originalPath).toBe("/Users/w/projects/github.com/o/mc-gpt");
  expect(out.entries[0].projectPath).toBe("/Users/w/projects/github.com/o/mc-gpt");
  expect(out.entries[0].fullPath).toBe(
    "/Users/w/.claude/projects/-Users-w-projects-github-com-o-mc-gpt/s1.jsonl",
  );
});

test("sessions-index: entries が無くても落ちない", () => {
  expect(rewriteSessionsIndex({ version: 1 }, r)).toEqual({ version: 1 });
});
