import { expect, test } from "bun:test";
import { makeRewriter } from "./rewrite.ts";

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
