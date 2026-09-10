import { expect, test } from "bun:test";
import { readFileSync } from "node:fs";
import { slug, slugChildRemainder, SLUG_MAX_LENGTH } from "./slug.ts";

const golden = readFileSync(new URL("../fixtures/slugs.tsv", import.meta.url), "utf8")
  .split("\n")
  .filter((l) => l.trim() !== "" && !l.startsWith("#"))
  .map((l) => l.split("\t") as [string, string]);

test("実データ 18 件のスラッグを全件再現する", () => {
  expect(golden.length).toBe(18);
  for (const [path, expected] of golden) {
    expect(slug(path)).toBe(expected);
  }
});

test("英数字以外は 1 文字ずつ - になり、連続 - は畳まれない", () => {
  expect(slug("/a/.b_c.d")).toBe("-a--b-c-d");
});

test("大文字小文字は保存する", () => {
  expect(slug("/BeecoV2")).toBe("-BeecoV2");
});

test("slug は 1 文字ずつの写像なので子パスは親スラッグの接頭辞を持つ", () => {
  const parent = "/Users/wamei/projects/BeecoV2";
  const child = "/Users/wamei/projects/BeecoV2/.claude-worktrees/pr1054-test-fix";
  expect(slug(child).startsWith(slug(parent))).toBe(true);
});

test("200 文字を超えたら例外で落ちる (ハッシュ接尾辞は実装しない)", () => {
  const long = "/" + "a".repeat(SLUG_MAX_LENGTH);
  expect(() => slug(long)).toThrow(/200/);
});

test("ちょうど 200 文字は正常に通る", () => {
  const exactly200 = "/" + "a".repeat(SLUG_MAX_LENGTH - 1);
  expect(slug(exactly200).length).toBe(SLUG_MAX_LENGTH);
  expect(() => slug(exactly200)).not.toThrow();
});

test("slugChildRemainder: 完全一致は空文字を返す", () => {
  expect(slugChildRemainder("-a-b", "-a-b")).toBe("");
});

test("slugChildRemainder: 子は - 始まりの残りを返す", () => {
  expect(slugChildRemainder("-a-b--c", "-a-b")).toBe("--c");
});

test("slugChildRemainder: /a/bc は /a/b に過剰一致しない", () => {
  expect(slugChildRemainder(slug("/a/bc"), slug("/a/b"))).toBeNull();
});
