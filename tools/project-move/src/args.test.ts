// tools/project-move/src/args.test.ts
//
// parseArgs は project-move / claude-state-move の両 CLI が共有する引数
// パーサ。もともと claude-state-move.ts にだけ実装されていたものを切り出した。
// 切り出す理由になったバグ (`args.filter(a => !a.startsWith("--"))` が値を
// 取るオプションの値まで positional として拾ってしまう) の回帰テストを
// ここに置く。
import { expect, test } from "bun:test";
import { parseArgs } from "./args.ts";

test("値を取らないオプションは flags に集まる", () => {
  const { flags, options, positional } = parseArgs(["--dry-run", "a", "b"], new Set());
  expect(flags.has("dry-run")).toBe(true);
  expect(options).toEqual({});
  expect(positional).toEqual(["a", "b"]);
});

test("値を取るオプションの値は positional に紛れ込まない (回帰テスト)", () => {
  // `args.filter(a => !a.startsWith("--"))` だと --claude-home の値である
  // /tmp/claude まで positional として拾ってしまい、渡した <old> <new> が
  // 無視されて既定ログにフォールバックする事故があった (Task 6 で実際に発生)。
  const { options, positional } = parseArgs(
    ["--claude-home", "/tmp/claude", "/old", "/new"],
    new Set(["claude-home"]),
  );
  expect(options["claude-home"]).toBe("/tmp/claude");
  expect(positional).toEqual(["/old", "/new"]);
});

test("複数の値オプションとフラグが混在しても正しく分離される", () => {
  const { flags, options, positional } = parseArgs(
    ["--dry-run", "--claude-home", "/tmp/c", "--history", "/tmp/h", "/old", "/new"],
    new Set(["claude-home", "history"]),
  );
  expect(flags.has("dry-run")).toBe(true);
  expect(options).toEqual({ "claude-home": "/tmp/c", history: "/tmp/h" });
  expect(positional).toEqual(["/old", "/new"]);
});

test("値が欠落している (次のトークンが無い) 場合は例外を投げる", () => {
  expect(() => parseArgs(["--claude-home"], new Set(["claude-home"]))).toThrow(/missing value/);
});

test("値が欠落している (次のトークンが別オプションに見える) 場合も例外を投げる", () => {
  // "--dry-run" のような文字列をうっかり値として拾ってしまうのを防ぐ。
  expect(() =>
    parseArgs(["--claude-home", "--dry-run"], new Set(["claude-home"])),
  ).toThrow(/missing value/);
});

test("valueOptions に無い -- オプションは flags に入るだけでエラーにしない", () => {
  // 「未知のオプションを拒否するか」は呼び出し側 (CLI) の関心事なので、
  // parseArgs 自身はここでは判定しない。
  const { flags } = parseArgs(["--foo"], new Set());
  expect(flags.has("foo")).toBe(true);
});

test("--to のような単一の値オプションも正しく消費する (project-move 用)", () => {
  const { options, positional } = parseArgs(
    ["--to", "/dest", "--dry-run", "/src"],
    new Set(["to"]),
  );
  expect(options.to).toBe("/dest");
  expect(positional).toEqual(["/src"]);
});
