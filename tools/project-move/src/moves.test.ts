import { expect, test } from "bun:test";
import { defaultMovesLogPath, detectMoveCollisions, formatMoveRow, parseMovesTsv } from "./moves.ts";

test("3 列 TSV を読む", () => {
  const text = [
    "# timestamp\tfrom\tto",
    "2026-09-10T00:00:00.000Z\t/Users/w/projects/a\t/Users/w/projects/github.com/o/a",
    "",
  ].join("\n");
  expect(parseMovesTsv(text)).toEqual([
    { from: "/Users/w/projects/a", to: "/Users/w/projects/github.com/o/a" },
  ]);
});

test("空行とコメント行を読み飛ばす", () => {
  expect(parseMovesTsv("\n# c\n\n")).toEqual([]);
});

test("列が足りない行は行番号付きで落とす", () => {
  expect(() => parseMovesTsv("2026-09-10T00:00:00.000Z\t/only-one")).toThrow(/line 1/);
});

test("列が多い行は行番号付きで落とす", () => {
  expect(() => parseMovesTsv("2026-09-10T00:00:00.000Z\t/a\t/b\textra")).toThrow(/line 1/);
});

test("行の書式は timestamp/from/to のタブ区切り", () => {
  const at = new Date("2026-09-10T12:34:56.000Z");
  expect(formatMoveRow({ from: "/a", to: "/b" }, at)).toBe(
    "2026-09-10T12:34:56.000Z\t/a\t/b\n",
  );
});

test("ログの既定位置は XDG state 配下", () => {
  expect(defaultMovesLogPath("/Users/w")).toBe(
    "/Users/w/.local/state/project-move/moves.tsv",
  );
});

test("detectMoveCollisions: 衝突が無ければ空配列", () => {
  expect(
    detectMoveCollisions([
      { from: "/Users/w/projects/a", to: "/Users/w/projects/github.com/o/a" },
      { from: "/Users/w/projects/b", to: "/Users/w/projects/github.com/o/b" },
    ]),
  ).toEqual([]);
});

test("detectMoveCollisions: 移動先の重複を検出する", () => {
  const warnings = detectMoveCollisions([
    { from: "/Users/w/projects/a", to: "/Users/w/projects/dest" },
    { from: "/Users/w/projects/b", to: "/Users/w/projects/dest" },
  ]);
  expect(warnings.length).toBe(1);
  expect(warnings[0]).toMatch(/dest/);
});

test("detectMoveCollisions: 連鎖書き換え (to が別の from と一致) を検出する", () => {
  const warnings = detectMoveCollisions([
    { from: "/Users/w/projects/a", to: "/Users/w/projects/b" },
    { from: "/Users/w/projects/b", to: "/Users/w/projects/c" },
  ]);
  expect(warnings.length).toBe(1);
  expect(warnings[0]).toMatch(/\/Users\/w\/projects\/b/);
});

test("detectMoveCollisions: 連鎖書き換え (to が別の from の配下) を検出する", () => {
  const warnings = detectMoveCollisions([
    { from: "/Users/w/projects/a", to: "/Users/w/projects/b" },
    { from: "/Users/w/projects/b/child", to: "/Users/w/projects/c" },
  ]);
  expect(warnings.length).toBe(1);
});

test("detectMoveCollisions: スラッグの衝突 (from 同士) を検出する", () => {
  // to 側はスラッグが衝突しないよう別名にして、from 側の衝突だけを見る
  const warnings = detectMoveCollisions([
    { from: "/Users/w/projects/mc-data-catalog", to: "/Users/w/projects/github.com/o/data-catalog" },
    { from: "/Users/w/projects/mc_data_catalog", to: "/Users/w/projects/github.com/o/other-catalog" },
  ]);
  expect(warnings.length).toBe(1);
  expect(warnings[0]).toMatch(/mc-data-catalog/);
});

test("detectMoveCollisions: スラッグの衝突 (to 同士) を検出する", () => {
  const warnings = detectMoveCollisions([
    { from: "/Users/w/projects/a", to: "/Users/w/projects/github.com/o/mc-data-catalog" },
    { from: "/Users/w/projects/b", to: "/Users/w/projects/github.com/o/mc_data_catalog" },
  ]);
  expect(warnings.length).toBe(1);
});
