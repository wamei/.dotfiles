import { expect, test } from "bun:test";
import { defaultMovesLogPath, formatMoveRow, parseMovesTsv } from "./moves.ts";

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
