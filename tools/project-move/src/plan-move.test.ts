import { expect, test } from "bun:test";
import { join } from "node:path";
import { planMove } from "./plan-move.ts";

const localRoot = "/Users/w/projects/local";

test("remote があれば ghq が決めた移動先を使う", () => {
  const probe = () => ({ ok: true as const, to: "/Users/w/projects/github.com/o/a" });
  expect(planMove("/Users/w/projects/a", { probe, localRoot })).toEqual({
    kind: "ghq",
    from: "/Users/w/projects/a",
    to: "/Users/w/projects/github.com/o/a",
  });
});

test("remote が無ければ local へ", () => {
  const probe = () => ({ ok: false as const, reason: "no remotes found" });
  expect(planMove("/Users/w/projects/clock", { probe, localRoot })).toEqual({
    kind: "local",
    from: "/Users/w/projects/clock",
    to: "/Users/w/projects/local/clock",
  });
});

test("非 git でも local へ", () => {
  const probe = () => ({ ok: false as const, reason: "failed to detect VCS backend" });
  expect(planMove("/Users/w/projects/pytest", { probe, localRoot })).toEqual({
    kind: "local",
    from: "/Users/w/projects/pytest",
    to: "/Users/w/projects/local/pytest",
  });
});

test("末尾スラッシュを落としてから名前を取る", () => {
  const probe = () => ({ ok: false as const, reason: "no remotes found" });
  expect(planMove("/Users/w/projects/clock/", { probe, localRoot }).to).toBe(
    "/Users/w/projects/local/clock",
  );
});

// --- C1: 相対パスを絶対パスに正規化する ------------------------------------

test("相対パスも絶対パスに解決してから from/probe に渡す (回帰テスト、C1)", () => {
  const seen: string[] = [];
  const probe = (dir: string) => {
    seen.push(dir);
    return { ok: false as const, reason: "no remotes found" };
  };
  const plan = planMove("relative-project", { probe, localRoot });
  const expectedFrom = join(process.cwd(), "relative-project");
  expect(plan.from).toBe(expectedFrom);
  expect(plan.to).toBe(join(localRoot, "relative-project"));
  // probe にも正規化後の絶対パスが渡ること (裸の名前のまま ghq へ渡すと
  // 誤判定・誤置換の原因になる)。
  expect(seen).toEqual([expectedFrom]);
});

test("相対パスの ../ や ./ も正規化する", () => {
  const probe = () => ({ ok: false as const, reason: "failed to detect VCS backend" });
  const plan = planMove("./a/../b", { probe, localRoot });
  expect(plan.from).toBe(join(process.cwd(), "b"));
});

// --- I7: ghq が判定不能な理由では local へ落とさず止める --------------------

test("remote 無し・非 git 以外の理由では例外を投げる (I7)", () => {
  const probe = () => ({ ok: false as const, reason: "ghq: command not found" });
  expect(() => planMove("/Users/w/projects/x", { probe, localRoot })).toThrow(
    /command not found/,
  );
});

test("ghq 未インストールなど判定不能なケースでは local に落とさない (I7)", () => {
  const probe = () => ({ ok: false as const, reason: "spawnSync ghq ENOENT" });
  expect(() => planMove("/Users/w/projects/x", { probe, localRoot })).toThrow();
});

test("'no remotes found' を含む長い理由文でも local 行きの許可リストにマッチする", () => {
  // 実際の ghq migrate の出力は "failed to get remote URL: no remotes found" の
  // ように前置きが付く。部分一致で判定できることを確認する。
  const probe = () => ({ ok: false as const, reason: "failed to get remote URL: no remotes found" });
  expect(planMove("/Users/w/projects/clock", { probe, localRoot }).kind).toBe("local");
});
