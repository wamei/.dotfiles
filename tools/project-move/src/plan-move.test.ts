import { expect, test } from "bun:test";
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
