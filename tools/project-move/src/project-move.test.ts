// tools/project-move/src/project-move.test.ts
import { afterEach, beforeEach, expect, test } from "bun:test";
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { applyFixups, isEmacsRunning } from "./project-move.ts";

let root: string;
let plan: { kind: "ghq"; from: string; to: string };

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "project-move-"));
  plan = { kind: "ghq", from: join(root, "old"), to: join(root, "new") };
  mkdirSync(join(plan.to, ".claude"), { recursive: true });
});
afterEach(() => rmSync(root, { recursive: true, force: true }));

test("settings.local.json の permission の旧パスを書き換える", async () => {
  const f = join(plan.to, ".claude", "settings.local.json");
  writeFileSync(f, JSON.stringify({ permissions: { allow: [`Bash(grep x ${plan.from}/a)`] } }));
  await applyFixups(plan, { emacsStateFiles: [], dryRun: false, emacsRunning: false });
  expect(readFileSync(f, "utf8")).toContain(`${plan.to}/a`);
});

test("Emacs 状態ファイルの旧パスを書き換える", async () => {
  const f = join(root, "recentf");
  writeFileSync(f, `("${plan.from}/a.el")\n`);
  await applyFixups(plan, { emacsStateFiles: [f], dryRun: false, emacsRunning: false });
  expect(readFileSync(f, "utf8")).toContain(`${plan.to}/a.el`);
});

test("Emacs 起動中は状態ファイルに触らず警告する", async () => {
  const f = join(root, "recentf");
  writeFileSync(f, `("${plan.from}/a.el")\n`);
  const r = await applyFixups(plan, { emacsStateFiles: [f], dryRun: false, emacsRunning: true });
  expect(readFileSync(f, "utf8")).toContain(`${plan.from}/a.el`);
  expect(r.warnings.join(" ")).toMatch(/emacs/i);
});

test("git hook に旧パスが残っていたら再生成手順を出す", async () => {
  mkdirSync(join(plan.to, ".git", "hooks"), { recursive: true });
  writeFileSync(
    join(plan.to, ".git", "hooks", "pre-push"),
    `#!/bin/sh\n${plan.from}/node_modules/lefthook-darwin-arm64/bin/lefthook run pre-push\n`,
  );
  const r = await applyFixups(plan, { emacsStateFiles: [], dryRun: false, emacsRunning: false });
  expect(r.manualSteps.join(" ")).toMatch(/lefthook/);
});

test(".envrc があれば direnv allow を促す", async () => {
  writeFileSync(join(plan.to, ".envrc"), "export A=1\n");
  const r = await applyFixups(plan, { emacsStateFiles: [], dryRun: false, emacsRunning: false });
  expect(r.manualSteps.join(" ")).toMatch(/direnv allow/);
});

test("dry-run は書き換えない", async () => {
  const f = join(root, "recentf");
  writeFileSync(f, `("${plan.from}/a.el")\n`);
  await applyFixups(plan, { emacsStateFiles: [f], dryRun: true, emacsRunning: false });
  expect(readFileSync(f, "utf8")).toContain(`${plan.from}/a.el`);
});

test("pgrep の結果で Emacs の起動を判定する", () => {
  expect(isEmacsRunning(() => 0)).toBe(true);
  expect(isEmacsRunning(() => 1)).toBe(false);
});

// --- 差し替え 1 (Ruling R10): dry-run はプロジェクト内のファイルを plan.from
// から読む ------------------------------------------------------------------
//
// 現実の dry-run では移動がまだ行われていないので plan.to は存在しない。
// ブリーフのコードは常に plan.to を見るため、dry-run では settings.local.json /
// .git/hooks / .envrc のいずれも見つからず「手当てなし」と黙って報告していた。
// ここでは plan.to を一切作らず (beforeEach が作った物も消す)、plan.from 側に
// だけファイルを置いて検出できることを確認する。

test("dry-run でも移動元 (plan.from) の settings.local.json を検出して報告する", async () => {
  rmSync(plan.to, { recursive: true, force: true });
  mkdirSync(join(plan.from, ".claude"), { recursive: true });
  const f = join(plan.from, ".claude", "settings.local.json");
  writeFileSync(f, JSON.stringify({ permissions: { allow: [`Bash(grep x ${plan.from}/a)`] } }));

  const r = await applyFixups(plan, { emacsStateFiles: [], dryRun: true, emacsRunning: false });

  const entry = r.rewrittenFiles.find((x) => x.path === f);
  expect(entry).toBeDefined();
  expect(entry?.changedLines).toBeGreaterThan(0);
  // dry-run なので実ファイルは書き換わっていない
  expect(readFileSync(f, "utf8")).toContain(plan.from);
});

test("dry-run でも移動元の git hook / .envrc を検出する", async () => {
  rmSync(plan.to, { recursive: true, force: true });
  mkdirSync(join(plan.from, ".git", "hooks"), { recursive: true });
  writeFileSync(
    join(plan.from, ".git", "hooks", "pre-push"),
    `#!/bin/sh\n${plan.from}/node_modules/lefthook-darwin-arm64/bin/lefthook run pre-push\n`,
  );
  writeFileSync(join(plan.from, ".envrc"), "export A=1\n");

  const r = await applyFixups(plan, { emacsStateFiles: [], dryRun: true, emacsRunning: false });

  expect(r.manualSteps.join(" ")).toMatch(/lefthook/);
  expect(r.manualSteps.join(" ")).toMatch(/direnv allow/);
});

test("実適用では移動先 (plan.to) を見る (plan.from はもう無い想定)", async () => {
  // 実適用時点では移動が完了しており plan.from は存在しないはずなので、
  // plan.from 自体を消してから、plan.to 側に置いたファイルだけで検出できる
  // ことを確認する (plan.to から探す従来どおりの経路が壊れていないことの確認)。
  rmSync(plan.from, { recursive: true, force: true });
  const f = join(plan.to, ".claude", "settings.local.json");
  writeFileSync(f, JSON.stringify({ permissions: { allow: [`Bash(grep x ${plan.from}/a)`] } }));

  const r = await applyFixups(plan, { emacsStateFiles: [], dryRun: false, emacsRunning: false });

  expect(r.rewrittenFiles.some((x) => x.path === f)).toBe(true);
  expect(readFileSync(f, "utf8")).toContain(`${plan.to}/a`);
});

// --- 差し替え 2: 変更行数を 1 回の split で数える ---------------------------
//
// パフォーマンスそのものは unit test で直接測らないが、「1 回だけ split して
// 比較しても、毎行 split し直すのと同じ結果になる」ことを行数の多いファイルで
// 確認しておく (リファクタで数え方を変えても正しさは変わらないことの担保)。

test("変更行数を複数行にわたって正しく数える", async () => {
  const f = join(root, "recentf");
  const lines = Array.from({ length: 50 }, (_, i) => `("${plan.from}/file-${i}.el")`);
  writeFileSync(f, `${lines.join("\n")}\n`);

  const r = await applyFixups(plan, { emacsStateFiles: [f], dryRun: false, emacsRunning: false });

  const entry = r.rewrittenFiles.find((x) => x.path === f);
  expect(entry?.changedLines).toBe(50);
});
