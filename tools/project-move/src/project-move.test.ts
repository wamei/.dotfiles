// tools/project-move/src/project-move.test.ts
import { afterEach, beforeEach, expect, test } from "bun:test";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { applyFixups, isEmacsRunning, preflightStatus } from "./project-move.ts";

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

// isEmacsRunning の exec seam は ps を 2 回呼ぶ (一覧 → 引数) ので、
// 引数で応答を出し分ける fake を組む。
function fakePs(list: string, args: string) {
  return (_cmd: string, a: string[]) =>
    a.some((x) => x.startsWith("-A")) ? { status: 0, stdout: list } : { status: 0, stdout: args };
}

test("ps の結果で Emacs の起動を判定する", () => {
  expect(
    isEmacsRunning(fakePs("62493 Emacs\n", "/Applications/Emacs.app/Contents/MacOS/Emacs\n")),
  ).toBe(true);
  expect(isEmacsRunning(fakePs("501 zsh\n", ""))).toBe(false);
});

test("emacsclient は Emacs 本体と区別する", () => {
  expect(isEmacsRunning(fakePs("62493 emacsclient\n", "emacsclient -n foo.el\n"))).toBe(false);
});

// --- I12: Emacs 検出が両方向に壊れていた -------------------------------------
//
// 実際に踏んだ形: Emacs を終了してから `--fixups-only` を実行したのに
// 「emacs is running」の警告が出続け、状態ファイルが手つかずのままだった。
//
// 原因は `pgrep -x Emacs` で、2 つの逆向きの誤りを同時に起こしていた:
//
//  1. 偽陽性: 2 日前の Claude セッションが tmux に置き去りにした
//     `emacs -Q -nw -l .../scratchpad/spin-probe.el` 2 プロセスを拾う。
//     自然に終了しないのでガードが永久に解けない。
//  2. 偽陰性: 肝心の GUI Emacs (Emacs.app, PPID 1) を pgrep がそもそも
//     拾わない。同じ UID・同じ ucomm=Emacs で ps からは見えるのに、
//     `pgrep -x` / `-f` / `-i` のどれでもヒットしない (sandbox 外でも再現)。
//
// つまり「守りたい相手を見逃し、無害な相手で止まる」状態だった。ps の一覧を
// 直接読み、ucomm が emacs のものだけを対象にし、init を読まない
// -Q / --batch を除く形に置き換える。

test("I12: pgrep が拾えない GUI Emacs も ps 経由で検出する", () => {
  // Emacs.app を Finder / launchd から起動した形。args にオプションが無い。
  const list = "62493 Emacs\n";
  const args = "/Applications/Emacs.app/Contents/MacOS/Emacs\n";
  expect(isEmacsRunning(fakePs(list, args))).toBe(true);
});

test("I12: -Q で起動した Emacs だけなら「起動中」と扱わない", () => {
  const list = "52111 Emacs\n61834 Emacs\n";
  const args =
    "/Applications/Emacs.app/Contents/MacOS/Emacs -Q -nw -l /tmp/scratch/spin-probe.el\n" +
    "/Applications/Emacs.app/Contents/MacOS/Emacs -Q -nw -l /tmp/scratch/tm-probe.el\n";
  expect(isEmacsRunning(fakePs(list, args))).toBe(false);
});

test("I12: --batch の Emacs も除外する", () => {
  expect(isEmacsRunning(fakePs("100 Emacs\n", "emacs --batch -l /tmp/x.el\n"))).toBe(false);
});

test("I12: -Q のプロセスに混じって通常の Emacs がいれば「起動中」", () => {
  const list = "52111 Emacs\n62493 Emacs\n";
  const args =
    "/Applications/Emacs.app/Contents/MacOS/Emacs -Q -nw -l /tmp/scratch/spin-probe.el\n" +
    "/Applications/Emacs.app/Contents/MacOS/Emacs\n";
  expect(isEmacsRunning(fakePs(list, args))).toBe(true);
});

test("I12: 一覧に Emacs がいるのに引数が読めなければ安全側に倒す", () => {
  // 判定を諦めるときは「触らない」(= 起動中) 側へ。状態ファイルを壊すより警告。
  expect(isEmacsRunning(fakePs("62493 Emacs\n", ""))).toBe(true);
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

// --- Task 9: symlink 経由の実行 ---------------------------------------------
//
// mise の [dotfiles] は cli/project-move.ts を ~/bin/project-move へ
// mode = "symlink" で配置する。cli/project-move.ts は "../src/..." という
// 相対 import を使っているので、~/bin 側の symlink 越しに起動したときに
// bun がその import をどう解決するかが未検証だった。目視では判断できない
// (bun がエントリポイントの realpath を基準に解決するのか、symlink 自体の
// パスを基準にするのかはドキュメントを読むだけでは確証が持てない) ので、
// 実際に symlink を張ってサブプロセス起動し、証拠として確認する。
// --- I9: --fixups-only 用の projectRoot 明示指定 ----------------------------

test("projectRoot: 'to' を指定すると dry-run でも移動先を見る (--fixups-only 用)", async () => {
  // --fixups-only は「移動は既に終わっている」ことが前提のモードなので、
  // dry-run と併用しても plan.from はもう存在しない。自動選択 (dry-run なら
  // plan.from) に任せると何も見つからず「手当てなし」と報告してしまう。
  rmSync(plan.from, { recursive: true, force: true });
  const f = join(plan.to, ".claude", "settings.local.json");
  writeFileSync(f, JSON.stringify({ permissions: { allow: [`Bash(grep x ${plan.from}/a)`] } }));

  const r = await applyFixups(plan, {
    emacsStateFiles: [],
    dryRun: true,
    emacsRunning: false,
    projectRoot: "to",
  });

  const entry = r.rewrittenFiles.find((x) => x.path === f);
  expect(entry).toBeDefined();
  // dry-run なので実ファイルは書き換わっていない
  expect(readFileSync(f, "utf8")).toContain(plan.from);
});

// --- Minor 1: settings.local.json は JSON として妥当かを確認してから書く ----

test("settings.local.json の書き換え結果が不正な JSON になる場合は書き込まず警告する", async () => {
  // 移動先パスにダブルクオートを含む極端なケースを使い、テキスト置換の結果が
  // JSON として壊れることを作為的に再現する (通常のパスではまず起きないが、
  // ガード自体はこのケースでしか確認できない)。ファイルシステム上は APFS/HFS
  // どちらも `"` を含むディレクトリ名を許容するので、実ディレクトリとして作れる。
  const weirdTo = join(root, 'new"quote');
  mkdirSync(join(weirdTo, ".claude"), { recursive: true });
  const weirdPlan = { kind: "ghq" as const, from: plan.from, to: weirdTo };
  const f = join(weirdTo, ".claude", "settings.local.json");
  const before = JSON.stringify({ permissions: { allow: [`Bash(grep x ${plan.from}/a)`] } });
  writeFileSync(f, before);

  const r = await applyFixups(weirdPlan, { emacsStateFiles: [], dryRun: false, emacsRunning: false });

  // 壊れた JSON は書き込まれず、原本のまま残る
  expect(readFileSync(f, "utf8")).toBe(before);
  expect(r.warnings.join(" ")).toMatch(/settings\.local\.json/);
  expect(r.rewrittenFiles.some((x) => x.path === f)).toBe(false);
});

// --- I8: 移動前に uncommitted / unpushed / stash を見せる -------------------

test("preflightStatus: uncommitted / unpushed / stash をそれぞれ拾う", () => {
  const dir = join(root, "repo");
  mkdirSync(dir, { recursive: true });
  spawnSync("git", ["init", "-q"], { cwd: dir });
  spawnSync("git", ["config", "user.email", "t@example.com"], { cwd: dir });
  spawnSync("git", ["config", "user.name", "t"], { cwd: dir });

  writeFileSync(join(dir, "a.txt"), "1\n");
  spawnSync("git", ["add", "a.txt"], { cwd: dir });
  spawnSync("git", ["commit", "-q", "-m", "init"], { cwd: dir });

  // remote が無いので、これだけでも "unpushed" (branches - remotes) に載る
  writeFileSync(join(dir, "b.txt"), "2\n");
  spawnSync("git", ["add", "b.txt"], { cwd: dir });
  spawnSync("git", ["commit", "-q", "-m", "second"], { cwd: dir });

  writeFileSync(join(dir, "a.txt"), "stashed change\n");
  spawnSync("git", ["stash", "push", "-q", "-m", "wip"], { cwd: dir });

  writeFileSync(join(dir, "c.txt"), "3\n");

  const status = preflightStatus(dir);
  expect(status.uncommitted).toContain("c.txt");
  expect(status.unpushed.split("\n").filter((l) => l.trim() !== "").length).toBe(2);
  expect(status.stash).toContain("wip");
});

test("preflightStatus: 何も無ければ全て空文字", () => {
  const dir = join(root, "clean-repo");
  mkdirSync(dir, { recursive: true });
  spawnSync("git", ["init", "-q"], { cwd: dir });
  const status = preflightStatus(dir);
  expect(status).toEqual({ uncommitted: "", unpushed: "", stash: "" });
});

test("symlink 経由でも相対 import が解決する", () => {
  const link = join(root, "project-move-link");
  symlinkSync(new URL("../cli/project-move.ts", import.meta.url).pathname, link);

  // project-move.ts はトップレベルで homedir() を読み、EMACS_STATE_FILES や
  // moves ログのパスをそこから組み立てる。--dry-run 単体 (位置引数なし) では
  // for ループが空になり実際にはどこにも書き込まないはずだが、実装が変わって
  // 書き込み経路が増えても実ホームを汚さないよう、他の CLI テスト
  // (cli/project-move.test.ts の runCli) と同じ流儀で HOME を tmpdir に
  // 差し替えておく。
  const fakeHome = join(root, "fake-home");
  mkdirSync(fakeHome, { recursive: true });
  const r = spawnSync(link, ["--dry-run"], {
    encoding: "utf8",
    env: { ...process.env, HOME: fakeHome },
  });

  expect(r.status).toBe(0);
  expect(r.stderr).not.toMatch(/Cannot find module/);
});

// --- I11: git hook の todo が偽陽性 / 誤ったパッケージマネージャを案内する ------
//
// 実際に踏んだ形: 移行後の SDXFW_TEMPLATE で `--fixups-only` が
// `.git/hooks` 19 本すべてに「旧パスを指しているので `npm install` しろ」と
// todo を出した。しかし husky v4 が旧パスを書いているのは
// `#   From: <old>/node_modules/husky` という**コメント行だけ**で、実行される
// のは husky.local.sh の `cd "."` (相対) なので hook は移動後もそのまま動く。
// 案内どおり `npm install` を実行すると、この repo は bun 管理 (bun.lock /
// husky.local.sh に packageManager=bun) なのに npm の peer 解決が走り
// ERESOLVE で失敗した。何もしなくてよかった上に、誤ったツールを案内していた。

test("I11: 旧パスがコメント行にしか無い git hook は todo にしない", async () => {
  mkdirSync(join(plan.to, ".git", "hooks"), { recursive: true });
  writeFileSync(
    join(plan.to, ".git", "hooks", "pre-commit"),
    // husky v4 が実際に生成する形。旧パスは "From:" のコメントだけ。
    `#!/bin/sh\n# husky\n\n# Created by Husky v4.3.8\n#   At: 6/9/2026, 10:57:06 AM\n#   From: ${plan.from}/node_modules/husky (undefined)\n\n. "$(dirname "$0")/husky.sh"\n`,
  );

  const r = await applyFixups(plan, { emacsStateFiles: [], dryRun: false, emacsRunning: false });

  expect(r.manualSteps.join(" ")).not.toContain("pre-commit");
});

test("I11: コメントと実行行の両方に旧パスがあれば todo に出す", async () => {
  mkdirSync(join(plan.to, ".git", "hooks"), { recursive: true });
  writeFileSync(
    join(plan.to, ".git", "hooks", "pre-push"),
    `#!/bin/sh\n#   From: ${plan.from}/node_modules/husky\nexec ${plan.from}/node_modules/.bin/lefthook run pre-push\n`,
  );

  const r = await applyFixups(plan, { emacsStateFiles: [], dryRun: false, emacsRunning: false });

  expect(r.manualSteps.join(" ")).toContain("pre-push");
});

test("I11: lockfile が bun.lock なら bun install を案内する", async () => {
  mkdirSync(join(plan.to, ".git", "hooks"), { recursive: true });
  writeFileSync(join(plan.to, "bun.lock"), "");
  writeFileSync(
    join(plan.to, ".git", "hooks", "pre-push"),
    `#!/bin/sh\nexec ${plan.from}/node_modules/.bin/husky-run pre-push\n`,
  );

  const r = await applyFixups(plan, { emacsStateFiles: [], dryRun: false, emacsRunning: false });

  expect(r.manualSteps.join(" ")).toContain("bun install");
  expect(r.manualSteps.join(" ")).not.toContain("npm install");
});

test("I11: lockfile が pnpm/yarn ならそれぞれの install を案内する", async () => {
  mkdirSync(join(plan.to, ".git", "hooks"), { recursive: true });
  writeFileSync(join(plan.to, "pnpm-lock.yaml"), "");
  writeFileSync(
    join(plan.to, ".git", "hooks", "pre-push"),
    `#!/bin/sh\nexec ${plan.from}/node_modules/.bin/husky-run pre-push\n`,
  );

  const r = await applyFixups(plan, { emacsStateFiles: [], dryRun: false, emacsRunning: false });

  expect(r.manualSteps.join(" ")).toContain("pnpm install");
});

test("I11: lockfile が無ければ従来どおり npm install を案内する", async () => {
  mkdirSync(join(plan.to, ".git", "hooks"), { recursive: true });
  writeFileSync(
    join(plan.to, ".git", "hooks", "pre-push"),
    `#!/bin/sh\nexec ${plan.from}/node_modules/.bin/husky-run pre-push\n`,
  );

  const r = await applyFixups(plan, { emacsStateFiles: [], dryRun: false, emacsRunning: false });

  expect(r.manualSteps.join(" ")).toContain("npm install");
});
