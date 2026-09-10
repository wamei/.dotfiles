// tools/project-move/cli/project-move.test.ts
//
// project-move CLI の --to オプションの配線を検証する統合テスト。
// applyFixups / isEmacsRunning のロジックは src/project-move.test.ts で
// カバー済みなので、ここでは CLI 固有の関心事 (--to が実際に移動先を
// 差し替えること、複数ディレクトリとの併用を拒むこと) だけを、CLI を
// サブプロセスとして実際に起動して確認する。
//
// 本物の ~/projects には一切触れない。すべて tmpdir 配下で完結させる。
import { afterEach, beforeEach, expect, test } from "bun:test";
import {
  chmodSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  readdirSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { spawnSync } from "node:child_process";
import { homedir, tmpdir } from "node:os";
import { dirname, join, relative } from "node:path";
import { defaultMovesLogPath } from "../src/moves.ts";

const CLI = join(import.meta.dir, "project-move.ts");

// --- 実ホームの汚染検知 -----------------------------------------------------
//
// 事故: このファイルの初版は CLI をサブプロセス起動する際に HOME を
// 差し替えていなかった。project-move.ts の local ルートと moves ログは
// `homedir()` から組み立てられ、Node の `os.homedir()` は POSIX では $HOME を
// そのまま読むため、`--to` の複数ディレクトリ併用エラーを検証するテスト
// (--dry-run を付けずに叩く必要があった) が、検証に失敗する前のコードに対して
// 実際に本物の `~/projects/local/{a,b}` を作り、
// `~/.local/state/project-move/moves.tsv` に実エントリを書き込んでしまった
// (レビューで発見、後始末済み)。
//
// runCli は毎回 env.HOME を tmpdir 配下に差し替えて再発を防ぐが、それでも
// 同じ穴が再発したときに目視に頼らず気付けるよう、実ホーム側のこの 2 箇所を
// テスト前後で比較する。
//
// レビュー指摘: 当初は existsSync の真偽値だけを比較していたが、これは
// 「今その場所に何も無い」環境でしか機能しない。この移行が進み Task 11 で
// ~/projects/local に正当なプロジェクトが 16 個並ぶようになると、
// 「存在する」→「存在する」で一致してしまい、その中に汚染ファイルが 1 つ
// 増えても検知できなくなる。存在の有無ではなく、**子エントリ名の一覧**を
// 比較することで、中身が増えた/減ったこと自体を検知できるようにする。
const REAL_HOME = homedir();
const REAL_PROJECTS_LOCAL = join(REAL_HOME, "projects", "local");
const REAL_MOVES_LOG_DIR = dirname(defaultMovesLogPath(REAL_HOME));

/** ディレクトリの「存在有無」と「直下の子エントリ名一覧 (ソート済み)」のスナップショット。 */
type DirSnapshot = { exists: boolean; entries: string[] };

function snapshotDir(dir: string): DirSnapshot {
  if (!existsSync(dir)) return { exists: false, entries: [] };
  return { exists: true, entries: readdirSync(dir).sort() };
}

/**
 * before と現在のスナップショットを比較し、増えた/減ったエントリ名を含む形で
 * 差分をアサートする。`expect(...).toEqual(...)` に増減の一覧を直接載せるので、
 * 失敗時のメッセージにそのままエントリ名が出る (「次に踏んだ人がすぐ原因に
 * 辿り着けるように」という要求への対応)。
 */
function assertDirUnchanged(dir: string, before: DirSnapshot, label: string) {
  const after = snapshotDir(dir);
  const beforeSet = new Set(before.entries);
  const afterSet = new Set(after.entries);
  const added = after.entries.filter((e) => !beforeSet.has(e));
  const removed = before.entries.filter((e) => !afterSet.has(e));
  expect({ label, exists: after.exists, added, removed }).toEqual({
    label,
    exists: before.exists,
    added: [],
    removed: [],
  });
}

const realProjectsLocalBefore = snapshotDir(REAL_PROJECTS_LOCAL);
const realMovesLogDirBefore = snapshotDir(REAL_MOVES_LOG_DIR);

function assertRealHomeUntouched() {
  assertDirUnchanged(REAL_PROJECTS_LOCAL, realProjectsLocalBefore, "~/projects/local");
  assertDirUnchanged(REAL_MOVES_LOG_DIR, realMovesLogDirBefore, "~/.local/state/project-move");
}

let root: string;
let fakeHome: string;

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "project-move-cli-"));
  fakeHome = join(root, "fake-home");
  mkdirSync(fakeHome, { recursive: true });
});
afterEach(() => {
  // 実ホームを一切変えていないことを、後片付けの前に毎回確認する。
  assertRealHomeUntouched();
  rmSync(root, { recursive: true, force: true });
});

/**
 * 「remote 無し / 非 git」を返す偽の `ghq` を PATH に差し込む。
 *
 * --to を使わない従来経路のテストが、実マシンに ghq がインストールされて
 * いるかに依存しないようにするため。ghqProbe (src/plan-move.ts) は
 * `ghq migrate --dry-run <dir>` の標準エラー出力を読むだけなので、その形式
 * だけを模す。
 */
function fakeGhqPath(): string {
  const binDir = mkdtempSync(join(tmpdir(), "project-move-cli-ghq-"));
  const script = join(binDir, "ghq");
  writeFileSync(
    script,
    `#!/bin/sh\necho "failed to detect VCS backend in \\"$3\\"" >&2\nexit 1\n`,
  );
  chmodSync(script, 0o755);
  return binDir;
}

/** 指定した理由で失敗する偽の `ghq` を PATH に差し込む (I7 用)。 */
function fakeGhqFailingWith(reason: string): string {
  const binDir = mkdtempSync(join(tmpdir(), "project-move-cli-ghq-fail-"));
  const script = join(binDir, "ghq");
  writeFileSync(script, `#!/bin/sh\necho "${reason}" >&2\nexit 1\n`);
  chmodSync(script, 0o755);
  return binDir;
}

function runCli(args: string[], opts: { fakeGhq?: boolean; ghqPath?: string; cwd?: string } = {}) {
  const env = { ...process.env };
  // HOME を tmpdir 配下に差し替える。project-move.ts は local ルート
  // (`homedir()/projects/local`) と moves ログ (`defaultMovesLogPath(homedir())`)
  // をどちらも `homedir()` から組み立てており、これを差し替えないと
  // (--to を使わない経路や、バリデーションに失敗する前のコードのように
  // 実際に mkdir/rename まで進んでしまうケースで) 本物のホームに書き込む。
  env.HOME = fakeHome;
  if (opts.fakeGhq) {
    env.PATH = `${fakeGhqPath()}:${process.env.PATH ?? ""}`;
  }
  if (opts.ghqPath) {
    env.PATH = `${opts.ghqPath}:${process.env.PATH ?? ""}`;
  }
  const proc = Bun.spawnSync(["bun", CLI, ...args], {
    env,
    cwd: opts.cwd,
    stdout: "pipe",
    stderr: "pipe",
  });
  return {
    exitCode: proc.exitCode,
    stdout: proc.stdout.toString(),
    stderr: proc.stderr.toString(),
  };
}

test("--to を指定すると planMove/ghqProbe の結果ではなくその path が移動先になる", () => {
  const src = join(root, "src-project");
  mkdirSync(src, { recursive: true });
  const to = join(root, "explicit-dest");

  const { exitCode, stdout } = runCli(["--dry-run", "--to", to, src]);

  expect(exitCode).toBe(0);
  expect(stdout).toContain(`local ${src} -> ${to}`);
  // dry-run なので実際には何も動いていない
  expect(existsSync(to)).toBe(false);
  expect(existsSync(src)).toBe(true);
});

test("--to と 2 つ以上の位置引数を併用するとエラーで終了し、何も移動しない", () => {
  const srcA = join(root, "a");
  const srcB = join(root, "b");
  mkdirSync(srcA, { recursive: true });
  mkdirSync(srcB, { recursive: true });
  const to = join(root, "dest");

  // --dry-run すら付けずに叩いても、複数ディレクトリとの併用エラーはループに
  // 入る前に検出されるので、どちらの移動元も一切手を付けられないはずである
  // (このテストは --dry-run を付けないことで、まさにそのバリデーションが
  // 「ループに入る前」であることを検証している。HOME を差し替えていないと、
  // バリデーションが効いていないコードに対しては実ホームへ実際に move して
  // しまう、というのが今回発見された事故そのものである)。
  const { exitCode, stderr } = runCli(["--to", to, srcA, srcB]);

  expect(exitCode).not.toBe(0);
  expect(stderr).toContain("--to");
  expect(existsSync(srcA)).toBe(true);
  expect(existsSync(srcB)).toBe(true);
  expect(existsSync(to)).toBe(false);
});

test("--to を指定しない従来の経路は壊れていない (回帰)", () => {
  const src = join(root, "plain-dir");
  mkdirSync(src, { recursive: true });

  const { exitCode, stdout } = runCli(["--dry-run", src], { fakeGhq: true });

  expect(exitCode).toBe(0);
  // ghq が remote 無しと判定した場合、planMove は local 扱いで
  // <fakeHome>/projects/local/<name> を返す (--to を使ったときの明示指定とは
  // 別経路)。fakeHome を使っているので実ホームの projects/local は参照しない。
  expect(stdout).toContain(`local ${src} -> `);
  expect(stdout).toContain(`${fakeHome}/projects/local/plain-dir`);
});

// --- C1: 相対パスを絶対パスに正規化する ------------------------------------

test("C1: 相対パスの位置引数と --to を絶対パスに正規化してから使う (回帰テスト)", () => {
  // 実際に移動まで行い、「絶対パスで呼んだのと同じ移動先になる」ことを
  // ファイルシステム上の結果で確認する (root 自体が /var -> /private/var の
  // シンボリックリンクを経由することがあり、サブプロセス内の process.cwd() は
  // 正規化された /private 側を返すため、stdout の文字列を素の root 文字列と
  // 比較すると環境依存で揺れる。移動が実際に正しい場所へ届いたかどうかで見る)。
  const src = join(root, "rel-src");
  mkdirSync(src, { recursive: true });
  const toAbs = join(root, "rel-dest");

  const relSrc = relative(root, src);
  const relTo = relative(root, toAbs);

  const { exitCode, stdout } = runCli(["--to", relTo, relSrc], { cwd: root });

  expect(exitCode).toBe(0);
  expect(stdout).toContain("local ");
  // 相対パスで呼んでも、絶対パスで呼んだのと同じ移動先に実際に移動している
  expect(existsSync(toAbs)).toBe(true);
  expect(existsSync(src)).toBe(false);
});

test("C1: 移動元が存在しない絶対パスに解決される場合はエラーで終了し、何も動かさない", () => {
  const missing = join(root, "does-not-exist");
  const { exitCode, stderr } = runCli(["--dry-run", missing]);
  expect(exitCode).not.toBe(0);
  expect(stderr).toContain(missing);
});

// --- I6/I10: 1 件の失敗が残りのバッチを道連れにしない -----------------------

test("I6: 1 件の移動が失敗しても他のディレクトリは処理を続け、失敗した分だけログに残らない", () => {
  const srcA = join(root, "dirA");
  const srcB = join(root, "dirB");
  mkdirSync(srcA, { recursive: true });
  mkdirSync(srcB, { recursive: true });
  // dirA の行き先を先回りして塞いでおき、"already exists" で失敗させる
  const localRoot = join(fakeHome, "projects", "local");
  mkdirSync(join(localRoot, "dirA"), { recursive: true });

  const { exitCode, stderr } = runCli([srcA, srcB], { fakeGhq: true });

  expect(exitCode).toBe(0);
  expect(stderr).toContain("failed to move");
  // 失敗した dirA は移動されていない
  expect(existsSync(srcA)).toBe(true);
  // 成功した dirB はちゃんと移動されている
  expect(existsSync(srcB)).toBe(false);
  expect(existsSync(join(localRoot, "dirB"))).toBe(true);

  // ログには成功した dirB だけが載る (失敗した dirA は載らない)
  const log = readFileSync(defaultMovesLogPath(fakeHome), "utf8");
  expect(log).toContain("dirB");
  expect(log).not.toContain("/dirA\t");
});

// --- I7: ghq が判定不能な理由を返したら local に落とさず止める --------------

test("I7: ghq が既知の理由 (remote 無し/非 git) 以外で失敗したら、local に落とさず理由を出して止まる", () => {
  const src = join(root, "weird-project");
  mkdirSync(src, { recursive: true });
  const ghqPath = fakeGhqFailingWith("ghq: unexpected internal error");

  const { exitCode, stderr } = runCli(["--dry-run", src], { ghqPath });

  expect(exitCode).not.toBe(0);
  expect(stderr).toContain("unexpected internal error");
  // local には落ちていない (何も出力されていないはず、あるいは少なくとも
  // "local " という行が出ていないこと)
  expect(existsSync(join(fakeHome, "projects", "local", "weird-project"))).toBe(false);
});

// --- I8: 移動前に uncommitted / unpushed / stash を見せる -------------------

test("I8: 移動前に unpushed と stash も表示する", () => {
  const src = join(root, "repo-with-history");
  mkdirSync(src, { recursive: true });
  spawnSync("git", ["init", "-q"], { cwd: src });
  spawnSync("git", ["config", "user.email", "t@example.com"], { cwd: src });
  spawnSync("git", ["config", "user.name", "t"], { cwd: src });
  writeFileSync(join(src, "a.txt"), "1\n");
  spawnSync("git", ["add", "a.txt"], { cwd: src });
  spawnSync("git", ["commit", "-q", "-m", "init"], { cwd: src });
  writeFileSync(join(src, "a.txt"), "changed\n");
  spawnSync("git", ["stash", "push", "-q", "-m", "wip"], { cwd: src });

  const { exitCode, stdout } = runCli(["--dry-run", src], { fakeGhq: true });

  expect(exitCode).toBe(0);
  expect(stdout).toContain("unpushed:");
  expect(stdout).toContain("stash:");
  expect(stdout).toContain("wip");
});

// --- I9: --fixups-only -------------------------------------------------------

test("I9: --fixups-only <old> <new> は移動せず手当てだけを走らせる", () => {
  const oldPath = join(root, "already-moved-old");
  const newPath = join(root, "already-moved-new");
  mkdirSync(join(newPath, ".claude"), { recursive: true });
  writeFileSync(
    join(newPath, ".claude", "settings.local.json"),
    JSON.stringify({ permissions: { allow: [`Bash(grep x ${oldPath}/a)`] } }),
  );

  const { exitCode, stdout } = runCli(["--fixups-only", oldPath, newPath]);

  expect(exitCode).toBe(0);
  expect(stdout).toContain(`fixups ${oldPath} -> ${newPath}`);
  const settings = readFileSync(join(newPath, ".claude", "settings.local.json"), "utf8");
  expect(settings).toContain(`${newPath}/a`);
  expect(settings).not.toContain(oldPath);
});

test("I9: --fixups-only は位置引数を省略すると移動ログの全行を対象にする", () => {
  const oldPath = join(root, "log-old");
  const newPath = join(root, "log-new");
  mkdirSync(join(newPath, ".claude"), { recursive: true });
  writeFileSync(
    join(newPath, ".claude", "settings.local.json"),
    JSON.stringify({ permissions: { allow: [`Bash(grep x ${oldPath}/a)`] } }),
  );
  const log = defaultMovesLogPath(fakeHome);
  mkdirSync(dirname(log), { recursive: true });
  writeFileSync(log, `2026-09-11T00:00:00.000Z\t${oldPath}\t${newPath}\n`);

  const { exitCode, stdout } = runCli(["--fixups-only"]);

  expect(exitCode).toBe(0);
  expect(stdout).toContain(`fixups ${oldPath} -> ${newPath}`);
  const settings = readFileSync(join(newPath, ".claude", "settings.local.json"), "utf8");
  expect(settings).toContain(`${newPath}/a`);
});

test("I9: --fixups-only と --to は併用できない", () => {
  const { exitCode, stderr } = runCli(["--fixups-only", "--to", "/x", "/y"]);
  expect(exitCode).not.toBe(0);
  expect(stderr).toContain("--fixups-only");
});

test("I9: --fixups-only に位置引数を 1 つだけ渡すとエラーで終了する", () => {
  const { exitCode, stderr } = runCli(["--fixups-only", "/only-one"]);
  expect(exitCode).not.toBe(0);
  expect(stderr).toContain("--fixups-only");
});
