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
  readdirSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { homedir, tmpdir } from "node:os";
import { dirname, join } from "node:path";
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

function runCli(args: string[], opts: { fakeGhq?: boolean } = {}) {
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
  const proc = Bun.spawnSync(["bun", CLI, ...args], { env, stdout: "pipe", stderr: "pipe" });
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
