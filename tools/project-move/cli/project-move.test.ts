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
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const CLI = join(import.meta.dir, "project-move.ts");

let root: string;

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "project-move-cli-"));
});
afterEach(() => rmSync(root, { recursive: true, force: true }));

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
  // 入る前に検出されるので、どちらの移動元も一切手を付けられないはずである。
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
  // <home>/projects/local/<name> を返す (--to を使ったときの明示指定とは別経路)。
  expect(stdout).toContain(`local ${src} -> `);
  expect(stdout).toContain(`/projects/local/plain-dir`);
});
