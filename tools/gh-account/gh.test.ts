// tools/gh-account/gh.test.ts
//
// gh ラッパーをサブプロセスとして起動する統合テスト。本物の gh と keyring には
// 触れない。PATH には「ラッパーのあるディレクトリ」と「偽の gh を置いた
// ディレクトリ」だけを並べ、偽の gh は受け取った GH_TOKEN と引数を出力する。
// `gh auth token --user <u>` にだけは `tok-<u>` を返す。
import { afterEach, beforeEach, expect, test } from "bun:test";
import {
  chmodSync,
  mkdirSync,
  mkdtempSync,
  realpathSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const WRAPPER = join(import.meta.dir, "gh");

let root: string;
let home: string;
let wrapDir: string;
let fakeDir: string;
let mapFile: string;

beforeEach(() => {
  root = realpathSync(mkdtempSync(join(tmpdir(), "gh-account-")));
  home = join(root, "home");
  wrapDir = join(root, "wrap");
  fakeDir = join(root, "fake");
  mapFile = join(home, ".config/gh/users-by-dir");
  for (const d of [home, wrapDir, fakeDir, join(home, ".config/gh")]) {
    mkdirSync(d, { recursive: true });
  }
  // dotfiles と同じく、PATH に載るのは symlink のあるディレクトリ。
  symlinkSync(WRAPPER, join(wrapDir, "gh"));
  writeFileSync(
    join(fakeDir, "gh"),
    `#!/bin/sh
if [ "$1 $2 $3" = "auth token --user" ]; then
  echo "tok-$4"
  exit 0
fi
echo "token=\${GH_TOKEN-<unset>} args=$*"
`,
  );
  chmodSync(join(fakeDir, "gh"), 0o755);
});

afterEach(() => {
  rmSync(root, { recursive: true, force: true });
});

function writeMap(content: string) {
  writeFileSync(mapFile, content);
}

function run(cwd: string, args: string[], env: Record<string, string> = {}) {
  mkdirSync(cwd, { recursive: true });
  const proc = Bun.spawnSync([join(wrapDir, "gh"), ...args], {
    cwd,
    env: { HOME: home, PATH: `${wrapDir}:${fakeDir}:/usr/bin:/bin`, ...env },
  });
  return {
    code: proc.exitCode,
    out: proc.stdout.toString().trim(),
    err: proc.stderr.toString().trim(),
  };
}

test("対応するディレクトリでは、そのアカウントのトークンを渡す", () => {
  writeMap(`${home}/projects/work work-user\n`);
  const r = run(join(home, "projects/work/repo"), ["pr", "list"]);
  expect(r.out).toBe("token=tok-work-user args=pr list");
  expect(r.code).toBe(0);
});

test("最も深く一致したディレクトリが勝つ (記述順に依らない)", () => {
  writeMap(`${home}/projects me\n${home}/projects/work work-user\n`);
  expect(run(join(home, "projects/work/repo"), ["api"]).out).toBe(
    "token=tok-work-user args=api",
  );
  expect(run(join(home, "projects/other"), ["api"]).out).toBe(
    "token=tok-me args=api",
  );
});

test("~ と行末の / とコメント行と空行を扱える", () => {
  writeMap("# comment\n\n~/projects/work/  work-user\n");
  expect(run(join(home, "projects/work"), ["api"]).out).toBe(
    "token=tok-work-user args=api",
  );
});

test("名前が前方一致するだけの兄弟ディレクトリには効かない", () => {
  writeMap(`${home}/projects/work work-user\n`);
  expect(run(join(home, "projects/work2"), ["api"]).out).toBe(
    "token=<unset> args=api",
  );
});

test("一致しなければトークンを渡さない", () => {
  writeMap(`${home}/projects/work work-user\n`);
  expect(run(join(home, "elsewhere"), ["api"]).out).toBe(
    "token=<unset> args=api",
  );
});

test("対応表が無くてもそのまま gh を呼ぶ", () => {
  expect(run(join(home, "x"), ["api"]).out).toBe("token=<unset> args=api");
});

test("GH_TOKEN が既にあれば上書きしない", () => {
  writeMap(`${home}/projects/work work-user\n`);
  expect(
    run(join(home, "projects/work"), ["api"], { GH_TOKEN: "explicit" }).out,
  ).toBe("token=explicit args=api");
});

test("gh auth にはトークンを渡さない (status / switch が狂うため)", () => {
  writeMap(`${home}/projects/work work-user\n`);
  expect(run(join(home, "projects/work"), ["auth", "status"]).out).toBe(
    "token=<unset> args=auth status",
  );
});

test("引数の空白や記号をそのまま渡す", () => {
  expect(run(join(home, "x"), ["issue", "create", "--title", "a b *"]).out).toBe(
    "token=<unset> args=issue create --title a b *",
  );
});

test("終了コードを本物の gh から引き継ぐ", () => {
  writeFileSync(join(fakeDir, "gh"), "#!/bin/sh\nexit 3\n");
  expect(run(join(home, "x"), ["api"]).code).toBe(3);
});

test("PATH に本物の gh が無ければ自分を呼び続けずに失敗する", () => {
  rmSync(join(fakeDir, "gh"));
  const r = run(join(home, "x"), ["api"]);
  expect(r.code).not.toBe(0);
  expect(r.err).toContain("gh");
});
