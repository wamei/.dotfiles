// tools/project-move/src/project-move.ts
import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { readdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { makeRewriter } from "./rewrite.ts";
import type { MovePlan } from "./plan-move.ts";

export type FixupReport = {
  rewrittenFiles: { path: string; changedLines: number }[];
  manualSteps: string[];
  warnings: string[];
};

/**
 * Emacs が動いているか。
 *
 * recentf / places / desktop はバッファを閉じるときや終了時に丸ごと書き戻される。
 * 起動中に書き換えても保存で上書きされるので、その場合は触らず警告に回す。
 */
export function isEmacsRunning(
  exec: (cmd: string, args: string[]) => number = (cmd, args) =>
    spawnSync(cmd, args).status ?? 1,
): boolean {
  return exec("pgrep", ["-x", "Emacs"]) === 0;
}

export type PreflightStatus = { uncommitted: string; unpushed: string; stash: string };

/**
 * 移動で失いうるものを一覧する (I8, CLAUDE.md の「破壊的操作」節の趣旨:
 * 消さない、見せるだけ)。3 種類を見るのは、コミット済みでも push 済みでも
 * stash に積んだだけでも、移動そのもの (ghq migrate / mv) が万一失敗したときに
 * 失うものの性質が変わるため。spec の「事前確認」節はこの 3 つを列挙しているが、
 * 実装は uncommitted (`git status --porcelain`) しか出していなかった。
 */
export function preflightStatus(dir: string): PreflightStatus {
  const run = (args: string[]): string =>
    (spawnSync("git", ["-C", dir, ...args], { encoding: "utf8" }).stdout ?? "").trimEnd();
  return {
    uncommitted: run(["status", "--porcelain"]),
    unpushed: run(["log", "--branches", "--not", "--remotes", "--oneline"]),
    stash: run(["stash", "list"]),
  };
}

async function rewriteFileInPlace(
  path: string,
  rewrite: (s: string) => string,
  dryRun: boolean,
): Promise<number> {
  const before = await readFile(path, "utf8");
  const after = rewrite(before);
  if (after === before) return 0;
  // after.split("\n") を filter のコールバック内で毎行呼び直すと、split 自体が
  // O(行数) なので全体で O(n^2) になる (Emacs の recentf / places は数千行ある
  // ことがある)。1 回だけ split してから突き合わせる (Ruling R5)。
  const beforeLines = before.split("\n");
  const afterLines = after.split("\n");
  const changed = beforeLines.filter((l, i) => l !== afterLines[i]).length;
  if (!dryRun) await writeFile(path, after);
  return changed;
}

/**
 * settings.local.json 専用の書き換え (Minor 1)。
 *
 * rewriteFileInPlace は素朴なテキスト置換で、結果が妥当な JSON かを確認しない。
 * このファイルだけは Claude Code 自身が起動時にパースする許可設定なので、
 * 書き換え後に壊れた JSON を書いてしまうと、そのプロジェクトで許可設定が
 * 一切読めなくなる (許可プロンプトが全部復活する程度では済まない)。書き込む前に
 * JSON.parse を通し、妥当でなければ書かずに警告へ回す。
 */
async function rewriteSettingsLocalJson(
  path: string,
  rewrite: (s: string) => string,
  dryRun: boolean,
): Promise<{ changed: number; invalid: boolean }> {
  const before = await readFile(path, "utf8");
  const after = rewrite(before);
  if (after === before) return { changed: 0, invalid: false };
  try {
    JSON.parse(after);
  } catch {
    return { changed: 0, invalid: true };
  }
  const beforeLines = before.split("\n");
  const afterLines = after.split("\n");
  const changed = beforeLines.filter((l, i) => l !== afterLines[i]).length;
  if (!dryRun) await writeFile(path, after);
  return { changed, invalid: false };
}

export async function applyFixups(
  plan: MovePlan,
  opts: {
    emacsStateFiles: string[];
    dryRun: boolean;
    emacsRunning: boolean;
    /**
     * プロジェクト内ファイルをどちらから読むかの明示指定 (I9 の `--fixups-only`
     * 用)。省略時は従来どおり dry-run かどうかで自動選択する。`--fixups-only`
     * は「移動は既に終わっている」状態を手当てするモードなので、--dry-run と
     * 併用しても常に plan.to (移動済みの実体) を見る必要があり、
     * 自動選択 (dry-run なら plan.from) に任せると plan.from はもう存在せず
     * 何も見つからない。
     */
    projectRoot?: "from" | "to";
  },
): Promise<FixupReport> {
  const r = makeRewriter([{ from: plan.from, to: plan.to }]);
  const report: FixupReport = { rewrittenFiles: [], manualSteps: [], warnings: [] };

  // プロジェクト内のファイルを探す起点 (Ruling R10)。
  // dry-run では移動がまだ実行されておらず plan.to はまだ存在しないので、
  // 常に plan.to を見ると settings.local.json / .git/hooks / .envrc の
  // どれも見つからず「手当てなし」と黙って報告してしまう。spec は dry-run を
  // 唯一の検証面と位置づけているので、これは実害になる。dry-run のときだけ
  // plan.from (まだそこにある実体) を見て、実適用のときは移動済みの plan.to を
  // 見るように切り替える。Emacs の状態ファイルは $HOME 配下の絶対パスであり
  // 移動そのものの影響を受けないため、この切り替えとは無関係に常にそのまま読む。
  const rootChoice = opts.projectRoot ?? (opts.dryRun ? "from" : "to");
  const projectRoot = rootChoice === "from" ? plan.from : plan.to;

  // 1) プロジェクト内の permission allowlist。移さないと許可プロンプトが増える。
  const settings = join(projectRoot, ".claude", "settings.local.json");
  if (existsSync(settings)) {
    const { changed, invalid } = await rewriteSettingsLocalJson(settings, r.rewriteText, opts.dryRun);
    if (invalid) {
      report.warnings.push(
        `${settings}: rewriting it would produce invalid JSON; left unchanged. check it by hand.`,
      );
    } else if (changed > 0) {
      report.rewrittenFiles.push({ path: settings, changedLines: changed });
    }
  }

  // 2) Emacs の状態ファイル
  if (opts.emacsRunning && opts.emacsStateFiles.length > 0) {
    report.warnings.push(
      "emacs is running; its state files were left alone because it rewrites them on exit. " +
        "quit emacs and rerun with --fixups-only.",
    );
  } else {
    for (const f of opts.emacsStateFiles) {
      if (!existsSync(f)) continue;
      const changed = await rewriteFileInPlace(f, r.rewriteText, opts.dryRun);
      if (changed > 0) report.rewrittenFiles.push({ path: f, changedLines: changed });
    }
  }

  // 3) git hooks。自動で書き換えず再生成を促す。lefthook / husky は
  //    node_modules のバイナリを絶対パスで起動するので、再インストールが正解。
  const hooks = join(projectRoot, ".git", "hooks");
  if (existsSync(hooks)) {
    const tool = installCommand(projectRoot);
    for (const name of await readdir(hooks)) {
      const p = join(hooks, name);
      const body = await readFile(p, "utf8").catch(() => "");
      // I11: コメント行を除いて判定する。husky v4 は生成した全 hook に
      // `#   From: <old>/node_modules/husky` を刻むが、実際に走るのは
      // husky.local.sh の `cd "."` (相対) なので移動しても動く。素の
      // includes() だと hook 19 本すべてが偽陽性の todo になり、しかも
      // 「何もしなくてよい」のに再インストールを促してしまう。
      if (!stripShellComments(body).includes(plan.from)) continue;
      const cmd = body.includes("lefthook") ? "npx lefthook install" : tool;
      report.manualSteps.push(`${p} still points at the old path; run \`${cmd}\` in ${plan.to}`);
    }
  }

  // 4) direnv は .envrc のパスをハッシュして allow を記録するので、移動で失効する
  if (existsSync(join(projectRoot, ".envrc"))) {
    report.manualSteps.push(`run \`direnv allow\` in ${plan.to}`);
  }

  return report;
}

/** 行頭 (空白を除く) が `#` の行を落とす。shell script の素朴なコメント除去。 */
function stripShellComments(body: string): string {
  return body
    .split("\n")
    .filter((l) => !l.trimStart().startsWith("#"))
    .join("\n");
}

/**
 * hook 再生成に使うべき install コマンドを lockfile から決める (I11)。
 *
 * 以前は無条件に `npm install` を案内していた。bun 管理のプロジェクト
 * (SDXFW_TEMPLATE) でそのとおり実行したところ、npm の strict な peer 解決で
 * ERESOLVE になって失敗した。npm が通っていたら通っていたで、bun.lock しか
 * 無い repo に package-lock.json を書き足すところだった。
 */
function installCommand(projectRoot: string): string {
  const has = (f: string) => existsSync(join(projectRoot, f));
  if (has("bun.lock") || has("bun.lockb")) return "bun install";
  if (has("pnpm-lock.yaml")) return "pnpm install";
  if (has("yarn.lock")) return "yarn install";
  return "npm install";
}
