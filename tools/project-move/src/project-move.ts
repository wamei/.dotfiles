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

export async function applyFixups(
  plan: MovePlan,
  opts: { emacsStateFiles: string[]; dryRun: boolean; emacsRunning: boolean },
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
  const projectRoot = opts.dryRun ? plan.from : plan.to;

  // 1) プロジェクト内の permission allowlist。移さないと許可プロンプトが増える。
  const settings = join(projectRoot, ".claude", "settings.local.json");
  if (existsSync(settings)) {
    const changed = await rewriteFileInPlace(settings, r.rewriteText, opts.dryRun);
    if (changed > 0) report.rewrittenFiles.push({ path: settings, changedLines: changed });
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
    for (const name of await readdir(hooks)) {
      const p = join(hooks, name);
      const body = await readFile(p, "utf8").catch(() => "");
      if (!body.includes(plan.from)) continue;
      const tool = body.includes("lefthook") ? "npx lefthook install" : "npm install";
      report.manualSteps.push(`${p} still points at the old path; run \`${tool}\` in ${plan.to}`);
    }
  }

  // 4) direnv は .envrc のパスをハッシュして allow を記録するので、移動で失効する
  if (existsSync(join(projectRoot, ".envrc"))) {
    report.manualSteps.push(`run \`direnv allow\` in ${plan.to}`);
  }

  return report;
}
