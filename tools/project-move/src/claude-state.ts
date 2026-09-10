// tools/project-move/src/claude-state.ts
import { createReadStream, createWriteStream, existsSync } from "node:fs";
import { copyFile, mkdir, readdir, readFile, rename, rmdir, unlink, writeFile } from "node:fs/promises";
import { createInterface } from "node:readline";
import { dirname, join } from "node:path";
import type { Move } from "./moves.ts";
import { slug, slugChildRemainder } from "./slug.ts";
import {
  makeRewriter,
  rewriteClaudeJson,
  rewriteJsonlLine,
  rewriteSessionsIndex,
  type Rewriter,
} from "./rewrite.ts";

export type ClaudeStatePaths = {
  claudeHome: string;
  claudeJson: string;
  historyJsonl: string;
};

export type ClaudeStateReport = {
  renames: { from: string; to: string; merged: boolean }[];
  rewrittenFiles: { path: string; changedLines: number }[];
  claudeJsonKeys: { from: string; to: string }[];
  warnings: string[];
};

/** 中身をマージしながら dir を dest へ移す。dest が無ければ単純な rename。 */
async function moveInto(dir: string, dest: string): Promise<boolean> {
  if (!existsSync(dest)) {
    await mkdir(dirname(dest), { recursive: true });
    await rename(dir, dest);
    return false;
  }
  // ファイル名は UUID なので実質衝突しない。既にあるものは触らない。
  for (const entry of await readdir(dir)) {
    const target = join(dest, entry);
    if (!existsSync(target)) await rename(join(dir, entry), target);
  }
  // 中身を移し終えた元ディレクトリは空になる。残すと ~/.claude/projects に
  // 実在しないパスの名前のディレクトリが溜まる。ただし移動先に同名が既にあって
  // 移せなかったものが残っている場合は消さない (中身を黙って捨てないため)。
  if ((await readdir(dir)).length === 0) await rmdir(dir);
  return true;
}

/**
 * 行ストリームで書き換える。全体をメモリに載せない (実機に 36 MB の jsonl がある)。
 *
 * 一時ファイルに書いてから rename で被せるのは、書き込み途中で落ちても元の
 * ファイルが壊れないようにするため。セッション履歴は失うと戻せない。
 */
async function rewriteLines(
  path: string,
  r: Rewriter,
  transform: (line: string, r: Rewriter) => string,
  dryRun: boolean,
): Promise<number> {
  const tmp = `${path}.project-move-tmp`;
  const out = dryRun ? null : createWriteStream(tmp);
  let changed = 0;
  const rl = createInterface({ input: createReadStream(path), crlfDelay: Infinity });
  for await (const line of rl) {
    const next = transform(line, r);
    if (next !== line) changed++;
    out?.write(`${next}\n`);
  }
  if (out) {
    await new Promise<void>((resolve, reject) => {
      out.on("error", reject);
      out.end(resolve);
    });
    // 変更が無いなら元のファイルに触らない (mtime を無駄に動かさない)
    if (changed > 0) await rename(tmp, path);
    else await unlink(tmp);
  }
  return changed;
}

async function jsonlFilesUnder(dir: string): Promise<string[]> {
  const found: string[] = [];
  for (const entry of await readdir(dir, { withFileTypes: true })) {
    const p = join(dir, entry.name);
    if (entry.isDirectory()) found.push(...(await jsonlFilesUnder(p)));
    else if (entry.name.endsWith(".jsonl")) found.push(p);
  }
  return found;
}

export async function claudeStateMove(
  moves: Move[],
  paths: ClaudeStatePaths,
  opts: { dryRun: boolean },
): Promise<ClaudeStateReport> {
  const r = makeRewriter(moves);
  const report: ClaudeStateReport = {
    renames: [],
    rewrittenFiles: [],
    claudeJsonKeys: [],
    warnings: [],
  };

  // 破壊の前に控える。CLAUDE.md の「破壊的操作」節の趣旨に沿う。
  if (!opts.dryRun) {
    for (const f of [paths.claudeJson, paths.historyJsonl]) {
      if (existsSync(f)) await copyFile(f, `${f}.project-move-backup`);
    }
  }

  const projectsDir = join(paths.claudeHome, "projects");
  const dirNames = existsSync(projectsDir) ? await readdir(projectsDir) : [];

  // 1) スラッグディレクトリの改名。slug(from) の接頭辞一致で子まで拾う。
  //    cwd からは worktree のディレクトリ名を導けないので、この方法しかない。
  for (const move of moves) {
    const fromSlug = slug(move.from);
    const toSlug = slug(move.to);
    for (const name of dirNames) {
      const remainder = slugChildRemainder(name, fromSlug);
      if (remainder === null) continue;
      const dest = toSlug + remainder;
      report.renames.push({ from: name, to: dest, merged: existsSync(join(projectsDir, dest)) });
      if (!opts.dryRun) await moveInto(join(projectsDir, name), join(projectsDir, dest));
    }
  }

  // 2) 中身を書き換える
  for (const { from, to } of report.renames) {
    // dry-run では改名していないので、まだ元の名前のディレクトリを読む。
    // ここを to 固定にすると dry-run が「書き換え 0 件」と嘘をつく。
    const dir = join(projectsDir, opts.dryRun ? from : to);
    if (!existsSync(dir)) continue;
    for (const file of await jsonlFilesUnder(dir)) {
      const changed = await rewriteLines(file, r, rewriteJsonlLine, opts.dryRun);
      if (changed > 0) report.rewrittenFiles.push({ path: file, changedLines: changed });
    }
    const index = join(dir, "sessions-index.json");
    if (existsSync(index)) {
      const before = await readFile(index, "utf8");
      const after = `${JSON.stringify(rewriteSessionsIndex(JSON.parse(before), r), null, 2)}\n`;
      if (after !== before) {
        report.rewrittenFiles.push({ path: index, changedLines: 1 });
        if (!opts.dryRun) await writeFile(index, after);
      }
    }
  }

  // 3) ~/.claude.json
  if (existsSync(paths.claudeJson)) {
    const json = JSON.parse(await readFile(paths.claudeJson, "utf8"));
    const projects = json.projects ?? {};
    for (const key of Object.keys(projects)) {
      const next = r.rewriteText(key);
      if (next !== key) report.claudeJsonKeys.push({ from: key, to: next });
    }
    if (!opts.dryRun) {
      await writeFile(paths.claudeJson, `${JSON.stringify(rewriteClaudeJson(json, r), null, 2)}\n`);
    }
  }

  // 4) ~/.claude/history.jsonl
  if (existsSync(paths.historyJsonl)) {
    const changed = await rewriteLines(paths.historyJsonl, r, rewriteJsonlLine, opts.dryRun);
    if (changed > 0) report.rewrittenFiles.push({ path: paths.historyJsonl, changedLines: changed });
  }

  return report;
}
