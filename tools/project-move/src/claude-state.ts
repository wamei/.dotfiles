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

function isRecord(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}

const SESSIONS_INDEX_FILE = "sessions-index.json";

/**
 * <slug>/sessions-index.json をマージする。
 *
 * moveInto の一般ルール (「ファイル名は UUID なので実質衝突しない、既にあれば
 * 触らない」) が、このファイルにだけ成り立たない。sessions-index.json はスラッグ
 * ディレクトリ直下の固定名なので、移動先が既存プロジェクトならほぼ確実に両方に
 * 存在する。スキップすると移動元の索引が宙に浮き、ディレクトリも空にならないので
 * rmdir されず残骸が永久に残る (dotfiles のような、移動先に既にセッションがある
 * プロジェクトを畳む経路そのもの)。
 *
 * version と originalPath は移動先側を使う (originalPath は後段の
 * rewriteSessionsIndex が新パスへ書き換える)。entries は連結し、重複した
 * sessionId は移動先側を残す (新しい場所で観測された情報の方が現在の状態に近い、
 * という裁定による)。ファイル I/O から切り離した純粋関数にしてあるので、
 * ディレクトリ構造を用意しなくてもテストできる。
 */
export function mergeSessionsIndexes(dest: unknown, src: unknown): unknown {
  if (!isRecord(dest)) return dest;

  const destEntries = Array.isArray(dest.entries) ? dest.entries : [];
  const srcEntries = isRecord(src) && Array.isArray(src.entries) ? src.entries : [];

  const destIds = new Set(
    destEntries
      .filter(isRecord)
      .map((e) => e.sessionId)
      .filter((id) => id !== undefined),
  );
  const merged = [
    ...destEntries,
    ...srcEntries.filter((e) => {
      const id = isRecord(e) ? e.sessionId : undefined;
      return id === undefined || !destIds.has(id);
    }),
  ];

  return { ...dest, entries: merged };
}

/** 中身をマージしながら dir を dest へ移す。dest が無ければ単純な rename。 */
async function moveInto(dir: string, dest: string): Promise<boolean> {
  if (!existsSync(dest)) {
    await mkdir(dirname(dest), { recursive: true });
    await rename(dir, dest);
    return false;
  }
  // ファイル名は UUID なので実質衝突しない。既にあるものは触らない。
  // ただし sessions-index.json だけは固定名で確実に衝突しうるので、上のとおりマージする。
  for (const entry of await readdir(dir)) {
    const target = join(dest, entry);
    if (entry === SESSIONS_INDEX_FILE && existsSync(target)) {
      const [srcJson, destJson] = await Promise.all([
        readFile(join(dir, entry), "utf8").then((s) => JSON.parse(s)),
        readFile(target, "utf8").then((s) => JSON.parse(s)),
      ]);
      const merged = `${JSON.stringify(mergeSessionsIndexes(destJson, srcJson), null, 2)}\n`;
      await writeFile(target, merged);
      await unlink(join(dir, entry));
      continue;
    }
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
export async function rewriteLines(
  path: string,
  r: Rewriter,
  transform: (line: string, r: Rewriter) => string,
  dryRun: boolean,
): Promise<number> {
  const tmp = `${path}.project-move-tmp`;
  const out = dryRun ? null : createWriteStream(tmp);
  let changed = 0;
  try {
    const rl = createInterface({ input: createReadStream(path), crlfDelay: Infinity });
    for await (const line of rl) {
      const next = transform(line, r);
      if (next !== line) changed++;
      out?.write(`${next}\n`);
    }
  } catch (err) {
    // transform や入力ストリームが例外を投げて抜けるときも一時ファイルを残さない。
    // 閉じずに unlink すると書き込み中の fd が残ったままになりうるので、先に閉じる。
    if (out) {
      await new Promise<void>((resolve) => out.end(() => resolve()));
      if (existsSync(tmp)) await unlink(tmp);
    }
    throw err;
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

/**
 * バックアップの書き込み先を決める。
 *
 * この移行は何回かに分けて実行する前提なので、`.project-move-backup` を無条件に
 * 上書きすると、2 回目以降の実行が「1 回目の、まだ何も触られていない原本の
 * バックアップ」を静かに潰してしまい、安全網の意味が無くなる。初回は今までどおりの
 * 名前を使い (既存の運用・テストが見ている名前)、既にあればタイムスタンプ付きの
 * 別名に逃がす。同一ミリ秒内の連続実行で名前が衝突しても上書きしないよう、
 * 空くまで連番を足す。
 */
function backupDestination(f: string): string {
  const base = `${f}.project-move-backup`;
  if (!existsSync(base)) return base;
  const stamp = new Date().toISOString().replace(/[:.]/g, "-");
  let candidate = `${base}.${stamp}`;
  let n = 1;
  while (existsSync(candidate)) {
    candidate = `${base}.${stamp}-${n++}`;
  }
  return candidate;
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
      if (existsSync(f)) await copyFile(f, backupDestination(f));
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
