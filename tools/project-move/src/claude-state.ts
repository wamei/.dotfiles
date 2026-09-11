// tools/project-move/src/claude-state.ts
import { createReadStream, createWriteStream, existsSync } from "node:fs";
import {
  copyFile,
  mkdir,
  readdir,
  readFile,
  rename,
  rmdir,
  stat,
  unlink,
  writeFile,
} from "node:fs/promises";
import { createInterface } from "node:readline";
import { dirname, join } from "node:path";
import { spawnSync } from "node:child_process";
import { detectMoveCollisions, type Move } from "./moves.ts";
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
  /** ~/.claude/projects のクローンバックアップ先。dry-run では取らないので null。 */
  projectsBackupPath: string | null;
};

/**
 * Claude Code が動いているか。
 *
 * ~/.claude.json は起動中ずっと書き戻されており (~/.claude/backups に数分おきの
 * バックアップが溜まる)、起動中に書き換えても上書きで消える。だから --force は
 * 用意せず、検出したら必ず拒否する。
 *
 * pgrep はベストエフォートで、すり抜ける起動形態は残りうる。spec のリスク 2。
 */
export function isClaudeRunning(
  exec: (cmd: string, args: string[]) => number = (cmd, args) =>
    spawnSync(cmd, args).status ?? 1,
): boolean {
  return exec("pgrep", ["-x", "claude"]) === 0;
}

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

/**
 * dir の中身を dest へ再帰的にマージする (I2)。
 *
 * 元の実装はトップレベルの1階層しか見ておらず、`memory/` のようなディレクトリ
 * が移動先に既にあると、移動元の `memory/` 配下は「同名エントリがある」の
 * 一言で丸ごと素通りされ、`rmdir` もされず、警告も出ずに残骸として残っていた
 * (`memory/` は実機 18 スラッグ中 15 に存在する。計画の Task 12「取り残された
 * dotfiles セッションのマージ」がこの経路そのもの)。
 *
 * ここでは同名エントリがディレクトリ同士なら中に潜って再帰的にマージし、
 * それでも移せなかったもの (同名ファイルの衝突) は warnings に積んで
 * 呼び出し側 (report.warnings) へ伝える。黙って残さない。
 */
async function mergeDirInto(dir: string, dest: string, warnings: string[]): Promise<void> {
  for (const entry of await readdir(dir, { withFileTypes: true })) {
    const src = join(dir, entry.name);
    const target = join(dest, entry.name);

    // ファイル名は UUID なので実質衝突しない。既にあるものは触らない。
    // ただし sessions-index.json だけは固定名で確実に衝突しうるので、マージする。
    if (entry.name === SESSIONS_INDEX_FILE && existsSync(target)) {
      const [srcJson, destJson] = await Promise.all([
        readFile(src, "utf8").then((s) => JSON.parse(s)),
        readFile(target, "utf8").then((s) => JSON.parse(s)),
      ]);
      const merged = `${JSON.stringify(mergeSessionsIndexes(destJson, srcJson), null, 2)}\n`;
      await writeFile(target, merged);
      await unlink(src);
      continue;
    }

    if (!existsSync(target)) {
      await rename(src, target);
      continue;
    }

    // ここに来るのは移動先に同名の何かが既にある場合。ディレクトリ同士なら
    // 中身だけ潜って再帰的にマージする。それ以外 (ファイル名の衝突) は
    // どちらを残すべきか決める材料が無いので、上書きも削除もせずその場に残し、
    // 必ず警告する (黙って捨てない)。
    const [srcStat, destStat] = await Promise.all([stat(src), stat(target)]);
    if (srcStat.isDirectory() && destStat.isDirectory()) {
      await mergeDirInto(src, target, warnings);
      if ((await readdir(src)).length === 0) {
        await rmdir(src);
      } else {
        warnings.push(`could not fully merge "${src}" into "${target}"; some entries were left in place`);
      }
    } else {
      warnings.push(`"${src}" was not moved because "${target}" already exists; left in place`);
    }
  }
}

/** 中身をマージしながら dir を dest へ移す。dest が無ければ単純な rename。 */
async function moveInto(dir: string, dest: string, warnings: string[]): Promise<boolean> {
  if (!existsSync(dest)) {
    await mkdir(dirname(dest), { recursive: true });
    await rename(dir, dest);
    return false;
  }
  await mergeDirInto(dir, dest, warnings);
  // 中身を移し終えた元ディレクトリは空になる。残すと ~/.claude/projects に
  // 実在しないパスの名前のディレクトリが溜まる。ただし移動先に同名が既にあって
  // 移せなかったものが残っている場合は消さない (中身を黙って捨てないため、
  // 上の mergeDirInto が既に警告を積んでいる)。
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

export type BackupProjectsDirResult = { ok: true } | { ok: false; message: string };
export type BackupProjectsDir = (src: string, dest: string) => BackupProjectsDirResult;

/**
 * ~/.claude/projects をクローンでバックアップする既定実装 (C2)。
 *
 * `cp -c` は APFS の clonefile(2) を使うので、538 MB でもほぼ瞬時・ほぼ
 * 容量を消費しない。ただし man cp にあるとおり、クローン非対応のボリューム
 * (異なるファイルシステム間など) では黙って通常コピーにフォールバックする
 * 仕様なので、"clonefile が使えたか" 自体は検出できない。ここで実際に守りたい
 * のは「バックアップが 1 つも取れないまま 538 MB を不可逆に書き換え始める」
 * ことなので、フォールバックしてでもコピーが完走したかどうか (exit code と
 * 実際にディレクトリが出来ているか) だけを見る。
 */
const defaultBackupProjectsDir: BackupProjectsDir = (src, dest) => {
  const r = spawnSync("cp", ["-c", "-R", src, dest], { encoding: "utf8" });
  if (r.status !== 0 || !existsSync(dest)) {
    const detail = (r.stderr ?? "").trim() || (r.error ? r.error.message : "") || `exit ${r.status}`;
    return { ok: false, message: detail };
  }
  return { ok: true };
};

export async function claudeStateMove(
  moves: Move[],
  paths: ClaudeStatePaths,
  opts: { dryRun: boolean; backupProjectsDir?: BackupProjectsDir },
): Promise<ClaudeStateReport> {
  const r = makeRewriter(moves);
  const report: ClaudeStateReport = {
    renames: [],
    rewrittenFiles: [],
    claudeJsonKeys: [],
    // 移動リストそのものの衝突 (移動先の重複・連鎖書き換え・スラッグの衝突) を
    // 最初に検査しておく。ここでは警告として載せるだけで止めない — dry-run は
    // まさにこの検査結果を確認するために回すので、claudeStateMove 自身が
    // 止めてしまうと使えなくなる。実適用を拒否するかどうかは呼び出し側 (CLI) の
    // 責務にする (衝突を無視して適用すると 538 MB のセッション履歴が別プロジェクト
    // のものと不可逆に混ざるため、CLI 側では拒否する)。
    warnings: detectMoveCollisions(moves),
    projectsBackupPath: null,
  };

  const projectsDir = join(paths.claudeHome, "projects");
  const backupProjectsDir = opts.backupProjectsDir ?? defaultBackupProjectsDir;

  // 破壊の前に控える。CLAUDE.md の「破壊的操作」節の趣旨に沿う。dry-run では
  // 何も書き換えないので取らない。
  if (!opts.dryRun) {
    // ~/.claude/projects (538 MB) を先に控える。ここで失敗したら、あとに続く
    // ディレクトリ改名や jsonl の書き換えは一切始めない (C2)。以前は
    // claudeJson と historyJsonl の 2 ファイルしかバックアップしておらず、
    // projects/ は rewriteLines が tmp を rename で原本に被せる実装のため
    // 何の控えも残らなかった。spec の「切り戻し」節が実質嘘になっていた。
    if (existsSync(projectsDir)) {
      const backupDir = backupDestination(projectsDir);
      const result = backupProjectsDir(projectsDir, backupDir);
      if (!result.ok) {
        throw new Error(
          `failed to back up ${projectsDir} to ${backupDir}; refusing to modify Claude session ` +
            `state without a backup (is this an APFS volume? clonefile may be unsupported here). ` +
            `${result.message}`,
        );
      }
      report.projectsBackupPath = backupDir;
    }
    for (const f of [paths.claudeJson, paths.historyJsonl]) {
      if (existsSync(f)) await copyFile(f, backupDestination(f));
    }
  }

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
      if (!opts.dryRun) await moveInto(join(projectsDir, name), join(projectsDir, dest), report.warnings);
    }
  }

  // 2) 中身を書き換える。
  //
  // I1: report.renames (今回改名した分) だけでなく、各 move の行き先スラッグに
  // 一致する既存ディレクトリも対象に加える。前回の実行が「改名は終わったが
  // 中身の書き換えで例外を投げて止まった」状態だと、今回は fromSlug に一致する
  // ディレクトリがもう存在しないため上のループでは 1 件も拾えず、renames が
  // 空になる。かつては renames が空 = 対象なし、として何もせず exit 0 になり、
  // 「静かに移行が失敗したまま成功と報告される」バグになっていた
  // (中身は旧パスのまま)。行き先スラッグで直接引き直すことで、改名が今回分か
  // 前回分かによらず、その move の行き先には必ず目を通す。
  const projectsDirNamesNow = opts.dryRun
    ? dirNames // dry-run では何も改名していないので、最初に読んだ一覧のままが正しい
    : existsSync(projectsDir)
      ? await readdir(projectsDir)
      : [];
  const rewriteDirNames = new Set(report.renames.map((rn) => rn.to));
  for (const move of moves) {
    const toSlug = slug(move.to);
    for (const name of projectsDirNamesNow) {
      if (slugChildRemainder(name, toSlug) !== null) rewriteDirNames.add(name);
    }
  }

  for (const to of rewriteDirNames) {
    const rename_ = report.renames.find((rn) => rn.to === to);
    // dry-run で今回分の改名候補なら、まだ元の名前 (from) のディレクトリを読む。
    // ここを to 固定にすると dry-run が「書き換え 0 件」と嘘をつく。
    // 前回までに改名済みの straggler は from という概念が無く、常に to (現在の
    // 実際の名前) で読む。
    const readName = opts.dryRun && rename_ ? rename_.from : to;
    const dir = join(projectsDir, readName);
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
