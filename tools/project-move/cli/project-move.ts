#!/usr/bin/env bun
// tools/project-move/cli/project-move.ts
//
// Claude が動いていても安全な作業だけをする。~/.claude 配下の追随は
// claude-state-move の担当 (~/.claude.json は Claude 起動中ずっと書き戻されるため)。
import { appendFile, mkdir, readFile, rename } from "node:fs/promises";
import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { ghqProbe, planMove, type MovePlan } from "../src/plan-move.ts";
import { applyFixups, isEmacsRunning, preflightStatus } from "../src/project-move.ts";
import {
  appliedMovesLogPath,
  defaultMovesLogPath,
  formatMoveRow,
  parseMovesTsv,
  type Move,
} from "../src/moves.ts";
import { parseArgs } from "../src/args.ts";

// 値を取るオプションはここ project-move では --to だけ。claude-state-move.ts と
// 同じ src/args.ts の parseArgs を使うのは、`args.filter(a => !a.startsWith("--"))`
// のような「-- で始まらないものは全部位置引数」という実装が、値を取るオプション
// の値まで位置引数として拾ってしまうバグを二重に持ち込まないため
// (claude-state-move で実際に踏んだ: <old> <new> が無視されて既定ログ
// (実ホーム側!) にフォールバックする事故があった)。
const VALUE_OPTIONS = new Set(["to"]);

let parsed: ReturnType<typeof parseArgs>;
try {
  parsed = parseArgs(process.argv.slice(2), VALUE_OPTIONS);
} catch (e) {
  console.error((e as Error).message);
  process.exit(2);
}
const { flags, options, positional: dirs } = parsed;

// 未知の `--xxx` は静かに無視せず落とす (dry-run / fixups-only 以外のフラグは想定していない)。
for (const f of flags) {
  if (f !== "dry-run" && f !== "fixups-only") {
    console.error(`unknown option: --${f}`);
    process.exit(2);
  }
}
const dryRun = flags.has("dry-run");
const fixupsOnly = flags.has("fixups-only");
const explicitTo = options.to;

// --to は 1 つのディレクトリにしか許さない。複数のディレクトリに同じ移動先を
// 指定しても意味を成さず、後から処理したものが先の移動結果を上書きしてしまう
// (mkdir + rename の宛先が全員同じになる)。spec の
// `project-move [--dry-run] [--to <path>] <dir>...` という形自体は複数
// ディレクトリを許すが、それは --to 無しのとき (ghq / localRoot が各々に
// 別の移動先を計算する) の話であって、--to で明示指定したときは 1 対 1 の
// 関係でなければ安全に扱えない。ここで早期に弾き、1 つも移動を始めない。
if (explicitTo !== undefined && dirs.length !== 1) {
  console.error("--to can only be used with exactly one directory");
  process.exit(2);
}
if (fixupsOnly && explicitTo !== undefined) {
  console.error("--fixups-only cannot be combined with --to");
  process.exit(2);
}

const home = homedir();

const EMACS_STATE_FILES = [
  ".emacs.d/recentf",
  ".emacs.d/places",
  ".emacs.d/.emacs.desktop",
  ".emacs.d/.emacs.desktop-nw",
  ".emacs.d/projects",
  ".emacs.d/.cache/treemacs-persist",
  ".emacs.d/history",
].map((p) => join(home, p));

const emacsRunning = isEmacsRunning();
const log = defaultMovesLogPath(home);

// --- I9: --fixups-only ------------------------------------------------------
//
// Emacs 起動中の警告が「--fixups-only を付けて再実行しろ」と案内していたのに
// そのオプション自体が存在せず、unknown option で exit 2 になっていた。移動は
// 既に終わっているので普通の再実行もできない (from がもう存在しない)。
//
// このモードは「移動は行わず、手当てだけを (再度) 走らせる」。対象は
// 位置引数で明示された <old> <new> 1 組か、省略時は移動ログの全行。
// ログの全行を毎回読むのは、project-move 自身は claude-state-move の I5 の
// ような「適用済みを刈り込む」仕組みを持たないため。手当ては何度実行しても
// 副作用が増えない (rewriteText は既に新パスになった箇所には一致しない) ので、
// 全件を読み直しても安全であり、これ以上複雑にする理由が無い。
if (fixupsOnly) {
  let targets: Move[];
  if (dirs.length === 2) {
    targets = [{ from: resolve(dirs[0]), to: resolve(dirs[1]) }];
  } else if (dirs.length === 0) {
    // I10: 退避済みのログ (moves.applied.tsv) も対象に含める。
    //
    // claude-state-move (I5) は適用に成功すると moves.tsv の中身を
    // moves.applied.tsv へ移して元を消す。つまり「移動もクレーム状態の
    // 書き換えも全部終わった直後」という --fixups-only を一番使いたい瞬間に、
    // 位置引数を省いた形は必ず `no moves log` で落ちていた (実際に踏んだ)。
    // 手当ては何度走らせても副作用が増えない (rewriteText は既に新パスに
    // なった箇所に一致しない) ので、退避済みと未適用の両方を素直に読む。
    const logs = [appliedMovesLogPath(log), log].filter((p) => existsSync(p));
    if (logs.length === 0) {
      console.error(`no moves log at ${log}; pass <old> <new> instead`);
      process.exit(2);
    }
    targets = [];
    for (const p of logs) targets.push(...parseMovesTsv(await readFile(p, "utf8")));
  } else {
    console.error("--fixups-only expects 0 or 2 positional arguments (<old> <new>)");
    process.exit(2);
  }

  for (const { from, to } of targets) {
    console.log(`fixups ${from} -> ${to}`);
    try {
      // --fixups-only は「移動は既に終わっている」ことが前提なので、
      // --dry-run と併用しても常に plan.to (移動済みの実体) を見る。dry-run
      // での自動選択 (plan.from) に任せると plan.from はもう存在しない。
      const report = await applyFixups(
        { kind: "local", from, to },
        { emacsStateFiles: EMACS_STATE_FILES, dryRun, emacsRunning, projectRoot: "to" },
      );
      for (const f of report.rewrittenFiles) console.log(`  rewrite ${f.path} (${f.changedLines} lines)`);
      for (const s of report.manualSteps) console.log(`  todo    ${s}`);
      for (const w of report.warnings) console.log(`  warn    ${w}`);
    } catch (e) {
      console.error(`  fixups failed for ${to}: ${(e as Error).message}`);
    }
  }

  console.log(dryRun ? "\ndry run. nothing was changed." : "\nfixups applied.");
  process.exit(0);
}

// --- 通常の移動経路 ----------------------------------------------------------

// C1: 位置引数を絶対パスに正規化してから使う。相対パスのまま渡すと "BeecoV2" の
// ような裸の名前が全ファイルの置換対象になり、~/.claude.json 等を二重に壊れた
// パスへ書き換えてしまう (makeRewriter がその文字列を全ファイルで置換するため)。
// 解決後に存在しなければここで止める。dry-run は完全に非破壊という前提を守る
// ため、1 件でも無効な引数があれば 1 件も動かさずに落とす。
const resolvedDirs = dirs.map((d) => resolve(d));
for (const d of resolvedDirs) {
  if (!existsSync(d)) {
    console.error(`${d}: no such directory`);
    process.exit(1);
  }
}
const resolvedTo = explicitTo !== undefined ? resolve(explicitTo) : undefined;

for (const dir of resolvedDirs) {
  // --to が与えられたときは ghq / localRoot の自動判定を使わず、指定された
  // path をそのまま移動先にする。種別は "local" 扱いにする (ghq が決めた場所
  // ではなく、ユーザーが明示指定した場所という意味で、以降の existsSync チェック
  // や rename も local と同じ経路を通す)。
  let plan: MovePlan;
  try {
    plan =
      resolvedTo !== undefined
        ? { kind: "local", from: dir, to: resolvedTo }
        : planMove(dir, { probe: ghqProbe, localRoot: join(home, "projects", "local") });
  } catch (e) {
    // I7: ghq が「remote 無し」「非 git」以外の理由で失敗したときは、1 件飛ばして
    // 次へ進めるのではなく、バッチ全体を止めて理由を見せる。ghq 未インストールや
    // 設定エラーは他のディレクトリでも同じように起きている可能性が高く、
    // 気付かないまま remote を持つリポジトリを local/ へ落とし続けかねない。
    console.error((e as Error).message);
    process.exit(1);
  }
  console.log(`${plan.kind === "ghq" ? "ghq  " : "local"} ${plan.from} -> ${plan.to}`);

  // 移動前に、失いうるものを見せる (I8, CLAUDE.md の「破壊的操作」節)。
  // uncommitted だけでなく unpushed (相手が居ない commit) と stash も見せる。
  const pre = preflightStatus(plan.from);
  if (pre.uncommitted) console.log(`  uncommitted:\n${pre.uncommitted}`);
  if (pre.unpushed) console.log(`  unpushed:\n${pre.unpushed}`);
  if (pre.stash) console.log(`  stash:\n${pre.stash}`);

  // I6/I10: 1 件の移動失敗が残りのバッチを道連れにしないよう、移動そのものを
  // try/catch で囲む。移動が失敗したディレクトリはログにも残さず、手当ても
  // 走らせない (何も変わっていないので当然)。
  let moveFailed = false;
  if (!dryRun) {
    try {
      if (plan.kind === "ghq") {
        // ghq に任せる。移動後の `git worktree repair` まで面倒を見てくれる。
        const r = spawnSync("ghq", ["migrate", "-y", plan.from], { stdio: "inherit" });
        if (r.status !== 0) throw new Error("ghq migrate failed");
      } else {
        await mkdir(dirname(plan.to), { recursive: true });
        if (existsSync(plan.to)) throw new Error(`${plan.to} already exists`);
        await rename(plan.from, plan.to);
      }
    } catch (e) {
      console.error(`  failed to move: ${(e as Error).message}; skipped`);
      moveFailed = true;
    }
  }
  if (moveFailed) continue;

  // I6: ログ追記を手当ての前に行う。順序を逆にすると、applyFixups が例外を
  // 投げたときに「ディレクトリは移動済みなのにログに残らない」まま次のコマンド
  // (claude-state-move) がその移動を一生知らないことになる。
  if (!dryRun) {
    await mkdir(dirname(log), { recursive: true });
    await appendFile(log, formatMoveRow({ from: plan.from, to: plan.to }, new Date()));
  }

  // I6/I10: 手当ての失敗も 1 件のディレクトリで打ち止めにし、バッチを続ける。
  // 移動とログ追記は既に終わっているので、ここで失敗しても後から
  // `--fixups-only <old> <new>` でやり直せる。
  try {
    const report = await applyFixups(plan, { emacsStateFiles: EMACS_STATE_FILES, dryRun, emacsRunning });
    for (const f of report.rewrittenFiles) console.log(`  rewrite ${f.path} (${f.changedLines} lines)`);
    for (const s of report.manualSteps) console.log(`  todo    ${s}`);
    for (const w of report.warnings) console.log(`  warn    ${w}`);
  } catch (e) {
    console.error(`  fixups failed: ${(e as Error).message}; rerun with --fixups-only ${plan.from} ${plan.to}`);
  }
}

console.log(
  dryRun
    ? "\ndry run. nothing was changed."
    : `\nlogged to ${log}. quit every claude session, then run:\n  claude-state-move --dry-run`,
);
