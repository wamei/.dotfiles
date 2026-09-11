import { join } from "node:path";
import { slug, slugChildRemainder } from "./slug.ts";

/** 1 件の移動。from も to も絶対パス。 */
export type Move = { from: string; to: string };

/**
 * 移動ログの既定位置。
 *
 * project-move が追記し、claude-state-move が読む。両者を別コマンドに分けたのは
 * ~/.claude.json が Claude 起動中ずっと書き戻されているためで、この受け渡しが
 * 2 つのコマンドの唯一の接点になる。
 */
export function defaultMovesLogPath(home: string): string {
  return join(home, ".local", "state", "project-move", "moves.tsv");
}

/**
 * 適用済みの moves ログの退避先 (I5)。
 *
 * `claude-state-move` はログを読むだけで消費した分を刈り込まない実装だった。
 * ログは `project-move` が追記し続ける一方なので、時間が経つと「もう改名も
 * 書き換えも終わった行」が溜まり続け、`detectMoveCollisions` の連鎖検出が
 * それら過去の行まで対象にしてしまう。たとえば `projects/foo → local/foo` が
 * 未刈り込みのまま残っていると、後日 `local/foo → github.com/o/foo` を昇格
 * したときにこの 2 行が「to が別の from と一致する」連鎖とみなされ、
 * 昇格そのものが `refusing to apply` で拒否される (実際に踏んだ形の 1 つは
 * `project-move` を retry して同じ行が 2 度入っただけでも起きる)。
 *
 * 最も単純な形で十分と判断した: 実適用が成功した直後にログファイルの中身を
 * まるごと `moves.applied.tsv` へ追記し、元のログファイルを消す。次回の実行は
 * 同じログパスを読みにいくが、消費済みの行はもう無いので「未処理分だけ」が
 * 自然に残る。これで detectMoveCollisions が見るのは常に「まだ適用していない
 * 行」だけになり、上のような偽陽性の連鎖検出が起きなくなる。
 * 監査目的で過去分を消さずに `moves.applied.tsv` 側へ追記で積んでいく
 * (上書きしない)。
 */
export function appliedMovesLogPath(logPath: string): string {
  return logPath.endsWith(".tsv") ? `${logPath.slice(0, -4)}.applied.tsv` : `${logPath}.applied.tsv`;
}

export function formatMoveRow(move: Move, at: Date): string {
  return `${at.toISOString()}\t${move.from}\t${move.to}\n`;
}

function collectSlugCollisions(paths: string[], label: string): string[] {
  const bySlug = new Map<string, Set<string>>();
  for (const p of paths) {
    const s = slug(p);
    const set = bySlug.get(s) ?? new Set<string>();
    set.add(p);
    bySlug.set(s, set);
  }
  const warnings: string[] = [];
  for (const [s, set] of bySlug) {
    if (set.size > 1) {
      warnings.push(`${label} slug to the same "${s}": ${[...set].join(", ")}`);
    }
  }
  return warnings;
}

/**
 * from 同士のスラッグの「接頭辞の曖昧さ」を検出する (Minor 2)。
 *
 * collectSlugCollisions は完全一致しか見ない。しかし slugChildRemainder は
 * 「dirName が parentSlug 自身か、その子 (parentSlug + "-" + 残り) か」を判定して
 * 実際の改名対象を拾う実装なので、完全一致でなくても ─ ある move の
 * slug(from) が別の move の slug(from) の「子スラッグ」として解釈できる場合 ─
 * claude-state-move のディレクトリ走査は両者を区別できない。
 *
 * 例: `local/memotan` と `local/memotan_knowledge` は完全一致しないが、
 * slug(local/memotan_knowledge) は slug(local/memotan) + "-knowledge" になり、
 * "-" は「非英数字 1 文字」の変換結果でもあるので、`local/memotan` の移動を
 * 処理する際に `local/memotan_knowledge` のスラッグディレクトリまで
 * 「子ディレクトリ (worktree 等)」として誤って引きずり込みうる。
 */
function collectSlugPrefixAmbiguities(paths: string[], label: string): string[] {
  const warnings: string[] = [];
  const withSlug = paths.map((p) => ({ path: p, slug: slug(p) }));
  for (const parent of withSlug) {
    for (const child of withSlug) {
      if (parent.path === child.path || parent.slug === child.slug) continue;
      if (slugChildRemainder(child.slug, parent.slug) === null) continue;
      warnings.push(
        `${label} slug("${parent.path}") is a prefix of slug("${child.path}") and could be ` +
          `misread as its child under ~/.claude/projects: ${parent.path}, ${child.path}`,
      );
    }
  }
  return warnings;
}

/**
 * 移動リストの衝突を検査する。dry-run の安全確認のために spec が要求している
 * チェックだが、どのタスクにも実装が割り当てられていなかったのでここに足す。
 *
 * 検出する 4 種類はいずれも「後から適用した書き換えが先の結果を黙って壊す/
 * 飲み込む」形で、しかも ~/.claude 配下の実ファイルに対する操作なので、
 * 検出できずに実適用してしまうと元に戻せない。
 *
 * 1. 移動先の重複: 2 件以上の move が同じ to を指すと、moveInto はディレクトリを
 *    マージする実装なので、複数の移動元の中身が同じディレクトリに合流してしまい、
 *    どのセッションがどのプロジェクト由来かを区別できなくなる。
 * 2. 連鎖書き換え: ある move の to が、別の move の from そのものか、その配下
 *    (from + "/") にあると、Rewriter は全 move のルールを同時に文字列へ適用する
 *    ため、a.to へ書き換わった箇所がさらに b (from === a.to) のルールにも
 *    マッチして二重に置換される。結果は a.to でも b.to でもない壊れたパスになる。
 * 3. スラッグの衝突: slug() は英数字以外を全て 1 個の "-" に潰す非可逆な多対一
 *    写像 (例: "mc-data-catalog" と "mc_data_catalog" はどちらも
 *    "mc-data-catalog" になる)。見た目が違う 2 つの from (または to) が同じ
 *    スラッグに落ちると、~/.claude/projects 配下では同じディレクトリを指すため、
 *    本来別プロジェクトのセッション履歴が 1 つのディレクトリに混ざる。
 * 4. スラッグの接頭辞の曖昧さ: 完全一致ではないが、一方の from スラッグが
 *    他方の from スラッグの「子スラッグ」として解釈できてしまう場合
 *    (collectSlugPrefixAmbiguities 参照)。
 */
export function detectMoveCollisions(moves: Move[]): string[] {
  const warnings: string[] = [];

  const byTo = new Map<string, Move[]>();
  for (const m of moves) {
    const group = byTo.get(m.to) ?? [];
    group.push(m);
    byTo.set(m.to, group);
  }
  for (const [to, group] of byTo) {
    if (group.length > 1) {
      warnings.push(
        `${group.length} moves share the same destination "${to}": ${group.map((m) => m.from).join(", ")}`,
      );
    }
  }

  for (const a of moves) {
    for (const b of moves) {
      if (a === b) continue;
      if (b.from === a.to || b.from.startsWith(`${a.to}/`)) {
        warnings.push(
          `move to "${a.to}" collides with move from "${b.from}" (chained rewrite)`,
        );
      }
    }
  }

  warnings.push(...collectSlugCollisions(moves.map((m) => m.from), "move sources"));
  warnings.push(...collectSlugCollisions(moves.map((m) => m.to), "move destinations"));
  warnings.push(...collectSlugPrefixAmbiguities(moves.map((m) => m.from), "move sources"));

  return warnings;
}

export function parseMovesTsv(text: string): Move[] {
  const moves: Move[] = [];
  text.split("\n").forEach((line, i) => {
    if (line.trim() === "" || line.startsWith("#")) return;
    const cols = line.split("\t");
    if (cols.length !== 3) {
      throw new Error(`moves.tsv line ${i + 1}: expected exactly 3 tab-separated columns`);
    }
    moves.push({ from: cols[1], to: cols[2] });
  });
  return moves;
}
