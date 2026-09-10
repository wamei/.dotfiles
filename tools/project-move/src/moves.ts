import { join } from "node:path";

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

export function formatMoveRow(move: Move, at: Date): string {
  return `${at.toISOString()}\t${move.from}\t${move.to}\n`;
}

export function parseMovesTsv(text: string): Move[] {
  const moves: Move[] = [];
  text.split("\n").forEach((line, i) => {
    if (line.trim() === "" || line.startsWith("#")) return;
    const cols = line.split("\t");
    if (cols.length < 3) {
      throw new Error(`moves.tsv line ${i + 1}: expected 3 tab-separated columns`);
    }
    moves.push({ from: cols[1], to: cols[2] });
  });
  return moves;
}
