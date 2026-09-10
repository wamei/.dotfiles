/** Claude Code がスラッグを打ち切る長さ。バイナリの qL と同じ。 */
export const SLUG_MAX_LENGTH = 200;

/**
 * 絶対パスを ~/.claude/projects 配下のディレクトリ名に変換する。
 *
 * Claude Code のバイナリの実装は `e.replace(/[^a-zA-Z0-9]/g, "-")` で、
 * 英数字以外の 1 文字が - 1 個になるだけ。連続 - は畳まれず、大文字小文字も残る。
 *
 * 200 文字を超えた場合の `slice(0,200) + "-" + base36(hash(path))` は実装しない。
 * この hash の算法を再現できておらず、推測で書くと静かに誤ったスラッグを作るため。
 * 現状の対象は全て 200 文字未満なので、落ちたらそのとき実装する。
 */
export function slug(absPath: string): string {
  const s = absPath.replace(/[^a-zA-Z0-9]/g, "-");
  if (s.length > SLUG_MAX_LENGTH) {
    throw new Error(
      `slug exceeds ${SLUG_MAX_LENGTH} chars and the hash suffix is not implemented: ${absPath}`,
    );
  }
  return s;
}

/**
 * dirName が parentSlug 自身か、その子のスラッグなら「残り」を返す。そうでなければ null。
 *
 * slug は 1 文字ずつの写像なので slug(parent + "/" + rest) === slug(parent) + "-" + slug(rest)
 * が成り立つ。これを使って worktree のような子ディレクトリを cwd に頼らず拾う。
 * 残りが "-" 始まりであることを要求しないと /a/b が /a/bc に過剰一致する。
 */
export function slugChildRemainder(dirName: string, parentSlug: string): string | null {
  if (dirName === parentSlug) return "";
  if (dirName.startsWith(parentSlug) && dirName[parentSlug.length] === "-") {
    return dirName.slice(parentSlug.length);
  }
  return null;
}
