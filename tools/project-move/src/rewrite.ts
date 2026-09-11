import type { Move } from "./moves.ts";
import { slug } from "./slug.ts";

export type Rewriter = {
  /** 置換対象を 1 つでも含むか。jsonl の行を JSON.parse するかの判定に使う。 */
  touches(s: string): boolean;
  rewriteText(s: string): string;
};

function escapeRegExp(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

/**
 * String.replace() の第 2 引数は置換パターンとして解釈され、
 * $&, $$, $1 などが特殊トークンになる。置換先に文字列 "$" が
 * 含まれていると壊れるため、$$ にエスケープする。
 */
function escapeReplacement(s: string): string {
  return s.replace(/\$/g, "$$$$");
}

/**
 * 旧パスと旧スラッグを新しいものに置き換える。
 *
 * 境界の扱いが 2 種類あるのは、パスとスラッグで「子が続く形」が違うため。
 *  - パス: /p/mc-gpt の次の文字が [A-Za-z0-9_.-] なら別物 (mc-gpt-old / mc-gpt.bak)。
 *    / や " や行末なら同じプロジェクトの配下。
 *  - スラッグ: 子は必ず - で続く (slug("/p/x/y") === slug("/p/x") + "-y") ので
 *    - の継続を許す。その代償として -mc-gpt と -mc-gpt-old は区別できないが、
 *    これはスラッグ変換が非可逆であることの帰結で、dry-run の衝突検査で拾う。
 *
 * 長いパスから先に処理するのは、/p/a と /p/a/b の両方が対象のとき前者が先に
 * 当たると後者が壊れるため。
 *
 * g フラグ付き正規表現を複数の文字列に使い回しても取りこぼさない。
 * String.replace() は毎回 lastIndex を 0 にリセットするため。
 * 注意: .test() や .exec() に変えると壊れるので注意。
 */
export function makeRewriter(moves: Move[]): Rewriter {
  const sorted = [...moves].sort((a, b) => b.from.length - a.from.length);

  const rules = sorted.flatMap((m) => [
    { re: new RegExp(`${escapeRegExp(m.from)}(?![A-Za-z0-9_.-])`, "g"), to: escapeReplacement(m.to) },
    { re: new RegExp(`${escapeRegExp(slug(m.from))}(?![A-Za-z0-9])`, "g"), to: escapeReplacement(slug(m.to)) },
  ]);

  const probes = sorted.flatMap((m) => [m.from, slug(m.from)]);

  return {
    touches: (s) => probes.some((p) => s.includes(p)),
    rewriteText: (s) => rules.reduce((acc, r) => acc.replace(r.re, r.to), s),
  };
}

/**
 * jsonl の 1 行を書き換える。
 *
 * まず touches() で弾くのは性能のため。~/.claude/projects は 538 MB あり、
 * 最大 36 MB の 1 ファイルもある。全行を JSON.parse するのは現実的でない。
 *
 * 文字列としてまるごと置換したうえで JSON として往復させるのは、cwd や
 * persistedOutputPath だけでなく会話本文中の絶対パスも揃えるため。
 * 「昔このパスだった」という歴史的記述も書き換わるが、実害はない。
 * 壊れた行は素通しする (移行で会話を落とさないことを優先する)。
 */
export function rewriteJsonlLine(line: string, r: Rewriter): string {
  if (!r.touches(line)) return line;
  const rewritten = r.rewriteText(line);
  try {
    JSON.parse(rewritten);
  } catch {
    return line;
  }
  return rewritten;
}

function isRecord(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}

/**
 * ~/.claude.json の .projects のキーと .githubRepoPaths の値を移す。
 *
 * .projects のキーを移さないと trust dialog が再表示され、allowedTools と
 * MCP の許可がリセットされる。移動先のキーが既にある場合は、移動先側を優先して
 * マージする (新しい場所で既に答えた許可を古い値で上書きしない)。
 */
export function rewriteClaudeJson(json: unknown, r: Rewriter): unknown {
  if (!isRecord(json)) return json;
  const out: Record<string, unknown> = { ...json };

  if (isRecord(out.projects)) {
    // 移動先のキーが元からあった場合は、そちらを優先して残す。移してきた側の値で
    // 上書きすると、新しい場所で既に答えた trust dialog や許可が巻き戻る。
    //
    // いったん「移された側」と「元からあった側」に分けてから重ねるのは、
    // Object.entries の反復順に結果を左右させないため。素朴に 1 回のループで
    // マージすると、旧キーが先に来たか後に来たかで勝つ側が変わる。
    const migrated: Record<string, unknown> = {};
    const native: Record<string, unknown> = {};
    for (const [key, value] of Object.entries(out.projects)) {
      const next = r.rewriteText(key);
      if (next === key) {
        native[key] = value;
        continue;
      }
      // 複数の旧キーが同じ新キーに畳まれることもありうる。その場合は先に見た方を
      // 優先する (どちらを採るべきかを決める材料が無いので、順序だけで決める)。
      const seen = migrated[next];
      migrated[next] =
        isRecord(seen) && isRecord(value) ? { ...value, ...seen } : (seen ?? value);
    }
    const projects: Record<string, unknown> = { ...migrated };
    for (const [key, value] of Object.entries(native)) {
      const existing = projects[key];
      projects[key] =
        isRecord(existing) && isRecord(value) ? { ...existing, ...value } : value;
    }
    out.projects = projects;
  }

  if (isRecord(out.githubRepoPaths)) {
    const repos: Record<string, unknown> = {};
    for (const [key, value] of Object.entries(out.githubRepoPaths)) {
      repos[key] = Array.isArray(value)
        ? [...new Set(value.map((p) => (typeof p === "string" ? r.rewriteText(p) : p)))]
        : value;
    }
    out.githubRepoPaths = repos;
  }

  return out;
}

/**
 * <slug>/sessions-index.json の 3 つのパスを移す。
 *
 * entries[].fullPath はスラッグ部分を含むので、ディレクトリ改名と揃える必要がある。
 * 実在しない jsonl を指すエントリがあっても落とさない (dominion-card-generator の
 * 5 エントリが実際にそうなっている)。
 */
export function rewriteSessionsIndex(json: unknown, r: Rewriter): unknown {
  if (!isRecord(json)) return json;
  const out: Record<string, unknown> = { ...json };

  if (typeof out.originalPath === "string") {
    out.originalPath = r.rewriteText(out.originalPath);
  }
  if (Array.isArray(out.entries)) {
    out.entries = out.entries.map((e) => {
      if (!isRecord(e)) return e;
      const next = { ...e };
      for (const key of ["projectPath", "fullPath"]) {
        if (typeof next[key] === "string") next[key] = r.rewriteText(next[key] as string);
      }
      return next;
    });
  }

  return out;
}
