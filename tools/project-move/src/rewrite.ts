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
 */
export function makeRewriter(moves: Move[]): Rewriter {
  const sorted = [...moves].sort((a, b) => b.from.length - a.from.length);

  const rules = sorted.flatMap((m) => [
    { re: new RegExp(`${escapeRegExp(m.from)}(?![A-Za-z0-9_.-])`, "g"), to: m.to },
    { re: new RegExp(`${escapeRegExp(slug(m.from))}(?![A-Za-z0-9])`, "g"), to: slug(m.to) },
  ]);

  const probes = sorted.flatMap((m) => [m.from, slug(m.from)]);

  return {
    touches: (s) => probes.some((p) => s.includes(p)),
    rewriteText: (s) => rules.reduce((acc, r) => acc.replace(r.re, r.to), s),
  };
}
