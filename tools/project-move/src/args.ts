// tools/project-move/src/args.ts
//
// project-move と claude-state-move の両 CLI が共有する引数パーサ。
// 元々は claude-state-move.ts に単独で実装されていたが、project-move.ts の
// CLI も同じ形の値オプション (`--to`) を持つため、別々に持つと
// `args.filter(a => !a.startsWith("--"))` のような「-- で始まらないものは
// 全部位置引数」という壊れたパターンが再発しうる。実際に claude-state-move の
// レビューで、`--claude-home <path>` の <path> が positional に紛れ込み、
// 渡した <old> <new> が無視されて既定の moves ログ (実ホーム側!) に
// フォールバックする事故が見つかっている。1 箇所にまとめて両方から使う。
//
// 副作用 (console.error / process.exit) を持たない純関数として置く。エラーは
// 例外で表現し、終了コードを決めるのは呼び出し側 (各 CLI) の責務とする。

export type ParsedArgs = {
  /** 値を伴わない `--xxx` のうち、実際に渡されたものの集合 (例: "dry-run") */
  flags: Set<string>;
  /** 値を伴う `--xxx value` の対応表 */
  options: Record<string, string>;
  /** `--` で始まらないトークン (登場順を保持) */
  positional: string[];
};

/**
 * 引数を先頭から順に 1 パスで走査する。
 *
 * valueOptions に載っている `--foo` に当たったら次のトークンを値として明示的に
 * 消費して options に入れる。それ以外の `--` 始まりは flags に加えるだけで
 * 次のトークンには手を付けず、残りだけを positional として集める。
 * こうすることで、値を取るオプションの値が positional に紛れ込むことはない。
 *
 * 値が欠落している (次のトークンが無い、または次のトークンも `--` で始まる)
 * 場合は例外を投げる。後者を弾くのは、`--dry-run` のような別のフラグを
 * うっかり値として拾ってしまうのを防ぐため。
 */
export function parseArgs(argv: string[], valueOptions: Set<string>): ParsedArgs {
  const flags = new Set<string>();
  const options: Record<string, string> = {};
  const positional: string[] = [];

  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg.startsWith("--")) {
      const name = arg.slice(2);
      if (!valueOptions.has(name)) {
        flags.add(name);
        continue;
      }
      const value = argv[i + 1];
      if (value === undefined || value.startsWith("--")) {
        throw new Error(`missing value for ${arg}`);
      }
      options[name] = value;
      i++; // 値のトークンを消費
      continue;
    }
    positional.push(arg);
  }

  return { flags, options, positional };
}
