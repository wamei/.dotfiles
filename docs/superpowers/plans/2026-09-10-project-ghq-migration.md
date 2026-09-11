# ~/projects の ghq 再配置と Claude セッション追随 実装計画

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `~/projects` 直下の 43 ディレクトリを ghq レイアウト (と `~/projects/local/`) へ移し、移動先から `claude --resume` が移動前のセッションを引ける状態にする。移動と追随は再利用可能な 2 コマンドとして dotfiles に残す。

**Architecture:** `tools/project-move/` に TypeScript で 2 つの CLI を作る。`project-move` は Claude 起動中でも安全な作業 (ghq migrate / local への mv、`.claude/settings.local.json` と Emacs 状態の書き換え、移動ログ追記) を担い、`claude-state-move` は Claude 全停止中に `~/.claude` 配下 6 箇所を追随させる。純関数 (slug / rewrite / plan) と I/O を分離し、純関数側は実データのゴールデンでテストする。

**Tech Stack:** TypeScript, bun (`bun test`), ghq 1.10.1, mise 2026.9.1

**Spec:** `docs/superpowers/specs/2026-09-10-project-ghq-migration-design.md`

## Global Constraints

- **`slug(p)` は `p.replace(/[^a-zA-Z0-9]/g, "-")`。** 大文字小文字は保存、連続 `-` は畳まない。200 文字を超えたら**実装せず例外で落とす** (ハッシュ接尾辞の算法を再現できていないため)。
- **スラッグディレクトリの特定は `slug(old)` の接頭辞一致で行う。** 残りは空か `-` 始まりであること。`cwd` からスラッグを再計算してはならない (BeecoV2 の worktree は `cwd` に親を記録している)。
- **jsonl は行ストリームで処理する。** 全体をメモリに載せない。最大 36 MB のファイルがある。`JSON.parse` は旧パス文字列を含む行だけに絞る。
- **`<sessionId>/subagents/*.jsonl` も対象。** 最大のファイルはここにある。
- **`claude-state-move` は Claude プロセスが生きていたら拒否する。** `--force` は用意しない。`~/.claude.json` は起動中ずっと書き戻されている。
- **テストは本物のホームに触らない。** `--claude-home` / `--claude-json` / `--history` でパスを差し替え、tmpdir 上で実行する。
- **スラッグは必ず `-` 始まり。** 手順書のコマンドとテストのヘルパでは `./` を前置する (`jq` が `-U` と誤認した実績あり)。
- コメントは日本語で「なぜそうしたか」を書く。既存の `mise.toml` / `bin/` のコメント密度に合わせる。
- TDD (Red → Green)。各タスクの最後にコミットする。
- コミットメッセージは英語 (公開リポジトリ)。末尾に以下を付ける:

  ```
  Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01YBs8RTSs9X8GDPTeKFMvmF
  ```

## 実機で確認済みの事実 (再調査不要)

- `ghq migrate --dry-run <dir>` は成功時に `Would migrate <from> to <to>` を stdout に 1 行出し、exit 0。remote 無しは `failed to get remote URL: no remotes found`、非 git は `failed to detect VCS backend` で exit 1。
- BeecoV2 は linked worktree を持ち、`ghq migrate` は移動後に `git worktree repair` を自動実行する (dry-run が `Would run 'git worktree repair' to update linked worktrees` を出す)。**worktree の手当てを自前で書く必要はない。**
- `~/.claude.json` は 102 KB、`.projects` に 21 キー、`.githubRepoPaths` に 12 エントリ。
- `~/.claude/history.jsonl` は 1985 行。各行のキーは `display,pastedContents,project,sessionId,timestamp`。
- `sessions-index.json` のキーは `entries,originalPath,version`。`entries` は配列で、各要素のキーは `created,fileMtime,firstPrompt,fullPath,gitBranch,isSidechain,messageCount,modified,projectPath,sessionId,summary`。**`dominion-card-generator` の 5 エントリの `fullPath` はいずれも実在しない。**
- `~/.claude/projects` は 538 MB。最大は `waminder/.../subagents/agent-abe94141c1f8bff37.jsonl` の 36.2 MB。

## File Structure

```
tools/project-move/
├── package.json                  bun のプロジェクト宣言 (private, type: module)
├── fixtures/slugs.tsv            実データのゴールデン (path <TAB> slug、18 行)
├── src/
│   ├── slug.ts                   slug() と接頭辞判定。純関数
│   ├── slug.test.ts
│   ├── moves.ts                  Move 型と moves.tsv の読み書き。純関数
│   ├── moves.test.ts
│   ├── rewrite.ts                Rewriter と 3 つの書き換え関数。純関数
│   ├── rewrite.test.ts
│   ├── plan-move.ts              移動先の決定 (ghq / local)。ghq 呼び出しは注入
│   ├── plan-move.test.ts
│   ├── claude-state.ts           ~/.claude 配下 6 箇所の I/O オーケストレーション
│   ├── claude-state.test.ts      tmpdir に偽 ~/.claude を組む統合テスト
│   ├── project-move.ts           移動と手当ての I/O オーケストレーション
│   └── project-move.test.ts      tmpdir に偽リポジトリを組む統合テスト
└── cli/
    ├── project-move.ts           #!/usr/bin/env bun
    └── claude-state-move.ts      #!/usr/bin/env bun

mise.toml                         [dotfiles] に 2 本、[tasks.test] を追加
```

---

### Task 1: slug() と実データのゴールデン

**Files:**
- Create: `tools/project-move/package.json`
- Create: `tools/project-move/fixtures/slugs.tsv`
- Create: `tools/project-move/src/slug.ts`
- Test: `tools/project-move/src/slug.test.ts`
- Modify: `mise.toml` (`[tasks.test]` を追加)

**Interfaces:**
- Consumes: なし
- Produces: `slug(absPath: string): string`、`SLUG_MAX_LENGTH: 200`、`slugChildRemainder(dirName: string, parentSlug: string): string | null`

- [ ] **Step 1: package.json を作る**

```json
{
  "name": "project-move",
  "private": true,
  "type": "module"
}
```

- [ ] **Step 2: ゴールデン fixture を置く**

`tools/project-move/fixtures/slugs.tsv` (タブ区切り、`#` 始まりはコメント)。
実機の `~/.claude/projects/` 全 18 ディレクトリから採取した対応表。

```
# 実パス <TAB> Claude Code が作るスラッグ。実機の ~/.claude/projects から採取した。
# BeecoV2 の worktree 行は jsonl の cwd (親を指す) ではなく worktree の実パスである
# ことに注意。cwd からはこのディレクトリ名を導けない。
/Users/wamei/.dotfiles	-Users-wamei--dotfiles
/Users/wamei/Documents/Dominion	-Users-wamei-Documents-Dominion
/Users/wamei/projects/BeecoV2	-Users-wamei-projects-BeecoV2
/Users/wamei/projects/BeecoV2/.claude-worktrees/pr1054-test-fix	-Users-wamei-projects-BeecoV2--claude-worktrees-pr1054-test-fix
/Users/wamei/projects/apple-reminder-sync	-Users-wamei-projects-apple-reminder-sync
/Users/wamei/projects/dominion-card-generator	-Users-wamei-projects-dominion-card-generator
/Users/wamei/projects/github.com/wamei/.dotfiles	-Users-wamei-projects-github-com-wamei--dotfiles
/Users/wamei/projects/hama-reminder	-Users-wamei-projects-hama-reminder
/Users/wamei/projects/ietateru	-Users-wamei-projects-ietateru
/Users/wamei/projects/ImportApps	-Users-wamei-projects-ImportApps
/Users/wamei/projects/mc-data-catalog	-Users-wamei-projects-mc-data-catalog
/Users/wamei/projects/mc-gpt	-Users-wamei-projects-mc-gpt
/Users/wamei/projects/mcwf-api	-Users-wamei-projects-mcwf-api
/Users/wamei/projects/mcwf-web	-Users-wamei-projects-mcwf-web
/Users/wamei/projects/rit-dev-book	-Users-wamei-projects-rit-dev-book
/Users/wamei/projects/rither	-Users-wamei-projects-rither
/Users/wamei/projects/SDXFW_TEMPLATE	-Users-wamei-projects-SDXFW-TEMPLATE
/Users/wamei/projects/waminder	-Users-wamei-projects-waminder
```

- [ ] **Step 3: 失敗するテストを書く**

```ts
// tools/project-move/src/slug.test.ts
import { expect, test } from "bun:test";
import { readFileSync } from "node:fs";
import { slug, slugChildRemainder, SLUG_MAX_LENGTH } from "./slug.ts";

const golden = readFileSync(new URL("../fixtures/slugs.tsv", import.meta.url), "utf8")
  .split("\n")
  .filter((l) => l.trim() !== "" && !l.startsWith("#"))
  .map((l) => l.split("\t") as [string, string]);

test("実データ 18 件のスラッグを全件再現する", () => {
  expect(golden.length).toBe(18);
  for (const [path, expected] of golden) {
    expect(slug(path)).toBe(expected);
  }
});

test("英数字以外は 1 文字ずつ - になり、連続 - は畳まれない", () => {
  expect(slug("/a/.b_c.d")).toBe("-a--b-c-d");
});

test("大文字小文字は保存する", () => {
  expect(slug("/BeecoV2")).toBe("-BeecoV2");
});

test("slug は 1 文字ずつの写像なので子パスは親スラッグの接頭辞を持つ", () => {
  const parent = "/Users/wamei/projects/BeecoV2";
  const child = "/Users/wamei/projects/BeecoV2/.claude-worktrees/pr1054-test-fix";
  expect(slug(child).startsWith(slug(parent))).toBe(true);
});

test("200 文字を超えたら例外で落ちる (ハッシュ接尾辞は実装しない)", () => {
  const long = "/" + "a".repeat(SLUG_MAX_LENGTH);
  expect(() => slug(long)).toThrow(/200/);
});

test("slugChildRemainder: 完全一致は空文字を返す", () => {
  expect(slugChildRemainder("-a-b", "-a-b")).toBe("");
});

test("slugChildRemainder: 子は - 始まりの残りを返す", () => {
  expect(slugChildRemainder("-a-b--c", "-a-b")).toBe("--c");
});

test("slugChildRemainder: /a/bc は /a/b に過剰一致しない", () => {
  expect(slugChildRemainder(slug("/a/bc"), slug("/a/b"))).toBeNull();
});
```

- [ ] **Step 4: テストが失敗することを確認する**

Run: `cd tools/project-move && bun test src/slug.test.ts`
Expected: FAIL — `Cannot find module './slug.ts'`

- [ ] **Step 5: 最小の実装を書く**

```ts
// tools/project-move/src/slug.ts

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
```

- [ ] **Step 6: テストが通ることを確認する**

Run: `cd tools/project-move && bun test src/slug.test.ts`
Expected: PASS (8 tests)

- [ ] **Step 7: mise の test タスクを足す**

`mise.toml` の末尾に追加する。

```toml
# ---------------------------------------------------------------------------
# tools/ のユニットテスト
# ---------------------------------------------------------------------------
[tasks.test]
description = "tools/ のユニットテストを走らせる"
dir = "tools/project-move"
run = ["bun test"]
```

- [ ] **Step 8: mise 経由でも通ることを確認する**

Run: `mise run test`
Expected: PASS

- [ ] **Step 9: コミット**

```bash
git add tools/project-move/package.json tools/project-move/fixtures/slugs.tsv \
        tools/project-move/src/slug.ts tools/project-move/src/slug.test.ts mise.toml
git commit -m "Reproduce Claude's project slug from the real directory names"
```

---

### Task 2: 移動ログ (moves.tsv) の読み書き

**Files:**
- Create: `tools/project-move/src/moves.ts`
- Test: `tools/project-move/src/moves.test.ts`

**Interfaces:**
- Consumes: なし
- Produces: `type Move = { from: string; to: string }`、`parseMovesTsv(text: string): Move[]`、`formatMoveRow(move: Move, at: Date): string`、`defaultMovesLogPath(home: string): string`

- [ ] **Step 1: 失敗するテストを書く**

```ts
// tools/project-move/src/moves.test.ts
import { expect, test } from "bun:test";
import { defaultMovesLogPath, formatMoveRow, parseMovesTsv } from "./moves.ts";

test("3 列 TSV を読む", () => {
  const text = [
    "# timestamp\tfrom\tto",
    "2026-09-10T00:00:00.000Z\t/Users/w/projects/a\t/Users/w/projects/github.com/o/a",
    "",
  ].join("\n");
  expect(parseMovesTsv(text)).toEqual([
    { from: "/Users/w/projects/a", to: "/Users/w/projects/github.com/o/a" },
  ]);
});

test("空行とコメント行を読み飛ばす", () => {
  expect(parseMovesTsv("\n# c\n\n")).toEqual([]);
});

test("列が足りない行は行番号付きで落とす", () => {
  expect(() => parseMovesTsv("2026-09-10T00:00:00.000Z\t/only-one")).toThrow(/line 1/);
});

test("行の書式は timestamp/from/to のタブ区切り", () => {
  const at = new Date("2026-09-10T12:34:56.000Z");
  expect(formatMoveRow({ from: "/a", to: "/b" }, at)).toBe(
    "2026-09-10T12:34:56.000Z\t/a\t/b\n",
  );
});

test("ログの既定位置は XDG state 配下", () => {
  expect(defaultMovesLogPath("/Users/w")).toBe(
    "/Users/w/.local/state/project-move/moves.tsv",
  );
});
```

- [ ] **Step 2: テストが失敗することを確認する**

Run: `cd tools/project-move && bun test src/moves.test.ts`
Expected: FAIL — `Cannot find module './moves.ts'`

- [ ] **Step 3: 実装を書く**

```ts
// tools/project-move/src/moves.ts
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
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd tools/project-move && bun test src/moves.test.ts`
Expected: PASS (5 tests)

- [ ] **Step 5: コミット**

```bash
git add tools/project-move/src/moves.ts tools/project-move/src/moves.test.ts
git commit -m "Carry moves between the two commands through a tsv log"
```

---

### Task 3: Rewriter — パス境界を守る文字列置換

**Files:**
- Create: `tools/project-move/src/rewrite.ts`
- Test: `tools/project-move/src/rewrite.test.ts`

**Interfaces:**
- Consumes: `Move` (Task 2)、`slug` (Task 1)
- Produces: `type Rewriter = { touches(s: string): boolean; rewriteText(s: string): string }`、`makeRewriter(moves: Move[]): Rewriter`

- [ ] **Step 1: 失敗するテストを書く**

```ts
// tools/project-move/src/rewrite.test.ts
import { expect, test } from "bun:test";
import { makeRewriter } from "./rewrite.ts";

const r = makeRewriter([
  { from: "/Users/w/projects/mc-gpt", to: "/Users/w/projects/github.com/o/mc-gpt" },
]);

test("パスを置換する", () => {
  expect(r.rewriteText('"cwd":"/Users/w/projects/mc-gpt"')).toBe(
    '"cwd":"/Users/w/projects/github.com/o/mc-gpt"',
  );
});

test("配下のパスも置換する", () => {
  expect(r.rewriteText("/Users/w/projects/mc-gpt/src/a.ts")).toBe(
    "/Users/w/projects/github.com/o/mc-gpt/src/a.ts",
  );
});

test("別プロジェクトへの過剰一致をしない", () => {
  expect(r.rewriteText("/Users/w/projects/mc-gpt-old/a")).toBe(
    "/Users/w/projects/mc-gpt-old/a",
  );
  expect(r.rewriteText("/Users/w/projects/mc-gpt.bak")).toBe(
    "/Users/w/projects/mc-gpt.bak",
  );
});

test("スラッグも置換する (persistedOutputPath 用)", () => {
  expect(
    r.rewriteText("/Users/w/.claude/projects/-Users-w-projects-mc-gpt/x.jsonl"),
  ).toBe("/Users/w/.claude/projects/-Users-w-projects-github-com-o-mc-gpt/x.jsonl");
});

test("子スラッグ (worktree) も置換する", () => {
  expect(r.rewriteText("-Users-w-projects-mc-gpt--claude-worktrees-foo")).toBe(
    "-Users-w-projects-github-com-o-mc-gpt--claude-worktrees-foo",
  );
});

test("touches は旧パスも旧スラッグも含まない文字列に false", () => {
  expect(r.touches("no paths here")).toBe(false);
  expect(r.touches("/Users/w/projects/mc-gpt")).toBe(true);
  expect(r.touches("-Users-w-projects-mc-gpt")).toBe(true);
});

test("長いパスを先に処理して部分置換を防ぐ", () => {
  const r2 = makeRewriter([
    { from: "/p/a", to: "/x/a" },
    { from: "/p/a/b", to: "/y/b" },
  ]);
  expect(r2.rewriteText("/p/a/b/c")).toBe("/y/b/c");
});
```

- [ ] **Step 2: テストが失敗することを確認する**

Run: `cd tools/project-move && bun test src/rewrite.test.ts`
Expected: FAIL — `Cannot find module './rewrite.ts'`

- [ ] **Step 3: 実装を書く**

```ts
// tools/project-move/src/rewrite.ts
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
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd tools/project-move && bun test src/rewrite.test.ts`
Expected: PASS (7 tests)

- [ ] **Step 5: コミット**

```bash
git add tools/project-move/src/rewrite.ts tools/project-move/src/rewrite.test.ts
git commit -m "Replace old paths and slugs without spilling into their neighbours"
```

---

### Task 4: JSON 構造の書き換え 3 種

**Files:**
- Modify: `tools/project-move/src/rewrite.ts`
- Modify: `tools/project-move/src/rewrite.test.ts`

**Interfaces:**
- Consumes: `Rewriter` (Task 3)
- Produces: `rewriteJsonlLine(line: string, r: Rewriter): string`、`rewriteClaudeJson(json: unknown, r: Rewriter): unknown`、`rewriteSessionsIndex(json: unknown, r: Rewriter): unknown`

- [ ] **Step 1: 失敗するテストを追記する**

```ts
// tools/project-move/src/rewrite.test.ts に追記
import { rewriteClaudeJson, rewriteJsonlLine, rewriteSessionsIndex } from "./rewrite.ts";

test("jsonl: 対象を含まない行は 1 文字も変えずに返す", () => {
  const line = '{"type":"user","cwd":"/other/place"}';
  expect(rewriteJsonlLine(line, r)).toBe(line);
});

test("jsonl: cwd と persistedOutputPath を書き換える", () => {
  const line = JSON.stringify({
    cwd: "/Users/w/projects/mc-gpt",
    toolUseResult: {
      persistedOutputPath: "/Users/w/.claude/projects/-Users-w-projects-mc-gpt/a.txt",
    },
  });
  const out = JSON.parse(rewriteJsonlLine(line, r));
  expect(out.cwd).toBe("/Users/w/projects/github.com/o/mc-gpt");
  expect(out.toolUseResult.persistedOutputPath).toBe(
    "/Users/w/.claude/projects/-Users-w-projects-github-com-o-mc-gpt/a.txt",
  );
});

test("jsonl: 壊れた行はそのまま素通しする (移行で会話を失わない)", () => {
  expect(rewriteJsonlLine("{not json/Users/w/projects/mc-gpt", r)).toBe(
    "{not json/Users/w/projects/mc-gpt",
  );
});

test("claude.json: projects のキーをリネームする", () => {
  const out = rewriteClaudeJson(
    { projects: { "/Users/w/projects/mc-gpt": { allowedTools: ["a"] } }, other: 1 },
    r,
  ) as any;
  expect(Object.keys(out.projects)).toEqual(["/Users/w/projects/github.com/o/mc-gpt"]);
  expect(out.projects["/Users/w/projects/github.com/o/mc-gpt"].allowedTools).toEqual(["a"]);
  expect(out.other).toBe(1);
});

test("claude.json: 移動先のキーが既にあれば既存値を優先して残す", () => {
  const out = rewriteClaudeJson(
    {
      projects: {
        "/Users/w/projects/mc-gpt": { allowedTools: ["old"], onlyOld: true },
        "/Users/w/projects/github.com/o/mc-gpt": { allowedTools: ["new"] },
      },
    },
    r,
  ) as any;
  const merged = out.projects["/Users/w/projects/github.com/o/mc-gpt"];
  expect(merged.allowedTools).toEqual(["new"]);
  expect(merged.onlyOld).toBe(true);
});

test("claude.json: githubRepoPaths の値を置換し重複を畳む", () => {
  const out = rewriteClaudeJson(
    {
      githubRepoPaths: {
        "o/mc-gpt": [
          "/Users/w/projects/mc-gpt",
          "/Users/w/projects/github.com/o/mc-gpt",
        ],
      },
    },
    r,
  ) as any;
  expect(out.githubRepoPaths["o/mc-gpt"]).toEqual([
    "/Users/w/projects/github.com/o/mc-gpt",
  ]);
});

test("sessions-index: originalPath と entries の 2 つのパスを置換する", () => {
  const out = rewriteSessionsIndex(
    {
      version: 1,
      originalPath: "/Users/w/projects/mc-gpt",
      entries: [
        {
          sessionId: "s1",
          projectPath: "/Users/w/projects/mc-gpt",
          fullPath: "/Users/w/.claude/projects/-Users-w-projects-mc-gpt/s1.jsonl",
        },
      ],
    },
    r,
  ) as any;
  expect(out.originalPath).toBe("/Users/w/projects/github.com/o/mc-gpt");
  expect(out.entries[0].projectPath).toBe("/Users/w/projects/github.com/o/mc-gpt");
  expect(out.entries[0].fullPath).toBe(
    "/Users/w/.claude/projects/-Users-w-projects-github-com-o-mc-gpt/s1.jsonl",
  );
});

test("sessions-index: entries が無くても落ちない", () => {
  expect(rewriteSessionsIndex({ version: 1 }, r)).toEqual({ version: 1 });
});
```

- [ ] **Step 2: テストが失敗することを確認する**

Run: `cd tools/project-move && bun test src/rewrite.test.ts`
Expected: FAIL — `rewriteJsonlLine is not a function`

- [ ] **Step 3: 実装を追記する**

```ts
// tools/project-move/src/rewrite.ts に追記

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
    const projects: Record<string, unknown> = {};
    for (const [key, value] of Object.entries(out.projects)) {
      const next = r.rewriteText(key);
      const existing = projects[next];
      projects[next] =
        isRecord(existing) && isRecord(value) ? { ...value, ...existing } : (existing ?? value);
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
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd tools/project-move && bun test src/rewrite.test.ts`
Expected: PASS (15 tests)

- [ ] **Step 5: コミット**

```bash
git add tools/project-move/src/rewrite.ts tools/project-move/src/rewrite.test.ts
git commit -m "Rewrite the three json shapes that key on the project path"
```

---

### Task 5: claude-state-move の I/O — 改名と書き換え

**Files:**
- Create: `tools/project-move/src/claude-state.ts`
- Test: `tools/project-move/src/claude-state.test.ts`

**Interfaces:**
- Consumes: `Move` (Task 2)、`makeRewriter` / `rewriteJsonlLine` / `rewriteClaudeJson` / `rewriteSessionsIndex` (Task 3, 4)、`slug` / `slugChildRemainder` (Task 1)
- Produces:

```ts
export type ClaudeStatePaths = { claudeHome: string; claudeJson: string; historyJsonl: string };
export type ClaudeStateReport = {
  renames: { from: string; to: string; merged: boolean }[];
  rewrittenFiles: { path: string; changedLines: number }[];
  claudeJsonKeys: { from: string; to: string }[];
  warnings: string[];
};
export async function claudeStateMove(
  moves: Move[], paths: ClaudeStatePaths, opts: { dryRun: boolean },
): Promise<ClaudeStateReport>;
```

- [ ] **Step 1: 失敗するテストを書く**

```ts
// tools/project-move/src/claude-state.test.ts
import { afterEach, beforeEach, expect, test } from "bun:test";
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync, existsSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { claudeStateMove, type ClaudeStatePaths } from "./claude-state.ts";

let root: string;
let paths: ClaudeStatePaths;
const OLD = "/Users/w/projects/mc-gpt";
const NEW = "/Users/w/projects/github.com/o/mc-gpt";
const OLD_SLUG = "-Users-w-projects-mc-gpt";
const NEW_SLUG = "-Users-w-projects-github-com-o-mc-gpt";

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "claude-state-"));
  paths = {
    claudeHome: join(root, ".claude"),
    claudeJson: join(root, ".claude.json"),
    historyJsonl: join(root, ".claude", "history.jsonl"),
  };
  const projDir = join(paths.claudeHome, "projects", OLD_SLUG);
  mkdirSync(join(projDir, "s1", "subagents"), { recursive: true });
  writeFileSync(join(projDir, "s1.jsonl"), `${JSON.stringify({ cwd: OLD })}\n`);
  writeFileSync(
    join(projDir, "s1", "subagents", "agent-a.jsonl"),
    `${JSON.stringify({ cwd: OLD })}\n${JSON.stringify({ cwd: "/elsewhere" })}\n`,
  );
  writeFileSync(
    join(projDir, "sessions-index.json"),
    JSON.stringify({ version: 1, originalPath: OLD, entries: [] }),
  );
  // worktree の子スラッグ。cwd は親を指すので、cwd からは名前を導けない
  mkdirSync(join(paths.claudeHome, "projects", `${OLD_SLUG}--claude-worktrees-wt`), {
    recursive: true,
  });
  writeFileSync(paths.claudeJson, JSON.stringify({ projects: { [OLD]: { allowedTools: [] } } }));
  writeFileSync(paths.historyJsonl, `${JSON.stringify({ display: "x", project: OLD })}\n`);
});
afterEach(() => rmSync(root, { recursive: true, force: true }));

const moves = [{ from: OLD, to: NEW }];

test("dry-run は何も書かず、やることだけを報告する", async () => {
  const report = await claudeStateMove(moves, paths, { dryRun: true });
  expect(report.renames.map((r) => r.to)).toContain(NEW_SLUG);
  expect(existsSync(join(paths.claudeHome, "projects", OLD_SLUG))).toBe(true);
  expect(existsSync(join(paths.claudeHome, "projects", NEW_SLUG))).toBe(false);
});

test("スラッグディレクトリを改名する", async () => {
  await claudeStateMove(moves, paths, { dryRun: false });
  expect(existsSync(join(paths.claudeHome, "projects", NEW_SLUG))).toBe(true);
  expect(existsSync(join(paths.claudeHome, "projects", OLD_SLUG))).toBe(false);
});

test("worktree の子スラッグも接頭辞一致で連れていく", async () => {
  await claudeStateMove(moves, paths, { dryRun: false });
  expect(
    existsSync(join(paths.claudeHome, "projects", `${NEW_SLUG}--claude-worktrees-wt`)),
  ).toBe(true);
});

test("subagents 配下の jsonl も書き換える", async () => {
  await claudeStateMove(moves, paths, { dryRun: false });
  const p = join(paths.claudeHome, "projects", NEW_SLUG, "s1", "subagents", "agent-a.jsonl");
  const lines = readFileSync(p, "utf8").trim().split("\n").map((l) => JSON.parse(l));
  expect(lines[0].cwd).toBe(NEW);
  expect(lines[1].cwd).toBe("/elsewhere");
});

test("sessions-index.json / claude.json / history.jsonl を書き換える", async () => {
  await claudeStateMove(moves, paths, { dryRun: false });
  const idx = JSON.parse(
    readFileSync(join(paths.claudeHome, "projects", NEW_SLUG, "sessions-index.json"), "utf8"),
  );
  expect(idx.originalPath).toBe(NEW);
  expect(Object.keys(JSON.parse(readFileSync(paths.claudeJson, "utf8")).projects)).toEqual([NEW]);
  expect(JSON.parse(readFileSync(paths.historyJsonl, "utf8").trim()).project).toBe(NEW);
});

test("移動先のスラッグが既にあればマージし、merged を立てる", async () => {
  const dest = join(paths.claudeHome, "projects", NEW_SLUG);
  mkdirSync(dest, { recursive: true });
  writeFileSync(join(dest, "existing.jsonl"), `${JSON.stringify({ cwd: NEW })}\n`);
  const report = await claudeStateMove(moves, paths, { dryRun: false });
  expect(report.renames.find((r) => r.to === NEW_SLUG)?.merged).toBe(true);
  expect(existsSync(join(dest, "existing.jsonl"))).toBe(true);
  expect(existsSync(join(dest, "s1.jsonl"))).toBe(true);
});

test("実行前にバックアップを取る", async () => {
  await claudeStateMove(moves, paths, { dryRun: false });
  expect(existsSync(`${paths.claudeJson}.project-move-backup`)).toBe(true);
});
```

- [ ] **Step 2: テストが失敗することを確認する**

Run: `cd tools/project-move && bun test src/claude-state.test.ts`
Expected: FAIL — `Cannot find module './claude-state.ts'`

- [ ] **Step 3: 実装を書く**

```ts
// tools/project-move/src/claude-state.ts
import { createReadStream, existsSync } from "node:fs";
import { copyFile, mkdir, readdir, readFile, rename, writeFile } from "node:fs/promises";
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
  return true;
}

/** 行ストリームで書き換える。全体をメモリに載せない (最大 36 MB のファイルがある)。 */
async function rewriteLines(
  path: string,
  r: Rewriter,
  transform: (line: string, r: Rewriter) => string,
  dryRun: boolean,
): Promise<number> {
  const out: string[] = [];
  let changed = 0;
  const rl = createInterface({ input: createReadStream(path), crlfDelay: Infinity });
  for await (const line of rl) {
    const next = transform(line, r);
    if (next !== line) changed++;
    out.push(next);
  }
  if (changed > 0 && !dryRun) await writeFile(path, `${out.join("\n")}\n`);
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

  // 2) 改名後のディレクトリの中身を書き換える
  for (const { to } of report.renames) {
    const dir = join(projectsDir, to);
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
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd tools/project-move && bun test src/claude-state.test.ts`
Expected: PASS (7 tests)

- [ ] **Step 5: コミット**

```bash
git add tools/project-move/src/claude-state.ts tools/project-move/src/claude-state.test.ts
git commit -m "Move Claude's per-project state by slug prefix, streaming the transcripts"
```

---

### Task 6: claude-state-move の CLI とプロセス検査

**Files:**
- Create: `tools/project-move/cli/claude-state-move.ts`
- Modify: `tools/project-move/src/claude-state.ts` (`isClaudeRunning` を追加)
- Modify: `tools/project-move/src/claude-state.test.ts`

**Interfaces:**
- Consumes: `claudeStateMove` (Task 5)、`parseMovesTsv` / `defaultMovesLogPath` (Task 2)
- Produces: `isClaudeRunning(exec?: (cmd: string, args: string[]) => number): boolean`、CLI 実行ファイル

- [ ] **Step 1: 失敗するテストを追記する**

```ts
// tools/project-move/src/claude-state.test.ts に追記
import { isClaudeRunning } from "./claude-state.ts";

test("pgrep が 0 を返したら起動中と判定する", () => {
  expect(isClaudeRunning(() => 0)).toBe(true);
});

test("pgrep が 1 を返したら停止中と判定する", () => {
  expect(isClaudeRunning(() => 1)).toBe(false);
});
```

- [ ] **Step 2: テストが失敗することを確認する**

Run: `cd tools/project-move && bun test src/claude-state.test.ts`
Expected: FAIL — `isClaudeRunning is not a function`

- [ ] **Step 3: 実装を追記する**

```ts
// tools/project-move/src/claude-state.ts に追記
import { spawnSync } from "node:child_process";

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
```

- [ ] **Step 4: CLI を書く**

```ts
#!/usr/bin/env bun
// tools/project-move/cli/claude-state-move.ts
//
// Claude を全部落としてから叩く。project-move が残した移動ログを読み、
// ~/.claude 配下 6 箇所を新しいパスに追随させる。
import { homedir } from "node:os";
import { join } from "node:path";
import { readFile } from "node:fs/promises";
import { existsSync } from "node:fs";
import { claudeStateMove, isClaudeRunning } from "../src/claude-state.ts";
import { defaultMovesLogPath, parseMovesTsv, type Move } from "../src/moves.ts";

const args = process.argv.slice(2);
const dryRun = args.includes("--dry-run");
const positional = args.filter((a) => !a.startsWith("--"));

function optionValue(name: string, fallback: string): string {
  const i = args.indexOf(`--${name}`);
  return i >= 0 && args[i + 1] ? args[i + 1] : fallback;
}

const home = homedir();
const paths = {
  claudeHome: optionValue("claude-home", join(home, ".claude")),
  claudeJson: optionValue("claude-json", join(home, ".claude.json")),
  historyJsonl: optionValue("history", join(home, ".claude", "history.jsonl")),
};

let moves: Move[];
if (positional.length === 2) {
  moves = [{ from: positional[0], to: positional[1] }];
} else {
  const log = optionValue("from-log", defaultMovesLogPath(home));
  if (!existsSync(log)) {
    console.error(`no moves log at ${log}; pass <old> <new> instead`);
    process.exit(2);
  }
  moves = parseMovesTsv(await readFile(log, "utf8"));
}

if (!dryRun && isClaudeRunning()) {
  console.error(
    "claude is running. ~/.claude.json is rewritten continuously while it runs,\n" +
      "so this would be overwritten. quit every claude session and retry.",
  );
  process.exit(1);
}

const report = await claudeStateMove(moves, paths, { dryRun });

console.log(dryRun ? "--- dry run ---" : "--- applied ---");
for (const rn of report.renames) {
  console.log(`rename  ${rn.from} -> ${rn.to}${rn.merged ? " (merged into existing)" : ""}`);
}
for (const f of report.rewrittenFiles) {
  console.log(`rewrite ${f.path} (${f.changedLines} lines)`);
}
for (const k of report.claudeJsonKeys) {
  console.log(`key     ${k.from} -> ${k.to}`);
}
for (const w of report.warnings) console.log(`warn    ${w}`);
```

- [ ] **Step 5: 実行権を与えてテストが通ることを確認する**

```bash
chmod +x tools/project-move/cli/claude-state-move.ts
cd tools/project-move && bun test src/claude-state.test.ts
```
Expected: PASS (9 tests)

- [ ] **Step 6: dry-run が本物のホームに対して落ちないことを確認する**

Run: `./tools/project-move/cli/claude-state-move.ts --dry-run /Users/wamei/projects/SDXFW_TEMPLATE /Users/wamei/projects/github.com/RIT-Inc-Dev/SDXFW_TEMPLATE`
Expected: `rename  -Users-wamei-projects-SDXFW-TEMPLATE -> -Users-wamei-projects-github-com-RIT-Inc-Dev-SDXFW-TEMPLATE` を含む出力。**ファイルは 1 つも変わらない。**

- [ ] **Step 7: コミット**

```bash
git add tools/project-move/cli/claude-state-move.ts tools/project-move/src/claude-state.ts \
        tools/project-move/src/claude-state.test.ts
git commit -m "Refuse to rewrite Claude state while Claude is running"
```

---

### Task 7: 移動先の決定 (planMove)

**Files:**
- Create: `tools/project-move/src/plan-move.ts`
- Test: `tools/project-move/src/plan-move.test.ts`

**Interfaces:**
- Consumes: なし
- Produces:

```ts
export type MovePlan = { kind: "ghq" | "local"; from: string; to: string };
export type GhqProbe = (dir: string) => { ok: true; to: string } | { ok: false; reason: string };
export function planMove(dir: string, opts: { probe: GhqProbe; localRoot: string }): MovePlan;
export function ghqProbe(dir: string): ReturnType<GhqProbe>;
```

- [ ] **Step 1: 失敗するテストを書く**

```ts
// tools/project-move/src/plan-move.test.ts
import { expect, test } from "bun:test";
import { planMove } from "./plan-move.ts";

const localRoot = "/Users/w/projects/local";

test("remote があれば ghq が決めた移動先を使う", () => {
  const probe = () => ({ ok: true as const, to: "/Users/w/projects/github.com/o/a" });
  expect(planMove("/Users/w/projects/a", { probe, localRoot })).toEqual({
    kind: "ghq",
    from: "/Users/w/projects/a",
    to: "/Users/w/projects/github.com/o/a",
  });
});

test("remote が無ければ local へ", () => {
  const probe = () => ({ ok: false as const, reason: "no remotes found" });
  expect(planMove("/Users/w/projects/clock", { probe, localRoot })).toEqual({
    kind: "local",
    from: "/Users/w/projects/clock",
    to: "/Users/w/projects/local/clock",
  });
});

test("非 git でも local へ", () => {
  const probe = () => ({ ok: false as const, reason: "failed to detect VCS backend" });
  expect(planMove("/Users/w/projects/pytest", { probe, localRoot })).toEqual({
    kind: "local",
    from: "/Users/w/projects/pytest",
    to: "/Users/w/projects/local/pytest",
  });
});

test("末尾スラッシュを落としてから名前を取る", () => {
  const probe = () => ({ ok: false as const, reason: "no remotes found" });
  expect(planMove("/Users/w/projects/clock/", { probe, localRoot }).to).toBe(
    "/Users/w/projects/local/clock",
  );
});
```

- [ ] **Step 2: テストが失敗することを確認する**

Run: `cd tools/project-move && bun test src/plan-move.test.ts`
Expected: FAIL — `Cannot find module './plan-move.ts'`

- [ ] **Step 3: 実装を書く**

```ts
// tools/project-move/src/plan-move.ts
import { spawnSync } from "node:child_process";
import { basename, join } from "node:path";

export type MovePlan = { kind: "ghq" | "local"; from: string; to: string };
export type GhqProbeResult = { ok: true; to: string } | { ok: false; reason: string };
export type GhqProbe = (dir: string) => GhqProbeResult;

/**
 * ghq に移動先を訊く。
 *
 * 移動先を自分で組み立てないのは、host/owner/repo の導出 (SSH 別名ホスト
 * github.com.linka や gitlab の入れ子グループ) を ghq と食い違わせないため。
 * 成功時は `Would migrate <from> to <to>` を 1 行返し、remote 無しは
 * `no remotes found`、非 git は `failed to detect VCS backend` で exit 1。
 */
export function ghqProbe(dir: string): GhqProbeResult {
  const r = spawnSync("ghq", ["migrate", "--dry-run", dir], { encoding: "utf8" });
  const m = (r.stdout ?? "").match(/^Would migrate .+ to (.+)$/m);
  if (m) return { ok: true, to: m[1].trim() };
  return { ok: false, reason: ((r.stderr ?? "") + (r.stdout ?? "")).trim() };
}

export function planMove(
  dir: string,
  opts: { probe: GhqProbe; localRoot: string },
): MovePlan {
  const from = dir.replace(/\/+$/, "");
  const probed = opts.probe(from);
  return probed.ok
    ? { kind: "ghq", from, to: probed.to }
    : { kind: "local", from, to: join(opts.localRoot, basename(from)) };
}
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd tools/project-move && bun test src/plan-move.test.ts`
Expected: PASS (4 tests)

- [ ] **Step 5: 実物の ghq と食い違わないことを確認する**

Run: `bun -e 'import {ghqProbe} from "./tools/project-move/src/plan-move.ts"; console.log(ghqProbe("/Users/wamei/projects/mcwf-api"), ghqProbe("/Users/wamei/projects/clock"))'`
Expected: 1 つ目が `{ ok: true, to: "/Users/wamei/projects/github.com/RIT-Inc-Dev/mcwf-api" }`、2 つ目が `ok: false`

- [ ] **Step 6: コミット**

```bash
git add tools/project-move/src/plan-move.ts tools/project-move/src/plan-move.test.ts
git commit -m "Ask ghq where a repository belongs instead of deriving it"
```

---

### Task 8: project-move の手当てと CLI

**Files:**
- Create: `tools/project-move/src/project-move.ts`
- Create: `tools/project-move/cli/project-move.ts`
- Test: `tools/project-move/src/project-move.test.ts`

**Interfaces:**
- Consumes: `planMove` / `ghqProbe` (Task 7)、`makeRewriter` (Task 3)、`formatMoveRow` / `defaultMovesLogPath` (Task 2)
- Produces:

```ts
export type FixupReport = {
  rewrittenFiles: { path: string; changedLines: number }[];
  manualSteps: string[];
  warnings: string[];
};
export function isEmacsRunning(exec?: (c: string, a: string[]) => number): boolean;
export async function applyFixups(
  plan: MovePlan, opts: { emacsStateFiles: string[]; dryRun: boolean; emacsRunning: boolean },
): Promise<FixupReport>;
```

- [ ] **Step 1: 失敗するテストを書く**

```ts
// tools/project-move/src/project-move.test.ts
import { afterEach, beforeEach, expect, test } from "bun:test";
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { applyFixups, isEmacsRunning } from "./project-move.ts";

let root: string;
let plan: { kind: "ghq"; from: string; to: string };

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "project-move-"));
  plan = { kind: "ghq", from: join(root, "old"), to: join(root, "new") };
  mkdirSync(join(plan.to, ".claude"), { recursive: true });
});
afterEach(() => rmSync(root, { recursive: true, force: true }));

test("settings.local.json の permission の旧パスを書き換える", async () => {
  const f = join(plan.to, ".claude", "settings.local.json");
  writeFileSync(f, JSON.stringify({ permissions: { allow: [`Bash(grep x ${plan.from}/a)`] } }));
  await applyFixups(plan, { emacsStateFiles: [], dryRun: false, emacsRunning: false });
  expect(readFileSync(f, "utf8")).toContain(`${plan.to}/a`);
});

test("Emacs 状態ファイルの旧パスを書き換える", async () => {
  const f = join(root, "recentf");
  writeFileSync(f, `("${plan.from}/a.el")\n`);
  await applyFixups(plan, { emacsStateFiles: [f], dryRun: false, emacsRunning: false });
  expect(readFileSync(f, "utf8")).toContain(`${plan.to}/a.el`);
});

test("Emacs 起動中は状態ファイルに触らず警告する", async () => {
  const f = join(root, "recentf");
  writeFileSync(f, `("${plan.from}/a.el")\n`);
  const r = await applyFixups(plan, { emacsStateFiles: [f], dryRun: false, emacsRunning: true });
  expect(readFileSync(f, "utf8")).toContain(`${plan.from}/a.el`);
  expect(r.warnings.join(" ")).toMatch(/emacs/i);
});

test("git hook に旧パスが残っていたら再生成手順を出す", async () => {
  mkdirSync(join(plan.to, ".git", "hooks"), { recursive: true });
  writeFileSync(
    join(plan.to, ".git", "hooks", "pre-push"),
    `#!/bin/sh\n${plan.from}/node_modules/lefthook-darwin-arm64/bin/lefthook run pre-push\n`,
  );
  const r = await applyFixups(plan, { emacsStateFiles: [], dryRun: false, emacsRunning: false });
  expect(r.manualSteps.join(" ")).toMatch(/lefthook/);
});

test(".envrc があれば direnv allow を促す", async () => {
  writeFileSync(join(plan.to, ".envrc"), "export A=1\n");
  const r = await applyFixups(plan, { emacsStateFiles: [], dryRun: false, emacsRunning: false });
  expect(r.manualSteps.join(" ")).toMatch(/direnv allow/);
});

test("dry-run は書き換えない", async () => {
  const f = join(root, "recentf");
  writeFileSync(f, `("${plan.from}/a.el")\n`);
  await applyFixups(plan, { emacsStateFiles: [f], dryRun: true, emacsRunning: false });
  expect(readFileSync(f, "utf8")).toContain(`${plan.from}/a.el`);
});

test("pgrep の結果で Emacs の起動を判定する", () => {
  expect(isEmacsRunning(() => 0)).toBe(true);
  expect(isEmacsRunning(() => 1)).toBe(false);
});
```

- [ ] **Step 2: テストが失敗することを確認する**

Run: `cd tools/project-move && bun test src/project-move.test.ts`
Expected: FAIL — `Cannot find module './project-move.ts'`

- [ ] **Step 3: 実装を書く**

```ts
// tools/project-move/src/project-move.ts
import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { readdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { makeRewriter } from "./rewrite.ts";
import type { MovePlan } from "./plan-move.ts";

export type FixupReport = {
  rewrittenFiles: { path: string; changedLines: number }[];
  manualSteps: string[];
  warnings: string[];
};

/**
 * Emacs が動いているか。
 *
 * recentf / places / desktop はバッファを閉じるときや終了時に丸ごと書き戻される。
 * 起動中に書き換えても保存で上書きされるので、その場合は触らず警告に回す。
 */
export function isEmacsRunning(
  exec: (cmd: string, args: string[]) => number = (cmd, args) =>
    spawnSync(cmd, args).status ?? 1,
): boolean {
  return exec("pgrep", ["-x", "Emacs"]) === 0;
}

async function rewriteFileInPlace(
  path: string,
  rewrite: (s: string) => string,
  dryRun: boolean,
): Promise<number> {
  const before = await readFile(path, "utf8");
  const after = rewrite(before);
  if (after === before) return 0;
  const changed = before
    .split("\n")
    .filter((l, i) => l !== after.split("\n")[i]).length;
  if (!dryRun) await writeFile(path, after);
  return changed;
}

export async function applyFixups(
  plan: MovePlan,
  opts: { emacsStateFiles: string[]; dryRun: boolean; emacsRunning: boolean },
): Promise<FixupReport> {
  const r = makeRewriter([{ from: plan.from, to: plan.to }]);
  const report: FixupReport = { rewrittenFiles: [], manualSteps: [], warnings: [] };

  // 1) プロジェクト内の permission allowlist。移さないと許可プロンプトが増える。
  const settings = join(plan.to, ".claude", "settings.local.json");
  if (existsSync(settings)) {
    const changed = await rewriteFileInPlace(settings, r.rewriteText, opts.dryRun);
    if (changed > 0) report.rewrittenFiles.push({ path: settings, changedLines: changed });
  }

  // 2) Emacs の状態ファイル
  if (opts.emacsRunning && opts.emacsStateFiles.length > 0) {
    report.warnings.push(
      "emacs is running; its state files were left alone because it rewrites them on exit. " +
        "quit emacs and rerun with --fixups-only.",
    );
  } else {
    for (const f of opts.emacsStateFiles) {
      if (!existsSync(f)) continue;
      const changed = await rewriteFileInPlace(f, r.rewriteText, opts.dryRun);
      if (changed > 0) report.rewrittenFiles.push({ path: f, changedLines: changed });
    }
  }

  // 3) git hooks。自動で書き換えず再生成を促す。lefthook / husky は
  //    node_modules のバイナリを絶対パスで起動するので、再インストールが正解。
  const hooks = join(plan.to, ".git", "hooks");
  if (existsSync(hooks)) {
    for (const name of await readdir(hooks)) {
      const p = join(hooks, name);
      const body = await readFile(p, "utf8").catch(() => "");
      if (!body.includes(plan.from)) continue;
      const tool = body.includes("lefthook") ? "npx lefthook install" : "npm install";
      report.manualSteps.push(`${p} still points at the old path; run \`${tool}\` in ${plan.to}`);
    }
  }

  // 4) direnv は .envrc のパスをハッシュして allow を記録するので、移動で失効する
  if (existsSync(join(plan.to, ".envrc"))) {
    report.manualSteps.push(`run \`direnv allow\` in ${plan.to}`);
  }

  return report;
}
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `cd tools/project-move && bun test src/project-move.test.ts`
Expected: PASS (7 tests)

- [ ] **Step 5: CLI を書く**

```ts
#!/usr/bin/env bun
// tools/project-move/cli/project-move.ts
//
// Claude が動いていても安全な作業だけをする。~/.claude 配下の追随は
// claude-state-move の担当 (~/.claude.json は Claude 起動中ずっと書き戻されるため)。
import { appendFile, mkdir, rename } from "node:fs/promises";
import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { spawnSync } from "node:child_process";
import { ghqProbe, planMove } from "../src/plan-move.ts";
import { applyFixups, isEmacsRunning } from "../src/project-move.ts";
import { defaultMovesLogPath, formatMoveRow } from "../src/moves.ts";

const args = process.argv.slice(2);
const dryRun = args.includes("--dry-run");
const dirs = args.filter((a) => !a.startsWith("--"));
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

for (const dir of dirs) {
  const plan = planMove(dir, { probe: ghqProbe, localRoot: join(home, "projects", "local") });
  console.log(`${plan.kind === "ghq" ? "ghq  " : "local"} ${plan.from} -> ${plan.to}`);

  // 移動前に、失いうるものを見せる (CLAUDE.md の「破壊的操作」節)
  const status = spawnSync("git", ["-C", plan.from, "status", "--porcelain"], { encoding: "utf8" });
  if ((status.stdout ?? "").trim() !== "") {
    console.log(`  uncommitted:\n${status.stdout.trimEnd()}`);
  }

  if (!dryRun) {
    if (plan.kind === "ghq") {
      // ghq に任せる。移動後の `git worktree repair` まで面倒を見てくれる。
      const r = spawnSync("ghq", ["migrate", "-y", plan.from], { stdio: "inherit" });
      if (r.status !== 0) { console.error(`  ghq migrate failed; skipped`); continue; }
    } else {
      await mkdir(dirname(plan.to), { recursive: true });
      if (existsSync(plan.to)) { console.error(`  ${plan.to} exists; skipped`); continue; }
      await rename(plan.from, plan.to);
    }
  }

  const report = await applyFixups(plan, { emacsStateFiles: EMACS_STATE_FILES, dryRun, emacsRunning });
  for (const f of report.rewrittenFiles) console.log(`  rewrite ${f.path} (${f.changedLines} lines)`);
  for (const s of report.manualSteps) console.log(`  todo    ${s}`);
  for (const w of report.warnings) console.log(`  warn    ${w}`);

  if (!dryRun) {
    await mkdir(dirname(log), { recursive: true });
    await appendFile(log, formatMoveRow({ from: plan.from, to: plan.to }, new Date()));
  }
}

console.log(
  dryRun
    ? "\ndry run. nothing was changed."
    : `\nlogged to ${log}. quit every claude session, then run:\n  claude-state-move --dry-run`,
);
```

- [ ] **Step 6: dry-run が本物の環境で落ちないことを確認する**

```bash
chmod +x tools/project-move/cli/project-move.ts
./tools/project-move/cli/project-move.ts --dry-run ~/projects/SDXFW_TEMPLATE ~/projects/clock
```
Expected: 1 つ目が `ghq   ... -> /Users/wamei/projects/github.com/RIT-Inc-Dev/SDXFW_TEMPLATE`、2 つ目が `local ... -> /Users/wamei/projects/local/clock`。**ファイルは 1 つも変わらない。**

- [ ] **Step 7: コミット**

```bash
git add tools/project-move/src/project-move.ts tools/project-move/src/project-move.test.ts \
        tools/project-move/cli/project-move.ts
git commit -m "Move a project and repair what the move breaks around it"
```

---

### Task 9: mise への配線と symlink 経由の実行

**Files:**
- Modify: `mise.toml` (`[dotfiles]` に 2 エントリ)
- Test: `tools/project-move/src/project-move.test.ts` (symlink 経由の実行テストを追記)

**Interfaces:**
- Consumes: 両 CLI (Task 6, 8)
- Produces: `~/bin/project-move` と `~/bin/claude-state-move`

- [ ] **Step 1: symlink 経由で import が解決することを確かめる失敗テストを書く**

`~/bin` から実行されるので、相対 import が symlink 越しに解決できないと動かない。
目視では分からないのでテストで押さえる。

```ts
// tools/project-move/src/project-move.test.ts に追記
import { symlinkSync } from "node:fs";
import { spawnSync } from "node:child_process";

test("symlink 経由でも相対 import が解決する", () => {
  const link = join(root, "project-move-link");
  symlinkSync(
    new URL("../cli/project-move.ts", import.meta.url).pathname,
    link,
  );
  const r = spawnSync(link, ["--dry-run"], { encoding: "utf8" });
  expect(r.status).toBe(0);
  expect(r.stderr).not.toMatch(/Cannot find module/);
});
```

- [ ] **Step 2: テストを走らせる**

Run: `cd tools/project-move && bun test src/project-move.test.ts`
Expected: PASS。失敗して `Cannot find module` が出る場合は、両 CLI の import を
`import.meta.dir` 基準の絶対パスに変える。

- [ ] **Step 3: mise.toml の [dotfiles] に 2 本足す**

`"~/bin/gls"` のエントリの直後に追加する。

```toml
# ~/projects のプロジェクトを ghq レイアウトへ移し、Claude のセッション履歴を
# 追随させる 2 本。分かれているのは ~/.claude.json が Claude 起動中ずっと
# 書き戻されており、移動と同じタイミングでは触れないため。
"~/bin/project-move" = { source = "tools/project-move/cli/project-move.ts", mode = "symlink" }
"~/bin/claude-state-move" = { source = "tools/project-move/cli/claude-state-move.ts", mode = "symlink" }
```

- [ ] **Step 4: bootstrap の dry-run で宣言が受理されることを確認する**

`[dotfiles]` のエントリ内の未知キーは警告なしに無視されるので、目視は検証にならない。

Run: `mise bootstrap -n --force-dotfiles`
Expected: 2 本の symlink が計画に出る

- [ ] **Step 5: 実際に symlink を張って PATH から引けることを確認する**

```bash
mise bootstrap --force-dotfiles
project-move --dry-run ~/projects/clock
```
Expected: `local /Users/wamei/projects/clock -> /Users/wamei/projects/local/clock`

- [ ] **Step 6: コミット**

```bash
git add mise.toml tools/project-move/src/project-move.test.ts
git commit -m "Put both commands on PATH through the dotfiles declaration"
```

---

### Task 10: canary 2 件で往復させる

**Files:**
- なし (実行のみ)。結果は次のタスクの前提になる

**Interfaces:**
- Consumes: `~/bin/project-move`、`~/bin/claude-state-move`

**この Task は Claude 自身では完結しない。** `claude-state-move` は Claude 全停止が
前提なので、Step 4 以降は人間が素のシェルで実行する。

- [ ] **Step 1: transcript 経路の canary を dry-run する**

Run: `project-move --dry-run ~/projects/SDXFW_TEMPLATE`
Expected: `ghq  ... -> /Users/wamei/projects/github.com/RIT-Inc-Dev/SDXFW_TEMPLATE`

- [ ] **Step 2: 移動する**

Run: `project-move ~/projects/SDXFW_TEMPLATE`
Expected: 移動が成功し、`~/.local/state/project-move/moves.tsv` に 1 行増える

- [ ] **Step 3: Claude 側の追随を dry-run する**

Run: `claude-state-move --dry-run`
Expected: `rename  -Users-wamei-projects-SDXFW-TEMPLATE -> -Users-wamei-projects-github-com-RIT-Inc-Dev-SDXFW-TEMPLATE`

- [ ] **Step 4: Claude を全部終了してから適用する (人間の手番)**

```bash
# すべての claude セッションを終了してから
claude-state-move
```
Expected: 起動中なら `claude is running` で exit 1。停止していれば rename と rewrite が出る

- [ ] **Step 5: 移動先で過去のセッションが引けることを確認する (人間の手番)**

```bash
cd ~/projects/github.com/RIT-Inc-Dev/SDXFW_TEMPLATE && claude --resume
```
Expected: 移動前のセッション 1 本が一覧に出る

- [ ] **Step 6: index 経路の canary を同じ手順で通す**

```bash
project-move ~/projects/mcwf-web
# claude を全部終了してから
claude-state-move
cd ~/projects/github.com/RIT-Inc-Dev/mcwf-web && claude --resume
```
Expected: 移動前のセッション 15 本が一覧に出る

- [ ] **Step 7: sessions-index.json が揃っていることを確認する**

```bash
jq -r '.originalPath, (.entries[0] | .projectPath, .fullPath)' \
  ~/.claude/projects/./-Users-wamei-projects-github-com-RIT-Inc-Dev-mcwf-web/sessions-index.json
```
Expected: 3 つとも新しいパスを指している

- [ ] **Step 8: 結果を記録する**

canary で問題が出たら、ここで止めて原因を潰してから次へ進む。
問題がなければ `docs/superpowers/plans/` のこのファイルにチェックを入れてコミットする。

---

### Task 11: 残り 41 件を移す

**Files:**
- なし (実行のみ)

- [ ] **Step 1: 全件を dry-run して移動先を確認する**

```bash
project-move --dry-run $(ls -d ~/projects/*/ | grep -v '/github\.com/$' | grep -v '/local/$' | sed 's|/$||')
```

**絶対パスで渡すこと。** `cd ~/projects` してから相対名を渡してはいけない。相対名だと
`from` が `BeecoV2` のような裸の文字列になり、その文字列が全ファイルで置換されて
`~/.claude.json` と `history.jsonl` と Emacs の状態ファイルが壊れる。壊れた結果も
JSON として妥当なので既存のガードを通り抜け、dry-run は行数しか出さないので
気づけない。コマンド側にも `resolve()` を入れてあるが、手順としても絶対パスで渡す。
Expected: 24 件が `ghq`、16 件が `local`、`ImportApps_2` も `ghq` として出る (次の Step で除外する)

- [ ] **Step 2: ImportApps_2 を除いて移す**

```bash
project-move $(ls -d ~/projects/*/ | grep -v '/github\.com/$' | grep -v '/local/$' | grep -v '/ImportApps_2/$' | sed 's|/$||')
```
Expected: 40 件の移動と、手当ての報告

- [ ] **Step 3: 手当ての todo を消化する**

出力の `todo` 行を上から実行する。少なくとも以下が出るはず。

```bash
cd ~/projects/github.com.linka/linka-admin/ietateru && npx lefthook install
cd ~/projects/github.com/g-a-d/aws-kms-sign-csr && direnv allow
```

- [ ] **Step 4: Claude を全部終了してから追随させる (人間の手番)**

```bash
claude-state-move --dry-run   # 先に確認する
```

**ここで `warn` 行が出る。想定済みなので慌てないこと。** スラッグの接頭辞の
曖昧さ検査が次の 2 組を拾う:

```
slug(.../mc-research) is a prefix of slug(.../mc-research-server)
slug(.../memotan)     is a prefix of slug(.../memotan_knowledge)
```

検査は正しく働いているが、**この 2 組については偽陽性**である。曖昧になりうる
スラッグディレクトリ (`-Users-wamei-projects-mc-research-server` など) は
`~/.claude/projects` に実在しないため、混ざるセッションが存在しない。
実在を確認してから進む:

```bash
cd ~/.claude/projects && ls -d ./-Users-wamei-projects-mc-research* ./-Users-wamei-projects-memotan* 2>&1
```

出力が「No such file or directory」なら混ざりようがない。

衝突があると `claude-state-move` は **exit 1 で何も書き換えずに止まる** (安全側)。
そこで、この 2 組だけログから外して個別に適用する:

**注意: 適用済みログの退避は `--from-log` に渡したファイルに対して働く。**
一時ファイルを渡すとそちらが `*.applied.tsv` になり、本物の `moves.tsv` は
40 行そのまま残る。実際にこれを踏んで「ログがおかしい」状態になった。
一時ファイルは使わず、**本物のログを退避してから絞り込んだものを元の場所に戻す**。

```bash
# 1. 該当 2 組を除いた分をまとめて適用
cd ~/.local/state/project-move
cp moves.tsv moves.split-backup.tsv                      # 全 40 行を控える
grep -v -e '/mc-research' -e '/memotan' moves.split-backup.tsv > moves.tsv
claude-state-move                                        # moves.tsv を消費して moves.applied.tsv へ

# 2. 残りを 1 組ずつ位置引数で適用 (位置引数経路は検査対象がその 1 組だけになる)
claude-state-move ~/projects/mc-research        ~/projects/local/mc-research
claude-state-move ~/projects/mc-research-server ~/projects/local/mc-research-server
claude-state-move ~/projects/memotan            ~/projects/local/memotan
claude-state-move ~/projects/memotan_knowledge  ~/projects/local/memotan_knowledge
```

**位置引数モードはログを退避しない** (渡されたログが無いため)。そこで最後に、
個別適用した 4 行を手で退避先へ寄せ、控えを片付ける:

```bash
cd ~/.local/state/project-move
grep -e '/mc-research' -e '/memotan' moves.split-backup.tsv >> moves.applied.tsv
rm moves.split-backup.tsv
wc -l moves.applied.tsv        # 42 行 (canary 2 + 本体 40) になっていること
```

Expected: 1 も 2 も rename と rewrite が出て、`refusing to apply` が出ないこと。
最後に `moves.tsv` が消えていて `moves.applied.tsv` が 42 行あること

- [ ] **Step 5: BeecoV2 の worktree が生きていることを確認する**

`ghq migrate` が `git worktree repair` を走らせているはず。

```bash
git -C ~/projects/github.com/Marubeni-BeecoProgram/BeecoV2 worktree list
```
Expected: worktree が新しいパスで一覧に出て、エラーが出ない

- [ ] **Step 6: `~/projects` 直下が片付いたことを確認する**

Run: `ls -d ~/projects/*/`
Expected: `github.com/`, `github.com.linka/`, `gitlab.com/`, `bitbucket.org/`, `local/` と、
まだ消していない `ImportApps_2/` のみ

- [ ] **Step 7: ghq が全件を認識していることを確認する**

Run: `ghq list | wc -l`
Expected: 27 (移した 26 + dotfiles 自身)

---

### Task 12: 取り残された dotfiles の 100 本を復旧する

**Files:**
- なし (実行のみ)

先行 spec の移行で `~/.dotfiles` → `~/projects/github.com/wamei/.dotfiles` と動かした際、
セッションが追随していない。移動先には既に 5 本あるので、マージ経路の実適用になる。

- [ ] **Step 1: 現状を確認する**

```bash
ls ./-Users-wamei--dotfiles/*.jsonl 2>/dev/null | wc -l          # cd ~/.claude/projects してから
ls ./-Users-wamei-projects-github-com-wamei--dotfiles/*.jsonl | wc -l
```
Expected: 100 と 5

- [ ] **Step 2: dry-run する**

```bash
claude-state-move --dry-run /Users/wamei/.dotfiles /Users/wamei/projects/github.com/wamei/.dotfiles
```
Expected: `rename ... (merged into existing)` が出る

- [ ] **Step 3: Claude を全部終了してから適用する (人間の手番)**

```bash
claude-state-move /Users/wamei/.dotfiles /Users/wamei/projects/github.com/wamei/.dotfiles
```

- [ ] **Step 4: 105 本がマージされたことを確認する (人間の手番)**

```bash
cd ~/.claude/projects && ls ./-Users-wamei-projects-github-com-wamei--dotfiles/*.jsonl | wc -l
cd ~/projects/github.com/wamei/.dotfiles && claude --resume
```
Expected: 105。resume の一覧に古いセッションが出る

---

### Task 13: ImportApps_2 を削除する

**Files:**
- なし (実行のみ)

- [ ] **Step 1: 消す前に状態を表示する**

CLAUDE.md の「破壊的操作」節に従い、消える中身を必ず一度会話に出す。

```bash
cd ~/projects/ImportApps_2
git status --porcelain
git log --branches --not --remotes --oneline
git stash list
git log -1 --format='%H %ci %s'
git remote -v
```
Expected: status 空、unpushed 空、stash 空、最終コミット `047bce7` (2026-02-28)、
remote が `Marubeni-BeecoProgram/ImportApps`。**この 5 つが揃わなければ削除しない。**

- [ ] **Step 2: 追跡外のファイルが無いことを確認する**

Run: `git -C ~/projects/ImportApps_2 status --porcelain --ignored=no -uall`
Expected: 空

- [ ] **Step 3: 削除する**

```bash
rm -rf ~/projects/ImportApps_2
```

- [ ] **Step 4: `~/projects` 直下が host ディレクトリと local だけになったことを確認する**

Run: `ls -d ~/projects/*/`
Expected: `bitbucket.org/`, `github.com/`, `github.com.linka/`, `gitlab.com/`, `local/`

---

### Task 14: 記録して仕上げる

**Files:**
- Modify: `docs/superpowers/plans/2026-09-10-project-ghq-migration.md` (チェック済みに)
- Modify: `README.md` (2 コマンドの説明を足す)

- [ ] **Step 1: README に 2 コマンドを書く**

`README.md` の既存の構成に合わせ、`~/bin` に入るコマンドとして説明を足す。
最低限、次の 3 点を書く。

- `project-move <dir>...` は ghq レイアウト (remote あり) か `~/projects/local/`
  (remote なし) へ移し、`.claude/settings.local.json` と Emacs の状態を追随させる
- `claude-state-move` は **Claude を全部終了してから**実行する。`~/.claude.json` が
  起動中ずっと書き戻されるため
- 後から remote を作った場合は
  `gh repo create <owner>/<name> --private --source=. --remote=origin --push` のあと
  `project-move ~/projects/local/<name>` で ghq レイアウトへ上げられる

- [ ] **Step 2: このファイルのチェックボックスを埋める**

実行済みの Step にチェックを入れる。Task 10 以降の「人間の手番」の Step は、
実際に実行された結果を確認してからチェックする。

- [ ] **Step 3: bootstrap 全体がまだ通ることを確認する**

Run: `mise run verify`
Expected: exit 0

- [ ] **Step 4: テストが全部通ることを確認する**

Run: `mise run test`
Expected: 全テスト PASS

- [ ] **Step 5: コミット**

```bash
git add README.md docs/superpowers/plans/2026-09-10-project-ghq-migration.md
git commit -m "Document the two commands and close out the migration"
```

- [ ] **Step 6: master にマージする**

```bash
git checkout master && git merge --no-ff project-ghq-migration
```

## Self-Review

- **Spec coverage:** 26 件の ghq 移行 (Task 11)、16 件の local 移行 (Task 11)、
  ImportApps_2 削除 (Task 13)、Claude 6 箇所の追随 (Task 4, 5)、接頭辞一致による
  worktree の追随 (Task 1, 5)、行ストリーム処理 (Task 5)、subagents 配下 (Task 5)、
  200 文字超で落とす (Task 1)、Claude 起動中の拒否 (Task 6)、`.claude/settings.local.json`
  と Emacs 状態と hooks と direnv の手当て (Task 8)、canary 2 件 (Task 10)、
  dotfiles 100 本の復旧 (Task 12)、再利用可能性 (Task 9 の symlink と Task 14 の README)。
  spec の全節に対応するタスクがある。
- **Placeholder scan:** 「適切に」「必要に応じて」の類は使っていない。全コードブロックが実物。
- **Type consistency:** `Move` は `{from, to}` で Task 2 以降すべて統一。`Rewriter` は
  `{touches, rewriteText}` で Task 3 以降統一。`MovePlan` は `{kind, from, to}` で
  Task 7 と Task 8 で一致。`slug` / `slugChildRemainder` の呼び出しは Task 5 と一致。
