# org ⇔ メモ.app 同期 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `~/org/` の opt-in な org メモと、メモ.app の `org` フォルダを双方向に同期する CLI (`org-notes-sync`) と、それを launchd / Emacs から回す仕組みを作る。

**Architecture:** 判断はすべて TypeScript の純粋関数 (書式変換・3-way マージ・reconcile) に置き、外界は 3 つのアダプタ (org ディレクトリ、メモ側、状態ファイル) に閉じ込める。メモの読み書きは NotesShared.framework を `dlopen` する ObjC の小さな CLI (`notes-bridge`) が JSON で行う。iCloud への送信はメモ.app の再起動で行う (flush)。

**Tech Stack:** bun (test / build --compile), TypeScript strict, `node-diff3`, ObjC + clang (bridge), osascript (JXA), launchd, Emacs Lisp (init.el)。

**Spec:** `docs/superpowers/specs/2026-10-13-org-notes-sync-design.md`

## Global Constraints

- 作業は worktree `/Users/wamei/projects/github.com/wamei/.dotfiles/.claude/worktrees/org-notes-sync` (ブランチ `org-notes-sync`) で行う。**全コマンドを `cd <worktree> && ` で始め、最初に `git rev-parse --show-toplevel && git branch --show-current` で着地を確認する。** `git stash` は使わない。
- ツールの置き場は `tools/org-notes-sync/`。TypeScript は `tsconfig.json` で `"strict": true`。型は `type` で定義する (`interface` は書式の契約 `LocalFormat` と `NotesPort` だけ)。
- テストは `bun test`。本物のメモ DB・`~/org` に触れるテストは書かない。bridge の結合テストだけは `ORG_NOTES_SYNC_IT=1` のときに **DB のコピー** に対して走る。
- メモ.app の id は `ICNote.identifier` (UUID 文字列)。
- 状態ディレクトリ既定値 `~/.local/state/org-notes-sync/`、org ディレクトリ既定値 `~/org/`、メモのフォルダ名 `org` (iCloud アカウント)。
- 制御行 `#+notes_sync: yes` (大文字小文字を区別しない)。画像は `~/org/.notes-media/<添付ID>.jpg`、ゴミ箱は `~/org/.notes-trash/`。
- コミットメッセージは英語、末尾に次の 2 行:
  ```
  Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01GRGjC9Hw2SbY4N5B33bKTz
  ```
- コード中のコメントは日本語 (リポジトリの慣習)。

## Review Focus

1. **org の普通の文章を書式と誤認しない**: `sslcert=../cert/Combined.crt.pem`、`foo_bar_baz`、`a + b`、`2 * 3` のような行は装飾にならず、そのまま往復する (Task 3 にテストを置く)。
2. **書式の表現差でループしない**: org で表現できない装飾 (行の途中の ASCII 単語に付いた太字、本文中のタイトルスタイル行など) を含むメモが、同期のたびに書き直され続けない (Task 5 の canon / rehydrate テスト)。
3. **org を少し直しただけで、メモ側の他の段落の書式 (ハイライト・チェック状態) が消えない** (Task 5 の rehydrate テストと Task 8 の差分適用テスト)。
4. **読み取りの失敗を「全部消えた」と解釈しない**: フォルダが無い・0 件・大量削除で全体を止める (Task 5)。
5. **Emacs で保存した直後の org を同期が上書きしない**: mtime が変わっていたら書かない (Task 6 / Task 10)。

---

### Task 1: 足場と中間表現 (`doc.ts`)

**Files:**
- Create: `tools/org-notes-sync/package.json`, `tools/org-notes-sync/tsconfig.json`, `tools/org-notes-sync/src/doc.ts`, `tools/org-notes-sync/src/doc.test.ts`
- Modify: `mise.toml` (`[tasks.test]` に org-notes-sync を足す)
- Modify: `docs/superpowers/specs/2026-10-13-org-notes-sync-design.md` (添付の型を `image: boolean` に)

**Interfaces:**
- Produces:
  ```ts
  export type Mark = "bold" | "italic" | "underline" | "strike"
  export const MARKS: readonly Mark[]               // この順で正規化する
  export type TextInline = { kind: "text"; text: string; marks: Mark[]; link?: string }
  export type AttachmentInline = { kind: "attachment"; id: string; image: boolean }
  export type Inline = TextInline | AttachmentInline
  export type ParaStyle =
    | { kind: "title" } | { kind: "heading" } | { kind: "subheading" } | { kind: "body" }
    | { kind: "mono" }
    | { kind: "list"; list: "bullet" | "dash" | "number" }
    | { kind: "check"; done: boolean }
  export type Paragraph = { style: ParaStyle; indent: number; quote: boolean; inlines: Inline[] }
  export type Doc = { paragraphs: Paragraph[] }
  export function text(t: string, marks?: Mark[], link?: string): TextInline
  export function para(style: ParaStyle, inlines: Inline[], opts?: { indent?: number; quote?: boolean }): Paragraph
  export function normalizeDoc(d: Doc): Doc
  export function docEquals(a: Doc, b: Doc): boolean
  export function paragraphEquals(a: Paragraph, b: Paragraph): boolean
  export function plainText(p: Paragraph): string
  export function docTitle(d: Doc): string
  export function imageIds(d: Doc): string[]
  export const EMPTY_DOC: Doc   // { paragraphs: [] }
  ```

- [ ] **Step 1: 足場を作る**

`tools/org-notes-sync/package.json`:
```json
{
  "name": "org-notes-sync",
  "private": true,
  "type": "module",
  "scripts": { "test": "bun test", "type-check": "tsc --noEmit" },
  "dependencies": { "node-diff3": "^3.1.2" },
  "devDependencies": { "@types/bun": "latest", "typescript": "^5.9.0" }
}
```
`tools/org-notes-sync/tsconfig.json`:
```json
{
  "compilerOptions": {
    "target": "ESNext", "module": "ESNext", "moduleResolution": "bundler",
    "strict": true, "noUncheckedIndexedAccess": true, "noEmit": true,
    "types": ["bun"], "skipLibCheck": true
  },
  "include": ["src", "cli"]
}
```
`cd tools/org-notes-sync && bun install` を実行し、`bun.lock` もコミット対象にする。`.gitignore` が `node_modules` を無視しているか `git check-ignore -v tools/org-notes-sync/node_modules` で確認し、無視されていなければ `tools/org-notes-sync/.gitignore` に `node_modules/` を書く。

`mise.toml` の `[tasks.test]` を次に置き換える:
```toml
[tasks.test]
description = "tools/ のユニットテストを走らせる"
run = [
  "cd tools/project-move && bun test",
  "cd tools/org-notes-sync && bun test",
]
```

- [ ] **Step 2: 失敗するテストを書く** (`src/doc.test.ts`)

```ts
import { describe, expect, test } from "bun:test"
import { docEquals, docTitle, imageIds, normalizeDoc, para, plainText, text } from "./doc"

describe("normalizeDoc", () => {
  test("同じ装飾の隣接テキストをまとめ、空テキストを捨て、marks を MARKS 順に並べる", () => {
    const d = normalizeDoc({ paragraphs: [para({ kind: "body" }, [
      text("a", ["italic", "bold"]), text("", ["bold"]), text("b", ["bold", "italic"]), text("c"),
    ])] })
    expect(d.paragraphs[0]!.inlines).toEqual([text("ab", ["bold", "italic"]), text("c", [])])
  })
  test("link が違えばまとめない", () => {
    const d = normalizeDoc({ paragraphs: [para({ kind: "body" }, [text("a", [], "https://x"), text("b")])] })
    expect(d.paragraphs[0]!.inlines.length).toBe(2)
  })
})

describe("docEquals", () => {
  test("正規化後の構造で比べる", () => {
    const a = { paragraphs: [para({ kind: "body" }, [text("a"), text("b")])] }
    const b = { paragraphs: [para({ kind: "body" }, [text("ab")])] }
    expect(docEquals(a, b)).toBe(true)
  })
  test("indent / quote / style の違いを区別する", () => {
    const a = { paragraphs: [para({ kind: "body" }, [text("a")])] }
    expect(docEquals(a, { paragraphs: [para({ kind: "body" }, [text("a")], { indent: 1 })] })).toBe(false)
    expect(docEquals(a, { paragraphs: [para({ kind: "body" }, [text("a")], { quote: true })] })).toBe(false)
    expect(docEquals(a, { paragraphs: [para({ kind: "check", done: false }, [text("a")])] })).toBe(false)
  })
})

test("plainText / docTitle / imageIds", () => {
  const d = { paragraphs: [
    para({ kind: "title" }, [text("題"), text("名", ["bold"])]),
    para({ kind: "body" }, [{ kind: "attachment", id: "IMG", image: true }, { kind: "attachment", id: "TBL", image: false }]),
  ] }
  expect(docTitle(d)).toBe("題名")
  expect(plainText(d.paragraphs[1]!)).toBe("")
  expect(imageIds(d)).toEqual(["IMG"])
  expect(docTitle({ paragraphs: [] })).toBe("")
})
```

- [ ] **Step 3: 失敗を確認**: `cd tools/org-notes-sync && bun test src/doc.test.ts` → モジュールが無いので FAIL。

- [ ] **Step 4: 実装** (`src/doc.ts`)。`text(t, marks = [], link)` は `link` が undefined ならプロパティを持たない。`para` の既定は `indent: 0, quote: false`。`normalizeDoc` は各段落の inlines について (1) 空文字の text を捨てる (2) marks を重複除去して `MARKS` 順に (3) 隣接 text で marks と link が等しいものを連結する。`docEquals` / `paragraphEquals` は正規化後の `JSON.stringify` を比較 (キー順が揺れないよう、オブジェクトは常に `text`/`para` と同じキー順で組み立てる。比較用に正規化関数の中で `{kind, text, marks, link?}` / `{style, indent, quote, inlines}` の順で作り直す)。`plainText` は text の連結 (添付は空文字)。`docTitle` は 1 段落目の plainText (無ければ "")。

- [ ] **Step 5: 通ることを確認**: `bun test src/doc.test.ts` → PASS、`bunx tsc --noEmit` → エラーなし。

- [ ] **Step 6: spec の添付の型を合わせる**: spec の `Inline` 定義の `{ kind: "attachment"; id: string; type: string }` を `{ kind: "attachment"; id: string; image: boolean }` に直し、「種類 (UTI) は bridge が image かどうかに落とす」と 1 行添える。

- [ ] **Step 7: コミット** (`Add org-notes-sync scaffold and document model`)

---

### Task 2: org の段落レベルの変換 (`formats/org.ts` 前半)

**Files:**
- Create: `tools/org-notes-sync/src/formats/format.ts`, `tools/org-notes-sync/src/formats/org.ts`, `tools/org-notes-sync/src/formats/org.test.ts`

**Interfaces:**
- Consumes: Task 1 の型と関数。
- Produces:
  ```ts
  // format.ts
  export type Parsed = { doc: Doc; optIn: boolean; preserved: string[] }
  export interface LocalFormat {
    ext: string
    parse(text: string, fallbackTitle: string): Parsed
    render(doc: Doc, preserved: string[]): string
    hasConflictMarkers(text: string): boolean
  }
  // org.ts
  export const orgFormat: LocalFormat
  export function parseInline(s: string): Inline[]     // Task 3 で本実装。この Task では text 1 個を返す仮実装
  export function renderInline(xs: Inline[]): string    // 同上 (text を連結するだけ)
  ```

段落レベルの規則 (spec「org の対応」):
- 末尾の改行で終わる最後の空行は段落にしない。`render` は常に末尾に `\n` を付ける。
- `#+title:` の最初の 1 行がタイトル段落 (`title`)。無ければ `fallbackTitle`。
- `#+KEY: value` の行 (KEY は `[A-Za-z_]+`) のうち `title` / `notes_attachment` 以外は `preserved` に入れ、本文には入れない。`notes_sync` の値が `yes` なら `optIn`。`render` は `#+title:` の直後に `preserved` をそのまま並べる。
- `#+notes_attachment: <id>` (後ろの語は無視) → `body` 段落で inlines が `[{kind:"attachment", id, image:false}]`。
- `* ` → heading、`** ` → subheading、`*** ` 以上 → body で、全 text inline に bold を足す。`render` は「inlines が空でなく、添付を含まず、全 text inline が bold を持つ body」を `*** ` + (bold を外した inlines) にする。
- リスト: `^( *)(?:([-+])|(\d+)[.)])\s+(?:\[( |X|x)\]\s+)?(.*)$`。indent = 先頭空白数 / 2 (切り捨て)。`[ ]`/`[X]` があれば `check`、`-` は bullet、`+` は dash、数字は number。`render` は `"  ".repeat(indent)` + `- ` / `+ ` / `N. ` / `- [ ] ` / `- [X] `。number の N は、同じ indent で連続する number 項目ごとに 1 から数える (リスト以外の段落を挟むと 1 に戻る、より浅いリスト項目が来たら深い側を 1 に戻す)。
- `#+begin_example` 〜 `#+end_example` (大文字小文字を区別しない、`#+begin_example` の後ろの語は無視) の各行 → `mono` 段落 (inlines は行全体の text 1 個、装飾は解釈しない)。`render` は連続する mono 段落を 1 つのブロックにまとめる。
- `#+begin_quote` 〜 `#+end_quote` の中の段落は `quote: true`。`render` は quote が変わる境目で開閉する (example より外側)。
- それ以外の行 → body。indent = 先頭空白数 / 2、本文は先頭の `indent*2` 文字を除いた残りを `parseInline`。空行は inlines が空の body。
- 本文中に来たタイトルスタイル (`title`) の段落は `* ` として描く (読み戻すと heading になる。差は Task 5 の canon が吸収する)。
- `hasConflictMarkers`: `/^(<{7}|={7}|>{7})( |$)/m`。

- [ ] **Step 1: 失敗するテストを書く** (`src/formats/org.test.ts`)

```ts
import { describe, expect, test } from "bun:test"
import { orgFormat } from "./org"
import { docEquals, para, text, type Doc } from "../doc"

const roundTrip = (s: string) => orgFormat.render(orgFormat.parse(s, "fallback").doc, orgFormat.parse(s, "fallback").preserved)

describe("org 段落: 往復で一字一句戻る", () => {
  const cases: Record<string, string> = {
    "タイトルと制御行": "#+title: 買い物\n#+notes_sync: yes\n\n本文\n",
    "見出し": "#+title: t\n* 見出し\n** 小見出し\n*** 太字行\n本文\n",
    "リスト": "#+title: t\n- a\n  - b\n+ c\n1. x\n2. y\n  1. z\n- [ ] todo\n- [X] done\n",
    "example": "#+title: t\n#+begin_example\n  const a = 1\n*not bold*\n#+end_example\n",
    "quote": "#+title: t\n#+begin_quote\n引用\n- 引用内リスト\n#+end_quote\n後\n",
    "インデント": "#+title: t\n  一段\n    二段\n",
    "添付": "#+title: t\n#+notes_attachment: A10B17CA\n",
  }
  for (const [name, s] of Object.entries(cases)) test(name, () => expect(roundTrip(s)).toBe(s))
})

describe("org 段落: parse の結果", () => {
  test("タイトルが無ければ fallback、opt-in を読む", () => {
    const r = orgFormat.parse("#+NOTES_SYNC: Yes\n本文\n", "memo")
    expect(r.optIn).toBe(true)
    expect(r.preserved).toEqual(["#+NOTES_SYNC: Yes"])
    expect(r.doc.paragraphs[0]).toEqual(para({ kind: "title" }, [text("memo")]))
  })
  test("opt-in していないファイル", () => {
    expect(orgFormat.parse("#+title: x\n", "x").optIn).toBe(false)
  })
  test("*** は太字の body", () => {
    const r = orgFormat.parse("#+title: t\n*** 強調\n", "t")
    expect(r.doc.paragraphs[1]).toEqual(para({ kind: "body" }, [text("強調", ["bold"])]))
  })
  test("**** も太字の body (描くと *** に揃う)", () => {
    const r = orgFormat.parse("#+title: t\n**** 深い\n", "t")
    expect(orgFormat.render(r.doc, [])).toBe("#+title: t\n*** 深い\n")
  })
  test("check と indent と quote", () => {
    const r = orgFormat.parse("#+title: t\n#+begin_quote\n  - [X] 済\n#+end_quote\n", "t")
    expect(r.doc.paragraphs[1]).toEqual(para({ kind: "check", done: true }, [text("済")], { indent: 1, quote: true }))
  })
  test("本文中のタイトルスタイルは見出しとして描く", () => {
    const d: Doc = { paragraphs: [para({ kind: "title" }, [text("t")]), para({ kind: "title" }, [text("途中")])] }
    expect(orgFormat.render(d, [])).toBe("#+title: t\n* 途中\n")
  })
  test("任意の Doc で parse(render(d)) が d に戻る (表現可能な範囲)", () => {
    const d: Doc = { paragraphs: [
      para({ kind: "title" }, [text("t")]),
      para({ kind: "heading" }, [text("h")]),
      para({ kind: "mono" }, [text("  x = 1")]),
      para({ kind: "list", list: "number" }, [text("n")], { indent: 1 }),
      para({ kind: "body" }, [], { quote: true }),
      para({ kind: "body" }, [{ kind: "attachment", id: "T", image: false }]),
    ] }
    expect(docEquals(orgFormat.parse(orgFormat.render(d, []), "t").doc, d)).toBe(true)
  })
})

test("衝突マーカー", () => {
  expect(orgFormat.hasConflictMarkers("a\n<<<<<<< org\nb\n=======\nc\n>>>>>>> notes\n")).toBe(true)
  expect(orgFormat.hasConflictMarkers("a <<<<<<< b\n")).toBe(false)
})
```

- [ ] **Step 2: 失敗を確認**: `bun test src/formats/org.test.ts` → FAIL。
- [ ] **Step 3: `format.ts` と `org.ts` を実装** (上の規則どおり。`parseInline` は `s === "" ? [] : [text(s)]`、`renderInline` は text の連結と、添付は Task 3 で対応するまで空文字)。添付だけの段落は `#+notes_attachment:` 行として描く分岐を `render` の段落処理に入れる (`image: false` のとき)。
- [ ] **Step 4: 通ることを確認**: `bun test` / `bunx tsc --noEmit`。
- [ ] **Step 5: コミット** (`Parse and render org paragraphs for notes sync`)

---

### Task 3: org の行内の変換 (`parseInline` / `renderInline`)

**Files:**
- Modify: `tools/org-notes-sync/src/formats/org.ts`
- Create: `tools/org-notes-sync/src/formats/org-inline.test.ts`

**Interfaces:**
- Produces: `parseInline(s: string): Inline[]`、`renderInline(xs: Inline[]): string` (本実装)。

規則:
- リンク `[[target][label]]` / `[[target]]`:
  - target が `file:.notes-media/<id>.jpg` → `{kind:"attachment", id, image:true}`
  - target が `notes-attachment:<id>` → `{kind:"attachment", id, image:false}`
  - それ以外 → `text(label ?? target, [], target)`
- 裸の URL `https?://[^\s\[\]<>"]+` → `text(url, [], url)`。
- 装飾 `*` bold、`/` italic、`_` underline、`+` strike。開き記号の直前が「行頭・空白・`-({'"`・非 ASCII」、開きの直後と閉じの直前が空白でない、閉じの直後が「行末・空白・`-.,:;!?'")}\]`・非 ASCII」のときだけ認める。中身は再帰的に `parseInline` (入れ子の装飾を許す)。
- `renderInline`: text は link があれば `text === link ? link : [[link][text]]`。marks は外側から `*` `/` `_` `+` の順で包む。ただし包んだ結果が `parseInline` の規則で装飾として読めない位置 (直前に描いた文字・直後の inline の最初の文字・中身の先頭末尾の空白で判定) なら marks を付けずに描く (このずれは Task 5 の canon が吸収する)。添付は `[[file:.notes-media/<id>.jpg]]` / `[[notes-attachment:<id>]]`。

- [ ] **Step 1: 失敗するテストを書く**

```ts
import { describe, expect, test } from "bun:test"
import { parseInline, renderInline } from "./org"
import { text } from "../doc"

describe("parseInline", () => {
  test("装飾", () => {
    expect(parseInline("a *b* /c/ _d_ +e+")).toEqual([
      text("a "), text("b", ["bold"]), text(" "), text("c", ["italic"]), text(" "),
      text("d", ["underline"]), text(" "), text("e", ["strike"]),
    ])
  })
  test("日本語の中でも装飾を認める", () => {
    expect(parseInline("これは*太字*です")).toEqual([text("これは"), text("太字", ["bold"]), text("です")])
  })
  test("入れ子", () => {
    expect(parseInline("*/x/*")).toEqual([text("x", ["bold", "italic"])])
  })
  test("リンクと裸 URL", () => {
    expect(parseInline("[[https://g.co][ぐーぐる]] https://a.b/c")).toEqual([
      text("ぐーぐる", [], "https://g.co"), text(" "), text("https://a.b/c", [], "https://a.b/c"),
    ])
  })
  test("添付", () => {
    expect(parseInline("[[file:.notes-media/AB-12.jpg]]")).toEqual([{ kind: "attachment", id: "AB-12", image: true }])
    expect(parseInline("[[notes-attachment:T1]]")).toEqual([{ kind: "attachment", id: "T1", image: false }])
  })
  // Review Focus 1
  test.each([
    "sslcert=../cert/Combined.crt.pem",
    "foo_bar_baz",
    "a + b + c",
    "2 * 3 * 4",
    "C++ と a/b/c",
    "* 行頭の星 *",
  ])("普通の文章は装飾にしない: %s", (s) => {
    expect(parseInline(s)).toEqual([text(s)])
  })
})

describe("renderInline", () => {
  test("往復", () => {
    for (const s of ["a *b* /c/ _d_ +e+", "これは*太字*です", "[[https://g.co][ぐーぐる]]", "https://a.b/c", "*/x/*"]) {
      expect(renderInline(parseInline(s))).toBe(s)
    }
  })
  test("装飾として読めない位置では marks を落とす", () => {
    expect(renderInline([text("ab"), text("cd", ["bold"]), text("ef")])).toBe("abcdef")
    expect(renderInline([text(" x ", ["bold"])])).toBe(" x ")
  })
})
```

- [ ] **Step 2: 失敗を確認** → FAIL。
- [ ] **Step 3: 実装** (左から 1 文字ずつ走査する小さなパーサ。リンク → URL → 装飾 → 普通の文字の順に試す。結果は `normalizeDoc` と同じ規則で隣接 text をまとめる。`doc.ts` に `normalizeInlines(xs: Inline[]): Inline[]` を export して使う)。
- [ ] **Step 4: Task 2 のテストも含めて全部通ることを確認**: `bun test` / `bunx tsc --noEmit`。
- [ ] **Step 5: コミット** (`Parse and render inline org markup conservatively`)

---

### Task 4: 3-way マージ (`merge.ts`)

**Files:**
- Create: `tools/org-notes-sync/src/merge.ts`, `tools/org-notes-sync/src/merge.test.ts`

**Interfaces:**
- Produces: `export type MergeResult = { conflict: boolean; text: string }`、`export function merge3(base: string, ours: string, theirs: string): MergeResult` (ours = org、theirs = メモ)。

- [ ] **Step 1: 失敗するテストを書く**

```ts
import { expect, test } from "bun:test"
import { merge3 } from "./merge"

test("別々の行の変更は両方残る", () => {
  expect(merge3("a\nb\nc\n", "A\nb\nc\n", "a\nb\nC\n")).toEqual({ conflict: false, text: "A\nb\nC\n" })
})
test("同じ変更は衝突にしない", () => {
  expect(merge3("a\n", "x\n", "x\n")).toEqual({ conflict: false, text: "x\n" })
})
test("同じ行の別々の変更は衝突マーカー", () => {
  expect(merge3("a\nb\n", "a\nORG\n", "a\nNOTES\n")).toEqual({
    conflict: true,
    text: "a\n<<<<<<< org\nORG\n=======\nNOTES\n>>>>>>> notes\n",
  })
})
test("base が空 (state 喪失時の採用) でも動く", () => {
  expect(merge3("", "x\n", "x\n")).toEqual({ conflict: false, text: "x\n" })
  expect(merge3("", "x\n", "y\n").conflict).toBe(true)
})
```

- [ ] **Step 2: 失敗を確認** → FAIL。
- [ ] **Step 3: 実装**: `node-diff3` の `diff3Merge(ours, base, theirs, { excludeFalseConflicts: true })` を行配列で呼ぶ (末尾の改行は分割前に 1 つ外し、結合後に付け直す。空文字は空配列)。返り値の `{ ok: string[] }` はそのまま、`{ conflict: { a, b } }` は `<<<<<<< org` / a / `=======` / b / `>>>>>>> notes` にする。`node-diff3` の型が合わなければ `bun pm view node-diff3` の版と `node_modules/node-diff3` の d.ts を確認して合わせる。
- [ ] **Step 4: 通ることを確認**。
- [ ] **Step 5: コミット** (`Merge org and notes text three ways`)

---

### Task 5: 同期の判断 (`reconcile.ts`)

**Files:**
- Create: `tools/org-notes-sync/src/reconcile.ts`, `tools/org-notes-sync/src/reconcile.test.ts`, `tools/org-notes-sync/src/filename.ts`, `tools/org-notes-sync/src/filename.test.ts`

**Interfaces:**
- Consumes: `LocalFormat`、`merge3`、doc の関数。
- Produces:
  ```ts
  // filename.ts
  export function noteFileName(title: string, taken: Set<string>, ext: string): string
  // reconcile.ts
  export type OrgFile = { file: string; text: string; mtimeMs: number }   // file は org ディレクトリからの相対 (例 "foo.org")
  export type NoteSnap = { id: string; uri: string; doc: Doc }
  export type Pair = { file: string; noteId: string; base: Doc }
  export type ReconcileInput = {
    format: LocalFormat; pairs: Pair[]; orgFiles: OrgFile[]
    notes: NoteSnap[] | null      // null = メモ.app に org フォルダが無い
    force: boolean
  }
  export type Action =
    | { kind: "noop"; file: string; noteId: string }
    | { kind: "pushNotes"; file: string; noteId: string; doc: Doc }
    | { kind: "pullOrg"; file: string; noteId: string; text: string; base: Doc }
    | { kind: "both"; file: string; noteId: string; text: string; doc: Doc }
    | { kind: "conflict"; file: string; noteId: string; text: string; base: Doc }
    | { kind: "blocked"; file: string; noteId: string }
    | { kind: "createNote"; file: string; doc: Doc }
    | { kind: "createOrg"; file: string; noteId: string; text: string; base: Doc }
    | { kind: "deleteNote"; file: string; noteId: string; uri: string }
    | { kind: "trashOrg"; file: string; noteId: string }
    | { kind: "drop"; file: string; noteId: string }
  export type Plan = { ok: true; actions: Action[] } | { ok: false; reason: "folder-missing" | "notes-empty" | "mass-delete" }
  export function canon(format: LocalFormat, d: Doc): Doc
  export function rehydrate(target: Doc, source: Doc, format: LocalFormat): Doc
  export function reconcile(input: ReconcileInput): Plan
  ```

規則 (spec「同期アルゴリズム」に加えて):
- `canon(f, d) = f.parse(f.render(d, []), docTitle(d)).doc` — org で表現したときの形。
- 「org が変わった」= `!docEquals(orgDoc, canon(base))`。「メモが変わった」= `!docEquals(notesDoc, base)`。base は常にメモ側の表現で持つ。
- `rehydrate(target, source)`: `canon(source)` の段落数が `source` と同じときだけ、`target` の各段落を `canon(source)` と LCS で対応付け、一致した段落を `source` の段落 (メモ側の忠実な形) に差し替える。段落数が違えば `target` をそのまま返す。
- pushNotes の doc = `rehydrate(orgDoc, base)`。both の doc = `rehydrate(rehydrate(parse(merged).doc, notesDoc), base)`。
- 3-way マージの入力は `render(base, preserved)` / `org.text` / `render(notesDoc, preserved)` (preserved は現在の org のもの)。
- 両方変わっていても `docEquals(orgDoc, canon(notesDoc))` なら pullOrg (base を揃えるだけ)。
- 衝突マーカーが残っている org のペアは blocked (どちらへも書かない)。
- ペアの無い opt-in org: タイトル (`docTitle`) が同じペア無しメモがあれば、base を `EMPTY_DOC` として「両方変わった」扱い (等しければ pullOrg、違えばマージ)。無ければ createNote。
- ペアの無いメモ: createOrg。ファイル名は `noteFileName(docTitle, taken, ".org")` (taken = 既存の全 org ファイル名 + この計画で使う名前)。本文は `render(notesDoc, ["#+notes_sync: yes"])`。
- 全体停止: `notes === null && pairs.length > 0` → folder-missing。`notes.length === 0 && pairs.length > 0` → notes-empty。deleteNote + trashOrg の件数 n が `n >= 3 && n * 2 >= pairs.length` で `!force` → mass-delete。
- `noteFileName`: `/[\/\\:*?"<>|\x00-\x1f]/g` を `-` に、前後の空白を除き、先頭の `.` を `-` に、空なら `untitled`。`<name><ext>` が taken なら `<name>-2<ext>`, `-3` …。

- [ ] **Step 1: filename のテスト**

```ts
import { expect, test } from "bun:test"
import { noteFileName } from "./filename"
test("ファイル名", () => {
  expect(noteFileName("買い物", new Set(), ".org")).toBe("買い物.org")
  expect(noteFileName("a/b: c?", new Set(), ".org")).toBe("a-b- c-.org")
  expect(noteFileName(".hidden", new Set(), ".org")).toBe("-hidden.org")
  expect(noteFileName("  ", new Set(), ".org")).toBe("untitled.org")
  expect(noteFileName("x", new Set(["x.org", "x-2.org"]), ".org")).toBe("x-3.org")
})
```

- [ ] **Step 2: reconcile のテスト** (表の各行 + Review Focus 2〜4)。テスト内の補助:

```ts
import { describe, expect, test } from "bun:test"
import { canon, reconcile, rehydrate, type NoteSnap, type OrgFile, type Pair } from "./reconcile"
import { orgFormat as f } from "./formats/org"
import { EMPTY_DOC, para, text, type Doc } from "./doc"

const doc = (title: string, ...lines: string[]): Doc => ({ paragraphs: [
  para({ kind: "title" }, [text(title)]), ...lines.map((l) => para({ kind: "body" }, l ? [text(l)] : [])),
] })
const orgText = (d: Doc) => f.render(d, ["#+notes_sync: yes"])
const org = (file: string, d: Doc, extra = ""): OrgFile => ({ file, text: orgText(d) + extra, mtimeMs: 1 })
const note = (id: string, d: Doc): NoteSnap => ({ id, uri: `x-coredata://${id}`, doc: d })
const run = (pairs: Pair[], orgFiles: OrgFile[], notes: NoteSnap[] | null, force = false) =>
  reconcile({ format: f, pairs, orgFiles, notes, force })

describe("ペア済み", () => {
  const base = doc("t", "a")
  const pair: Pair = { file: "t.org", noteId: "N", base }
  test("どちらも変化なし → noop", () => {
    expect(run([pair], [org("t.org", base)], [note("N", base)])).toEqual({ ok: true, actions: [{ kind: "noop", file: "t.org", noteId: "N" }] })
  })
  test("org だけ変更 → pushNotes", () => {
    const p = run([pair], [org("t.org", doc("t", "a", "b"))], [note("N", base)])
    expect(p.ok && p.actions[0]?.kind).toBe("pushNotes")
  })
  test("メモだけ変更 → pullOrg (制御行を保つ)", () => {
    const p = run([pair], [org("t.org", base)], [note("N", doc("t", "a", "iphone"))])
    expect(p.ok && p.actions[0]).toMatchObject({ kind: "pullOrg", text: "#+title: t\n#+notes_sync: yes\na\niphone\n" })
  })
  test("両方の別々の行 → both", () => {
    const b3 = doc("t", "a", "b", "c")
    const p = run([{ ...pair, base: b3 }], [org("t.org", doc("t", "A", "b", "c"))], [note("N", doc("t", "a", "b", "C"))])
    expect(p.ok && p.actions[0]).toMatchObject({ kind: "both", text: "#+title: t\n#+notes_sync: yes\nA\nb\nC\n" })
  })
  test("同じ行 → conflict、base はメモ側", () => {
    const n = doc("t", "N")
    const p = run([pair], [org("t.org", doc("t", "O"))], [note("N", n)])
    expect(p.ok && p.actions[0]).toMatchObject({ kind: "conflict", base: n })
  })
  test("衝突マーカーが残っている → blocked", () => {
    const p = run([pair], [org("t.org", base, "<<<<<<< org\nx\n=======\ny\n>>>>>>> notes\n")], [note("N", doc("t", "z"))])
    expect(p.ok && p.actions[0]?.kind).toBe("blocked")
  })
  test("org 削除 → deleteNote / opt-in 解除も同じ", () => {
    expect(run([pair], [], [note("N", base)])).toMatchObject({ ok: true, actions: [{ kind: "deleteNote", uri: "x-coredata://N" }] })
    const optOut: OrgFile = { file: "t.org", text: f.render(base, []), mtimeMs: 1 }
    expect(run([pair], [optOut], [note("N", base)])).toMatchObject({ ok: true, actions: [{ kind: "deleteNote" }] })
  })
  test("メモ削除 → trashOrg、両方無い → drop", () => {
    const other = note("X", doc("other"))
    expect(run([pair], [org("t.org", base)], [other])).toMatchObject({ ok: true, actions: [{ kind: "trashOrg" }, { kind: "createOrg" }] })
    expect(run([pair], [], [other])).toMatchObject({ ok: true, actions: [{ kind: "drop" }, { kind: "createOrg" }] })
  })
})

describe("ペア無し", () => {
  test("opt-in の org → createNote、opt-in でない org は無視", () => {
    const p = run([], [org("a.org", doc("a")), { file: "b.org", text: "#+title: b\n", mtimeMs: 1 }], [])
    expect(p).toMatchObject({ ok: true, actions: [{ kind: "createNote", file: "a.org" }] })
  })
  test("メモ → createOrg (名前の衝突を避ける)", () => {
    const p = run([], [{ file: "買い物.org", text: "#+title: 買い物\n", mtimeMs: 1 }], [note("N", doc("買い物", "卵"))])
    expect(p).toMatchObject({ ok: true, actions: [{ kind: "createOrg", file: "買い物-2.org", noteId: "N", text: "#+title: 買い物\n#+notes_sync: yes\n卵\n" }] })
  })
  test("同じタイトルなら採用 (同じ内容は pullOrg、違えばマージ)", () => {
    expect(run([], [org("a.org", doc("a", "x"))], [note("N", doc("a", "x"))])).toMatchObject({ ok: true, actions: [{ kind: "pullOrg", noteId: "N" }] })
    expect(run([], [org("a.org", doc("a", "x"))], [note("N", doc("a", "y"))])).toMatchObject({ ok: true, actions: [{ kind: "conflict", noteId: "N" }] })
  })
})

describe("全体停止 (Review Focus 4)", () => {
  const pairs = ["a", "b", "c", "d"].map((x): Pair => ({ file: `${x}.org`, noteId: x, base: doc(x) }))
  const orgs = pairs.map((p) => org(p.file, p.base))
  test("フォルダが無い / 0 件", () => {
    expect(run(pairs, orgs, null)).toEqual({ ok: false, reason: "folder-missing" })
    expect(run(pairs, orgs, [])).toEqual({ ok: false, reason: "notes-empty" })
  })
  test("大量削除は --force が要る", () => {
    const notes = [note("a", doc("a"))]
    expect(run(pairs, orgs, notes)).toEqual({ ok: false, reason: "mass-delete" })
    expect(run(pairs, orgs, notes, true).ok).toBe(true)
  })
})

describe("表現差でループしない (Review Focus 2, 3)", () => {
  test("org で表せない装飾を持つメモは、org を書いた後に変化なしと判定される", () => {
    const fancy: Doc = { paragraphs: [para({ kind: "title" }, [text("t")]), para({ kind: "body" }, [text("ab"), text("cd", ["bold"]), text("ef")])] }
    const p1 = run([], [], [note("N", fancy)])
    if (!p1.ok || p1.actions[0]?.kind !== "createOrg") throw new Error("expected createOrg")
    const a = p1.actions[0]
    const p2 = run([{ file: a.file, noteId: "N", base: a.base }], [{ file: a.file, text: a.text, mtimeMs: 1 }], [note("N", fancy)])
    expect(p2).toEqual({ ok: true, actions: [{ kind: "noop", file: a.file, noteId: "N" }] })
  })
  test("org で 1 行足しても、他の段落のメモ側の書式は残る", () => {
    const fancy: Doc = { paragraphs: [para({ kind: "title" }, [text("t")]), para({ kind: "body" }, [text("ab"), text("cd", ["bold"]), text("ef")])] }
    const edited = f.render(canon(f, fancy), ["#+notes_sync: yes"]) + "追記\n"
    const p = run([{ file: "t.org", noteId: "N", base: fancy }], [{ file: "t.org", text: edited, mtimeMs: 1 }], [note("N", fancy)])
    expect(p.ok && p.actions[0]).toMatchObject({ kind: "pushNotes", doc: { paragraphs: [fancy.paragraphs[0], fancy.paragraphs[1], para({ kind: "body" }, [text("追記")])] } })
  })
  test("rehydrate は段落数がずれたら何もしない", () => {
    const t = doc("t", "x")
    expect(rehydrate(t, EMPTY_DOC, f)).toEqual(t)
  })
})
```

- [ ] **Step 3: 失敗を確認** → FAIL。
- [ ] **Step 4: 実装** (`reconcile` はペア → ペア無し org → ペア無しメモの順に actions を積み、最後に全体停止の判定をする。ペア無しメモの判定では、ペア済みの noteId と採用したメモを除く)。
- [ ] **Step 5: 通ることを確認**: `bun test` / `bunx tsc --noEmit`。
- [ ] **Step 6: コミット** (`Decide sync actions from org files, notes and state`)

---

### Task 6: 状態と org ディレクトリ (`state.ts` / `orgfs.ts`)

**Files:**
- Create: `tools/org-notes-sync/src/state.ts`, `src/state.test.ts`, `src/orgfs.ts`, `src/orgfs.test.ts`

**Interfaces:**
- Produces:
  ```ts
  // state.ts
  export type State = { version: 1; format: "org"; pairs: Pair[]; flushPending: boolean }
  export function loadState(dir: string): State          // 無ければ空の State
  export function saveState(dir: string, s: State): void  // dir/state.json に一時ファイル + rename
  // orgfs.ts
  export function listOrgFiles(dir: string, ext: string): OrgFile[]   // 直下の通常ファイルのみ (先頭が . のファイルも含む)
  export function writeOrgFile(dir: string, file: string, text: string, expectedMtimeMs: number | null): "written" | "stale"
  export function trashOrgFile(dir: string, file: string): string     // 移動先の絶対パス
  export function mediaPath(dir: string, id: string): string          // dir/.notes-media/<id>.jpg
  export function gcMedia(dir: string, keep: Set<string>): string[]   // 消した id
  ```
- `writeOrgFile`: `expectedMtimeMs === null` は「無いはず」、ファイルがあれば stale。数値なら現在の mtime と一致しなければ stale (Review Focus 5)。書くときは同じディレクトリの `.<file>.tmp-<pid>` に書いて rename。
- `trashOrgFile`: `dir/.notes-trash/<file>`。既にあれば `<stem>-<YYYYMMDDHHmmss><ext>`。

- [ ] **Step 1: テスト** (一時ディレクトリは `fs.mkdtempSync(path.join(os.tmpdir(), "ons-"))`)

```ts
import { expect, test } from "bun:test"
import fs from "node:fs"; import os from "node:os"; import path from "node:path"
import { gcMedia, listOrgFiles, mediaPath, trashOrgFile, writeOrgFile } from "./orgfs"
import { loadState, saveState } from "./state"

const tmp = () => fs.mkdtempSync(path.join(os.tmpdir(), "ons-"))

test("state: 無ければ空、保存して読み直せる", () => {
  const d = tmp()
  expect(loadState(d)).toEqual({ version: 1, format: "org", pairs: [], flushPending: false })
  saveState(d, { version: 1, format: "org", pairs: [{ file: "a.org", noteId: "N", base: { paragraphs: [] } }], flushPending: true })
  expect(loadState(d).pairs[0]?.noteId).toBe("N")
  expect(fs.readdirSync(d)).toEqual(["state.json"])
})

test("listOrgFiles: 直下の .org だけ (dotfile 含む)", () => {
  const d = tmp()
  fs.writeFileSync(path.join(d, "a.org"), "x"); fs.writeFileSync(path.join(d, ".dot.org"), "y")
  fs.writeFileSync(path.join(d, "b.txt"), "z"); fs.mkdirSync(path.join(d, ".notes-trash"))
  expect(listOrgFiles(d, ".org").map((f) => f.file).sort()).toEqual([".dot.org", "a.org"])
})

test("writeOrgFile: mtime が変わっていたら書かない (Review Focus 5)", () => {
  const d = tmp(); const p = path.join(d, "a.org")
  fs.writeFileSync(p, "old")
  const m = fs.statSync(p).mtimeMs
  fs.utimesSync(p, new Date(), new Date(Date.now() + 5000))
  expect(writeOrgFile(d, "a.org", "new", m)).toBe("stale")
  expect(fs.readFileSync(p, "utf8")).toBe("old")
  expect(writeOrgFile(d, "a.org", "new", fs.statSync(p).mtimeMs)).toBe("written")
  expect(writeOrgFile(d, "a.org", "x", null)).toBe("stale")
  expect(writeOrgFile(d, "b.org", "x", null)).toBe("written")
})

test("trashOrgFile と gcMedia", () => {
  const d = tmp()
  fs.writeFileSync(path.join(d, "a.org"), "1"); trashOrgFile(d, "a.org")
  fs.writeFileSync(path.join(d, "a.org"), "2"); const second = trashOrgFile(d, "a.org")
  expect(fs.readdirSync(path.join(d, ".notes-trash")).length).toBe(2)
  expect(second).not.toBe(path.join(d, ".notes-trash", "a.org"))
  fs.mkdirSync(path.join(d, ".notes-media"))
  fs.writeFileSync(mediaPath(d, "KEEP"), ""); fs.writeFileSync(mediaPath(d, "DROP"), "")
  expect(gcMedia(d, new Set(["KEEP"]))).toEqual(["DROP"])
})
```

- [ ] **Step 2: 失敗を確認** → **Step 3: 実装** → **Step 4: 通ることを確認**。
- [ ] **Step 5: コミット** (`Persist sync state and touch org files safely`)

---

### Task 7: bridge — 診断と読み取り (`notes-bridge.m`)

**Files:**
- Create: `tools/org-notes-sync/bridge/notes-bridge.m`, `tools/org-notes-sync/bridge/build.sh`, `tools/org-notes-sync/src/bridge.it.test.ts`
- Modify: `tools/org-notes-sync/.gitignore` (`bridge/notes-bridge` のビルド成果物を無視)

**Interfaces:**
- Produces (CLI、出力は stdout の JSON 1 つ。失敗時は stderr にメッセージ、終了コード 3 = selfcheck 失敗、4 = DB を開けない、1 = その他):
  - `notes-bridge [--store <dir>] selfcheck` → `{"ok":true}` / 欠けた名前を stderr に並べて終了コード 3
  - `notes-bridge [--store <dir>] list --folder <name>` → `{"folderMissing":false,"notes":[{"id":"…","uri":"x-coredata://…","doc":Doc}]}`
  - `notes-bridge [--store <dir>] read --id <id>` → `{"doc":Doc}`
  - `notes-bridge [--store <dir>] identify --uri <x-coredata uri>` → `{"id":"…"}`
  - `notes-bridge [--store <dir>] export-media --attachment <id> --to <path.jpg>` → `{"written":true}` (HEIC などは `/usr/bin/sips -s format jpeg <src> --out <path>`、JPEG はコピー)

実物で確認済みの事実 (2026-10-13, macOS 26):
- `dlopen("/System/Library/PrivateFrameworks/NotesShared.framework/NotesShared", RTLD_NOW)`
- `--store` は `+[ICPaths setApplicationDocumentsURL:]` (ディレクトリの file URL) を `ICNoteContext` 起動前に呼ぶ。
- `+[ICNoteContext startSharedContextWithOptions:0]` → `+[ICNoteContext sharedContext]` → `-managedObjectContext`。操作は `performBlockAndWait:` の中で行う。
- `ICNote` のフェッチ: エンティティ名 `ICNote`。属性 `identifier`、`title`、関係 `folder` (`ICFolder`、`title`)、`account`。削除済みは `markedForDeletion == YES` を除外する。iCloud アカウントの判定は `ICAccount` の `accountType` / `name` を実物で確かめる (Step 2)。
- `-[ICNote attributedString]` が段落と装飾を返す。属性キー:
  - `TTStyle` = `ICTTParagraphStyle` (`style`: 0 title, 1 heading, 2 subheading, 3 body, 4 mono, 100 bullet, 101 dash, 102 number, 103 check / `indent` / `blockQuoteLevel` / `todo` (`ICTTTodo`, `done`))
  - `TTHints` (NSNumber ビット: 1 bold, 2 italic)、`TTUnderline`、`TTStrikethrough` (非 0 で有効)、`TTEmphasis` (ハイライト。Doc には持たない)
  - `NSLink` (NSURL または NSString)
  - `NSAttachment` = `ICTTAttachment` (`attachmentUTI`、`attachmentIdentifier`)、文字は U+FFFC
- 段落 → Doc: `\n` で分割 (末尾の `\n` の後の空要素は捨てる)。段落スタイルは段落の最初の文字 (空段落はその `\n`) の `TTStyle`。`quote = blockQuoteLevel > 0`。添付の image は UTI が `public.jpeg|public.png|public.heic|public.heif|public.tiff|public.gif` もしくは `public.image` に適合 (`UTType` の `conformsToType:UTTypeImage`)。

- [ ] **Step 1: build.sh を書く**

```sh
#!/bin/sh
# notes-bridge をビルドする。出力先は第 1 引数 (既定は bridge/notes-bridge)
set -eu
here=$(cd "$(dirname "$0")" && pwd)
out=${1:-"$here/notes-bridge"}
clang -fobjc-arc -O2 -Wall -Wno-arc-performSelector-leaks \
  -framework Foundation -framework CoreData -framework UniformTypeIdentifiers \
  "$here/notes-bridge.m" -o "$out"
codesign -s - -f "$out" >/dev/null 2>&1 || true
```

- [ ] **Step 2: 実物確認 (コピーで)**: 結合テストの準備関数で DB をコピーする (下の Step 3 の `makeStoreCopy`)。そのコピーに対して小さな使い捨てプログラムで `ICAccount` の `accountType` と `name`、`ICFolder` の `title`、`ICAttachment` の media を辿るセレクタ (`media` → `mediaURL` など) を調べ、結果を bridge 冒頭のコメントに書く。メソッド一覧は ObjC ランタイム (`class_copyMethodList`) で取る。

- [ ] **Step 3: 結合テストを書く** (`src/bridge.it.test.ts`、`ORG_NOTES_SYNC_IT=1` のときだけ)

```ts
import { beforeAll, describe, expect, test } from "bun:test"
import { $ } from "bun"
import fs from "node:fs"; import os from "node:os"; import path from "node:path"

const IT = process.env.ORG_NOTES_SYNC_IT === "1"
const here = path.resolve(import.meta.dir, "..")
const live = path.join(os.homedir(), "Library/Group Containers/group.com.apple.notes")
let store = "", bridge = ""

// 本物の DB は読むだけ。sqlite の .backup で一貫したコピーを取り、他のファイルは rsync で写す
export async function makeStoreCopy(): Promise<string> {
  const d = fs.mkdtempSync(path.join(os.tmpdir(), "ons-store-"))
  await $`rsync -a --exclude ${"NoteStore.sqlite*"} ${live + "/"} ${d + "/"}`.quiet()
  await $`sqlite3 ${"file:" + live + "/NoteStore.sqlite?mode=ro"} ${".backup " + path.join(d, "NoteStore.sqlite")}`.quiet()
  return d
}
const call = async (...args: string[]) => JSON.parse(await $`${bridge} --store ${store} ${args}`.text())

describe.skipIf(!IT)("notes-bridge (DB のコピー)", () => {
  beforeAll(async () => {
    bridge = path.join(fs.mkdtempSync(path.join(os.tmpdir(), "ons-bin-")), "notes-bridge")
    await $`sh ${path.join(here, "bridge/build.sh")} ${bridge}`
    store = await makeStoreCopy()
  })
  test("selfcheck", async () => expect(await call("selfcheck")).toEqual({ ok: true }))
  test("list: 無いフォルダ", async () => expect(await call("list", "--folder", "存在しないフォルダ-ons")).toEqual({ folderMissing: true, notes: [] }))
  test("list と read が同じ Doc を返し、1 段落目はタイトル", async () => {
    const folder = process.env.ORG_NOTES_SYNC_IT_FOLDER ?? "org-sync-probe"
    const r = await call("list", "--folder", folder)
    expect(r.folderMissing).toBe(false)
    for (const n of r.notes) {
      expect(n.doc.paragraphs[0].style.kind).toBe("title")
      expect((await call("read", "--id", n.id)).doc).toEqual(n.doc)
      expect((await call("identify", "--uri", n.uri)).id).toBe(n.id)
    }
  })
  test("本物の DB のスキーマを変えない", async () => {
    const schema = async (p: string) => $`sqlite3 ${"file:" + p + "?mode=ro"} ${"select sql from sqlite_master order by name"}`.text()
    expect(await schema(path.join(store, "NoteStore.sqlite"))).toBe(await schema(path.join(live, "NoteStore.sqlite")))
  })
})
```

- [ ] **Step 4: 失敗を確認**: `ORG_NOTES_SYNC_IT=1 bun test src/bridge.it.test.ts` → ビルド失敗で FAIL。
- [ ] **Step 5: 実装**: `notes-bridge.m` に、引数解析、`--store`、`selfcheck` (必要なクラス `ICPaths ICNoteContext ICNote ICFolder ICTTMergeableAttributedString ICTTParagraphStyle ICTTTodo ICTTAttachment` と、使うセレクタを `respondsToSelector:` / `instancesRespondToSelector:` で確認)、`list` / `read` / `identify` / `export-media`、attributedString → Doc 変換 (JSON は `NSJSONSerialization`、キー順は Doc の型定義の順に並べる必要はない — TS 側で `normalizeDoc` してから比較する)。Doc の JSON は Task 1 の型と完全に一致させる (`link` は無ければキーを出さない、`marks` は常に配列)。
- [ ] **Step 6: 通ることを確認**: `ORG_NOTES_SYNC_IT=1 bun test src/bridge.it.test.ts`、通常の `bun test` では skip されること。
- [ ] **Step 7: コミット** (`Read Apple Notes through NotesShared in a small bridge`)

---

### Task 8: bridge — 差分の書き込み

**Files:**
- Modify: `tools/org-notes-sync/bridge/notes-bridge.m`, `tools/org-notes-sync/src/bridge.it.test.ts`

**Interfaces:**
- Produces: `notes-bridge [--store <dir>] write --id <id>` (stdin: `{"doc":Doc}`) → `{"doc":Doc}` (適用後に読み戻したもの)。添付の扱いを守れないときは終了コード 5 と理由。

書き込みの手順 (Apple のコードに CRDT を任せる。2026-10-13 にコピーと本番で確認済み):
1. 現在の attributedString を段落に分け、各段落を Doc 段落にした配列 `cur` と、文字範囲 `ranges` を作る。目標は `want = doc.paragraphs`。
2. 段落の等しさ (JSON 比較) で LCS を取り、変わらない段落はそのまま。連続する「消える段落」と「入る段落」の塊ごとに、後ろの塊から順に:
   - 旧範囲の文字列 `old` と、新しい段落を `\n` でつないだ文字列 `new` について、共通の先頭と末尾を除いた中間だけを `-[ICTTMergeableString replaceCharactersInRange:withString:]` で置き換える (`beginEditing` / `endEditing` で囲む)。
   - 中間に U+FFFC (添付) が含まれていて、目標側にもその添付 id が残るなら、その塊は書けない → 終了コード 5。目標側から消えているなら置き換えてよい (添付が消える)。
   - 置き換え後の塊の各段落について、`setAttributes:range:` で属性を設定する。既存の属性辞書を取り、`TTStyle` (既存の `ICTTParagraphStyle` を `mutableCopy` して `style` / `indent` / `blockQuoteLevel` / `todo` を設定。新しい段落なら `uuid` に新しい NSUUID。check で既存の todo が無ければ `ICTTTodo` を作る — 生成メソッドは Task 7 Step 2 と同じ要領で実物から確かめる)、`TTHints` / `TTUnderline` / `TTStrikethrough` / `NSLink` を目標どおりに上書き・削除し、それ以外のキー (`TTEmphasis` など) は残す。U+FFFC の文字には触れない。
3. `-[ICNote saveNoteData]` → `-[ICNote updateChangeCountWithReason:@"org-notes-sync"]` → `[moc save:]`。メモ一覧のタイトル (`title` 属性) が自動で更新されなければ、更新するメソッドを実物から探して呼ぶ (Step 2 で確認する)。
4. 読み戻した Doc を返す。

- [ ] **Step 1: 結合テストを足す** (書き込み先はコピー上のメモだけ)

```ts
  test("write: 段落の追加・変更・スタイル変更が読み戻せ、他の段落の属性を残す", async () => {
    const folder = process.env.ORG_NOTES_SYNC_IT_FOLDER ?? "org-sync-probe"
    const r = await call("list", "--folder", folder)
    const n = r.notes[0]
    if (!n) throw new Error(`テスト用フォルダ ${folder} にメモが要る`)
    const want = structuredClone(n.doc)
    want.paragraphs.push(
      { style: { kind: "heading" }, indent: 0, quote: false, inlines: [{ kind: "text", text: "追記した見出し", marks: [] }] },
      { style: { kind: "check", done: true }, indent: 1, quote: true, inlines: [{ kind: "text", text: "済み", marks: ["bold"], link: "https://example.com/" }] },
    )
    const got = (await $`${bridge} --store ${store} write --id ${n.id} < ${new Response(JSON.stringify({ doc: want }))}`.json()).doc
    expect(got).toEqual(want)
    expect((await call("read", "--id", n.id)).doc).toEqual(want)
  })
  test("write: 変えていない段落のハイライト (TTEmphasis) は残る", async () => {
    const folder = process.env.ORG_NOTES_SYNC_IT_FOLDER ?? "org-sync-probe"
    const r = await call("list", "--folder", folder)
    let target: { id: string; doc: any; idx: number } | undefined
    for (const n of r.notes) {
      const attrs: string[][] = (await call("dump-attrs", "--id", n.id)).paragraphs
      const idx = attrs.findIndex((ks) => ks.includes("TTEmphasis"))
      if (idx >= 0) { target = { id: n.id, doc: n.doc, idx }; break }
    }
    if (!target) throw new Error(`${folder} にハイライト付きの段落を持つメモが要る (iPhone で作る)`)
    const want = structuredClone(target.doc)
    want.paragraphs.push({ style: { kind: "body" }, indent: 0, quote: false, inlines: [{ kind: "text", text: "末尾だけ変更", marks: [] }] })
    await $`${bridge} --store ${store} write --id ${target.id} < ${new Response(JSON.stringify({ doc: want }))}`.quiet()
    const after: string[][] = (await call("dump-attrs", "--id", target.id)).paragraphs
    expect(after[target.idx]).toContain("TTEmphasis")
  })
```
`dump-attrs --id <id>` はデバッグ用のサブコマンドで、段落ごとに、その段落の文字に付いている属性名の集合 (重複なし、昇順) を `{"paragraphs":[["TTEmphasis","TTStyle"],…]}` の形で返す。Step 3 で一緒に実装する。

- [ ] **Step 2: 失敗を確認** → **Step 3: 実装** → **Step 4: 通ることを確認** (`ORG_NOTES_SYNC_IT=1`)。
- [ ] **Step 5: コミット** (`Write paragraph diffs to Apple Notes through its own CRDT`)

---

### Task 9: メモ側アダプタ (`notes.ts`)

**Files:**
- Create: `tools/org-notes-sync/src/notes.ts`, `tools/org-notes-sync/src/notes.test.ts`

**Interfaces:**
- Consumes: bridge の CLI (Task 7, 8)。
- Produces:
  ```ts
  export type Runner = (cmd: string[], stdin?: string) => Promise<{ code: number; stdout: string; stderr: string }>
  export type FlushResult = "done" | "deferred" | "timeout" | "not-needed"
  export class BridgeError extends Error { constructor(message: string, readonly code: number) }
  export interface NotesPort {
    selfcheck(): Promise<void>
    list(): Promise<NoteSnap[] | null>
    write(id: string, doc: Doc): Promise<Doc>
    create(title: string): Promise<{ id: string; uri: string }>
    remove(uri: string): Promise<void>
    exportMedia(attachmentId: string, to: string): Promise<void>
    flush(ids: string[]): Promise<FlushResult>
  }
  export type NotesPortOptions = {
    bridge: string; folder: string; run?: Runner
    sleep?: (ms: number) => Promise<void>; now?: () => number
    syncState?: (ids: string[]) => { local: number; synced: number }[]   // 既定は bun:sqlite で読む
  }
  export function createNotesPort(o: NotesPortOptions): NotesPort
  ```
- `create`: JXA で `org` フォルダ (iCloud アカウント) を無ければ作り、`Notes.make({new:"note", at: folder, withProperties:{body: "<div>" + escapeHtml(title) + "</div>"}})` の `id()` (x-coredata uri) を得て、bridge `identify --uri` で id にする。
- `remove`: JXA で `Notes.delete(Notes.notes.byId(uri))`。
- `flush(ids)`: ids が空で、呼び出し側が flushPending でもなければ `not-needed` (呼び出し側が判断して空なら呼ばない)。
  - 起動確認: JXA `Application("Notes").running()`。手前の判定: `lsappinfo info -only bundleid $(lsappinfo front)` の出力に `com.apple.Notes` を含むか。
  - 未起動 → `open -g -a Notes` → 送信済み待ち → JXA で `quit` → `done`。
  - 起動中で手前に無い → `quit` → 起動していない状態を 10 秒まで待つ → `open -g -a Notes` → 送信済み待ち → `done`。
  - 手前 → `deferred`。
  - 送信済み待ち: 2 秒おきに `syncState(ids)` を見て、全件 `local === synced` になれば完了。60 秒で `timeout`。
- `syncState` の既定実装: `bun:sqlite` で `~/Library/Group Containers/group.com.apple.notes/NoteStore.sqlite` を `readonly` で開き、
  `select s.ZCURRENTLOCALVERSION, s.ZLATESTVERSIONSYNCEDTOCLOUD from ZICCLOUDSYNCINGOBJECT o join ZICCLOUDSTATE s on s.Z_PK = o.ZCLOUDSTATE where o.ZIDENTIFIER = ?`。

- [ ] **Step 1: テスト** (Runner を偽物にして、呼ばれたコマンドの列を検証する)

```ts
import { expect, test } from "bun:test"
import { BridgeError, createNotesPort, type Runner } from "./notes"

function fakeRunner(handlers: [RegExp, (cmd: string[], stdin?: string) => { code?: number; stdout?: string; stderr?: string }][]) {
  const calls: string[] = []
  const run: Runner = async (cmd, stdin) => {
    const line = cmd.join(" "); calls.push(line)
    for (const [re, h] of handlers) if (re.test(line)) { const r = h(cmd, stdin); return { code: r.code ?? 0, stdout: r.stdout ?? "", stderr: r.stderr ?? "" } }
    throw new Error(`unexpected command: ${line}`)
  }
  return { run, calls }
}
const opts = (run: Runner, extra = {}) => ({ bridge: "/b", folder: "org", run, sleep: async () => {}, ...extra })

test("selfcheck の失敗は BridgeError (code 3)", async () => {
  const { run } = fakeRunner([[/selfcheck/, () => ({ code: 3, stderr: "missing ICNote" })]])
  await expect(createNotesPort(opts(run)).selfcheck()).rejects.toBeInstanceOf(BridgeError)
})
test("list: フォルダが無ければ null", async () => {
  const { run } = fakeRunner([[/list --folder org/, () => ({ stdout: '{"folderMissing":true,"notes":[]}' })]])
  expect(await createNotesPort(opts(run)).list()).toBeNull()
})
test("write は stdin に doc を渡し、読み戻しを返す", async () => {
  const doc = { paragraphs: [] }
  const { run } = fakeRunner([[/write --id N/, (_c, stdin) => ({ stdout: stdin ?? "" })]])
  expect(await createNotesPort(opts(run)).write("N", doc)).toEqual(doc)
})
test("flush: 未起動なら起動して送信を待ち、終了させる", async () => {
  const { run, calls } = fakeRunner([
    [/running\(\)/, () => ({ stdout: "false" })],
    [/open -g -a Notes/, () => ({})],
    [/quit/, () => ({})],
  ])
  let n = 0
  const r = await createNotesPort(opts(run, { syncState: () => [n++ < 2 ? { local: 2, synced: 1 } : { local: 2, synced: 2 }] })).flush(["N"])
  expect(r).toBe("done")
  expect(calls.some((c) => c.includes("open -g -a Notes"))).toBe(true)
  expect(calls.at(-1)).toContain("quit")
})
test("flush: 手前で使っているなら保留", async () => {
  const { run, calls } = fakeRunner([
    [/running\(\)/, () => ({ stdout: "true" })],
    [/lsappinfo/, () => ({ stdout: '"CFBundleIdentifier"="com.apple.Notes"' })],
  ])
  expect(await createNotesPort(opts(run, { syncState: () => [] })).flush(["N"])).toBe("deferred")
  expect(calls.some((c) => c.includes("quit"))).toBe(false)
})
test("flush: 60 秒で揃わなければ timeout", async () => {
  let t = 0
  const { run } = fakeRunner([[/running\(\)/, () => ({ stdout: "false" })], [/open -g/, () => ({})], [/quit/, () => ({})]])
  const r = await createNotesPort(opts(run, { now: () => (t += 3000), syncState: () => [{ local: 2, synced: 1 }] })).flush(["N"])
  expect(r).toBe("timeout")
})
```
(`lsappinfo front` の結果を受け取ってから `lsappinfo info` を呼ぶ 2 段の呼び出しは、`sh -c` 1 回にまとめてよい。そのときはテストの正規表現をそれに合わせる。)

- [ ] **Step 2: 失敗を確認** → **Step 3: 実装** (`run` の既定は `Bun.spawn` で stdout / stderr を集める) → **Step 4: 通ることを確認**。
- [ ] **Step 5: コミット** (`Drive the notes bridge, AppleScript and Notes restarts`)

---

### Task 10: 1 周の同期と CLI (`sync.ts` / `cli/org-notes-sync.ts`)

**Files:**
- Create: `tools/org-notes-sync/src/sync.ts`, `src/sync.test.ts`, `src/lock.ts`, `src/lock.test.ts`, `src/log.ts`, `tools/org-notes-sync/cli/org-notes-sync.ts`

**Interfaces:**
- Consumes: Task 5 (`reconcile`)、Task 6、Task 9 (`NotesPort`)。
- Produces:
  ```ts
  // sync.ts
  export type SyncDeps = { format: LocalFormat; notes: NotesPort; orgDir: string; stateDir: string; force: boolean; log: (msg: string) => void; warn: (msg: string) => void }
  export type SyncOutcome = { status: "ok" | "aborted"; reason?: string; wroteNotes: string[] }
  export async function syncOnce(d: SyncDeps): Promise<SyncOutcome>
  // lock.ts
  export function withLock(stateDir: string, body: () => Promise<number>): Promise<number>
  // log.ts
  export function makeLogger(stateDir: string): { log(msg: string): void; warn(msg: string): void }
  ```
- `syncOnce` の流れ: `selfcheck` (失敗 → aborted "bridge") → `loadState` → `list` → `listOrgFiles` → `reconcile` (`ok:false` → aborted、reason をそのまま) → actions を順に実行 (各 action を try/catch。失敗は warn して次へ) → state をペア 1 件ごとに保存 → 画像: ペア済みのメモの Doc に出てくる画像 id のうち `.notes-media` に無いものを `exportMedia` → opt-in な org 全部から参照される画像 id 以外を `gcMedia` → 書いたメモがあるか `flushPending` なら `flush` (deferred / timeout → `flushPending = true`、done → false) → 保存。
- action の実行:
  - pushNotes: `base = await notes.write(noteId, doc)`
  - pullOrg / createOrg / conflict: `writeOrgFile` (pullOrg と conflict は読んだときの mtime、createOrg は null)。stale なら何もしない (base も変えない)。書けたら base を更新。conflict は warn (`<file>: 衝突しました。Emacs で解消して保存してください`)。
  - both: org を書く (stale なら全部中止) → `base = await notes.write(...)`
  - blocked: warn (`<file>: 衝突マーカーが残っているので同期を止めています`)
  - createNote: `{id} = await notes.create(docTitle(doc))` → `base = await notes.write(id, doc)` → ペア追加
  - deleteNote: `notes.remove(uri)` → ペア削除。trashOrg: `trashOrgFile` → ペア削除。drop: ペア削除。
- `withLock`: `stateDir/lock` を `O_EXCL` で作って pid を書く。取れなければ、中の pid が生きていれば `stateDir/pending` を作って 0 を返す。死んでいれば消して取り直す。本体のあと `pending` があれば消してもう 1 周する。
- `makeLogger`: `stateDir/log` に `ISO時刻 メッセージ` を追記、1 MB を超えたら後半 512 KB だけ残す。`warn` は加えて stderr に `org-notes-sync: <msg>` を出す (Emacs がこれを拾う)。
- CLI: `--force`、`--org-dir`、`--state-dir`、`--folder` (メモ.app のフォルダ名、既定 `org`。実機確認でテスト用フォルダに差し替える)、`--bridge` (既定 `~/.local/bin/notes-bridge`、環境変数 `ORG_NOTES_SYNC_BRIDGE` が優先)。終了コード: ok 0、aborted 2。aborted の reason ごとの警告文:
  - bridge: `メモ.app の内部 API が見つかりません (macOS の更新で変わった可能性)。同期を止めています`
  - DB を開けない (BridgeError code 4): `メモの DB を読めません。システム設定 > プライバシーとセキュリティ > フルディスクアクセス に ~/.local/bin/org-notes-sync を追加してください`
  - folder-missing / notes-empty: `メモ.app の org フォルダが見つからない / 空です。誤削除を避けるため同期を止めています`
  - mass-delete: `一度に多くのメモを削除しようとしています。意図どおりなら --force を付けて実行してください`

- [ ] **Step 1: シナリオテストを書く** (`src/sync.test.ts`、偽の `NotesPort` をメモリ上に作る)

```ts
import { beforeEach, expect, test } from "bun:test"
import fs from "node:fs"; import os from "node:os"; import path from "node:path"
import { syncOnce } from "./sync"
import { orgFormat } from "./formats/org"
import { para, text, type Doc } from "./doc"
import type { NotesPort } from "./notes"
import type { NoteSnap } from "./reconcile"

class FakeNotes implements NotesPort {
  notes = new Map<string, Doc>(); folder = true; flushed: string[][] = []; seq = 0; exported: string[] = []
  async selfcheck() {}
  async list(): Promise<NoteSnap[] | null> { return this.folder ? [...this.notes].map(([id, doc]) => ({ id, uri: `uri:${id}`, doc })) : null }
  async write(id: string, doc: Doc) { this.notes.set(id, structuredClone(doc)); return structuredClone(doc) }
  async create(title: string) { const id = `N${++this.seq}`; this.notes.set(id, { paragraphs: [para({ kind: "title" }, [text(title)])] }); return { id, uri: `uri:${id}` } }
  async remove(uri: string) { this.notes.delete(uri.replace("uri:", "")) }
  async exportMedia(id: string, to: string) { this.exported.push(id); fs.writeFileSync(to, "") }
  async flush(ids: string[]) { this.flushed.push(ids); return "done" as const }
}

let orgDir = "", stateDir = "", fake = new FakeNotes(), warns: string[] = []
const sync = (force = false) => syncOnce({ format: orgFormat, notes: fake, orgDir, stateDir, force, log: () => {}, warn: (m) => warns.push(m) })
const read = (f: string) => fs.readFileSync(path.join(orgDir, f), "utf8")
const write = (f: string, s: string) => { fs.writeFileSync(path.join(orgDir, f), s); const t = new Date(Date.now() + 1000 * Math.random()); fs.utimesSync(path.join(orgDir, f), t, t) }
const body = (title: string, ...lines: string[]): Doc => ({ paragraphs: [para({ kind: "title" }, [text(title)]), ...lines.map((l) => para({ kind: "body" }, [text(l)]))] })

beforeEach(() => {
  orgDir = fs.mkdtempSync(path.join(os.tmpdir(), "ons-org-")); stateDir = fs.mkdtempSync(path.join(os.tmpdir(), "ons-st-"))
  fake = new FakeNotes(); warns = []
})

test("org で opt-in → メモができ、iPhone の編集が org に戻る", async () => {
  write("買い物.org", "#+title: 買い物\n#+notes_sync: yes\n卵\n")
  await sync()
  const [id] = [...fake.notes.keys()]
  expect(fake.notes.get(id!)).toEqual(body("買い物", "卵"))
  expect(fake.flushed).toEqual([[id]])
  fake.notes.set(id!, body("買い物", "卵", "牛乳"))
  await sync()
  expect(read("買い物.org")).toBe("#+title: 買い物\n#+notes_sync: yes\n卵\n牛乳\n")
})

test("衝突 → マーカー → 解消して保存 → メモへ", async () => {
  write("a.org", "#+title: a\n#+notes_sync: yes\nx\n"); await sync()
  const [id] = [...fake.notes.keys()]
  write("a.org", "#+title: a\n#+notes_sync: yes\nORG\n"); fake.notes.set(id!, body("a", "NOTES"))
  await sync()
  expect(read("a.org")).toContain("<<<<<<< org")
  expect(fake.notes.get(id!)).toEqual(body("a", "NOTES"))
  await sync()   // マーカーが残っている間は止まる
  expect(warns.some((w) => w.includes("衝突マーカー"))).toBe(true)
  write("a.org", "#+title: a\n#+notes_sync: yes\nORG\nNOTES\n"); await sync()
  expect(fake.notes.get(id!)).toEqual(body("a", "ORG", "NOTES"))
})

test("iPhone で作ったメモ → org ができる、メモ削除 → ゴミ箱", async () => {
  fake.notes.set("P", body("思いつき", "アイデア")); await sync()
  expect(read("思いつき.org")).toBe("#+title: 思いつき\n#+notes_sync: yes\nアイデア\n")
  fake.notes.delete("P"); fake.notes.set("Q", body("別")); await sync()
  expect(fs.existsSync(path.join(orgDir, "思いつき.org"))).toBe(false)
  expect(fs.readdirSync(path.join(orgDir, ".notes-trash"))).toEqual(["思いつき.org"])
})

test("opt-in を外す → メモを消す", async () => {
  write("a.org", "#+title: a\n#+notes_sync: yes\nx\n"); write("keep.org", "#+title: keep\n#+notes_sync: yes\n"); await sync()
  write("a.org", "#+title: a\nx\n"); await sync()
  expect([...fake.notes.values()].map((d) => d.paragraphs[0]!.inlines)).toEqual([[text("keep")]])
})

test("画像を書き出し、参照されなくなったら消す", async () => {
  const img = { kind: "attachment" as const, id: "IMG1", image: true }
  fake.notes.set("P", { paragraphs: [para({ kind: "title" }, [text("写真")]), para({ kind: "body" }, [img])] }); await sync()
  expect(read("写真.org")).toContain("[[file:.notes-media/IMG1.jpg]]")
  expect(fs.existsSync(path.join(orgDir, ".notes-media/IMG1.jpg"))).toBe(true)
  fake.notes.set("P", body("写真")); await sync()
  expect(fs.existsSync(path.join(orgDir, ".notes-media/IMG1.jpg"))).toBe(false)
})

test("state を失っても重複しない", async () => {
  write("a.org", "#+title: a\n#+notes_sync: yes\nx\n"); await sync()
  fs.rmSync(path.join(stateDir, "state.json")); await sync()
  expect(fake.notes.size).toBe(1)
})

test("フォルダが消えたら何もしない", async () => {
  write("a.org", "#+title: a\n#+notes_sync: yes\nx\n"); await sync()
  fake.folder = false
  expect((await sync()).status).toBe("aborted")
  expect(fs.existsSync(path.join(orgDir, "a.org"))).toBe(true)
})

test("送信を保留したら次の周で再挑戦する", async () => {
  write("a.org", "#+title: a\n#+notes_sync: yes\nx\n")
  fake.flush = async (ids) => { fake.flushed.push(ids); return "deferred" }
  await sync()
  fake.flush = async (ids) => { fake.flushed.push(ids); return "done" }
  await sync()
  expect(fake.flushed.length).toBe(2)
})
```

`src/lock.test.ts`:
```ts
import { expect, test } from "bun:test"
import fs from "node:fs"; import os from "node:os"; import path from "node:path"
import { withLock } from "./lock"
test("実行中に呼ばれたら pending を残し、終わり際にもう 1 周する", async () => {
  const d = fs.mkdtempSync(path.join(os.tmpdir(), "ons-lock-")); let runs = 0
  await withLock(d, async () => {
    runs++
    if (runs === 1) expect(await withLock(d, async () => { throw new Error("走ってはいけない") })).toBe(0)
    return 0
  })
  expect(runs).toBe(2)
  expect(fs.existsSync(path.join(d, "lock"))).toBe(false)
})
test("死んだプロセスの lock は取り直す", async () => {
  const d = fs.mkdtempSync(path.join(os.tmpdir(), "ons-lock-"))
  fs.writeFileSync(path.join(d, "lock"), "99999999")
  expect(await withLock(d, async () => 7)).toBe(7)
})
```

- [ ] **Step 2: 失敗を確認** → **Step 3: 実装** → **Step 4: 通ることを確認**: `bun test` / `bunx tsc --noEmit`。
- [ ] **Step 5: CLI** (`cli/org-notes-sync.ts`、先頭行 `#!/usr/bin/env bun`。`createNotesPort` / `makeLogger` / `withLock` / `syncOnce` をつなぐだけ)。`bun cli/org-notes-sync.ts --org-dir <空の一時ディレクトリ> --state-dir <一時ディレクトリ> --bridge /nonexistent` が警告を出して終了コード 2 になることを手で確かめる。
- [ ] **Step 6: コミット** (`Run one sync pass from the CLI with locking and logs`)

---

### Task 11: インストールと launchd

**Files:**
- Create: `tools/org-notes-sync/launchd/jp.wamei.org-notes-sync.plist.in`, `tools/org-notes-sync/install.sh`, `tools/org-notes-sync/README.md`
- Modify: `mise.toml` (`[tasks."org-notes-sync:install"]`)

**Interfaces:**
- Produces: `mise run org-notes-sync:install` → `~/.local/bin/notes-bridge`、`~/.local/bin/org-notes-sync` (`bun build --compile`)、`~/Library/LaunchAgents/jp.wamei.org-notes-sync.plist` を置いて `launchctl bootout` → `launchctl bootstrap gui/$(id -u)`。

- [ ] **Step 1: plist テンプレート** (`@HOME@` を install.sh が置き換える)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key><string>jp.wamei.org-notes-sync</string>
  <key>ProgramArguments</key>
  <array><string>@HOME@/.local/bin/org-notes-sync</string></array>
  <key>StartInterval</key><integer>300</integer>
  <key>RunAtLoad</key><true/>
  <key>StandardOutPath</key><string>@HOME@/.local/state/org-notes-sync/launchd.out</string>
  <key>StandardErrorPath</key><string>@HOME@/.local/state/org-notes-sync/launchd.err</string>
</dict>
</plist>
```

- [ ] **Step 2: install.sh**

```sh
#!/bin/sh
# org-notes-sync を ~/.local/bin に入れ、launchd に登録する。
# フルディスクアクセスは手で 1 回与える (README 参照)。作り直すと署名が変わるので与え直す。
set -eu
here=$(cd "$(dirname "$0")" && pwd)
bin="$HOME/.local/bin"; state="$HOME/.local/state/org-notes-sync"; agents="$HOME/Library/LaunchAgents"
label=jp.wamei.org-notes-sync
mkdir -p "$bin" "$state" "$agents"
sh "$here/bridge/build.sh" "$bin/notes-bridge"
(cd "$here" && bun install --frozen-lockfile && bun build --compile cli/org-notes-sync.ts --outfile "$bin/org-notes-sync")
sed "s|@HOME@|$HOME|g" "$here/launchd/$label.plist.in" > "$agents/$label.plist"
launchctl bootout "gui/$(id -u)/$label" 2>/dev/null || true
launchctl bootstrap "gui/$(id -u)" "$agents/$label.plist"
echo "installed. フルディスクアクセスに $bin/org-notes-sync を追加してください"
```

- [ ] **Step 3: mise.toml**

```toml
[tasks."org-notes-sync:install"]
description = "org-notes-sync をビルドして ~/.local/bin と launchd に入れる"
run = ["sh tools/org-notes-sync/install.sh"]
```

- [ ] **Step 4: README.md** に次を書く: 何をするか (spec へのリンク)、`mise run org-notes-sync:install`、フルディスクアクセスの付け方、`#+notes_sync: yes` の使い方、メモ.app の `org` フォルダ、衝突の解消、`--force`、ログの場所、アンインストール (`launchctl bootout` と plist / バイナリの削除)、**実機確認の手順** (Task 13 の手順をそのまま載せる)。
- [ ] **Step 5: 確認**: `sh -n tools/org-notes-sync/install.sh`、`plutil -lint` に置き換え後の plist を通す (`sed "s|@HOME@|$HOME|g" … | plutil -lint -`)。install.sh 自体の実行は Task 13 で行う (本物の環境を変えるため)。
- [ ] **Step 6: コミット** (`Install org-notes-sync and schedule it with launchd`)

---

### Task 12: Emacs から使う (init.el)

**Files:**
- Modify: `.emacs.d/init.el` (`leaf project-memo` の `:config` の末尾に追記。数行の設定と関数 2 つなので init.el に直接書く)
- Test: scratchpad の `org-notes-sync-test.el` (repo には残さない)

**Interfaces:**
- Produces:
  ```elisp
  (defvar wamei/org-notes-sync-program (expand-file-name "~/.local/bin/org-notes-sync"))
  (defun wamei/org-notes-sync-buffer-p (&optional buffer) ...)   ; org-directory 配下 かつ #+notes_sync: yes
  (defun org-notes-sync () (interactive) ...)                      ; 非同期に 1 回走らせる。実行中なら何もしない
  (defun wamei/org-notes-sync--after-save () ...)                  ; 対象バッファなら org-notes-sync
  (defun wamei/org-notes-sync--setup-buffer () ...)                ; org-mode-hook: 対象なら auto-revert-mode と画像表示
  ```
- 振る舞い:
  - `wamei/org-notes-sync-buffer-p`: `buffer-file-name` が `(file-in-directory-p file org-directory)` で、バッファ先頭 4KB に `^#\+notes_sync:[ \t]*yes[ \t]*$` (大文字小文字を区別しない) がある。
  - `org-notes-sync`: プログラムが実行可能でなければ `message` だけ。`make-process` (`:name "org-notes-sync"`, `:buffer " *org-notes-sync*"`, `:noquery t`)。sentinel で終了したら、出力バッファの `org-notes-sync: ` で始まる行を `message` で出す。プロセスが生きていれば新しく起こさない (CLI 側の pending が拾うので取りこぼさない)。
  - `org-mode-hook` に `wamei/org-notes-sync--setup-buffer`: 対象バッファなら `(add-hook 'after-save-hook #'wamei/org-notes-sync--after-save nil t)`、`(auto-revert-mode 1)`、GUI (`display-graphic-p`) なら `(setq-local org-image-actual-width '(480))` と `(org-display-inline-images)`、`after-revert-hook` (バッファローカル) に GUI なら `org-display-inline-images` を足す。tty では画像の処理をしない (後日)。

- [ ] **Step 1: テストを書く** (scratchpad、init.el の defun フォームだけを read して eval する。手法はメモリの `init-el-nested-defun-batch-testing`)

```elisp
;;; org-notes-sync-test.el --- batch: emacs -Q --batch -l ert -l org-notes-sync-test.el -f ert-run-tests-batch-and-exit
(require 'ert) (require 'org)
(defvar test-init (expand-file-name ".emacs.d/init.el" (getenv "WORKTREE")))
(defun test--search-defun (form name)
  (cond ((not (consp form)) nil)
        ((and (memq (car form) '(defun defvar)) (eq (car-safe (cdr form)) name)) form)
        (t (let ((rest form) found)
             (while (and (consp rest) (not found))
               (setq found (test--search-defun (car rest) name)) (setq rest (cdr rest)))
             found))))
(with-temp-buffer
  (insert-file-contents test-init)
  (let (form)
    (condition-case nil
        (while (setq form (read (current-buffer)))
          (dolist (name '(wamei/org-notes-sync-program wamei/org-notes-sync-buffer-p org-notes-sync
                          wamei/org-notes-sync--after-save wamei/org-notes-sync--setup-buffer))
            (let ((f (test--search-defun form name))) (when f (eval f t)))))
      (end-of-file nil))))

(defmacro test--with-memo (content &rest body)
  `(let* ((org-directory (make-temp-file "ons-org" t))
          (file (expand-file-name "m.org" org-directory)))
     (with-temp-file file (insert ,content))
     (with-current-buffer (find-file-noselect file)
       (unwind-protect (progn ,@body) (set-buffer-modified-p nil) (kill-buffer)))))

(ert-deftest ons-buffer-p ()
  (test--with-memo "#+title: m\n#+NOTES_SYNC: Yes\n" (should (wamei/org-notes-sync-buffer-p)))
  (test--with-memo "#+title: m\n" (should-not (wamei/org-notes-sync-buffer-p))))

(ert-deftest ons-after-save-runs-only-for-target ()
  (let ((calls 0))
    (cl-letf (((symbol-function 'org-notes-sync) (lambda () (setq calls (1+ calls)))))
      (test--with-memo "#+title: m\n#+notes_sync: yes\n" (wamei/org-notes-sync--after-save))
      (test--with-memo "#+title: m\n" (wamei/org-notes-sync--after-save)))
    (should (= calls 1))))

(ert-deftest ons-setup-buffer ()
  (test--with-memo "#+title: m\n#+notes_sync: yes\n"
    (wamei/org-notes-sync--setup-buffer)
    (should auto-revert-mode)
    (should (memq #'wamei/org-notes-sync--after-save after-save-hook)))
  (test--with-memo "#+title: m\n"
    (wamei/org-notes-sync--setup-buffer)
    (should-not auto-revert-mode)))

(ert-deftest ons-command-without-program ()
  (let ((wamei/org-notes-sync-program "/nonexistent/org-notes-sync") (msgs nil))
    (cl-letf (((symbol-function 'message) (lambda (&rest a) (push (apply #'format a) msgs))))
      (org-notes-sync))
    (should (string-match-p "org-notes-sync" (car msgs)))))
```
`ons-setup-buffer` は org-mode-hook 経由ではなく関数を直接呼ぶので、`find-file-noselect` で org-mode が先に有効になっている前提でよい。`:hook` の配線は、init.el を read して `leaf project-memo` 内に `(add-hook 'org-mode-hook #'wamei/org-notes-sync--setup-buffer)` があることを、同じ test--search の要領で別のテストで確かめる。

- [ ] **Step 2: 失敗を確認**: `WORKTREE=<worktree> emacs -Q --batch -l ert -l <scratchpad>/org-notes-sync-test.el -f ert-run-tests-batch-and-exit` → 関数が無いので FAIL。
- [ ] **Step 3: init.el に実装を足す** (`leaf project-memo` の `:config` の末尾。コメントで「同期本体は tools/org-notes-sync、ここは保存で蹴るだけ」「tty の画像表示は後日」と書く)。
- [ ] **Step 4: 通ることを確認** (同じコマンド)。worktree の init.el 全体が読めることも確認: `emacs -Q --batch --eval '(setq user-init-file "<worktree>/.emacs.d/init.el")' --eval '(load user-init-file)'` がエラーで止まらないこと。
- [ ] **Step 5: `git diff .emacs.d/init.el` で自分のハンクだけか確認してからコミット** (`Sync opted-in org memos to Notes when saved in Emacs`)

---

### Task 13: 実機での確認 (メインセッションでユーザーと行う)

subagent ではなくメインセッションで、ユーザーの操作を挟みながら行う。

- [ ] **Step 1**: master へのマージ前に、worktree から `ORG_NOTES_SYNC_IT=1 ORG_NOTES_SYNC_IT_FOLDER=org-sync-probe bun test` (DB のコピー相手) が通る。
- [ ] **Step 2**: `mise run org-notes-sync:install` を worktree で実行し、ユーザーにフルディスクアクセスを付けてもらう。launchd は一旦 `launchctl bootout gui/$(id -u)/jp.wamei.org-notes-sync` で止めておき、手動で回す。
- [ ] **Step 3**: 一時的な org ディレクトリ (`--org-dir`) とメモ.app のテスト用フォルダ (`ORG_NOTES_SYNC_FOLDER` 環境変数で `org` を差し替えられるようにする — Task 10 の CLI に `--folder` を足す) で、次を順に確かめる:
  1. org で opt-in したメモがメモ.app に出る (Mac → iPhone で表示確認)
  2. iPhone で追記・チェック・見出し → org に戻る
  3. iPhone で写真を貼る → org に画像リンクと `.notes-media` の JPEG
  4. 両側で同じ行を編集 → org に衝突マーカー、解消して保存 → iPhone に反映
  5. iPhone でメモを削除 → org が `.notes-trash` へ
  6. メモ.app を手前で開いたまま同期 → 保留され、背後に回してから同期すると送信される
- [ ] **Step 4**: 問題が無ければ launchd を有効にし、`~/org` の本番で `#+notes_sync: yes` を 1 つのメモに付けて確かめる。
