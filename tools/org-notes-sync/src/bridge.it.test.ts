import { afterAll, beforeAll, describe, expect, test } from "bun:test"
import { $ } from "bun"
import fs from "node:fs"; import os from "node:os"; import path from "node:path"
import { docTitle, imageIds, plainText, type Doc, type Paragraph } from "./doc"
import type { BridgeNote } from "./bridge-types"

const IT = process.env.ORG_NOTES_SYNC_IT === "1"
const here = path.resolve(import.meta.dir, "..")
const live = path.join(os.homedir(), "Library/Group Containers/group.com.apple.notes")
let store = "", bridge = ""
const tmpDirs: string[] = []
const tmp = (prefix: string) => {
  const d = fs.mkdtempSync(path.join(os.tmpdir(), prefix))
  tmpDirs.push(d)
  return d
}

// 本物の DB は読むだけ。sqlite の .backup で一貫したコピーを取り、他のファイルは rsync で写す
export async function makeStoreCopy(): Promise<string> {
  const d = fs.mkdtempSync(path.join(os.tmpdir(), "ons-store-"))
  await $`rsync -a --exclude ${"NoteStore.sqlite*"} ${live + "/"} ${d + "/"}`.quiet()
  await $`sqlite3 ${"file:" + live + "/NoteStore.sqlite?mode=ro"} ${".backup " + path.join(d, "NoteStore.sqlite")}`.quiet()
  return d
}
const callIn = async (dir: string, ...args: string[]) => JSON.parse(await $`${bridge} --store ${dir} ${args}`.text())
const call = (...args: string[]) => callIn(store, ...args)
const run = (...args: string[]) => $`${bridge} ${args}`.nothrow().quiet()
const folder = process.env.ORG_NOTES_SYNC_IT_FOLDER ?? "org-sync-probe"
// 既定のフォルダ (org-sync-probe) には中身の分かっているメモ「太文字タイトル」がある
const usingFixture = process.env.ORG_NOTES_SYNC_IT_FOLDER === undefined
const FIXTURE = "太文字タイトル"

type ListResult = { folderMissing: boolean; notes: BridgeNote[] }
const docs = (r: ListResult): { id: string; uri: string; doc: Doc }[] =>
  r.notes.flatMap((n) => ("doc" in n ? [n] : []))
const fixtureDoc = async (): Promise<Doc> => {
  const n = docs(await call("list", "--folder", folder)).find((x) => docTitle(x.doc) === FIXTURE)
  if (!n) throw new Error(`${FIXTURE} が ${folder} に無い`)
  return n.doc
}
const findPara = (d: Doc, f: (p: Paragraph) => boolean) => d.paragraphs.find(f)

describe.skipIf(!IT)("notes-bridge (DB のコピー)", () => {
  beforeAll(async () => {
    bridge = path.join(tmp("ons-bin-"), "notes-bridge")
    await $`sh ${path.join(here, "bridge/build.sh")} ${bridge}`
    store = await makeStoreCopy()
    tmpDirs.push(store)
  })
  afterAll(() => {
    for (const d of tmpDirs) fs.rmSync(d, { recursive: true, force: true })
  })
  test("selfcheck", async () => expect(await call("selfcheck")).toEqual({ ok: true }))
  test("list: 無いフォルダ", async () => expect(await call("list", "--folder", "存在しないフォルダ-ons")).toEqual({ folderMissing: true, notes: [] }))
  // AppleScript (HTML) で作られたメモは 1 段落目が body のことがある (実物で確認) ので、
  // タイトルで始まるのは「メモ.app で作ったメモがある」ことだけ確かめる
  test("list と read が同じ Doc を返し、メモ.app で作ったメモの 1 段落目はタイトル", async () => {
    const r: ListResult = await call("list", "--folder", folder)
    expect(r.folderMissing).toBe(false)
    const ns = docs(r)
    expect(ns.some((n) => n.doc.paragraphs[0]?.style.kind === "title")).toBe(true)
    for (const n of ns) {
      expect(n.doc.paragraphs.length).toBeGreaterThan(0)
      expect((await call("read", "--id", n.id)).doc).toEqual(n.doc)
      expect((await call("identify", "--uri", n.uri)).id).toBe(n.id)
    }
  })
  test.skipIf(!usingFixture)("太文字タイトル の中身を Doc にする", async () => {
    const d = await fixtureDoc()
    expect(d.paragraphs.slice(0, 3)).toEqual([
      { style: { kind: "title" }, indent: 0, quote: false, inlines: [{ kind: "text", text: "太文字タイトル", marks: [] }] },
      { style: { kind: "heading" }, indent: 0, quote: false, inlines: [{ kind: "text", text: "太文字見出し", marks: [] }] },
      { style: { kind: "subheading" }, indent: 0, quote: false, inlines: [{ kind: "text", text: "太文字小見出し", marks: [] }] },
    ])
    expect(findPara(d, (p) => plainText(p) === "太文字")?.inlines).toEqual([{ kind: "text", text: "太文字", marks: ["bold"] }])
    expect(findPara(d, (p) => plainText(p) === "取り消し線")?.inlines).toEqual([{ kind: "text", text: "取り消し線", marks: ["strike"] }])
    expect(findPara(d, (p) => plainText(p) === "チェックリスト済")?.style).toEqual({ kind: "check", done: true })
    expect(findPara(d, (p) => plainText(p) === "チェックリスト")?.style).toEqual({ kind: "check", done: false })
    expect(findPara(d, (p) => plainText(p) === "引用")?.quote).toBe(true)
    expect(findPara(d, (p) => plainText(p) === "更にインデント")).toMatchObject({ style: { kind: "body" }, indent: 2 })
    expect(d.paragraphs.filter((p) => p.style.kind === "list").map((p) => p.indent)).toContain(8)
    const inlines = d.paragraphs.flatMap((p) => p.inlines)
    expect(inlines).toContainEqual({ kind: "text", text: "ぐーぐる", marks: [], link: "https://google.com" })
    const atts = inlines.filter((i) => i.kind === "attachment")
    expect(atts).toEqual([
      { kind: "attachment", id: "A10B17CA-5092-4F5C-B1F5-4EBE45FFD647", image: false },
      { kind: "attachment", id: "EA6F4566-502B-4356-A5B1-FF5B304366EC", image: true },
    ])
    // link の無い text にはキー自体が無い
    expect(inlines.filter((i) => i.kind === "text" && "link" in i && i.link === undefined)).toEqual([])
  })
  test("read: 無い id は終了コード 1", async () => {
    expect((await run("--store", store, "read", "--id", "00000000-0000-0000-0000-000000000000")).exitCode).toBe(1)
  })
  test("--store に DB が無ければ終了コード 4", async () => {
    const empty = tmp("ons-empty-")
    expect((await run("--store", empty, "list", "--folder", folder)).exitCode).toBe(4)
    expect(fs.readdirSync(empty)).toEqual([])
  })
  test("--store はサブコマンドの後ろでも効く", async () => {
    const empty = tmp("ons-empty-")
    // 後ろの --store が無視されて本物を開くなら 0 で終わってしまう
    expect((await run("list", "--store", empty, "--folder", folder)).exitCode).toBe(4)
    const r = JSON.parse((await $`${bridge} list --store ${store} --folder ${folder}`.text()))
    expect(r).toEqual(await call("list", "--folder", folder))
  })
  test("知らないオプション・重複・空の --store・余分な引数は終了コード 1", async () => {
    const cases = [
      ["--store", store, "list", "--folder", folder, "--bogus", "x"],
      ["--store", store, "read", "--folder", folder],
      ["--store", store, "list", "--folder", folder, "--folder", folder],
      ["--store", store, "--store", store, "selfcheck"],
      ["--store", "", "list", "--folder", folder],
      ["--store", store, "list", "--folder"],
      ["--store", store, "selfcheck", "extra"],
    ]
    for (const c of cases) expect({ c, code: (await run(...c)).exitCode }).toEqual({ c, code: 1 })
  })
  test("ロックされたメモは doc を出さず locked、read は終了コード 1", async () => {
    const locked = await makeStoreCopy()
    tmpDirs.push(locked)
    const target = docs(await callIn(locked, "list", "--folder", folder))[0]
    if (!target) throw new Error(`${folder} にメモが無い`)
    // コピーの DB だけを書き換える
    await $`sqlite3 ${path.join(locked, "NoteStore.sqlite")} ${`update ZICCLOUDSYNCINGOBJECT set ZISPASSWORDPROTECTED = 1 where ZIDENTIFIER = '${target.id}'`}`.quiet()
    const r: ListResult = await callIn(locked, "list", "--folder", folder)
    expect(r.notes).toContainEqual({ id: target.id, uri: target.uri, locked: true })
    const rd = await $`${bridge} --store ${locked} read --id ${target.id}`.nothrow().quiet()
    expect(rd.exitCode).toBe(1)
    expect(rd.stderr.toString()).toContain("ロック")
  })
  test.skipIf(!usingFixture)("export-media: 画像を JPEG で書き出し、既存のファイルは置き換える", async () => {
    const ids = imageIds(await fixtureDoc())
    expect(ids.length).toBeGreaterThan(0)
    const dir = tmp("ons-media-")
    const out = path.join(dir, "x.jpg")
    fs.writeFileSync(out, "old")
    expect(await call("export-media", "--attachment", ids[0]!, "--to", out)).toEqual({ written: true })
    expect([...fs.readFileSync(out).subarray(0, 3)]).toEqual([0xff, 0xd8, 0xff])
    expect(fs.readdirSync(dir)).toEqual(["x.jpg"])
  })
  test.skipIf(!usingFixture)("export-media: 書き出し先がディレクトリなら終了コード 1 で何も消さない", async () => {
    const ids = imageIds(await fixtureDoc())
    const dir = tmp("ons-media-")
    const out = path.join(dir, "x.jpg")
    fs.mkdirSync(out)
    fs.writeFileSync(path.join(out, "keep"), "k")
    expect((await run("--store", store, "export-media", "--attachment", ids[0]!, "--to", out)).exitCode).toBe(1)
    expect(fs.readdirSync(out)).toEqual(["keep"])
    expect(fs.readdirSync(dir)).toEqual(["x.jpg"])
  })
  const writeIn = (dir: string, id: string, doc: Doc) =>
    $`${bridge} --store ${dir} write --id ${id} < ${new Response(JSON.stringify({ doc }))}`.nothrow().quiet()
  test("write: 段落の追加・変更・スタイル変更が読み戻せ、他の段落の属性を残す", async () => {
    const r: ListResult = await call("list", "--folder", folder)
    const n = docs(r)[0]
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
    const r: ListResult = await call("list", "--folder", folder)
    let target: { id: string; doc: Doc; idx: number } | undefined
    for (const n of docs(r)) {
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
  test("dump-attrs: 段落ごとの属性名は重複なしの昇順", async () => {
    const n = docs(await call("list", "--folder", folder))[0]!
    const ps: string[][] = (await call("dump-attrs", "--id", n.id)).paragraphs
    expect(ps.length).toBe((await call("read", "--id", n.id)).doc.paragraphs.length)
    for (const ks of ps) expect(ks).toEqual([...new Set(ks)].sort())
  })
  // 添付・中間の書き換え・チェックの切り替えは、中身の分かっている 太文字タイトル を別のコピーで書き換えて確かめる
  describe.skipIf(!usingFixture)("write: 太文字タイトル (別のコピー)", () => {
    let ws = "", id = "", base: Doc = { paragraphs: [] }
    beforeAll(async () => {
      ws = await makeStoreCopy()
      tmpDirs.push(ws)
    })
    const reset = async () => {
      // 前のテストの書き込みを捨てて、もとのコピーから取り直す
      fs.rmSync(ws, { recursive: true, force: true })
      ws = await makeStoreCopy()
      tmpDirs.push(ws)
      const n = docs(await callIn(ws, "list", "--folder", folder)).find((x) => docTitle(x.doc) === FIXTURE)
      if (!n) throw new Error(`${FIXTURE} が ${folder} に無い`)
      id = n.id
      base = n.doc
    }
    const attIdx = (d: Doc) => d.paragraphs.findIndex((p) => p.inlines.some((i) => i.kind === "attachment"))
    test("中間の段落の文字とチェック状態を変え、他の段落はそのまま", async () => {
      await reset()
      const want = structuredClone(base)
      const ci = want.paragraphs.findIndex((p) => plainText(p) === "チェックリスト")
      want.paragraphs[ci]!.style = { kind: "check", done: true }
      const bi = want.paragraphs.findIndex((p) => plainText(p) === "太文字")
      want.paragraphs[bi]!.inlines = [{ kind: "text", text: "太", marks: ["bold"] }, { kind: "text", text: "い文字", marks: ["italic"] }]
      want.paragraphs.splice(1, 0, { style: { kind: "list", list: "number" }, indent: 0, quote: false, inlines: [{ kind: "text", text: "差し込み", marks: [] }] })
      const r = await writeIn(ws, id, want)
      expect(r.exitCode).toBe(0)
      expect(JSON.parse(r.stdout.toString()).doc).toEqual(want)
      expect((await callIn(ws, "read", "--id", id)).doc).toEqual(want)
    })
    test("添付のある段落に文字を足しても添付は残る", async () => {
      await reset()
      const want = structuredClone(base)
      want.paragraphs[attIdx(want)]!.inlines.push({ kind: "text", text: "後ろに足した", marks: [] })
      const r = await writeIn(ws, id, want)
      expect(r.exitCode).toBe(0)
      expect((await callIn(ws, "read", "--id", id)).doc).toEqual(want)
    })
    test("目標から消えた添付は消してよい", async () => {
      await reset()
      const want = structuredClone(base)
      const i = attIdx(want)
      const gone = want.paragraphs[i]!.inlines.filter((x) => x.kind === "attachment").map((x) => (x as { id: string }).id)
      want.paragraphs.splice(i, 1)
      const r = await writeIn(ws, id, want)
      expect(r.exitCode).toBe(0)
      const got: Doc = (await callIn(ws, "read", "--id", id)).doc
      expect(got).toEqual(want)
      expect(got.paragraphs.flatMap((p) => p.inlines).filter((x) => x.kind === "attachment" && gone.includes(x.id))).toEqual([])
    })
    test("目標に残る添付を消す・動かす書き込みは終了コード 5 で何も変えない", async () => {
      await reset()
      const want = structuredClone(base)
      const [moved] = want.paragraphs.splice(attIdx(want), 1)
      want.paragraphs.push(moved!)
      const r = await writeIn(ws, id, want)
      expect(r.exitCode).toBe(5)
      expect(r.stderr.toString()).toContain("添付")
      expect((await callIn(ws, "read", "--id", id)).doc).toEqual(base)
    })
    test("今のメモに無い添付 id は終了コード 5", async () => {
      await reset()
      const want = structuredClone(base)
      want.paragraphs.push({ style: { kind: "body" }, indent: 0, quote: false, inlines: [{ kind: "attachment", id: "00000000-0000-0000-0000-000000000000", image: true }] })
      const r = await writeIn(ws, id, want)
      expect(r.exitCode).toBe(5)
      expect((await callIn(ws, "read", "--id", id)).doc).toEqual(base)
    })
  })
  test("write: 無い id は終了コード 1、壊れた入力は終了コード 1", async () => {
    const n = docs(await call("list", "--folder", folder))[0]!
    expect((await writeIn(store, "00000000-0000-0000-0000-000000000000", { paragraphs: [] })).exitCode).toBe(1)
    const bad = await $`${bridge} --store ${store} write --id ${n.id} < ${new Response("{\"doc\":{}}")}`.nothrow().quiet()
    expect(bad.exitCode).toBe(1)
    expect((await call("read", "--id", n.id)).doc).toEqual(docs(await call("list", "--folder", folder))[0]!.doc)
  })
  test("本物の DB のスキーマを変えない", async () => {
    const schema = async (p: string) => $`sqlite3 ${"file:" + p + "?mode=ro"} ${"select sql from sqlite_master order by name"}`.text()
    expect(await schema(path.join(store, "NoteStore.sqlite"))).toBe(await schema(path.join(live, "NoteStore.sqlite")))
  })
})
