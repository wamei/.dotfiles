import { afterAll, beforeAll, describe, expect, test } from "bun:test"
import { $ } from "bun"
import fs from "node:fs"; import os from "node:os"; import path from "node:path"
import { imageIds, type Doc } from "./doc"

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
const folder = process.env.ORG_NOTES_SYNC_IT_FOLDER ?? "org-sync-probe"

describe.skipIf(!IT)("notes-bridge (DB のコピー)", () => {
  beforeAll(async () => {
    bridge = path.join(fs.mkdtempSync(path.join(os.tmpdir(), "ons-bin-")), "notes-bridge")
    await $`sh ${path.join(here, "bridge/build.sh")} ${bridge}`
    store = await makeStoreCopy()
  })
  afterAll(() => {
    if (store) fs.rmSync(store, { recursive: true, force: true })
    if (bridge) fs.rmSync(path.dirname(bridge), { recursive: true, force: true })
  })
  test("selfcheck", async () => expect(await call("selfcheck")).toEqual({ ok: true }))
  test("list: 無いフォルダ", async () => expect(await call("list", "--folder", "存在しないフォルダ-ons")).toEqual({ folderMissing: true, notes: [] }))
  // AppleScript (HTML) で作られたメモは 1 段落目が body のことがある (実物で確認) ので、
  // タイトルで始まるのは「メモ.app で作ったメモがある」ことだけ確かめる
  test("list と read が同じ Doc を返し、メモ.app で作ったメモの 1 段落目はタイトル", async () => {
    const r = await call("list", "--folder", folder)
    expect(r.folderMissing).toBe(false)
    expect(r.notes.some((n: { doc: Doc }) => n.doc.paragraphs[0]?.style.kind === "title")).toBe(true)
    for (const n of r.notes) {
      expect(n.doc.paragraphs.length).toBeGreaterThan(0)
      expect((await call("read", "--id", n.id)).doc).toEqual(n.doc)
      expect((await call("identify", "--uri", n.uri)).id).toBe(n.id)
    }
  })
  test("read: 無い id は終了コード 1", async () => {
    const r = await $`${bridge} --store ${store} read --id ${"00000000-0000-0000-0000-000000000000"}`.nothrow().quiet()
    expect(r.exitCode).toBe(1)
  })
  test("--store に DB が無ければ終了コード 4", async () => {
    const empty = fs.mkdtempSync(path.join(os.tmpdir(), "ons-empty-"))
    const r = await $`${bridge} --store ${empty} list --folder ${folder}`.nothrow().quiet()
    expect(r.exitCode).toBe(4)
    expect(fs.readdirSync(empty)).toEqual([])
    fs.rmSync(empty, { recursive: true, force: true })
  })
  test("export-media: 画像を JPEG で書き出す", async () => {
    const r = await call("list", "--folder", folder)
    const ids = r.notes.flatMap((n: { doc: Doc }) => imageIds(n.doc))
    if (ids.length === 0) return
    const out = path.join(fs.mkdtempSync(path.join(os.tmpdir(), "ons-media-")), "x.jpg")
    expect(await call("export-media", "--attachment", ids[0], "--to", out)).toEqual({ written: true })
    const head = fs.readFileSync(out).subarray(0, 3)
    expect([...head]).toEqual([0xff, 0xd8, 0xff])
    fs.rmSync(path.dirname(out), { recursive: true, force: true })
  })
  test("本物の DB のスキーマを変えない", async () => {
    const schema = async (p: string) => $`sqlite3 ${"file:" + p + "?mode=ro"} ${"select sql from sqlite_master order by name"}`.text()
    expect(await schema(path.join(store, "NoteStore.sqlite"))).toBe(await schema(path.join(live, "NoteStore.sqlite")))
  })
})
