import { beforeEach, expect, test } from "bun:test"
import fs from "node:fs"; import os from "node:os"; import path from "node:path"
import { syncOnce } from "./sync"
import { orgFormat } from "./formats/org"
import { para, text, type Doc } from "./doc"
import { BridgeError, type FlushResult, type NotesPort } from "./notes"
import type { BridgeNote } from "./bridge-types"

class FakeNotes implements NotesPort {
  notes = new Map<string, Doc>(); locked = new Set<string>(); folder = true; flushed: string[][] = []; seq = 0; exported: string[] = []
  removed: string[] = []
  writeError: BridgeError | null = null
  async selfcheck() {}
  async list(): Promise<BridgeNote[] | null> {
    if (!this.folder) return null
    return [...this.notes].map(([id, doc]): BridgeNote => (this.locked.has(id) ? { id, uri: `uri:${id}`, locked: true } : { id, uri: `uri:${id}`, doc }))
  }
  async write(id: string, doc: Doc) {
    if (this.writeError) throw this.writeError
    this.notes.set(id, structuredClone(doc)); return structuredClone(doc)
  }
  async create(title: string) { const id = `N${++this.seq}`; this.notes.set(id, { paragraphs: [para({ kind: "title" }, [text(title)])] }); return { id, uri: `uri:${id}` } }
  async remove(uri: string) { this.removed.push(uri); this.notes.delete(uri.replace("uri:", "")) }
  async exportMedia(id: string, to: string) { this.exported.push(id); fs.writeFileSync(to, "") }
  async flush(ids: string[]): Promise<FlushResult> { this.flushed.push(ids); return "done" }
}

let orgDir = "", stateDir = "", fake = new FakeNotes(), warns: string[] = []
const sync = (force = false) => syncOnce({ format: orgFormat, notes: fake, orgDir, stateDir, force, log: () => {}, warn: (m) => warns.push(m) })
const read = (f: string) => fs.readFileSync(path.join(orgDir, f), "utf8")
const write = (f: string, s: string) => { fs.writeFileSync(path.join(orgDir, f), s); const t = new Date(Date.now() + 1000 * Math.random()); fs.utimesSync(path.join(orgDir, f), t, t) }
const body = (title: string, ...lines: string[]): Doc => ({ paragraphs: [para({ kind: "title" }, [text(title)]), ...lines.map((l) => para({ kind: "body" }, [text(l)]))] })
const statePairs = () => JSON.parse(fs.readFileSync(path.join(stateDir, "state.json"), "utf8")).pairs as { file: string; noteId: string }[]

beforeEach(() => {
  orgDir = fs.mkdtempSync(path.join(os.tmpdir(), "ons-org-")); stateDir = fs.mkdtempSync(path.join(os.tmpdir(), "ons-st-"))
  fake = new FakeNotes(); warns = []
})

test("org で opt-in → メモができ、iPhone の編集が org に戻る", async () => {
  write("買い物.org", "#+title: 買い物\n#+notes_sync: yes\n卵\n")
  await sync()
  const [id] = [...fake.notes.keys()]
  expect(fake.notes.get(id!)).toEqual(body("買い物", "卵"))
  expect(fake.flushed).toEqual([[id!]])
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
  expect(warns).toContain("a.org: 衝突しました。Emacs で解消して保存してください")
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
  await sync()
  expect(fake.exported).toEqual(["IMG1"]) // 既にあれば書き出し直さない
  fake.notes.set("P", body("写真")); await sync()
  expect(fs.existsSync(path.join(orgDir, ".notes-media/IMG1.jpg"))).toBe(false)
})

test("画像の書き出しに失敗しても続ける", async () => {
  const img = { kind: "attachment" as const, id: "IMG1", image: true }
  fake.exportMedia = async () => { throw new BridgeError("export failed", 1) }
  fake.notes.set("P", { paragraphs: [para({ kind: "title" }, [text("写真")]), para({ kind: "body" }, [img])] })
  expect((await sync()).status).toBe("ok")
  expect(warns.some((w) => w.includes("export failed"))).toBe(true)
  expect(statePairs()).toEqual([expect.objectContaining({ file: "写真.org", noteId: "P" })])
})

test("state を失っても重複しない", async () => {
  write("a.org", "#+title: a\n#+notes_sync: yes\nx\n"); await sync()
  fs.rmSync(path.join(stateDir, "state.json")); await sync()
  expect(fake.notes.size).toBe(1)
  expect(statePairs()).toEqual([expect.objectContaining({ file: "a.org" })])
})

test("フォルダが消えたら何もしない", async () => {
  write("a.org", "#+title: a\n#+notes_sync: yes\nx\n"); await sync()
  fake.folder = false
  const r = await sync()
  expect(r).toEqual({ status: "aborted", reason: "folder-missing", wroteNotes: [] })
  expect(fs.existsSync(path.join(orgDir, "a.org"))).toBe(true)
  expect(warns.some((w) => w.includes("誤削除を避けるため"))).toBe(true)
})

test("送信を保留したら次の周で再挑戦する", async () => {
  write("a.org", "#+title: a\n#+notes_sync: yes\nx\n")
  fake.flush = async (ids) => { fake.flushed.push(ids); return "deferred" }
  await sync()
  fake.flush = async (ids) => { fake.flushed.push(ids); return "done" }
  await sync()
  expect(fake.flushed.length).toBe(2)
  expect(fake.flushed[1]!.length).toBe(1) // 書いたメモが無くても、ペア済みのメモを送信し直す
  await sync()
  expect(fake.flushed.length).toBe(2)
})

test("ロックされたメモのペアは触らず警告する", async () => {
  write("a.org", "#+title: a\n#+notes_sync: yes\nx\n"); await sync()
  const [id] = [...fake.notes.keys()]
  fake.locked.add(id!)
  fake.notes.set("U", body("u")); fake.locked.add("U") // ペア無しのロック済みメモは黙って無視
  write("a.org", "#+title: a\n#+notes_sync: yes\nchanged\n")
  expect((await sync()).status).toBe("ok")
  expect(warns).toEqual(["a.org: メモがロックされているので同期しません"])
  expect(fake.removed).toEqual([])
  expect(fs.existsSync(path.join(orgDir, "a.org"))).toBe(true)
  expect(fs.existsSync(path.join(orgDir, "u.org"))).toBe(false)
  expect(statePairs()).toEqual([expect.objectContaining({ file: "a.org", noteId: id })])
})

test("ペア無しの org に衝突マーカー → 警告して作らない", async () => {
  write("a.org", "#+title: a\n#+notes_sync: yes\n<<<<<<< org\nx\n=======\ny\n>>>>>>> notes\n")
  await sync()
  expect(fake.notes.size).toBe(0)
  expect(warns).toEqual(["a.org: 衝突マーカーが残っているので同期を止めています"])
})

test("添付を含む段落の書き換え拒否 (code 5) は警告して base を変えない", async () => {
  write("a.org", "#+title: a\n#+notes_sync: yes\nx\n"); await sync()
  const base = statePairs()
  fake.writeError = new BridgeError("refused", 5)
  write("a.org", "#+title: a\n#+notes_sync: yes\ny\n")
  expect((await sync()).status).toBe("ok")
  expect(warns).toEqual(["a.org: 添付を含む段落を書き換えられないので、メモへの反映を止めました"])
  expect(statePairs()).toEqual(base)
  fake.writeError = new BridgeError("mismatch", 6); warns = []
  await sync()
  expect(warns).toEqual(["a.org: メモへの書き込みを検証できなかったので保存しませんでした"])
  fake.writeError = new BridgeError("boom", 1); warns = []
  await sync()
  expect(warns).toEqual(["a.org: boom"])
  fake.writeError = null
  await sync()
  expect(fake.notes.get([...fake.notes.keys()][0]!)).toEqual(body("a", "y"))
})

test("Emacs が保存し直していたら org を上書きしない", async () => {
  fake.notes.set("A", body("a", "1")); fake.notes.set("B", body("b", "1")); await sync()
  const before = statePairs()
  write("a.org", "#+title: a\n#+notes_sync: yes\n1\nA\n") // pushNotes (先に実行される)
  fake.notes.set("B", body("b", "1", "B"))                    // pullOrg
  const realWrite = fake.write.bind(fake)
  // listOrgFiles の後、b.org を書く前に Emacs が保存したことにする
  fake.write = async (id, doc) => { write("b.org", "#+title: b\n#+notes_sync: yes\nEMACS\n"); return realWrite(id, doc) }
  await sync()
  expect(read("b.org")).toBe("#+title: b\n#+notes_sync: yes\nEMACS\n")
  expect(statePairs().find((p) => p.file === "b.org")).toEqual(before.find((p) => p.file === "b.org")!)
})

test("全体停止: bridge / DB / org ディレクトリ / state.json / 大量削除", async () => {
  fake.selfcheck = async () => { throw new BridgeError("no class", 3) }
  expect(await sync()).toEqual({ status: "aborted", reason: "bridge", wroteNotes: [] })
  expect(warns.at(-1)).toBe("メモ.app の内部 API が見つかりません (macOS の更新で変わった可能性)。同期を止めています")

  fake = new FakeNotes()
  fake.list = async () => { throw new BridgeError("unable to open", 4) }
  expect((await sync()).reason).toBe("db")
  expect(warns.at(-1)).toBe("メモの DB を読めません。システム設定 > プライバシーとセキュリティ > フルディスクアクセス に ~/.local/bin/org-notes-sync を追加してください")

  fake = new FakeNotes()
  fs.writeFileSync(path.join(stateDir, "state.json"), "{")
  expect((await sync()).reason).toBe("state")
  expect(warns.at(-1)).toBe(`state.json が壊れています: ${path.join(stateDir, "state.json")}。中身を確認して直すか消してください`)
  fs.rmSync(path.join(stateDir, "state.json"))

  const missing = path.join(orgDir, "nope")
  const r = await syncOnce({ format: orgFormat, notes: fake, orgDir: missing, stateDir, force: false, log: () => {}, warn: (m) => warns.push(m) })
  expect(r.reason).toBe("org-dir")
  expect(warns.at(-1)).toBe(`org ディレクトリを読めません: ${missing}`)
  expect(fs.existsSync(path.join(stateDir, "state.json"))).toBe(false)

  for (const n of ["a", "b", "c", "d"]) write(`${n}.org`, `#+title: ${n}\n#+notes_sync: yes\n`)
  await sync()
  for (const n of ["a", "b", "c"]) write(`${n}.org`, `#+title: ${n}\n`)
  expect((await sync()).reason).toBe("mass-delete")
  expect(warns.at(-1)).toBe("一度に多くのメモを削除しようとしています。意図どおりなら --force を付けて実行してください")
  expect(fake.notes.size).toBe(4)
  expect((await sync(true)).status).toBe("ok")
  expect(fake.notes.size).toBe(1)
})
