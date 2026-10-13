import { expect, test } from "bun:test"
import { Database } from "bun:sqlite"
import { mkdtempSync } from "node:fs"
import { tmpdir } from "node:os"
import { join } from "node:path"
import { BridgeError, createNotesPort, defaultSyncStatePath, type Runner } from "./notes"

function fakeRunner(handlers: [RegExp, (cmd: string[], stdin?: string) => { code?: number; stdout?: string; stderr?: string }][]) {
  const calls: string[] = []
  const run: Runner = async (cmd, stdin) => {
    const line = cmd.join(" "); calls.push(line)
    for (const [re, h] of handlers) if (re.test(line)) { const r = h(cmd, stdin); return { code: r.code ?? 0, stdout: r.stdout ?? "", stderr: r.stderr ?? "" } }
    throw new Error(`unexpected command: ${line}`)
  }
  return { run, calls }
}
const TERM = '"bundleid"="com.apple.Terminal"'
const opts = (run: Runner, extra = {}) => ({ bridge: "/b", folder: "org", run, sleep: async () => {}, ...extra })

test("selfcheck の失敗は BridgeError (code 3)", async () => {
  const { run } = fakeRunner([[/selfcheck/, () => ({ code: 3, stderr: "missing ICNote\n" })]])
  const e = await createNotesPort(opts(run)).selfcheck().catch((x) => x)
  expect(e).toBeInstanceOf(BridgeError)
  expect(e.code).toBe(3)
  expect(e.message).toBe("missing ICNote")
})
test("bridge の stdout が JSON でなければ BridgeError (code 1)", async () => {
  const { run } = fakeRunner([[/list/, () => ({ stdout: "oops" })]])
  const e = await createNotesPort(opts(run)).list().catch((x) => x)
  expect(e).toBeInstanceOf(BridgeError)
  expect(e.code).toBe(1)
})
test("list: フォルダが無ければ null", async () => {
  const { run } = fakeRunner([[/list --folder org/, () => ({ stdout: '{"folderMissing":true,"notes":[]}' })]])
  expect(await createNotesPort(opts(run)).list()).toBeNull()
})
test("list: ロックされたメモもそのまま返す", async () => {
  const notes = [{ id: "A", uri: "u", locked: true as const }]
  const { run } = fakeRunner([[/list --folder org/, () => ({ stdout: JSON.stringify({ folderMissing: false, notes }) })]])
  expect(await createNotesPort(opts(run)).list()).toEqual(notes)
})
test("store を指定すると --store を渡す", async () => {
  const { run, calls } = fakeRunner([[/selfcheck/, () => ({ stdout: '{"ok":true}' })]])
  await createNotesPort(opts(run, { store: "/s" })).selfcheck()
  expect(calls[0]).toBe("/b --store /s selfcheck")
})
test("write は stdin に doc を渡し、読み戻しを返す", async () => {
  const doc = { paragraphs: [] }
  const { run } = fakeRunner([[/write --id N/, (_c, stdin) => ({ stdout: stdin ?? "" })]])
  expect(await createNotesPort(opts(run)).write("N", doc)).toEqual({ paragraphs: [] })
})
test("write は expect があれば stdin に一緒に渡す", async () => {
  const doc = { paragraphs: [] }, expectDoc = { paragraphs: [{ style: { kind: "title" as const }, indent: 0, quote: false, inlines: [] }] }
  const seen: unknown[] = []
  const { run } = fakeRunner([[/write --id N/, (_c, stdin) => { seen.push(JSON.parse(stdin ?? "")); return { stdout: JSON.stringify({ doc }) } }]])
  await createNotesPort(opts(run)).write("N", doc, expectDoc)
  await createNotesPort(opts(run)).write("N", doc)
  expect(seen).toEqual([{ doc, expect: expectDoc }, { doc }])
})
test("write の exit 7 (読んだ後に更新された) は BridgeError", async () => {
  const { run } = fakeRunner([[/write/, () => ({ code: 7, stderr: "changed" })]])
  const e = await createNotesPort(opts(run)).write("N", { paragraphs: [] }, { paragraphs: [] }).catch((x) => x)
  expect(e).toBeInstanceOf(BridgeError)
  expect(e.code).toBe(7)
})
test("write の exit 5 は BridgeError", async () => {
  const { run } = fakeRunner([[/write/, () => ({ code: 5, stderr: "attachments" })]])
  const e = await createNotesPort(opts(run)).write("N", { paragraphs: [] }).catch((x) => x)
  expect(e.code).toBe(5)
})
test("create: JXA にタイトルを argv で渡し、identify で id にする", async () => {
  const title = `He said "hi" 'x' \\ <b>`
  const { run, calls } = fakeRunner([
    [/osascript/, () => ({ stdout: "x-coredata://U/ICNote/p1\n" })],
    [/identify --uri/, () => ({ stdout: '{"id":"ID1"}' })],
  ])
  expect(await createNotesPort(opts(run)).create(title)).toEqual({ id: "ID1", uri: "x-coredata://U/ICNote/p1" })
  const osa = calls[0]!
  expect(osa).toContain(`-- org ${title}`)
  expect(osa.split("-- ")[0]).not.toContain("He said")
  expect(calls[1]).toBe("/b identify --uri x-coredata://U/ICNote/p1")
})
test("remove は uri を argv で渡す", async () => {
  const { run, calls } = fakeRunner([[/osascript/, () => ({})]])
  await createNotesPort(opts(run)).remove("x-coredata://U/ICNote/p1")
  expect(calls[0]).toContain("-- x-coredata://U/ICNote/p1")
})
test("exportMedia", async () => {
  const { run, calls } = fakeRunner([[/export-media/, () => ({ stdout: '{"written":true}' })]])
  await createNotesPort(opts(run)).exportMedia("A", "/t.jpg")
  expect(calls[0]).toBe("/b export-media --attachment A --to /t.jpg")
})
test("flush: 未起動なら起動して送信を待ち、起動したままにする", async () => {
  const { run, calls } = fakeRunner([
    [/running\(\)/, () => ({ stdout: "false" })],
    [/lsappinfo/, () => ({ stdout: TERM })],
    [/open -g -a Notes/, () => ({})],
    [/quit/, () => ({})],
  ])
  let n = 0
  const r = await createNotesPort(opts(run, { syncState: () => [n++ < 2 ? { local: 2, synced: 1 } : { local: 2, synced: 2 }] })).flush(["N"])
  expect(r).toBe("done")
  expect(calls.some((c) => c.includes("open -g -a Notes"))).toBe(true)
  expect(calls.some((c) => c.includes("quit"))).toBe(false)
})
test("flush: 起動中で手前に無ければ quit → 停止待ち → 起動 → 送信待ち", async () => {
  let running = true
  const { run, calls } = fakeRunner([
    [/running\(\)/, () => ({ stdout: String(running) })],
    [/lsappinfo/, () => ({ stdout: TERM })],
    [/quit/, () => { running = false; return {} }],
    [/open -g -a Notes/, () => ({})],
  ])
  let polls = 0, opened = false
  const r = await createNotesPort(opts(run, { syncState: () => { if (calls.some((c) => c.includes("open -g"))) opened = true; if (opened) polls++; return [opened ? { local: 2, synced: 2 } : { local: 2, synced: 1 }] } })).flush(["N"])
  expect(r).toBe("done")
  expect(polls).toBeGreaterThan(0)
  const q = calls.findIndex((c) => c.includes("quit")), o = calls.findIndex((c) => c.includes("open -g -a Notes"))
  expect(q).toBeGreaterThan(-1)
  expect(o).toBeGreaterThan(q)
  expect(calls.filter((c) => c.includes("quit")).length).toBe(1)
  expect(calls.at(-1)).not.toContain("quit")
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
  const { run, calls } = fakeRunner([[/running\(\)/, () => ({ stdout: "false" })], [/open -g/, () => ({})], [/quit/, () => ({})]])
  const r = await createNotesPort(opts(run, { now: () => (t += 3000), syncState: () => [{ local: 2, synced: 1 }] })).flush(["N"])
  expect(r).toBe("timeout")
  expect(calls.slice(calls.findIndex((c) => c.includes("open -g"))).some((c) => c.includes("quit"))).toBe(false)
})

test("create の JXA は Notes.make で note を作る", async () => {
  const { run, calls } = fakeRunner([[/osascript/, () => ({ stdout: "u" })], [/identify/, () => ({ stdout: '{"id":"I"}' })]])
  await createNotesPort(opts(run)).create("t")
  expect(calls[0]).toContain("Notes.make(")
  expect(calls[0]).toContain('new: "note"')
  expect(calls[0]).toContain('accounts.byName("iCloud")')
})
test("flush: syncState に行が無い id は未送信で timeout", async () => {
  let t = 0
  const { run } = fakeRunner([[/running\(\)/, () => ({ stdout: "false" })], [/open -g/, () => ({})], [/quit/, () => ({})]])
  expect(await createNotesPort(opts(run, { now: () => (t += 3000), syncState: () => [] })).flush(["N"])).toBe("timeout")
})
test("flush: 手前の判定が空出力なら保留", async () => {
  const { run } = fakeRunner([[/running\(\)/, () => ({ stdout: "true" })], [/lsappinfo/, () => ({ stdout: "" })]])
  expect(await createNotesPort(opts(run, { syncState: () => [] })).flush(["N"])).toBe("deferred")
})
test("flush: 10 秒で止まらなければ open せず timeout", async () => {
  let t = 0
  const { run, calls } = fakeRunner([
    [/running\(\)/, () => ({ stdout: "true" })],
    [/lsappinfo/, () => ({ stdout: TERM })],
    [/quit/, () => ({})],
  ])
  expect(await createNotesPort(opts(run, { now: () => (t += 3000), syncState: () => [] })).flush(["N"])).toBe("timeout")
  expect(calls.some((c) => c.includes("open -g"))).toBe(false)
})
test("flush: Runner が固まっても commandTimeoutMs で timeout を返す", async () => {
  const run: Runner = () => new Promise(() => {})
  const t0 = Date.now()
  expect(await createNotesPort(opts(run, { commandTimeoutMs: 20, syncState: () => [] })).flush(["N"])).toBe("timeout")
  expect(Date.now() - t0).toBeLessThan(1000)
})
test("Runner には commandTimeoutMs が渡る", async () => {
  const seen: (number | undefined)[] = []
  const run: Runner = async (_c, _s, ms) => { seen.push(ms); return { code: 0, stdout: '{"ok":true}', stderr: "" } }
  await createNotesPort(opts(run, { commandTimeoutMs: 1234 })).selfcheck()
  expect(seen).toEqual([1234])
})
test("bridge の JSON の形が違えば BridgeError (code 1)", async () => {
  for (const [re, call] of [
    [/list/, (p: ReturnType<typeof createNotesPort>) => p.list()],
    [/write/, (p: ReturnType<typeof createNotesPort>) => p.write("N", { paragraphs: [] })],
  ] as const) {
    const { run } = fakeRunner([[re, () => ({ stdout: "{}" })]])
    const e = await call(createNotesPort(opts(run))).catch((x: unknown) => x)
    expect(e).toBeInstanceOf(BridgeError)
    expect((e as BridgeError).code).toBe(1)
  }
  const { run } = fakeRunner([[/osascript/, () => ({ stdout: "u" })], [/identify/, () => ({ stdout: '{"id":3}' })]])
  expect(((await createNotesPort(opts(run)).create("t").catch((x) => x)) as BridgeError).code).toBe(1)
})

function makeDb() {
  const path = join(mkdtempSync(join(tmpdir(), "ns-")), "NoteStore.sqlite")
  const db = new Database(path)
  db.run("create table ZICCLOUDSTATE (Z_PK integer primary key, ZCURRENTLOCALVERSION integer, ZLATESTVERSIONSYNCEDTOCLOUD integer)")
  db.run("create table ZICCLOUDSYNCINGOBJECT (Z_PK integer primary key, ZIDENTIFIER text, ZCLOUDSTATE integer)")
  db.run("insert into ZICCLOUDSTATE values (1, 3, 3), (2, 3, 2)")
  db.run("insert into ZICCLOUDSYNCINGOBJECT values (1, 'SYNCED', 1), (2, 'PENDING', 2)")
  db.close()
  return path
}
const flushWith = (path: string, ids: string[]) => {
  let t = 0
  const { run } = fakeRunner([[/running\(\)/, () => ({ stdout: "false" })], [/lsappinfo/, () => ({ stdout: TERM })], [/open -g/, () => ({})], [/quit/, () => ({})]])
  return createNotesPort(opts(run, { notesDbPath: path, now: () => (t += 3000) })).flush(ids)
}
test("既定の syncState: sqlite の版を比べる", async () => {
  const path = makeDb()
  expect(await flushWith(path, ["SYNCED"])).toBe("done")
  expect(await flushWith(path, ["PENDING"])).toBe("timeout")
  expect(await flushWith(path, ["MISSING"])).toBe("timeout")
})
test("既定の syncState: DB が読めなければ BridgeError (code 4)", async () => {
  const e = await flushWith("/nonexistent/x.sqlite", ["A"]).catch((x) => x)
  expect(e).toBeInstanceOf(BridgeError)
  expect(e.code).toBe(4)
})

test("flush: 送信済みの id は待たず、全部送信済みなら メモ.app に触らず done", async () => {
  const { run, calls } = fakeRunner([])
  const asked: string[][] = []
  const port = createNotesPort(opts(run, { syncState: (ids: string[]) => { asked.push(ids); return ids.map(() => ({ local: 3, synced: 3 })) } }))
  expect(await port.flush(["A", "B"])).toBe("done")
  expect(calls).toEqual([])
  expect(asked).toEqual([["A", "B"]])
})
test("flush: 未送信の id だけを待つ", async () => {
  const { run } = fakeRunner([[/running\(\)/, () => ({ stdout: "false" })], [/lsappinfo/, () => ({ stdout: TERM })], [/open -g/, () => ({})], [/quit/, () => ({})]])
  const asked: string[][] = []
  const port = createNotesPort(opts(run, { syncState: (ids: string[]) => {
    asked.push(ids)
    return ids.map((id) => (id === "B" && asked.length === 1 ? { local: 2, synced: 1 } : { local: 2, synced: 2 }))
  } }))
  expect(await port.flush(["A", "B"])).toBe("done")
  expect(asked).toEqual([["A", "B"], ["B"]])
})
test("store 指定時は作成・削除をせず、flush は not-needed (本物のメモ.app に触らない)", async () => {
  const { run, calls } = fakeRunner([])
  const port = createNotesPort(opts(run, { store: "/s", syncState: () => { throw new Error("読まない") } }))
  for (const p of [port.create("t"), port.remove("u")]) {
    const e = (await (p as Promise<unknown>).catch((x) => x)) as BridgeError
    expect(e).toBeInstanceOf(BridgeError)
    expect([e.code, e.message]).toEqual([1, "--store では作成・削除しません"])
  }
  expect(await port.flush(["N"])).toBe("not-needed")
  expect(calls).toEqual([])
})
test("既定の syncState が読む DB: notesDbPath > <store>/NoteStore.sqlite > 本物", () => {
  expect(defaultSyncStatePath({ store: "/s" })).toBe("/s/NoteStore.sqlite")
  expect(defaultSyncStatePath({ store: "/s", notesDbPath: "/d.sqlite" })).toBe("/d.sqlite")
  expect(defaultSyncStatePath({})).toBe(`${process.env.HOME}/Library/Group Containers/group.com.apple.notes/NoteStore.sqlite`)
})

// wake: メモ.app が止まっていると iCloud の変更が DB に届かないので、起動して届くのを待つ
const clockOpts = () => {
  let t = 0
  return { now: () => t, sleep: async (ms: number) => { t += ms }, at: () => t }
}
test("wake: store 指定時は何もしない", async () => {
  const { run, calls } = fakeRunner([])
  expect(await createNotesPort(opts(run, { store: "/s" })).wake()).toBe("not-needed")
  expect(calls).toEqual([])
})
test("wake: 起動中なら何もしない", async () => {
  const { run, calls } = fakeRunner([[/running\(\)/, () => ({ stdout: "true" })]])
  expect(await createNotesPort(opts(run)).wake()).toBe("running")
  expect(calls.some((c) => c.includes("open -g"))).toBe(false)
})
test("wake: 未起動なら裏で起動し、DB の更新が落ち着くまで待つ", async () => {
  const c = clockOpts()
  const { run, calls } = fakeRunner([[/running\(\)/, () => ({ stdout: "false" })], [/open -g -a Notes/, () => ({})]])
  // 起動から 10 秒間 DB が書き換わり続け、その後は止まる
  const r = await createNotesPort(opts(run, { ...c, dbStamp: () => Math.min(c.at(), 10_000) })).wake()
  expect(r).toBe("launched")
  expect(calls.some((x) => x.includes("open -g -a Notes"))).toBe(true)
  expect(calls.some((x) => x.includes("quit"))).toBe(false)
  expect(c.at()).toBeGreaterThanOrEqual(14_000)
  expect(c.at()).toBeLessThan(30_000)
})
test("wake: DB が書き換わらなくても最低 6 秒は待つ", async () => {
  const c = clockOpts()
  const { run } = fakeRunner([[/running\(\)/, () => ({ stdout: "false" })], [/open -g -a Notes/, () => ({})]])
  expect(await createNotesPort(opts(run, { ...c, dbStamp: () => 1 })).wake()).toBe("launched")
  expect(c.at()).toBeGreaterThanOrEqual(6_000)
})
test("wake: 30 秒たっても落ち着かなければ timeout", async () => {
  const c = clockOpts()
  const { run } = fakeRunner([[/running\(\)/, () => ({ stdout: "false" })], [/open -g -a Notes/, () => ({})]])
  expect(await createNotesPort(opts(run, { ...c, dbStamp: () => c.at() })).wake()).toBe("timeout")
  expect(c.at()).toBeGreaterThanOrEqual(30_000)
  expect(c.at()).toBeLessThan(33_000)
})
