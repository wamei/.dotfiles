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
test("flush: 起動中で手前に無ければ quit → 停止待ち → 起動 → 送信待ち", async () => {
  let running = true
  const { run, calls } = fakeRunner([
    [/running\(\)/, () => ({ stdout: String(running) })],
    [/lsappinfo/, () => ({ stdout: '"CFBundleIdentifier"="com.apple.Terminal"' })],
    [/quit/, () => { running = false; return {} }],
    [/open -g -a Notes/, () => ({})],
  ])
  const r = await createNotesPort(opts(run, { syncState: () => [{ local: 1, synced: 1 }] })).flush(["N"])
  expect(r).toBe("done")
  const q = calls.findIndex((c) => c.includes("quit")), o = calls.findIndex((c) => c.includes("open -g -a Notes"))
  expect(q).toBeGreaterThan(-1)
  expect(o).toBeGreaterThan(q)
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
