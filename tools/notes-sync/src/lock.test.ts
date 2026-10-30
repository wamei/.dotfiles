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
test("本体が throw しても lock を外す", async () => {
  const d = fs.mkdtempSync(path.join(os.tmpdir(), "ons-lock-"))
  await expect(withLock(d, async () => { throw new Error("x") })).rejects.toThrow("x")
  expect(fs.existsSync(path.join(d, "lock"))).toBe(false)
})
