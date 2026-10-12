import { expect, spyOn, test } from "bun:test"
import fs from "node:fs"; import os from "node:os"; import path from "node:path"
import { makeLogger } from "./log"

test("時刻付きで追記し、warn は stderr にも出す", () => {
  const d = fs.mkdtempSync(path.join(os.tmpdir(), "ons-log-"))
  const err = spyOn(process.stderr, "write").mockImplementation(() => true)
  try {
    const l = makeLogger(d)
    l.log("one"); l.warn("two")
    expect(err.mock.calls.map((c) => c[0])).toEqual(["org-notes-sync: two\n"])
  } finally {
    err.mockRestore()
  }
  const lines = fs.readFileSync(path.join(d, "log"), "utf8").split("\n")
  expect(lines[0]).toMatch(/^\d{4}-\d\d-\d\dT[\d:.]+Z one$/)
  expect(lines[1]).toMatch(/ two$/)
})

test("1 MB を超えたら後半 512 KB だけ残す", () => {
  const d = fs.mkdtempSync(path.join(os.tmpdir(), "ons-log-"))
  fs.writeFileSync(path.join(d, "log"), `${"x".repeat(99)}\n`.repeat(11000)) // 1.1 MB
  makeLogger(d).log("last")
  const s = fs.readFileSync(path.join(d, "log"), "utf8")
  expect(s.length).toBeLessThanOrEqual(512 * 1024)
  expect(s.length).toBeGreaterThan(500 * 1024)
  expect(s.startsWith("x")).toBe(true) // 行の途中から始まらない
  expect(s.endsWith(" last\n")).toBe(true)
})
