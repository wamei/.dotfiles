import { expect, test } from "bun:test"
import fs from "node:fs"
import os from "node:os"
import path from "node:path"
import { loadState, saveState } from "./state"

const tmp = () => fs.mkdtempSync(path.join(os.tmpdir(), "ons-"))

test("state: 無ければ空、保存して読み直せる", () => {
  const d = tmp()
  expect(loadState(d)).toEqual({ version: 1, format: "org", pairs: [], flushPending: false })
  saveState(d, { version: 1, format: "org", pairs: [{ file: "a.org", noteId: "N", base: { paragraphs: [] } }], flushPending: true })
  expect(loadState(d).pairs[0]?.noteId).toBe("N")
  expect(fs.readdirSync(d)).toEqual(["state.json"])
})

test("state: 壊れた state.json は空に戻さず throw する", () => {
  const d = tmp()
  fs.writeFileSync(path.join(d, "state.json"), "{not json")
  expect(() => loadState(d)).toThrow()
})

test("state: 形が違う state.json も throw する", () => {
  const d = tmp()
  fs.writeFileSync(path.join(d, "state.json"), JSON.stringify({ version: 2 }))
  expect(() => loadState(d)).toThrow()
})

test("state: 保存先ディレクトリが無ければ作る", () => {
  const d = path.join(tmp(), "sub", "dir")
  saveState(d, { version: 1, format: "org", pairs: [], flushPending: false })
  expect(loadState(d).pairs).toEqual([])
})
