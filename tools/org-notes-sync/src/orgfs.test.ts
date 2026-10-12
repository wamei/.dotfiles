import { expect, test } from "bun:test"
import fs from "node:fs"
import os from "node:os"
import path from "node:path"
import { gcMedia, listOrgFiles, mediaPath, trashOrgFile, writeOrgFile } from "./orgfs"

const tmp = () => fs.mkdtempSync(path.join(os.tmpdir(), "ons-"))

test("listOrgFiles: 直下の .org だけ (dotfile 含む)", () => {
  const d = tmp()
  fs.writeFileSync(path.join(d, "a.org"), "x")
  fs.writeFileSync(path.join(d, ".dot.org"), "y")
  fs.writeFileSync(path.join(d, "b.txt"), "z")
  fs.mkdirSync(path.join(d, ".notes-trash"))
  const files = listOrgFiles(d, ".org")
  expect(files.map((f) => f.file).sort()).toEqual([".dot.org", "a.org"])
  expect(files.find((f) => f.file === "a.org")?.text).toBe("x")
})

test("listOrgFiles: ディレクトリが無ければ throw する (空配列にしない)", () => {
  expect(() => listOrgFiles(path.join(tmp(), "missing"), ".org")).toThrow()
})

test("listOrgFiles: ディレクトリでないパスも throw する", () => {
  const d = tmp()
  const p = path.join(d, "file")
  fs.writeFileSync(p, "")
  expect(() => listOrgFiles(p, ".org")).toThrow()
})

test("writeOrgFile: mtime が変わっていたら書かない (Review Focus 5)", () => {
  const d = tmp()
  const p = path.join(d, "a.org")
  fs.writeFileSync(p, "old")
  const m = fs.statSync(p).mtimeMs
  fs.utimesSync(p, new Date(), new Date(Date.now() + 5000))
  expect(writeOrgFile(d, "a.org", "new", m)).toBe("stale")
  expect(fs.readFileSync(p, "utf8")).toBe("old")
  expect(writeOrgFile(d, "a.org", "new", fs.statSync(p).mtimeMs)).toBe("written")
  expect(writeOrgFile(d, "a.org", "x", null)).toBe("stale")
  expect(writeOrgFile(d, "b.org", "x", null)).toBe("written")
  expect(fs.readdirSync(d).sort()).toEqual(["a.org", "b.org"])
})

test("writeOrgFile: 期待 mtime があるのにファイルが無ければ stale", () => {
  const d = tmp()
  expect(writeOrgFile(d, "gone.org", "x", 123)).toBe("stale")
  expect(fs.existsSync(path.join(d, "gone.org"))).toBe(false)
})

test("writeOrgFile: stale のとき tmp を残さない (null 期待で既存あり / mtime 不一致)", () => {
  const d = tmp()
  fs.writeFileSync(path.join(d, "a.org"), "old")
  expect(writeOrgFile(d, "a.org", "new", null)).toBe("stale")
  expect(writeOrgFile(d, "a.org", "new", 1)).toBe("stale")
  expect(fs.readdirSync(d)).toEqual(["a.org"])
  expect(fs.readFileSync(path.join(d, "a.org"), "utf8")).toBe("old")
})

test("writeOrgFile: 書き込みに成功しても tmp は残らない", () => {
  const d = tmp()
  expect(writeOrgFile(d, "n.org", "x", null)).toBe("written")
  expect(fs.readdirSync(d)).toEqual(["n.org"])
  expect(fs.readFileSync(path.join(d, "n.org"), "utf8")).toBe("x")
})

test("writeOrgFile: 失敗しても tmp を残さない (対象がディレクトリ)", () => {
  const d = tmp()
  fs.mkdirSync(path.join(d, "a.org"))
  const m = fs.statSync(path.join(d, "a.org")).mtimeMs
  expect(() => writeOrgFile(d, "a.org", "x", m)).toThrow()
  expect(fs.readdirSync(d)).toEqual(["a.org"])
})

test("trashOrgFile と gcMedia", () => {
  const d = tmp()
  fs.writeFileSync(path.join(d, "a.org"), "1")
  trashOrgFile(d, "a.org")
  fs.writeFileSync(path.join(d, "a.org"), "2")
  const second = trashOrgFile(d, "a.org")
  expect(fs.readdirSync(path.join(d, ".notes-trash")).length).toBe(2)
  expect(second).not.toBe(path.join(d, ".notes-trash", "a.org"))
  expect(second).toMatch(/\.notes-trash\/a-\d{14}\.org$/)
  fs.mkdirSync(path.join(d, ".notes-media"))
  fs.writeFileSync(mediaPath(d, "KEEP"), "")
  fs.writeFileSync(mediaPath(d, "DROP"), "")
  expect(gcMedia(d, new Set(["KEEP"]))).toEqual(["DROP"])
  expect(fs.existsSync(mediaPath(d, "KEEP"))).toBe(true)
  expect(fs.existsSync(mediaPath(d, "DROP"))).toBe(false)
})

test("trashOrgFile: 同じ秒に3回捨てても上書きしない", () => {
  const d = tmp()
  for (const t of ["1", "2", "3"]) {
    fs.writeFileSync(path.join(d, "a.org"), t)
    trashOrgFile(d, "a.org")
  }
  const kept = fs.readdirSync(path.join(d, ".notes-trash")).map((n) => fs.readFileSync(path.join(d, ".notes-trash", n), "utf8"))
  expect(kept.sort()).toEqual(["1", "2", "3"])
})

test("gcMedia: media ディレクトリが無ければ何もしない", () => {
  expect(gcMedia(tmp(), new Set())).toEqual([])
})
