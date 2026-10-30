import { describe, expect, test } from "bun:test"
import fs from "node:fs"
import os from "node:os"
import path from "node:path"
import { applyMigration, convertOrg, planMigration, toFrontMatter } from "./migrate"

const tmp = () => fs.mkdtempSync(path.join(os.tmpdir(), "ons-mig-"))

describe("toFrontMatter", () => {
  test("#+key: value を key: value にする", () => {
    expect(toFrontMatter(["#+notes_sync: yes", "#+FILETAGS: :a:b:", "#+startup:"])).toEqual(["notes_sync: yes", "FILETAGS: :a:b:", "startup:"])
  })
  test("制御行でないものは変換できない", () => {
    expect(toFrontMatter(["foo"])).toBeUndefined()
  })
})

describe("convertOrg", () => {
  test("見出し・リスト・装飾・添付・引用・example を Markdown にする", () => {
    const org = [
      "#+title: 買い物", "#+notes_sync: yes", "* 見出し", "** 小見出し", "*** 太字", "- [X] 済", "  1. 一", "+ dash",
      "*b* /i/ _u_ +s+", "[[https://g.co][ぐーぐる]] https://a.b", "[[file:.notes-media/IMG.jpg]]", "#+notes_attachment: T1",
      "#+begin_quote", "引用", "#+end_quote", "#+begin_example", "  code *x*", "#+end_example", "",
    ].join("\n") + "\n"
    const r = convertOrg(org, "買い物")
    expect(r).toEqual({ ok: true, optIn: true, md: [
      "---", "notes_sync: yes", "---", "# 買い物", "## 見出し", "### 小見出し", "#### 太字", "- [x] 済", "  1. 一", "+ dash",
      "**b** *i* <u>u</u> ~~s~~", "[ぐーぐる](https://g.co) https://a.b", "![](.notes-media/IMG.jpg)", "[attachment](notes-attachment:T1)",
      "> 引用", "```", "  code *x*", "```", "",
    ].join("\n") + "\n" })
  })
  test("タイトルの無い org はファイル名がタイトルになる", () => {
    expect(convertOrg("本文\n", "memo")).toEqual({ ok: true, optIn: false, md: "# memo\n本文\n" })
  })
  test("Markdown で別の意味になる行は、書かずに理由を返す", () => {
    const r = convertOrg("#+title: t\n# org のコメント風の行\n", "t")
    expect(r.ok).toBe(false)
  })
})

describe("planMigration / applyMigration", () => {
  const setup = () => {
    const root = tmp()
    const orgDir = path.join(root, "org")
    const dir = path.join(root, "notes")
    fs.mkdirSync(path.join(orgDir, ".notes-media"), { recursive: true })
    fs.mkdirSync(path.join(orgDir, ".notes-trash"))
    fs.mkdirSync(path.join(orgDir, "sub"))
    fs.writeFileSync(path.join(orgDir, "a.org"), "#+title: A\n#+notes_sync: yes\nx\n")
    fs.writeFileSync(path.join(orgDir, ".dotfiles.org"), "#+title: D\ny\n")
    fs.writeFileSync(path.join(orgDir, "sub", "c.org"), "#+title: C\n")
    fs.writeFileSync(path.join(orgDir, "readme.txt"), "z")
    fs.writeFileSync(path.join(orgDir, ".notes-media", "IMG.jpg"), "jpg")
    fs.writeFileSync(path.join(orgDir, ".notes-trash", "old.org"), "old")
    return { orgDir, dir }
  }

  test("直下の .org (ドットで始まるものも) を変換し、.notes-media と .notes-trash を移す。org ディレクトリは残す", () => {
    const { orgDir, dir } = setup()
    const plan = planMigration(orgDir, dir)
    expect(plan.errors).toEqual([])
    expect(plan.files.map((f) => f.to).sort()).toEqual([path.join(dir, ".dotfiles.md"), path.join(dir, "a.md")])
    expect(fs.existsSync(dir)).toBe(false) // plan は書かない
    applyMigration(plan)
    expect(fs.readFileSync(path.join(dir, "a.md"), "utf8")).toBe("---\nnotes_sync: yes\n---\n# A\nx\n")
    expect(fs.readFileSync(path.join(dir, ".dotfiles.md"), "utf8")).toBe("# D\ny\n")
    expect(fs.readFileSync(path.join(dir, ".notes-media", "IMG.jpg"), "utf8")).toBe("jpg")
    expect(fs.readFileSync(path.join(dir, ".notes-trash", "old.org"), "utf8")).toBe("old")
    expect(fs.existsSync(path.join(orgDir, ".notes-media"))).toBe(false)
    expect(fs.readdirSync(orgDir).sort()).toEqual([".dotfiles.org", "a.org", "readme.txt", "sub"])
  })

  test("移す先に .md や .notes-media が既にあれば中止する", () => {
    const { orgDir, dir } = setup()
    fs.mkdirSync(path.join(dir, ".notes-media"), { recursive: true })
    fs.writeFileSync(path.join(dir, "a.md"), "keep")
    const plan = planMigration(orgDir, dir)
    expect(plan.errors.length).toBe(2)
    expect(() => applyMigration(plan)).toThrow()
    expect(fs.readFileSync(path.join(dir, "a.md"), "utf8")).toBe("keep")
    expect(fs.existsSync(path.join(dir, ".dotfiles.md"))).toBe(false)
  })

  test("変換を検証できないファイルがあれば中止する", () => {
    const { orgDir, dir } = setup()
    fs.writeFileSync(path.join(orgDir, "b.org"), "#+title: B\n# コメント\n")
    const plan = planMigration(orgDir, dir)
    expect(plan.errors).toEqual([expect.stringContaining("b.org")])
    expect(() => applyMigration(plan)).toThrow()
    expect(fs.existsSync(dir)).toBe(false)
  })

  test("org ディレクトリが無ければ throw する", () => {
    expect(() => planMigration(path.join(tmp(), "none"), tmp())).toThrow()
  })
})
