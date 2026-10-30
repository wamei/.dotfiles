// org のメモを Markdown に移す (一度きりの移行)。読むのは plan、書くのは apply だけ
import fs from "node:fs"
import path from "node:path"
import { docEquals } from "./doc"
import { markdownFormat } from "./formats/markdown"
import { orgFormat } from "./formats/org"
import { listLocalFiles } from "./localfs"

const META = /^#\+([A-Za-z_]+):\s*(.*)$/
// org ディレクトリから一緒に移すもの
const MOVED_DIRS = [".notes-media", ".notes-trash"]

// org の制御行 (#+key: value) を front matter の行 (key: value) にする。制御行でないものがあれば undefined
export function toFrontMatter(preserved: string[]): string[] | undefined {
  const out: string[] = []
  for (const line of preserved) {
    const m = META.exec(line)
    if (!m) return undefined
    out.push(m[2] === "" ? `${m[1]}:` : `${m[1]}: ${m[2]}`)
  }
  return out
}

export type Converted = { ok: true; md: string; optIn: boolean } | { ok: false; reason: string }

// 変換した Markdown を読み戻して、org と同じ doc と opt-in になることを確かめる
export function convertOrg(text: string, stem: string): Converted {
  const org = orgFormat.parse(text, stem)
  const fm = toFrontMatter(org.preserved)
  if (!fm) return { ok: false, reason: "front matter にできない制御行があります" }
  const md = markdownFormat.render(org.doc, fm)
  const back = markdownFormat.parse(md, stem)
  if (back.optIn !== org.optIn) return { ok: false, reason: "opt-in が変わります" }
  if (!docEquals(back.doc, org.doc)) {
    const i = org.doc.paragraphs.findIndex((p, k) => !back.doc.paragraphs[k] || !docEquals({ paragraphs: [p] }, { paragraphs: [back.doc.paragraphs[k]!] }))
    return { ok: false, reason: `Markdown で読み戻すと内容が変わります (${i < 0 ? "段落の数" : `${i + 1} 段落目`})` }
  }
  return { ok: true, md, optIn: org.optIn }
}

export type MigrationPlan = {
  files: { from: string; to: string; md: string; optIn: boolean }[]
  moves: { from: string; to: string }[]
  dir: string
  errors: string[]
}

// orgDir 直下の .org (ドットで始まるものも) を dir/<stem>.md に変換する計画。何も書かない
export function planMigration(orgDir: string, dir: string): MigrationPlan {
  const plan: MigrationPlan = { files: [], moves: [], dir, errors: [] }
  for (const f of listLocalFiles(orgDir, orgFormat.ext)) {
    const stem = f.file.slice(0, -orgFormat.ext.length)
    const to = path.join(dir, `${stem}${markdownFormat.ext}`)
    const c = convertOrg(f.text, stem)
    if (!c.ok) plan.errors.push(`${f.file}: ${c.reason}`)
    else if (fs.existsSync(to)) plan.errors.push(`${to} が既にあります`)
    else plan.files.push({ from: path.join(orgDir, f.file), to, md: c.md, optIn: c.optIn })
  }
  for (const name of MOVED_DIRS) {
    const from = path.join(orgDir, name)
    if (!fs.existsSync(from)) continue
    const to = path.join(dir, name)
    if (fs.existsSync(to)) plan.errors.push(`${to} が既にあります`)
    else plan.moves.push({ from, to })
  }
  return plan
}

function moveDir(from: string, to: string): void {
  try {
    fs.renameSync(from, to)
  } catch (e) {
    if ((e as NodeJS.ErrnoException).code !== "EXDEV") throw e
    // 別のボリュームへは複製してから消す
    fs.cpSync(from, to, { recursive: true, errorOnExist: true, force: false })
    fs.rmSync(from, { recursive: true })
  }
}

// errors が 1 つでもあれば何も書かない。元の org ファイルは残す
export function applyMigration(plan: MigrationPlan): void {
  if (plan.errors.length > 0) throw new Error(`移行を中止しました:\n${plan.errors.join("\n")}`)
  fs.mkdirSync(plan.dir, { recursive: true })
  // wx: plan の後にできたファイルは上書きしない
  for (const f of plan.files) fs.writeFileSync(f.to, f.md, { flag: "wx" })
  for (const m of plan.moves) moveDir(m.from, m.to)
}
