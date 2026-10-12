import fs from "node:fs"
import path from "node:path"
import type { OrgFile } from "./reconcile"

const TRASH = ".notes-trash"
const MEDIA = ".notes-media"

// 直下の通常ファイルのみ。dir を読めなければ throw する (空配列は「全部消えた」と解釈されるため)
export function listOrgFiles(dir: string, ext: string): OrgFile[] {
  const out: OrgFile[] = []
  for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
    if (!e.isFile() || !e.name.endsWith(ext)) continue
    const p = path.join(dir, e.name)
    out.push({ file: e.name, text: fs.readFileSync(p, "utf8"), mtimeMs: fs.statSync(p).mtimeMs })
  }
  return out
}

// expectedMtimeMs が null なら「無いはず」、数値なら現在の mtime と一致するはず。違えば書かない
export function writeOrgFile(dir: string, file: string, text: string, expectedMtimeMs: number | null): "written" | "stale" {
  const p = path.join(dir, file)
  let current: number | null = null
  try {
    current = fs.statSync(p).mtimeMs
  } catch (e) {
    if ((e as NodeJS.ErrnoException).code !== "ENOENT") throw e
  }
  if (current !== expectedMtimeMs) return "stale"
  const tmp = path.join(dir, `.${file}.tmp-${process.pid}`)
  fs.writeFileSync(tmp, text)
  fs.renameSync(tmp, p)
  return "written"
}

const stamp = (d: Date): string => {
  const z = (n: number) => String(n).padStart(2, "0")
  return `${d.getFullYear()}${z(d.getMonth() + 1)}${z(d.getDate())}${z(d.getHours())}${z(d.getMinutes())}${z(d.getSeconds())}`
}

// dir/.notes-trash/<file> へ移す。既にあれば <stem>-<YYYYMMDDHHmmss><ext>
export function trashOrgFile(dir: string, file: string): string {
  const trash = path.join(dir, TRASH)
  fs.mkdirSync(trash, { recursive: true })
  let dest = path.join(trash, file)
  if (fs.existsSync(dest)) {
    const ext = path.extname(file)
    const base = `${path.basename(file, ext)}-${stamp(new Date())}`
    dest = path.join(trash, `${base}${ext}`)
    // 同じ秒に何度も捨てても上書きしない
    for (let n = 2; fs.existsSync(dest); n++) dest = path.join(trash, `${base}-${n}${ext}`)
  }
  fs.renameSync(path.join(dir, file), dest)
  return dest
}

export function mediaPath(dir: string, id: string): string {
  return path.join(dir, MEDIA, `${id}.jpg`)
}

// keep に無い画像を消し、消した id を返す
export function gcMedia(dir: string, keep: Set<string>): string[] {
  const media = path.join(dir, MEDIA)
  if (!fs.existsSync(media)) return []
  const dropped: string[] = []
  for (const name of fs.readdirSync(media)) {
    if (!name.endsWith(".jpg")) continue
    const id = name.slice(0, -".jpg".length)
    if (keep.has(id)) continue
    fs.rmSync(path.join(media, name))
    dropped.push(id)
  }
  return dropped
}
