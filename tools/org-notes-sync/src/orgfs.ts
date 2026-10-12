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
    // 同じ fd で fstat してから読む。stat が本文より先なので、間に Emacs が保存しても
    // 「新しい mtime + 古い本文」にはならず、古い mtime が後の stale 判定で弾かれる
    const fd = fs.openSync(path.join(dir, e.name), "r")
    try {
      const mtimeMs = fs.fstatSync(fd).mtimeMs
      out.push({ file: e.name, text: fs.readFileSync(fd, "utf8"), mtimeMs })
    } finally {
      fs.closeSync(fd)
    }
  }
  return out
}

const currentMtime = (p: string): number | null => {
  try {
    return fs.statSync(p).mtimeMs
  } catch (e) {
    if ((e as NodeJS.ErrnoException).code === "ENOENT") return null
    throw e
  }
}

// expectedMtimeMs が null なら「無いはず」、数値なら現在の mtime と一致するはず。違えば書かない
export function writeOrgFile(dir: string, file: string, text: string, expectedMtimeMs: number | null): "written" | "stale" {
  const p = path.join(dir, file)
  if (currentMtime(p) !== expectedMtimeMs) return "stale"
  const tmp = path.join(dir, `.${file}.tmp-${process.pid}`)
  try {
    fs.writeFileSync(tmp, text)
    // 公開の直前にもう一度確かめる (tmp を書いている間に Emacs が保存したかもしれない)
    if (currentMtime(p) !== expectedMtimeMs) return "stale"
    if (expectedMtimeMs === null) {
      // 既存ファイルがあれば EEXIST で失敗する (rename と違い上書きしない)
      try {
        fs.linkSync(tmp, p)
      } catch (e) {
        if ((e as NodeJS.ErrnoException).code === "EEXIST") return "stale"
        throw e
      }
    } else {
      fs.renameSync(tmp, p)
    }
    return "written"
  } finally {
    fs.rmSync(tmp, { force: true })
  }
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
