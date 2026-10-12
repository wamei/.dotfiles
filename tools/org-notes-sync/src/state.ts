import fs from "node:fs"
import path from "node:path"
import type { Pair } from "./reconcile"

export type State = { version: 1; format: "org"; pairs: Pair[]; flushPending: boolean }

const FILE = "state.json"

const empty = (): State => ({ version: 1, format: "org", pairs: [], flushPending: false })

const isState = (v: unknown): v is State => {
  if (typeof v !== "object" || v === null) return false
  const s = v as Record<string, unknown>
  return s.version === 1 && s.format === "org" && Array.isArray(s.pairs) && typeof s.flushPending === "boolean"
}

// 無ければ空の State。壊れていたら throw する (空に戻すと全件を再採用してしまうため)
export function loadState(dir: string): State {
  const p = path.join(dir, FILE)
  let raw: string
  try {
    raw = fs.readFileSync(p, "utf8")
  } catch (e) {
    if ((e as NodeJS.ErrnoException).code === "ENOENT") return empty()
    throw e
  }
  let parsed: unknown
  try {
    parsed = JSON.parse(raw)
  } catch (e) {
    throw new Error(`state.json が壊れています: ${p}: ${(e as Error).message}`)
  }
  if (!isState(parsed)) throw new Error(`state.json の形式が不正です: ${p}`)
  return parsed
}

// 一時ファイルに書いてから rename する
export function saveState(dir: string, s: State): void {
  fs.mkdirSync(dir, { recursive: true })
  const p = path.join(dir, FILE)
  const tmp = `${p}.tmp-${process.pid}`
  try {
    fs.writeFileSync(tmp, JSON.stringify(s, null, 2))
    fs.renameSync(tmp, p)
  } finally {
    fs.rmSync(tmp, { force: true }) // 失敗時の残骸 (成功時は rename 済みで無い)
  }
}
