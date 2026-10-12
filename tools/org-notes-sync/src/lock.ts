// 同期の多重起動を防ぐ。走っている間に呼ばれたら pending を残し、走っている側が終わり際にもう 1 周する
import fs from "node:fs"
import path from "node:path"

// pid を書く前に落ちたプロセスの空の lock は、この時間を過ぎたら死んだとみなす
const EMPTY_LOCK_STALE_MS = 60_000

const alive = (pid: number): boolean => {
  try {
    process.kill(pid, 0)
    return true
  } catch (e) {
    return (e as NodeJS.ErrnoException).code === "EPERM"
  }
}

const holderAlive = (lock: string): boolean => {
  let raw: string
  let mtimeMs: number
  try {
    raw = fs.readFileSync(lock, "utf8")
    mtimeMs = fs.statSync(lock).mtimeMs
  } catch (e) {
    if ((e as NodeJS.ErrnoException).code === "ENOENT") return false
    throw e
  }
  const pid = Number.parseInt(raw.trim(), 10)
  if (Number.isNaN(pid)) return Date.now() - mtimeMs < EMPTY_LOCK_STALE_MS
  return alive(pid)
}

// O_EXCL で作って pid を書く。持ち主が死んでいれば消して取り直す
function acquire(lock: string): boolean {
  for (let attempt = 0; attempt < 2; attempt++) {
    try {
      const fd = fs.openSync(lock, "wx")
      try {
        fs.writeSync(fd, String(process.pid))
      } finally {
        fs.closeSync(fd)
      }
      return true
    } catch (e) {
      if ((e as NodeJS.ErrnoException).code !== "EEXIST") throw e
    }
    if (holderAlive(lock)) return false
    fs.rmSync(lock, { force: true })
  }
  return false
}

export async function withLock(stateDir: string, body: () => Promise<number>): Promise<number> {
  fs.mkdirSync(stateDir, { recursive: true })
  const lock = path.join(stateDir, "lock")
  const pending = path.join(stateDir, "pending")
  let code = 0
  for (let first = true; ; first = false) {
    if (!acquire(lock)) {
      // 持ち主に任せる (2 周目以降は別のプロセスが pending を引き受けている)
      if (first) fs.writeFileSync(pending, "")
      return code
    }
    try {
      do {
        fs.rmSync(pending, { force: true })
        code = await body()
      } while (fs.existsSync(pending))
    } finally {
      fs.rmSync(lock, { force: true })
    }
    // lock を外す直前に pending が置かれていたら取り直して拾う
    if (!fs.existsSync(pending)) return code
  }
}
