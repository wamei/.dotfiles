// stateDir/log へ時刻付きで追記する。warn は stderr にも出す (Emacs が拾う)
import fs from "node:fs"
import path from "node:path"

const MAX_BYTES = 1024 * 1024
const KEEP_BYTES = 512 * 1024

// 後半 KEEP_BYTES だけ残す。行の途中から始まらないよう、最初の改行の次から
function trim(file: string): void {
  const size = fs.statSync(file).size
  if (size <= MAX_BYTES) return
  const fd = fs.openSync(file, "r")
  const buf = Buffer.alloc(KEEP_BYTES)
  try {
    fs.readSync(fd, buf, 0, KEEP_BYTES, size - KEEP_BYTES)
  } finally {
    fs.closeSync(fd)
  }
  const nl = buf.indexOf(0x0a)
  fs.writeFileSync(file, nl === -1 ? buf : buf.subarray(nl + 1))
}

export function makeLogger(stateDir: string): { log(msg: string): void; warn(msg: string): void } {
  const file = path.join(stateDir, "log")
  const log = (msg: string) => {
    // ログが書けなくても同期は続ける
    try {
      fs.mkdirSync(stateDir, { recursive: true })
      fs.appendFileSync(file, `${new Date().toISOString()} ${msg}\n`)
      trim(file)
    } catch {}
  }
  return {
    log,
    warn(msg) {
      log(msg)
      process.stderr.write(`notes-sync: ${msg}\n`)
    },
  }
}
