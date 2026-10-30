import { mkdtempSync, rmSync, writeFileSync } from "node:fs"
import { tmpdir } from "node:os"
import { join } from "node:path"

export type MergeResult = { conflict: boolean; text: string }

// ours = ローカル、theirs = メモ。git merge-file で行単位の 3-way マージをし、衝突は git 形式のマーカーにする
// (node-diff3 は重複行があると非衝突のマージで行を落とすため使わない)
export function merge3(base: string, ours: string, theirs: string): MergeResult {
  const dir = mkdtempSync(join(tmpdir(), "notes-merge-"))
  try {
    const paths = { ours: join(dir, "ours"), base: join(dir, "base"), theirs: join(dir, "theirs") }
    writeFileSync(paths.ours, ours)
    writeFileSync(paths.base, base)
    writeFileSync(paths.theirs, theirs)
    const proc = Bun.spawnSync(
      ["git", "-c", "merge.conflictStyle=merge", "merge-file", "-p", "-L", "local", "-L", "base", "-L", "notes", paths.ours, paths.base, paths.theirs],
      { stdout: "pipe", stderr: "pipe" },
    )
    // 0 = 衝突なし、正の値 = 衝突数、負の値 (signal 終了含む) は失敗
    const code = proc.exitCode
    if (code === null || code < 0 || code > 127) {
      throw new Error(`git merge-file failed (exit ${code}): ${proc.stderr.toString()}`)
    }
    return { conflict: code > 0, text: proc.stdout.toString() }
  } finally {
    rmSync(dir, { recursive: true, force: true })
  }
}
