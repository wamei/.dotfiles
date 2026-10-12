#!/usr/bin/env bun
// org メモと メモ.app を 1 周同期する。終了コード: 0 ok / 2 全体停止 / 1 想定外のエラー
import os from "node:os"
import path from "node:path"
import { parseArgs } from "node:util"
import { orgFormat } from "../src/formats/org"
import { withLock } from "../src/lock"
import { makeLogger } from "../src/log"
import { createNotesPort } from "../src/notes"
import { syncOnce } from "../src/sync"

const home = os.homedir()
const { values } = parseArgs({
  options: {
    force: { type: "boolean", default: false },
    "org-dir": { type: "string", default: path.join(home, "org") },
    "state-dir": { type: "string", default: path.join(home, ".local/state/org-notes-sync") },
    folder: { type: "string", default: "org" },
    bridge: { type: "string" },
    // 手動確認用: メモの DB のコピーに対して読み書きする (メモの作成・削除と iCloud への送信はしない)
    store: { type: "string" },
  },
  strict: true,
})

const stateDir = values["state-dir"]
const logger = makeLogger(stateDir)
const notes = createNotesPort({
  bridge: process.env.ORG_NOTES_SYNC_BRIDGE || values.bridge || path.join(home, ".local/bin/notes-bridge"),
  folder: values.folder,
  store: values.store,
})

try {
  const code = await withLock(stateDir, async () => {
    const r = await syncOnce({ format: orgFormat, notes, orgDir: values["org-dir"], stateDir, force: values.force, log: logger.log, warn: logger.warn })
    logger.log(r.status === "ok" ? `ok (書いたメモ ${r.wroteNotes.length} 件)` : `aborted: ${r.reason}`)
    return r.status === "ok" ? 0 : 2
  })
  process.exit(code)
} catch (e) {
  logger.warn(`同期に失敗しました: ${e instanceof Error ? e.message : String(e)}`)
  process.exit(1)
}
