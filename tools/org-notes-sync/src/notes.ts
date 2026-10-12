// メモ.app 側のアダプタ。bridge CLI (読み書き)、JXA (作成・削除・フォルダ)、Notes.app の再起動による iCloud 送信を扱う
import type { BridgeNote } from "./bridge-types"
import type { Doc } from "./doc"

export type Runner = (cmd: string[], stdin?: string) => Promise<{ code: number; stdout: string; stderr: string }>
export type FlushResult = "done" | "deferred" | "timeout" | "not-needed"

export class BridgeError extends Error {
  constructor(message: string, readonly code: number) {
    super(message)
    this.name = "BridgeError"
  }
}

export interface NotesPort {
  selfcheck(): Promise<void>
  list(): Promise<BridgeNote[] | null>
  write(id: string, doc: Doc): Promise<Doc>
  create(title: string): Promise<{ id: string; uri: string }>
  remove(uri: string): Promise<void>
  exportMedia(attachmentId: string, to: string): Promise<void>
  flush(ids: string[]): Promise<FlushResult>
}

export type NotesPortOptions = {
  bridge: string
  folder: string
  store?: string
  run?: Runner
  sleep?: (ms: number) => Promise<void>
  now?: () => number
  // 既定は NoteStore.sqlite を読む
  syncState?: (ids: string[]) => { local: number; synced: number }[]
}

const STORE_DB = `${process.env.HOME}/Library/Group Containers/group.com.apple.notes/NoteStore.sqlite`
const WAIT_SYNC_MS = 60_000
const POLL_MS = 2_000
const WAIT_QUIT_MS = 10_000

export const defaultRun: Runner = async (cmd, stdin) => {
  const p = Bun.spawn(cmd, { stdin: stdin === undefined ? "ignore" : new Blob([stdin]), stdout: "pipe", stderr: "pipe" })
  const [stdout, stderr, code] = await Promise.all([new Response(p.stdout).text(), new Response(p.stderr).text(), p.exited])
  return { code, stdout, stderr }
}

// 既定の送信状況: ZICCLOUDSTATE の手元の版と iCloud へ送信済みの版
export function defaultSyncState(ids: string[]): { local: number; synced: number }[] {
  const { Database } = require("bun:sqlite") as typeof import("bun:sqlite")
  const db = new Database(STORE_DB, { readonly: true })
  try {
    const q = db.query(
      "select s.ZCURRENTLOCALVERSION as local, s.ZLATESTVERSIONSYNCEDTOCLOUD as synced from ZICCLOUDSYNCINGOBJECT o join ZICCLOUDSTATE s on s.Z_PK = o.ZCLOUDSTATE where o.ZIDENTIFIER = ?",
    )
    return ids.flatMap((id) => (q.all(id) as { local: number; synced: number }[]).map((r) => ({ local: r.local ?? 0, synced: r.synced ?? 0 })))
  } finally {
    db.close()
  }
}

const escapeHtml = (s: string) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")

// JXA。利用者のデータは必ず argv で受け取る (スクリプトへ文字列連結しない)
const JXA_RUNNING = `Application("Notes").running()`
const JXA_QUIT = `Application("Notes").quit()`
const JXA_CREATE = `function run(argv) {
  var folderName = argv[0], title = argv[1];
  var esc = function (s) { return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;"); };
  var Notes = Application("Notes");
  var acc = Notes.accounts.byName("iCloud");
  var found = acc.folders.whose({name: folderName});
  var folder;
  if (found.length === 0) {
    folder = Notes.Folder({name: folderName});
    acc.folders.push(folder);
    folder = acc.folders.whose({name: folderName})[0];
  } else {
    folder = found[0];
  }
  var note = Notes.Note({body: "<div>" + esc(title) + "</div>"});
  folder.notes.push(note);
  return note.id();
}`
const JXA_REMOVE = `function run(argv) { var Notes = Application("Notes"); Notes.delete(Notes.notes.byId(argv[0])); }`

export function createNotesPort(o: NotesPortOptions): NotesPort {
  const run = o.run ?? defaultRun
  const sleep = o.sleep ?? ((ms: number) => new Promise<void>((r) => setTimeout(r, ms)))
  const now = o.now ?? Date.now
  const syncState = o.syncState ?? defaultSyncState

  const exec = async (cmd: string[], stdin?: string) => {
    const r = await run(cmd, stdin)
    if (r.code !== 0) throw new BridgeError(r.stderr.trim(), r.code)
    return r.stdout
  }
  const bridge = async <T>(args: string[], stdin?: string): Promise<T> => {
    const out = await exec([o.bridge, ...(o.store ? ["--store", o.store] : []), ...args], stdin)
    try {
      return JSON.parse(out) as T
    } catch {
      throw new BridgeError(`notes-bridge の出力が JSON ではない: ${out.slice(0, 200)}`, 1)
    }
  }
  const osa = (script: string, ...args: string[]) => exec(["osascript", "-l", "JavaScript", "-e", script, "--", ...args])

  const isRunning = async () => (await osa(JXA_RUNNING)).trim() === "true"
  const isFront = async () => {
    const out = await exec(["sh", "-c", "lsappinfo info -only bundleid $(lsappinfo front)"])
    return out.includes("com.apple.Notes")
  }
  const waitUntil = async (done: () => boolean | Promise<boolean>, limitMs: number): Promise<boolean> => {
    const start = now()
    for (;;) {
      if (await done()) return true
      if (now() - start >= limitMs) return false
      await sleep(POLL_MS)
    }
  }
  const synced = (ids: string[]) => syncState(ids).every((s) => s.local === s.synced)

  return {
    async selfcheck() {
      await bridge(["selfcheck"])
    },
    async list() {
      const r = await bridge<{ folderMissing: boolean; notes: BridgeNote[] }>(["list", "--folder", o.folder])
      return r.folderMissing ? null : r.notes
    },
    async write(id, doc) {
      return (await bridge<{ doc: Doc }>(["write", "--id", id], JSON.stringify({ doc }))).doc
    },
    async create(title) {
      const uri = (await osa(JXA_CREATE, o.folder, title)).trim()
      const { id } = await bridge<{ id: string }>(["identify", "--uri", uri])
      return { id, uri }
    },
    async remove(uri) {
      await osa(JXA_REMOVE, uri)
    },
    async exportMedia(attachmentId, to) {
      await bridge(["export-media", "--attachment", attachmentId, "--to", to])
    },
    async flush(ids) {
      if (ids.length === 0) return "not-needed"
      if (await isRunning()) {
        if (await isFront()) return "deferred"
        // 一度終了させ、起動し直すことで未送信の変更を iCloud へ押し出す
        await osa(JXA_QUIT)
        await waitUntil(async () => !(await isRunning()), WAIT_QUIT_MS)
      }
      await exec(["open", "-g", "-a", "Notes"])
      const ok = await waitUntil(() => synced(ids), WAIT_SYNC_MS)
      // 時間切れのときは送信を続けられるよう起動したままにする
      if (!ok) return "timeout"
      await osa(JXA_QUIT)
      return "done"
    },
  }
}
