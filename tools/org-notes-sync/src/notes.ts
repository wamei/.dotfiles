// メモ.app 側のアダプタ。bridge CLI (読み書き)、JXA (作成・削除・フォルダ)、Notes.app の再起動による iCloud 送信を扱う
import type { BridgeNote } from "./bridge-types"
import type { Doc } from "./doc"

// timeoutMs を超えたら子プロセスを殺し、非 0 の code (124) で返す
export type Runner = (cmd: string[], stdin?: string, timeoutMs?: number) => Promise<{ code: number; stdout: string; stderr: string }>
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
  // expect を渡すと、メモが expect のままのときだけ書く (違えば BridgeError code 7)
  write(id: string, doc: Doc, expect?: Doc): Promise<Doc>
  create(title: string): Promise<{ id: string; uri: string }>
  remove(uri: string): Promise<void>
  exportMedia(attachmentId: string, to: string): Promise<void>
  flush(ids: string[]): Promise<FlushResult>
}

export type NotesPortOptions = {
  bridge: string
  folder: string
  // メモの DB のコピー。指定時は作成・削除・iCloud への送信をしない (本物のメモ.app に触らない)
  store?: string
  run?: Runner
  sleep?: (ms: number) => Promise<void>
  now?: () => number
  // 既定は NoteStore.sqlite (store 指定時は <store>/NoteStore.sqlite) を読む
  syncState?: (ids: string[]) => { local: number; synced: number }[]
  notesDbPath?: string
  // 1 コマンドあたりの上限 (既定 30 秒、osascript の軽い呼び出しと lsappinfo は最大 15 秒)
  commandTimeoutMs?: number
}

const DEFAULT_COMMAND_MS = 30_000
const SHORT_COMMAND_MS = 15_000
const STORE_DB = `${process.env.HOME}/Library/Group Containers/group.com.apple.notes/NoteStore.sqlite`
const WAIT_SYNC_MS = 60_000
const POLL_MS = 2_000
const WAIT_QUIT_MS = 10_000

export const defaultRun: Runner = async (cmd, stdin, timeoutMs = DEFAULT_COMMAND_MS) => {
  const p = Bun.spawn(cmd, { stdin: stdin === undefined ? "ignore" : new Blob([stdin]), stdout: "pipe", stderr: "pipe" })
  let timedOut = false
  const timer = setTimeout(() => { timedOut = true; p.kill("SIGKILL") }, timeoutMs)
  try {
    const [stdout, stderr, code] = await Promise.all([new Response(p.stdout).text(), new Response(p.stderr).text(), p.exited])
    return timedOut ? { code: 124, stdout, stderr: `timeout after ${timeoutMs}ms: ${cmd[0]}` } : { code, stdout, stderr }
  } finally {
    clearTimeout(timer)
  }
}

export function defaultSyncStatePath(o: { store?: string; notesDbPath?: string }): string {
  return o.notesDbPath ?? (o.store ? `${o.store}/NoteStore.sqlite` : STORE_DB)
}

// 既定の送信状況: ZICCLOUDSTATE の手元の版と iCloud へ送信済みの版。行が無い id は未送信扱い
export function defaultSyncState(ids: string[], dbPath: string = STORE_DB): { local: number; synced: number }[] {
  try {
    const { Database } = require("bun:sqlite") as typeof import("bun:sqlite")
    const db = new Database(dbPath, { readonly: true })
    try {
      const q = db.query(
        "select s.ZCURRENTLOCALVERSION as local, s.ZLATESTVERSIONSYNCEDTOCLOUD as synced from ZICCLOUDSYNCINGOBJECT o join ZICCLOUDSTATE s on s.Z_PK = o.ZCLOUDSTATE where o.ZIDENTIFIER = ?",
      )
      return ids.map((id) => {
        const r = q.get(id) as { local: number | null; synced: number | null } | null
        return r ? { local: r.local ?? 0, synced: r.synced ?? 0 } : { local: 1, synced: 0 }
      })
    } finally {
      db.close()
    }
  } catch (e) {
    throw new BridgeError(`NoteStore を読めない: ${e instanceof Error ? e.message : String(e)}`, 4)
  }
}

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
  var note = Notes.make({new: "note", at: folder, withProperties: {body: "<div>" + esc(title) + "</div>"}});
  return note.id();
}`
const JXA_REMOVE = `function run(argv) { var Notes = Application("Notes"); Notes.delete(Notes.notes.byId(argv[0])); }`

type Front = "front" | "back" | "unknown"

export function createNotesPort(o: NotesPortOptions): NotesPort {
  const run = o.run ?? defaultRun
  const sleep = o.sleep ?? ((ms: number) => new Promise<void>((r) => setTimeout(r, ms)))
  const now = o.now ?? Date.now
  const cmdMs = o.commandTimeoutMs ?? DEFAULT_COMMAND_MS
  const shortMs = Math.min(SHORT_COMMAND_MS, cmdMs)
  const syncState = o.syncState ?? ((ids: string[]) => defaultSyncState(ids, defaultSyncStatePath(o)))
  const noStore = () => {
    if (o.store) throw new BridgeError("--store では作成・削除しません", 1)
  }

  // Runner が固まっても返ってこられるよう、こちらでも上限を設ける
  const exec = async (cmd: string[], stdin?: string, ms = cmdMs) => {
    let timer: ReturnType<typeof setTimeout> | undefined
    const guard = new Promise<never>((_, rej) => { timer = setTimeout(() => rej(new BridgeError(`timeout after ${ms}ms: ${cmd[0]}`, 124)), ms) })
    try {
      const r = await Promise.race([run(cmd, stdin, ms), guard])
      if (r.code !== 0) throw new BridgeError(r.stderr.trim(), r.code)
      return r.stdout
    } finally {
      clearTimeout(timer)
    }
  }
  const bridge = async <T>(args: string[], check: (v: any) => boolean, stdin?: string): Promise<T> => {
    const out = await exec([o.bridge, ...(o.store ? ["--store", o.store] : []), ...args], stdin)
    let v: unknown
    try {
      v = JSON.parse(out)
    } catch {
      throw new BridgeError(`notes-bridge の出力が JSON ではない: ${out.slice(0, 200)}`, 1)
    }
    if (!check(v)) throw new BridgeError(`notes-bridge の出力の形が不正: ${out.slice(0, 200)}`, 1)
    return v as T
  }
  const isObj = (v: any) => typeof v === "object" && v !== null
  const osa = (script: string, ms = cmdMs, ...args: string[]) => exec(["osascript", "-l", "JavaScript", "-e", script, "--", ...args], undefined, ms)

  const isRunning = async () => (await osa(JXA_RUNNING, shortMs)).trim() === "true"
  const frontState = async (): Promise<Front> => {
    const out = await exec(["sh", "-c", "lsappinfo info -only bundleid $(lsappinfo front)"], undefined, shortMs)
    if (out.includes("com.apple.Notes")) return "front"
    return /=\s*"[^"]+"/.test(out) ? "back" : "unknown"
  }
  const waitUntil = async (done: () => boolean | Promise<boolean>, limitMs: number): Promise<boolean> => {
    const start = now()
    for (;;) {
      if (await done()) return true
      if (now() - start >= limitMs) return false
      await sleep(POLL_MS)
    }
  }
  // 未送信 (手元の版が送信済みの版より新しい) の id。行が無い id は未送信
  const unsynced = (ids: string[]) => {
    const st = syncState(ids)
    return ids.filter((_, i) => {
      const s = st[i]
      return !s || s.local > s.synced
    })
  }

  const flush = async (all: string[]): Promise<FlushResult> => {
    if (all.length === 0) return "not-needed"
    // 送信済みのものは待たない。全部送信済みならメモ.app に触らない
    const ids = unsynced(all)
    if (ids.length === 0) return "done"
    if (await isRunning()) {
      if ((await frontState()) !== "back") return "deferred"
      // 一度終了させ、起動し直すことで未送信の変更を iCloud へ押し出す
      await osa(JXA_QUIT, shortMs)
      if (!(await waitUntil(async () => !(await isRunning()), WAIT_QUIT_MS))) return "timeout"
    }
    await exec(["open", "-g", "-a", "Notes"])
    // 時間切れのときは送信を続けられるよう起動したままにする
    if (!(await waitUntil(() => unsynced(ids).length === 0, WAIT_SYNC_MS))) return "timeout"
    // 待つ間に手前へ出されていたら終了させない
    if ((await frontState()) !== "back") return "deferred"
    await osa(JXA_QUIT, shortMs)
    return "done"
  }

  return {
    async selfcheck() {
      await bridge(["selfcheck"], isObj)
    },
    async list() {
      const r = await bridge<{ folderMissing: boolean; notes: BridgeNote[] }>(
        ["list", "--folder", o.folder],
        (v) => isObj(v) && typeof v.folderMissing === "boolean" && Array.isArray(v.notes),
      )
      return r.folderMissing ? null : r.notes
    },
    async write(id, doc, expect) {
      return (await bridge<{ doc: Doc }>(["write", "--id", id], (v) => isObj(v) && isObj(v.doc), JSON.stringify({ doc, expect }))).doc
    },
    async create(title) {
      noStore()
      const uri = (await osa(JXA_CREATE, cmdMs, o.folder, title)).trim()
      const { id } = await bridge<{ id: string }>(["identify", "--uri", uri], (v) => isObj(v) && typeof v.id === "string")
      return { id, uri }
    },
    async remove(uri) {
      noStore()
      await osa(JXA_REMOVE, cmdMs, uri)
    },
    async exportMedia(attachmentId, to) {
      await bridge(["export-media", "--attachment", attachmentId, "--to", to], isObj)
    },
    async flush(ids) {
      if (o.store) return "not-needed"
      try {
        return await flush(ids)
      } catch (e) {
        if (e instanceof BridgeError && e.code === 124) return "timeout"
        throw e
      }
    },
  }
}
