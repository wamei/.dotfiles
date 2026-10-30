// 1 周の同期: 読み取り → reconcile → 動作の実行 → 画像 → iCloud への送信
import fs from "node:fs"
import path from "node:path"
import { docTitle, imageIds, type Doc } from "./doc"
import type { LocalFormat } from "./formats/format"
import { BridgeError, type NotesPort } from "./notes"
import { gcMedia, listLocalFiles, mediaPath, trashLocalFile, writeLocalFile } from "./localfs"
import { reconcile, type Action, type NoteSnap, type LocalFile, type Pair } from "./reconcile"
import { loadState, saveState, type State } from "./state"

export type SyncDeps = {
  format: LocalFormat; notes: NotesPort; dir: string; stateDir: string; force: boolean
  log: (msg: string) => void; warn: (msg: string) => void
  now?: () => number // 既定は Date.now
}
export type SyncOutcome = { status: "ok" | "aborted"; reason?: string; wroteNotes: string[] }

const MSG = {
  bridge: "メモ.app の内部 API が見つかりません (macOS の更新で変わった可能性)。同期を止めています",
  db: "メモの DB を読めません。システム設定 > プライバシーとセキュリティ > フルディスクアクセス に ~/.local/bin/notes-sync (launchd から動かすとき) と、同期を起動したアプリ (Emacs.app やターミナル) を追加してください",
  folder: "メモ.app の同期フォルダが見つからない / 空です。誤削除を避けるため同期を止めています",
  massDelete: "一度に多くのメモを削除しようとしています。意図どおりなら --force を付けて実行してください",
  dir: (p: string) => `ディレクトリを読めません: ${p}`,
  state: (p: string) => `state.json が壊れています: ${p}。中身を確認して直すか消してください`,
  locked: (f: string) => `${f}: メモがロックされているので同期しません`,
  markers: (f: string) => `${f}: 衝突マーカーが残っているので同期を止めています`,
  conflict: (f: string) => `${f}: 衝突しました。Emacs で解消して保存してください`,
  attachment: (f: string) => `${f}: 添付を含む段落を書き換えられないので、メモへの反映を止めました`,
  unverified: (f: string) => `${f}: メモへの書き込みを検証できなかったので保存しませんでした`,
  changed: (f: string) => `${f}: メモが同期中に更新されたので次回に回します`,
  deferred: "メモ.app を使っている間は iCloud への送信を保留しています",
  timeouts: "メモ.app から iCloud への送信が続けて終わりません",
}

// 送信が時間切れになった後 (オフラインなど)、メモ.app を起動し直すまでの間隔
const FLUSH_BACKOFF_MS = 15 * 60_000
const TIMEOUT_WARN_COUNT = 3

const errMessage = (e: unknown) => (e instanceof Error ? e.message : String(e))

// 1 周の作業状態。ペアの変更は 1 件ごとに state.json へ保存する
type Run = {
  d: SyncDeps
  state: State
  localByFile: Map<string, LocalFile>
  noteDocs: Map<string, Doc> // この周の後のメモの中身
  wrote: string[]
}

const save = (r: Run) => saveState(r.d.stateDir, r.state)

function setPair(r: Run, file: string, noteId: string, base: Doc): void {
  r.state.pairs = [...r.state.pairs.filter((p) => p.file !== file && p.noteId !== noteId), { file, noteId, base }]
  save(r)
}

function removePair(r: Run, file: string, noteId: string): void {
  r.state.pairs = r.state.pairs.filter((p) => !(p.file === file && p.noteId === noteId))
  save(r)
}

// expect: 一覧で読んだメモの中身。書く直前にメモがこれと違えば bridge が書かない (code 7)
async function writeNote(r: Run, noteId: string, doc: Doc, expect?: Doc): Promise<Doc> {
  const base = await r.d.notes.write(noteId, doc, expect)
  r.noteDocs.set(noteId, base)
  r.wrote.push(noteId)
  return base
}

// 読んだときと違う本文だけ書く。Emacs が保存し直していたら (stale) false
function writeLocal(r: Run, file: string, text: string): boolean {
  const local = r.localByFile.get(file)
  if (local && local.text === text) return true
  const res = writeLocalFile(r.d.dir, file, text, local ? local.mtimeMs : null)
  if (res === "stale") r.d.log(`${file}: 読んだ後に変更されたので書かない`)
  return res === "written"
}

function isOptInNow(r: Run, file: string): boolean {
  let text: string
  try {
    text = fs.readFileSync(path.join(r.d.dir, file), "utf8")
  } catch {
    return false
  }
  return r.d.format.parse(text, file).optIn
}

async function execute(r: Run, a: Action): Promise<void> {
  switch (a.kind) {
    case "noop":
      return
    case "pushNotes":
      setPair(r, a.file, a.noteId, await writeNote(r, a.noteId, a.doc, r.noteDocs.get(a.noteId)))
      return
    case "pullLocal":
      if (writeLocal(r, a.file, a.text)) setPair(r, a.file, a.noteId, a.base)
      return
    case "conflict":
      if (!writeLocal(r, a.file, a.text)) return
      setPair(r, a.file, a.noteId, a.base)
      r.d.warn(MSG.conflict(a.file))
      return
    case "both":
      if (!writeLocal(r, a.file, a.text)) return
      setPair(r, a.file, a.noteId, await writeNote(r, a.noteId, a.doc, r.noteDocs.get(a.noteId)))
      return
    case "blocked":
    case "skipped":
      r.d.warn(MSG.markers(a.file))
      return
    case "createNote": {
      const { id } = await r.d.notes.create(docTitle(a.doc))
      setPair(r, a.file, id, await writeNote(r, id, a.doc))
      return
    }
    case "createLocal":
      if (writeLocalFile(r.d.dir, a.file, a.text, null) === "written") setPair(r, a.file, a.noteId, a.base)
      else r.d.log(`${a.file}: 既にあるので作らない`)
      return
    case "rename":
      r.state.pairs = r.state.pairs.map((p) => (p.file === a.from && p.noteId === a.noteId ? { ...p, file: a.to } : p))
      save(r)
      return
    case "deleteNote":
      // 一覧を読んだ後にローカルファイルが opt-in で戻っていたら消さない (次の周で改めて判断する)
      if (isOptInNow(r, a.file)) {
        r.d.log(`${a.file}: 消す前に opt-in で戻っていたのでメモを消さない`)
        return
      }
      await r.d.notes.remove(a.uri)
      r.noteDocs.delete(a.noteId)
      removePair(r, a.file, a.noteId)
      return
    case "trashLocal":
      trashLocalFile(r.d.dir, a.file)
      removePair(r, a.file, a.noteId)
      return
    case "drop":
      removePair(r, a.file, a.noteId)
      return
  }
}

const actionFile = (a: Action) => (a.kind === "rename" ? a.to : a.file)

function warnActionError(d: SyncDeps, a: Action, e: unknown): void {
  const file = actionFile(a)
  if (e instanceof BridgeError && e.code === 5) d.warn(MSG.attachment(file))
  else if (e instanceof BridgeError && e.code === 6) d.warn(MSG.unverified(file))
  else if (e instanceof BridgeError && e.code === 7) d.warn(MSG.changed(file))
  else d.warn(`${file}: ${errMessage(e)}`)
}

// ペア済みのメモに出てくる画像のうち手元に無いものを書き出し、どのローカルファイルからも参照されない画像を消す
// (opt-in を外したローカルファイルもまだ画像を参照しているので残す)
async function syncMedia(r: Run): Promise<void> {
  const { d } = r
  const wanted = new Set(r.state.pairs.flatMap((p) => imageIds(r.noteDocs.get(p.noteId) ?? { paragraphs: [] })))
  for (const id of wanted) {
    const to = mediaPath(d.dir, id)
    if (fs.existsSync(to)) continue
    try {
      fs.mkdirSync(path.dirname(to), { recursive: true })
      await d.notes.exportMedia(id, to)
    } catch (e) {
      d.warn(`画像 ${id} を書き出せません: ${errMessage(e)}`)
    }
  }
  let localFiles: LocalFile[]
  try {
    localFiles = listLocalFiles(d.dir, d.format.ext)
  } catch (e) {
    d.warn(`画像の掃除を飛ばします: ${errMessage(e)}`)
    return
  }
  const keep = new Set<string>()
  for (const o of localFiles) {
    for (const id of imageIds(d.format.parse(o.text, o.file).doc)) keep.add(id)
  }
  for (const id of gcMedia(d.dir, keep)) d.log(`画像 ${id} を消しました`)
}

export async function syncOnce(d: SyncDeps): Promise<SyncOutcome> {
  const abort = (reason: string, msg: string): SyncOutcome => {
    d.warn(msg)
    return { status: "aborted", reason, wroteNotes: [] }
  }

  // 失敗しても DB にある分で同期を続ける
  try {
    const w = await d.notes.wake()
    if (w === "launched" || w === "timeout") d.log(`wake: ${w}`)
  } catch (e) {
    d.log(`wake: ${errMessage(e)}`)
  }

  let state: State
  try {
    state = loadState(d.stateDir)
  } catch (e) {
    d.log(errMessage(e))
    return abort("state", MSG.state(path.join(d.stateDir, "state.json")))
  }

  let listed: Awaited<ReturnType<NotesPort["list"]>>
  try {
    listed = await d.notes.list()
  } catch (e) {
    // bridge はどのコマンドでも先に内部 API を確かめる (selfcheck を別に呼ばない)
    if (e instanceof BridgeError && e.code === 3) {
      d.log(`list: ${e.message}`)
      return abort("bridge", MSG.bridge)
    }
    if (e instanceof BridgeError && e.code === 4) {
      d.log(`list: ${e.message}`)
      return abort("db", MSG.db)
    }
    return abort("list", `メモの一覧を読めません: ${errMessage(e)}`)
  }

  let allLocalFiles: LocalFile[]
  try {
    allLocalFiles = listLocalFiles(d.dir, d.format.ext)
  } catch (e) {
    d.log(errMessage(e))
    return abort("dir", MSG.dir(d.dir))
  }

  // ロックされたメモのペアは、ペアもローカルファイルも reconcile に渡さない (state はそのまま残す)
  const lockedIds = new Set((listed ?? []).filter((n) => "locked" in n).map((n) => n.id))
  const notes: NoteSnap[] | null = listed === null ? null : listed.flatMap((n) => ("locked" in n ? [] : [n]))
  const withheld = state.pairs.filter((p) => lockedIds.has(p.noteId))
  const withheldFiles = new Set(withheld.map((p) => p.file))
  const pairs: Pair[] = state.pairs.filter((p) => !lockedIds.has(p.noteId))
  const localFiles = allLocalFiles.filter((o) => !withheldFiles.has(o.file))

  const plan = reconcile({ format: d.format, pairs, localFiles, notes, force: d.force })
  if (!plan.ok) return abort(plan.reason, plan.reason === "mass-delete" ? MSG.massDelete : MSG.folder)

  for (const p of withheld) d.warn(MSG.locked(p.file))

  const r: Run = {
    d,
    state,
    localByFile: new Map(localFiles.map((o) => [o.file, o])),
    noteDocs: new Map((notes ?? []).map((n) => [n.id, n.doc])),
    wrote: [],
  }
  for (const a of plan.actions) {
    if (a.kind === "rename") d.log(`rename ${a.from} → ${a.to}`)
    else if (a.kind !== "noop") d.log(`${a.kind} ${a.file}`)
    try {
      await execute(r, a)
    } catch (e) {
      warnActionError(d, a, e)
    }
  }

  // 画像の失敗で送信と state の保存を飛ばさない
  try {
    await syncMedia(r)
  } catch (e) {
    d.warn(`画像の同期に失敗しました: ${errMessage(e)}`)
  }

  await flush(r, lockedIds)
  save(r)
  return { status: "ok", wroteNotes: r.wrote }
}

// iCloud への送信。保留していたら、書いたメモが無くてもペア済みのメモ全部を送り直す
async function flush(r: Run, lockedIds: Set<string>): Promise<void> {
  const { d, state } = r
  if (r.wrote.length === 0 && !state.flushPending) return
  const now = (d.now ?? Date.now)()
  // 時間切れ (オフラインなど) の後は、15 分経つまでメモ.app を起動し直さない
  if (state.flushPending && state.flushTimeouts > 0 && now - state.lastFlushAttempt < FLUSH_BACKOFF_MS) {
    d.log("flush: 時間切れの後なので見送り")
    return
  }
  const ids = state.flushPending
    ? [...new Set([...r.wrote, ...state.pairs.map((p) => p.noteId).filter((id) => !lockedIds.has(id))])]
    : r.wrote
  state.lastFlushAttempt = now
  try {
    const res = await d.notes.flush(ids)
    d.log(`flush: ${res}`)
    state.flushPending = res === "deferred" || res === "timeout"
    state.flushTimeouts = res === "timeout" ? state.flushTimeouts + 1 : 0
    if (res === "deferred") d.warn(MSG.deferred)
    if (state.flushTimeouts >= TIMEOUT_WARN_COUNT) d.warn(MSG.timeouts)
  } catch (e) {
    d.warn(`iCloud への送信に失敗しました: ${errMessage(e)}`)
    state.flushPending = true
    state.flushTimeouts = 0
  }
}
