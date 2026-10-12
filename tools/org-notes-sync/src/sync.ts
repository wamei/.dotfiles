// 1 周の同期: 読み取り → reconcile → 動作の実行 → 画像 → iCloud への送信
import fs from "node:fs"
import path from "node:path"
import { docTitle, imageIds, type Doc } from "./doc"
import type { LocalFormat } from "./formats/format"
import { BridgeError, type NotesPort } from "./notes"
import { gcMedia, listOrgFiles, mediaPath, trashOrgFile, writeOrgFile } from "./orgfs"
import { reconcile, type Action, type NoteSnap, type OrgFile, type Pair } from "./reconcile"
import { loadState, saveState, type State } from "./state"

export type SyncDeps = { format: LocalFormat; notes: NotesPort; orgDir: string; stateDir: string; force: boolean; log: (msg: string) => void; warn: (msg: string) => void }
export type SyncOutcome = { status: "ok" | "aborted"; reason?: string; wroteNotes: string[] }

const MSG = {
  bridge: "メモ.app の内部 API が見つかりません (macOS の更新で変わった可能性)。同期を止めています",
  db: "メモの DB を読めません。システム設定 > プライバシーとセキュリティ > フルディスクアクセス に ~/.local/bin/org-notes-sync を追加してください",
  folder: "メモ.app の org フォルダが見つからない / 空です。誤削除を避けるため同期を止めています",
  massDelete: "一度に多くのメモを削除しようとしています。意図どおりなら --force を付けて実行してください",
  orgDir: (p: string) => `org ディレクトリを読めません: ${p}`,
  state: (p: string) => `state.json が壊れています: ${p}。中身を確認して直すか消してください`,
  locked: (f: string) => `${f}: メモがロックされているので同期しません`,
  markers: (f: string) => `${f}: 衝突マーカーが残っているので同期を止めています`,
  conflict: (f: string) => `${f}: 衝突しました。Emacs で解消して保存してください`,
  attachment: (f: string) => `${f}: 添付を含む段落を書き換えられないので、メモへの反映を止めました`,
  unverified: (f: string) => `${f}: メモへの書き込みを検証できなかったので保存しませんでした`,
}

const errMessage = (e: unknown) => (e instanceof Error ? e.message : String(e))

// 1 周の作業状態。ペアの変更は 1 件ごとに state.json へ保存する
type Run = {
  d: SyncDeps
  state: State
  orgByFile: Map<string, OrgFile>
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

async function writeNote(r: Run, noteId: string, doc: Doc): Promise<Doc> {
  const base = await r.d.notes.write(noteId, doc)
  r.noteDocs.set(noteId, base)
  r.wrote.push(noteId)
  return base
}

// 読んだときと違う本文だけ書く。Emacs が保存し直していたら (stale) false
function writeOrg(r: Run, file: string, text: string): boolean {
  const org = r.orgByFile.get(file)
  if (org && org.text === text) return true
  const res = writeOrgFile(r.d.orgDir, file, text, org ? org.mtimeMs : null)
  if (res === "stale") r.d.log(`${file}: 読んだ後に変更されたので書かない`)
  return res === "written"
}

async function execute(r: Run, a: Action): Promise<void> {
  switch (a.kind) {
    case "noop":
      return
    case "pushNotes":
      setPair(r, a.file, a.noteId, await writeNote(r, a.noteId, a.doc))
      return
    case "pullOrg":
      if (writeOrg(r, a.file, a.text)) setPair(r, a.file, a.noteId, a.base)
      return
    case "conflict":
      if (!writeOrg(r, a.file, a.text)) return
      setPair(r, a.file, a.noteId, a.base)
      r.d.warn(MSG.conflict(a.file))
      return
    case "both":
      if (!writeOrg(r, a.file, a.text)) return
      setPair(r, a.file, a.noteId, await writeNote(r, a.noteId, a.doc))
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
    case "createOrg":
      if (writeOrgFile(r.d.orgDir, a.file, a.text, null) === "written") setPair(r, a.file, a.noteId, a.base)
      else r.d.log(`${a.file}: 既にあるので作らない`)
      return
    case "deleteNote":
      await r.d.notes.remove(a.uri)
      r.noteDocs.delete(a.noteId)
      removePair(r, a.file, a.noteId)
      return
    case "trashOrg":
      trashOrgFile(r.d.orgDir, a.file)
      removePair(r, a.file, a.noteId)
      return
    case "drop":
      removePair(r, a.file, a.noteId)
      return
  }
}

function warnActionError(d: SyncDeps, a: Action, e: unknown): void {
  if (e instanceof BridgeError && e.code === 5) d.warn(MSG.attachment(a.file))
  else if (e instanceof BridgeError && e.code === 6) d.warn(MSG.unverified(a.file))
  else d.warn(`${a.file}: ${errMessage(e)}`)
}

// ペア済みのメモに出てくる画像のうち手元に無いものを書き出し、opt-in な org から参照されない画像を消す
async function syncMedia(r: Run): Promise<void> {
  const { d } = r
  const wanted = new Set(r.state.pairs.flatMap((p) => imageIds(r.noteDocs.get(p.noteId) ?? { paragraphs: [] })))
  for (const id of wanted) {
    const to = mediaPath(d.orgDir, id)
    if (fs.existsSync(to)) continue
    try {
      fs.mkdirSync(path.dirname(to), { recursive: true })
      await d.notes.exportMedia(id, to)
    } catch (e) {
      d.warn(`画像 ${id} を書き出せません: ${errMessage(e)}`)
    }
  }
  let orgFiles: OrgFile[]
  try {
    orgFiles = listOrgFiles(d.orgDir, d.format.ext)
  } catch (e) {
    d.warn(`画像の掃除を飛ばします: ${errMessage(e)}`)
    return
  }
  const keep = new Set<string>()
  for (const o of orgFiles) {
    const parsed = d.format.parse(o.text, o.file)
    if (parsed.optIn) for (const id of imageIds(parsed.doc)) keep.add(id)
  }
  for (const id of gcMedia(d.orgDir, keep)) d.log(`画像 ${id} を消しました`)
}

export async function syncOnce(d: SyncDeps): Promise<SyncOutcome> {
  const abort = (reason: string, msg: string): SyncOutcome => {
    d.warn(msg)
    return { status: "aborted", reason, wroteNotes: [] }
  }

  try {
    await d.notes.selfcheck()
  } catch (e) {
    d.log(`selfcheck: ${errMessage(e)}`)
    return abort("bridge", MSG.bridge)
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
    if (e instanceof BridgeError && e.code === 4) {
      d.log(`list: ${e.message}`)
      return abort("db", MSG.db)
    }
    return abort("list", `メモの一覧を読めません: ${errMessage(e)}`)
  }

  let allOrgFiles: OrgFile[]
  try {
    allOrgFiles = listOrgFiles(d.orgDir, d.format.ext)
  } catch (e) {
    d.log(errMessage(e))
    return abort("org-dir", MSG.orgDir(d.orgDir))
  }

  // ロックされたメモのペアは、ペアも org も reconcile に渡さない (state はそのまま残す)
  const lockedIds = new Set((listed ?? []).filter((n) => "locked" in n).map((n) => n.id))
  const notes: NoteSnap[] | null = listed === null ? null : listed.flatMap((n) => ("locked" in n ? [] : [n]))
  const withheld = state.pairs.filter((p) => lockedIds.has(p.noteId))
  const withheldFiles = new Set(withheld.map((p) => p.file))
  const pairs: Pair[] = state.pairs.filter((p) => !lockedIds.has(p.noteId))
  const orgFiles = allOrgFiles.filter((o) => !withheldFiles.has(o.file))

  const plan = reconcile({ format: d.format, pairs, orgFiles, notes, force: d.force })
  if (!plan.ok) return abort(plan.reason, plan.reason === "mass-delete" ? MSG.massDelete : MSG.folder)

  for (const p of withheld) d.warn(MSG.locked(p.file))

  const r: Run = {
    d,
    state,
    orgByFile: new Map(orgFiles.map((o) => [o.file, o])),
    noteDocs: new Map((notes ?? []).map((n) => [n.id, n.doc])),
    wrote: [],
  }
  for (const a of plan.actions) {
    if (a.kind !== "noop") d.log(`${a.kind} ${a.file}`)
    try {
      await execute(r, a)
    } catch (e) {
      warnActionError(d, a, e)
    }
  }

  await syncMedia(r)

  // 送信を保留していたら、書いたメモが無くてもペア済みのメモ全部を送り直す
  if (r.wrote.length > 0 || state.flushPending) {
    const ids = state.flushPending
      ? [...new Set([...r.wrote, ...r.state.pairs.map((p) => p.noteId).filter((id) => !lockedIds.has(id))])]
      : r.wrote
    try {
      const res = await d.notes.flush(ids)
      d.log(`flush: ${res}`)
      state.flushPending = res === "deferred" || res === "timeout"
    } catch (e) {
      d.warn(`iCloud への送信に失敗しました: ${errMessage(e)}`)
      state.flushPending = true
    }
  }
  save(r)
  return { status: "ok", wroteNotes: r.wrote }
}
