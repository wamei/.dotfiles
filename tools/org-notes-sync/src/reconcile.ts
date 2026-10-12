// org ファイル・メモ・前回の base から、この周に行う動作を決める (副作用なし)
import { docEquals, docTitle, EMPTY_DOC, paragraphEquals, type Doc, type Paragraph } from "./doc"
import type { LocalFormat } from "./formats/format"
import { merge3 } from "./merge"
import { fileNameKey, noteFileName } from "./filename"

export type OrgFile = { file: string; text: string; mtimeMs: number } // file は org ディレクトリからの相対 (例 "foo.org")
export type NoteSnap = { id: string; uri: string; doc: Doc }
export type Pair = { file: string; noteId: string; base: Doc }
export type ReconcileInput = {
  format: LocalFormat; pairs: Pair[]; orgFiles: OrgFile[]
  notes: NoteSnap[] | null // null = メモ.app に org フォルダが無い
  force: boolean
}
export type Action =
  | { kind: "noop"; file: string; noteId: string }
  | { kind: "pushNotes"; file: string; noteId: string; doc: Doc }
  | { kind: "pullOrg"; file: string; noteId: string; text: string; base: Doc }
  | { kind: "both"; file: string; noteId: string; text: string; doc: Doc }
  | { kind: "conflict"; file: string; noteId: string; text: string; base: Doc }
  | { kind: "blocked"; file: string; noteId: string }
  | { kind: "createNote"; file: string; doc: Doc }
  | { kind: "createOrg"; file: string; noteId: string; text: string; base: Doc }
  | { kind: "deleteNote"; file: string; noteId: string; uri: string }
  | { kind: "trashOrg"; file: string; noteId: string }
  | { kind: "drop"; file: string; noteId: string }
  | { kind: "skipped"; file: string; reason: "conflict-markers" }
export type Plan = { ok: true; actions: Action[] } | { ok: false; reason: "folder-missing" | "notes-empty" | "mass-delete" }

const OPT_IN_LINE = "#+notes_sync: yes"

// org で表現したときの形
export function canon(format: LocalFormat, d: Doc): Doc {
  return format.parse(format.render(d, []), docTitle(d)).doc
}

// target のうち source と (org 表現で) 同じ段落を、source の段落 (メモ側の忠実な形) に戻す
export function rehydrate(target: Doc, source: Doc, format: LocalFormat): Doc {
  const c = canon(format, source)
  if (c.paragraphs.length !== source.paragraphs.length) return target
  const matched = lcsMatches(target.paragraphs, c.paragraphs)
  return { paragraphs: target.paragraphs.map((p, i) => {
    const j = matched.get(i)
    return j === undefined ? p : source.paragraphs[j]!
  }) }
}

// a と b の段落の最長共通部分列。a の添字 → b の添字
function lcsMatches(a: Paragraph[], b: Paragraph[]): Map<number, number> {
  const len: number[][] = Array.from({ length: a.length + 1 }, () => new Array<number>(b.length + 1).fill(0))
  for (let i = a.length - 1; i >= 0; i--) {
    for (let j = b.length - 1; j >= 0; j--) {
      len[i]![j] = paragraphEquals(a[i]!, b[j]!) ? len[i + 1]![j + 1]! + 1 : Math.max(len[i + 1]![j]!, len[i]![j + 1]!)
    }
  }
  const out = new Map<number, number>()
  let i = 0
  let j = 0
  while (i < a.length && j < b.length) {
    if (paragraphEquals(a[i]!, b[j]!)) { out.set(i, j); i++; j++ }
    else if (len[i + 1]![j]! >= len[i]![j + 1]!) i++
    else j++
  }
  return out
}

function stem(file: string, ext: string): string {
  return file.endsWith(ext) ? file.slice(0, -ext.length) : file
}

// メモだけ変わったときの org の本文。org の触っていない行 (#+TITLE: などの表記) はそのまま残し、
// 行マージが衝突したらメモの描画で置き換える
function pulledText(format: LocalFormat, base: Doc, orgText: string, preserved: string[], notesDoc: Doc): string {
  const theirs = format.render(notesDoc, preserved)
  const m = merge3(format.render(base, preserved), orgText, theirs)
  return m.conflict ? theirs : m.text
}

// org とメモの両方が base から変わったときの判断
function decideBothChanged(
  format: LocalFormat, file: string, noteId: string, base: Doc,
  orgText: string, orgDoc: Doc, preserved: string[], notesDoc: Doc,
): Action {
  // org 表現で同じなら、base をメモ側に揃えるだけ
  if (docEquals(orgDoc, canon(format, notesDoc))) return { kind: "pullOrg", file, noteId, text: orgText, base: notesDoc }
  const m = merge3(format.render(base, preserved), orgText, format.render(notesDoc, preserved))
  if (m.conflict) return { kind: "conflict", file, noteId, text: m.text, base: notesDoc }
  const merged = format.parse(m.text, stem(file, format.ext)).doc
  return { kind: "both", file, noteId, text: m.text, doc: rehydrate(rehydrate(merged, notesDoc, format), base, format) }
}

function decidePair(format: LocalFormat, pair: Pair, org: OrgFile | undefined, note: NoteSnap | undefined): Action {
  const { file, noteId, base } = pair
  const parsed = org ? format.parse(org.text, stem(org.file, format.ext)) : undefined
  if (!org || !parsed?.optIn) {
    // org の削除 / opt-in 解除
    return note ? { kind: "deleteNote", file, noteId, uri: note.uri } : { kind: "drop", file, noteId }
  }
  if (!note) return { kind: "trashOrg", file, noteId }
  if (format.hasConflictMarkers(org.text)) return { kind: "blocked", file, noteId }

  const orgChanged = !docEquals(parsed.doc, canon(format, base))
  const notesChanged = !docEquals(note.doc, base)
  if (!orgChanged && !notesChanged) return { kind: "noop", file, noteId }
  if (orgChanged && !notesChanged) return { kind: "pushNotes", file, noteId, doc: rehydrate(parsed.doc, base, format) }
  if (!orgChanged) return { kind: "pullOrg", file, noteId, text: pulledText(format, base, org.text, parsed.preserved, note.doc), base: note.doc }
  return decideBothChanged(format, file, noteId, base, org.text, parsed.doc, parsed.preserved, note.doc)
}

export function reconcile(input: ReconcileInput): Plan {
  const { format, pairs, orgFiles, notes, force } = input
  const orgByFile = new Map(orgFiles.map((o) => [o.file, o]))
  const noteById = new Map((notes ?? []).map((n) => [n.id, n]))
  const actions: Action[] = []

  // ペア済み
  for (const pair of pairs) {
    actions.push(decidePair(format, pair, orgByFile.get(pair.file), noteById.get(pair.noteId)))
  }

  // ペア無しの org
  const pairedFiles = new Set(pairs.map((p) => p.file))
  const usedNoteIds = new Set(pairs.map((p) => p.noteId))
  for (const org of orgFiles) {
    if (pairedFiles.has(org.file)) continue
    const parsed = format.parse(org.text, stem(org.file, format.ext))
    if (!parsed.optIn) continue
    // 衝突マーカー入りの本文をメモへ送らない (作成も採用もしない)
    if (format.hasConflictMarkers(org.text)) {
      actions.push({ kind: "skipped", file: org.file, reason: "conflict-markers" })
      continue
    }
    // 同じタイトルのペア無しメモがあれば、base を空として採用する (state を失ったときの重複防止)
    const title = docTitle(parsed.doc)
    const match = (notes ?? []).find((n) => !usedNoteIds.has(n.id) && docTitle(n.doc) === title)
    if (match) {
      usedNoteIds.add(match.id)
      actions.push(decideBothChanged(format, org.file, match.id, EMPTY_DOC, org.text, parsed.doc, parsed.preserved, match.doc))
    } else {
      actions.push({ kind: "createNote", file: org.file, doc: parsed.doc })
    }
  }

  // ペア無しのメモ
  const taken = new Set(orgFiles.map((o) => fileNameKey(o.file)))
  for (const note of notes ?? []) {
    if (usedNoteIds.has(note.id)) continue
    const file = noteFileName(docTitle(note.doc), taken, format.ext)
    taken.add(fileNameKey(file))
    actions.push({ kind: "createOrg", file, noteId: note.id, text: format.render(note.doc, [OPT_IN_LINE]), base: note.doc })
  }

  // 全体停止: 読み取りの失敗を「全部消えた」と解釈しない
  if (notes === null && pairs.length > 0) return { ok: false, reason: "folder-missing" }
  if (notes !== null && notes.length === 0 && pairs.length > 0) return { ok: false, reason: "notes-empty" }
  const deletions = actions.filter((a) => a.kind === "deleteNote" || a.kind === "trashOrg").length
  if (deletions >= 3 && deletions * 2 >= pairs.length && !force) return { ok: false, reason: "mass-delete" }
  return { ok: true, actions }
}
