// ローカルファイル・メモ・前回の base から、この周に行う動作を決める (副作用なし)
import { docEquals, docTitle, EMPTY_DOC, paragraphEquals, type Doc, type Paragraph } from "./doc"
import type { LocalFormat } from "./formats/format"
import { merge3 } from "./merge"
import { fileNameKey, noteFileName } from "./filename"

export type LocalFile = { file: string; text: string; mtimeMs: number } // file はディレクトリからの相対 (例 "foo.org")
export type NoteSnap = { id: string; uri: string; doc: Doc }
export type Pair = { file: string; noteId: string; base: Doc }
export type ReconcileInput = {
  format: LocalFormat; pairs: Pair[]; localFiles: LocalFile[]
  notes: NoteSnap[] | null // null = メモ.app に同期フォルダが無い
  force: boolean
}
export type Action =
  | { kind: "noop"; file: string; noteId: string }
  | { kind: "pushNotes"; file: string; noteId: string; doc: Doc }
  | { kind: "pullLocal"; file: string; noteId: string; text: string; base: Doc }
  | { kind: "both"; file: string; noteId: string; text: string; doc: Doc }
  | { kind: "conflict"; file: string; noteId: string; text: string; base: Doc }
  | { kind: "blocked"; file: string; noteId: string }
  | { kind: "createNote"; file: string; doc: Doc }
  | { kind: "createLocal"; file: string; noteId: string; text: string; base: Doc }
  | { kind: "deleteNote"; file: string; noteId: string; uri: string }
  | { kind: "trashLocal"; file: string; noteId: string }
  | { kind: "drop"; file: string; noteId: string }
  | { kind: "skipped"; file: string; reason: "conflict-markers" }
  | { kind: "rename"; from: string; to: string; noteId: string } // ペアのローカルファイル名を付け替える (メモはそのまま)
export type Plan = { ok: true; actions: Action[] } | { ok: false; reason: "folder-missing" | "notes-empty" | "mass-delete" }

// ローカルの書式で表現したときの形
export function canon(format: LocalFormat, d: Doc): Doc {
  return format.parse(format.render(d, []), docTitle(d)).doc
}

// target のうち source と (ローカルの表現で) 同じ段落を、source の段落 (メモ側の忠実な形) に戻す
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

// メモだけ変わったときのローカルファイルの本文。ローカルファイルの触っていない行 (#+TITLE: などの表記) はそのまま残し、
// 行マージが衝突したらメモの描画で置き換える
function pulledText(format: LocalFormat, base: Doc, localText: string, preserved: string[], notesDoc: Doc): string {
  const theirs = format.render(notesDoc, preserved)
  const m = merge3(format.render(base, preserved), localText, theirs)
  return m.conflict ? theirs : m.text
}

// ローカルファイルとメモの両方が base から変わったときの判断
function decideBothChanged(
  format: LocalFormat, file: string, noteId: string, base: Doc,
  localText: string, localDoc: Doc, preserved: string[], notesDoc: Doc,
): Action {
  // ローカルの表現で同じなら、base をメモ側に揃えるだけ
  if (docEquals(localDoc, canon(format, notesDoc))) return { kind: "pullLocal", file, noteId, text: localText, base: notesDoc }
  const m = merge3(format.render(base, preserved), localText, format.render(notesDoc, preserved))
  if (m.conflict) return { kind: "conflict", file, noteId, text: m.text, base: notesDoc }
  const merged = format.parse(m.text, stem(file, format.ext)).doc
  return { kind: "both", file, noteId, text: m.text, doc: rehydrate(rehydrate(merged, notesDoc, format), base, format) }
}

function decidePair(format: LocalFormat, pair: Pair, local: LocalFile | undefined, note: NoteSnap | undefined): Action {
  const { file, noteId, base } = pair
  const parsed = local ? format.parse(local.text, stem(local.file, format.ext)) : undefined
  if (!local || !parsed?.optIn) {
    // ローカルファイルの削除 / opt-in 解除
    return note ? { kind: "deleteNote", file, noteId, uri: note.uri } : { kind: "drop", file, noteId }
  }
  if (!note) return { kind: "trashLocal", file, noteId }
  if (format.hasConflictMarkers(local.text)) return { kind: "blocked", file, noteId }

  const localChanged = !docEquals(parsed.doc, canon(format, base))
  const notesChanged = !docEquals(note.doc, base)
  if (!localChanged && !notesChanged) return { kind: "noop", file, noteId }
  if (localChanged && !notesChanged) return { kind: "pushNotes", file, noteId, doc: rehydrate(parsed.doc, base, format) }
  if (!localChanged) return { kind: "pullLocal", file, noteId, text: pulledText(format, base, local.text, parsed.preserved, note.doc), base: note.doc }
  return decideBothChanged(format, file, noteId, base, local.text, parsed.doc, parsed.preserved, note.doc)
}

// ペアのローカルファイルが消えたとき、リネーム先らしいペア無しの opt-in なローカルファイル。
// 内容が base と同じものを優先し、無ければタイトルが同じものが 1 つだけのとき
function findRenamed(format: LocalFormat, pair: Pair, localFiles: LocalFile[], pairedFiles: Set<string>): LocalFile | undefined {
  const candidates = localFiles.flatMap((o) => {
    if (pairedFiles.has(o.file)) return []
    const parsed = format.parse(o.text, stem(o.file, format.ext))
    return parsed.optIn ? [{ local: o, doc: parsed.doc }] : []
  })
  const base = canon(format, pair.base)
  const same = candidates.find((c) => docEquals(c.doc, base))
  if (same) return same.local
  const titled = candidates.filter((c) => docTitle(c.doc) === docTitle(pair.base))
  return titled.length === 1 ? titled[0]!.local : undefined
}

export function reconcile(input: ReconcileInput): Plan {
  const { format, pairs, localFiles, notes, force } = input
  const localByFile = new Map(localFiles.map((o) => [o.file, o]))
  const noteById = new Map((notes ?? []).map((n) => [n.id, n]))
  const actions: Action[] = []

  const pairedFiles = new Set(pairs.map((p) => p.file))

  // ペア済み
  for (const pair of pairs) {
    const local = localByFile.get(pair.file)
    const note = noteById.get(pair.noteId)
    // ローカルファイルが無くなった: 消す前に、リネームされた先 (ペア無しの opt-in なローカルファイル) を探す
    const to = !local && note ? findRenamed(format, pair, localFiles, pairedFiles) : undefined
    if (to) {
      pairedFiles.add(to.file)
      actions.push({ kind: "rename", from: pair.file, to: to.file, noteId: pair.noteId })
      actions.push(decidePair(format, { ...pair, file: to.file }, to, note))
    } else {
      actions.push(decidePair(format, pair, local, note))
    }
  }

  // ペア無しのローカルファイル
  const usedNoteIds = new Set(pairs.map((p) => p.noteId))
  for (const local of localFiles) {
    if (pairedFiles.has(local.file)) continue
    const parsed = format.parse(local.text, stem(local.file, format.ext))
    if (!parsed.optIn) continue
    // 衝突マーカー入りの本文をメモへ送らない (作成も採用もしない)
    if (format.hasConflictMarkers(local.text)) {
      actions.push({ kind: "skipped", file: local.file, reason: "conflict-markers" })
      continue
    }
    // 同じタイトルのペア無しメモがあれば、base を空として採用する (state を失ったときの重複防止)
    const title = docTitle(parsed.doc)
    const match = (notes ?? []).find((n) => !usedNoteIds.has(n.id) && docTitle(n.doc) === title)
    if (match) {
      usedNoteIds.add(match.id)
      actions.push(decideBothChanged(format, local.file, match.id, EMPTY_DOC, local.text, parsed.doc, parsed.preserved, match.doc))
    } else {
      actions.push({ kind: "createNote", file: local.file, doc: parsed.doc })
    }
  }

  // ペア無しのメモ
  const taken = new Set(localFiles.map((o) => fileNameKey(o.file)))
  for (const note of notes ?? []) {
    if (usedNoteIds.has(note.id)) continue
    const file = noteFileName(docTitle(note.doc), taken, format.ext)
    taken.add(fileNameKey(file))
    actions.push({ kind: "createLocal", file, noteId: note.id, text: format.render(note.doc, [format.optInLine]), base: note.doc })
  }

  // 全体停止: 読み取りの失敗を「全部消えた」と解釈しない
  if (notes === null && pairs.length > 0) return { ok: false, reason: "folder-missing" }
  if (notes !== null && notes.length === 0 && pairs.length > 0) return { ok: false, reason: "notes-empty" }
  const deletions = actions.filter((a) => a.kind === "deleteNote" || a.kind === "trashLocal").length
  if (deletions >= 3 && deletions * 2 >= pairs.length && !force) return { ok: false, reason: "mass-delete" }
  return { ok: true, actions }
}
