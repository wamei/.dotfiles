// 書式に依存しない文書の中間表現。org とメモ.app の双方がこの型へ変換される。

export type Mark = "bold" | "italic" | "underline" | "strike"
// 正規化時の marks の並び順
export const MARKS: readonly Mark[] = ["bold", "italic", "underline", "strike"]

export type TextInline = { kind: "text"; text: string; marks: Mark[]; link?: string }
export type AttachmentInline = { kind: "attachment"; id: string; image: boolean }
export type Inline = TextInline | AttachmentInline

export type ParaStyle =
  | { kind: "title" }
  | { kind: "heading" }
  | { kind: "subheading" }
  | { kind: "body" }
  | { kind: "mono" }
  | { kind: "list"; list: "bullet" | "dash" | "number" }
  | { kind: "check"; done: boolean }

export type Paragraph = { style: ParaStyle; indent: number; quote: boolean; inlines: Inline[] }
export type Doc = { paragraphs: Paragraph[] }

export const EMPTY_DOC: Doc = { paragraphs: [] }

export function text(t: string, marks: Mark[] = [], link?: string): TextInline {
  return link === undefined ? { kind: "text", text: t, marks } : { kind: "text", text: t, marks, link }
}

export function para(
  style: ParaStyle,
  inlines: Inline[],
  opts: { indent?: number; quote?: boolean } = {},
): Paragraph {
  return { style, indent: opts.indent ?? 0, quote: opts.quote ?? false, inlines }
}

// JSON.stringify で比べるため、キー順を固定して作り直す
function normalizeStyle(s: ParaStyle): ParaStyle {
  switch (s.kind) {
    case "list": return { kind: "list", list: s.list }
    case "check": return { kind: "check", done: s.done }
    default: return { kind: s.kind }
  }
}

function normalizeInlines(inlines: Inline[]): Inline[] {
  const out: Inline[] = []
  for (const i of inlines) {
    if (i.kind === "attachment") {
      out.push({ kind: "attachment", id: i.id, image: i.image })
      continue
    }
    if (i.text === "") continue
    const marks = MARKS.filter((m) => i.marks.includes(m))
    const prev = out[out.length - 1]
    if (
      prev && prev.kind === "text" && prev.link === i.link &&
      prev.marks.length === marks.length && prev.marks.every((m, k) => m === marks[k])
    ) {
      prev.text += i.text
    } else {
      out.push(text(i.text, marks, i.link))
    }
  }
  return out
}

function normalizeParagraph(p: Paragraph): Paragraph {
  return { style: normalizeStyle(p.style), indent: p.indent, quote: p.quote, inlines: normalizeInlines(p.inlines) }
}

export function normalizeDoc(d: Doc): Doc {
  return { paragraphs: d.paragraphs.map(normalizeParagraph) }
}

export function docEquals(a: Doc, b: Doc): boolean {
  return JSON.stringify(normalizeDoc(a)) === JSON.stringify(normalizeDoc(b))
}

export function paragraphEquals(a: Paragraph, b: Paragraph): boolean {
  return JSON.stringify(normalizeParagraph(a)) === JSON.stringify(normalizeParagraph(b))
}

export function plainText(p: Paragraph): string {
  return p.inlines.map((i) => (i.kind === "text" ? i.text : "")).join("")
}

export function docTitle(d: Doc): string {
  const first = d.paragraphs[0]
  return first ? plainText(first) : ""
}

export function imageIds(d: Doc): string[] {
  return d.paragraphs.flatMap((p) =>
    p.inlines.flatMap((i) => (i.kind === "attachment" && i.image ? [i.id] : [])))
}
