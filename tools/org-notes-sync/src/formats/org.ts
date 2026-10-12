import { para, text, normalizeDoc, type Inline, type Paragraph } from "../doc"
import type { LocalFormat, Parsed } from "./format"

// 行内の装飾は Task 3 で実装する。それまでは全体を 1 つの text として扱う仮実装
export function parseInline(s: string): Inline[] {
  return s === "" ? [] : [text(s)]
}

export function renderInline(xs: Inline[]): string {
  return xs.map((i) => (i.kind === "text" ? i.text : "")).join("")
}

const META = /^#\+([A-Za-z_]+):\s*(.*)$/
const ATTACHMENT = /^#\+notes_attachment:\s*(\S+)/i
const BEGIN_EXAMPLE = /^#\+begin_example(\s.*)?$/i
const END_EXAMPLE = /^#\+end_example\s*$/i
const BEGIN_QUOTE = /^#\+begin_quote\s*$/i
const END_QUOTE = /^#\+end_quote\s*$/i
const HEADING = /^(\*+) (.*)$/
const LIST = /^( *)(?:([-+])|(\d+)[.)])\s+(?:\[( |X|x)\]\s+)?(.*)$/

function splitLines(s: string): string[] {
  const lines = s.split("\n")
  // 末尾の改行で終わる最後の空行は段落にしない
  if (lines[lines.length - 1] === "") lines.pop()
  return lines
}

function parse(src: string, fallbackTitle: string): Parsed {
  const lines = splitLines(src)
  const preserved: string[] = []
  let optIn = false
  let titleInlines: Inline[] | undefined
  const body: Paragraph[] = []
  let inExample = false
  let quote = false

  for (const line of lines) {
    if (inExample) {
      if (END_EXAMPLE.test(line)) inExample = false
      else body.push(para({ kind: "mono" }, line === "" ? [] : [text(line)], { quote }))
      continue
    }
    if (BEGIN_EXAMPLE.test(line)) { inExample = true; continue }
    if (BEGIN_QUOTE.test(line)) { quote = true; continue }
    if (END_QUOTE.test(line)) { quote = false; continue }

    const meta = META.exec(line)
    if (meta) {
      const key = meta[1]!.toLowerCase()
      if (key === "title" && titleInlines === undefined) {
        titleInlines = parseInline(meta[2]!)
        continue
      }
      if (key !== "title" && key !== "notes_attachment") {
        preserved.push(line)
        if (key === "notes_sync" && meta[2]!.trim().toLowerCase() === "yes") optIn = true
        continue
      }
    }
    const att = ATTACHMENT.exec(line)
    if (att) {
      body.push(para({ kind: "body" }, [{ kind: "attachment", id: att[1]!, image: false }], { quote }))
      continue
    }
    const h = HEADING.exec(line)
    if (h) {
      const n = h[1]!.length
      const inlines = parseInline(h[2]!)
      if (n === 1) body.push(para({ kind: "heading" }, inlines, { quote }))
      else if (n === 2) body.push(para({ kind: "subheading" }, inlines, { quote }))
      else body.push(para({ kind: "body" }, inlines.map((i) => (i.kind === "text" ? text(i.text, ["bold"], i.link) : i)), { quote }))
      continue
    }
    const l = LIST.exec(line)
    if (l) {
      const indent = Math.floor(l[1]!.length / 2)
      const style: Paragraph["style"] = l[4] !== undefined
        ? { kind: "check", done: l[4] !== " " }
        : l[2] === "-" ? { kind: "list", list: "bullet" }
        : l[2] === "+" ? { kind: "list", list: "dash" }
        : { kind: "list", list: "number" }
      body.push(para(style, parseInline(l[5]!), { indent, quote }))
      continue
    }
    const lead = line.length - line.trimStart().length
    const indent = Math.floor(lead / 2)
    body.push(para({ kind: "body" }, parseInline(line.slice(indent * 2)), { indent, quote }))
  }

  const title = para({ kind: "title" }, titleInlines ?? parseFallback(fallbackTitle))
  return { doc: normalizeDoc({ paragraphs: [title, ...body] }), optIn, preserved }
}

function parseFallback(s: string): Inline[] {
  return s === "" ? [] : [text(s)]
}

function isBoldLine(p: Paragraph): boolean {
  return p.inlines.length > 0 && p.inlines.every((i) => i.kind === "text" && i.marks.includes("bold"))
}

function stripBold(inlines: Inline[]): Inline[] {
  return inlines.map((i) => (i.kind === "text" ? text(i.text, i.marks.filter((m) => m !== "bold"), i.link) : i))
}

function render(doc: { paragraphs: Paragraph[] }, preserved: string[]): string {
  const out: string[] = []
  let rest = doc.paragraphs
  const first = rest[0]
  if (first && first.style.kind === "title") {
    const t = renderInline(first.inlines)
    out.push(t === "" ? "#+title:" : `#+title: ${t}`, ...preserved)
    rest = rest.slice(1)
  } else {
    out.push(...preserved)
  }

  let inQuote = false
  let inExample = false
  // 同じ indent で連続する number 項目の番号。リスト以外の段落で全て戻る
  let counters: number[] = []
  const closeExample = () => { if (inExample) { out.push("#+end_example"); inExample = false } }

  for (const p of rest) {
    if (p.quote !== inQuote) {
      closeExample()
      out.push(p.quote ? "#+begin_quote" : "#+end_quote")
      inQuote = p.quote
    }
    if (p.style.kind === "mono") {
      if (!inExample) { out.push("#+begin_example"); inExample = true }
      out.push(renderInline(p.inlines))
      counters = []
      continue
    }
    closeExample()

    const pad = "  ".repeat(p.indent)
    const body = renderInline(p.inlines)
    const s = p.style
    if (s.kind === "list" || s.kind === "check") {
      counters.length = p.indent + 1
      let marker: string
      if (s.kind === "check") marker = s.done ? "- [X] " : "- [ ] "
      else if (s.list === "bullet") marker = "- "
      else if (s.list === "dash") marker = "+ "
      else {
        counters[p.indent] = (counters[p.indent] ?? 0) + 1
        marker = `${counters[p.indent]}. `
      }
      if (s.kind === "check" || s.list !== "number") counters[p.indent] = 0
      out.push(pad + marker + body)
      continue
    }
    counters = []
    if (s.kind === "title") out.push(`* ${body}`)
    else if (s.kind === "heading") out.push(`* ${body}`)
    else if (s.kind === "subheading") out.push(`** ${body}`)
    else if (p.inlines.length === 1 && p.inlines[0]!.kind === "attachment" && !p.inlines[0]!.image) {
      out.push(`#+notes_attachment: ${p.inlines[0]!.id}`)
    } else if (!p.inlines.some((i) => i.kind === "attachment") && isBoldLine(p)) {
      out.push(`*** ${renderInline(stripBold(p.inlines))}`)
    } else {
      out.push(body === "" ? "" : pad + body)
    }
  }
  closeExample()
  if (inQuote) out.push("#+end_quote")
  return out.join("\n") + "\n"
}

export const orgFormat: LocalFormat = {
  ext: ".org",
  parse,
  render,
  hasConflictMarkers: (s) => /^(<{7}|={7}|>{7})( |$)/m.test(s),
}
