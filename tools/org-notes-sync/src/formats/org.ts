import { para, plainText, text, normalizeDoc, normalizeInlines, type Inline, type Mark, type Paragraph } from "../doc"
import type { LocalFormat, Parsed } from "./format"

// --- 行内の変換 ---
// 普通の文章 (パス、snake_case、算術式など) を装飾と誤認しないよう、装飾は保守的に認める

const MARK_CHARS: ReadonlyArray<readonly [Mark, string]> = [
  ["bold", "*"], ["italic", "/"], ["underline", "_"], ["strike", "+"],
]
const MARK_OF: Record<string, Mark> = Object.fromEntries(MARK_CHARS.map(([m, c]) => [c, m]))

const LINK = /^\[\[([^\[\]]+)\](?:\[([^\[\]]*)\])?\]/
const URL_RE = /^https?:\/\/[^\s\[\]<>"]+/
const MEDIA = /^file:\.notes-media\/(.+)\.jpg$/
const ATTACH = /^notes-attachment:(.+)$/

const isSpace = (c: string) => /\s/.test(c)
const isNonAscii = (c: string) => c.charCodeAt(0) > 127

// 開き記号の直前に置いてよい文字 (undefined は行頭)
function canPrecede(c: string | undefined): boolean {
  return c === undefined || isSpace(c) || "-({'\"".includes(c) || isNonAscii(c)
}
// 閉じ記号の直後に置いてよい文字 (undefined は行末)
function canFollow(c: string | undefined): boolean {
  return c === undefined || isSpace(c) || "-.,:;!?'\")}]".includes(c) || isNonAscii(c)
}

// 記号の直後 (位置 n) が識別子・パスの続きか。英数字、または後ろに更に続く ._~-
// ("*b*." の文末の . は続きではない)
function insideIdentifier(s: string, n: number): boolean {
  const c = s[n] ?? ""
  if (/[A-Za-z0-9]/.test(c)) return true
  return /[._~-]/.test(c) && /[A-Za-z0-9._~-]/.test(s[n + 1] ?? "")
}

// 位置 i の記号が開きとして成立するとき、閉じの位置を返す
function findClose(s: string, i: number): number | undefined {
  const m = s[i]!
  if (!canPrecede(s[i - 1])) return undefined
  const first = s[i + 1]
  if (first === undefined || isSpace(first)) return undefined
  // 中身に自分の記号を含む範囲は装飾にしない (/etc/nginx/ のようなパスを守る)。最初の同じ記号だけが閉じ候補
  const j = s.indexOf(m, i + 2)
  if (j < 0 || isSpace(s[j - 1]!) || !canFollow(s[j + 1])) return undefined
  // 行内に単語の途中の同じ記号 (~/.config/ の 1 つ目の / など。直後が英数字か ._~- のもの) があれば、パスや式とみなして装飾にしない
  for (let k = 0; k < s.length; k++) {
    if (k === i || k === j || s[k] !== m || k === 0) continue
    const p = s[k - 1]!
    if (!isSpace(p) && p.charCodeAt(0) <= 127 && !"-({'\"".includes(p) && insideIdentifier(s, k + 1)) return undefined
  }
  return j
}

export function parseInline(s: string): Inline[] {
  const out: Inline[] = []
  let buf = ""
  const flush = () => { if (buf !== "") { out.push(text(buf)); buf = "" } }
  let i = 0
  while (i < s.length) {
    const rest = s.slice(i)
    const link = LINK.exec(rest)
    if (link) {
      flush()
      const target = link[1]!
      const label = link[2]
      const media = MEDIA.exec(target)
      const att = ATTACH.exec(target)
      if (media) out.push({ kind: "attachment", id: media[1]!, image: true })
      else if (att) out.push({ kind: "attachment", id: att[1]!, image: false })
      else out.push(text(label ?? target, [], target))
      i += link[0].length
      continue
    }
    const url = URL_RE.exec(rest)
    if (url) {
      flush()
      out.push(text(url[0], [], url[0]))
      i += url[0].length
      continue
    }
    const mark = MARK_OF[s[i]!]
    if (mark) {
      const close = findClose(s, i)
      if (close !== undefined) {
        flush()
        for (const inner of parseInline(s.slice(i + 1, close))) {
          out.push(inner.kind === "text" ? text(inner.text, [...inner.marks, mark], inner.link) : inner)
        }
        i = close + 1
        continue
      }
    }
    buf += s[i]
    i++
  }
  flush()
  return normalizeInlines(out)
}

function renderPlain(i: Inline): string {
  if (i.kind === "attachment") return i.image ? `[[file:.notes-media/${i.id}.jpg]]` : `[[notes-attachment:${i.id}]]`
  if (i.link === undefined) return i.text
  if (i.text !== i.link) return `[[${i.link}][${i.text}]]`
  return /^https?:\/\//.test(i.link) ? i.link : `[[${i.link}]]`
}

export function renderInline(xs: Inline[]): string {
  const items = normalizeInlines(xs)
  let out = ""
  items.forEach((i, k) => {
    let body = renderPlain(i)
    // 中身に自分の記号を含む marks は parse が装飾と読まないので付けない
    if (i.kind === "text" && i.marks.length > 0 && !i.marks.some((m) => i.text.includes(MARK_CHARS.find(([n]) => n === m)![1]))) {
      const next = items[k + 1]
      // 直後の inline の最初の文字 (装飾が付く場合の記号までは見ない。ずれは canon が吸収する)
      const after = next ? renderPlain(next)[0] : undefined
      const readable =
        canPrecede(out[out.length - 1]) && canFollow(after) &&
        !isSpace(body[0]!) && !isSpace(body[body.length - 1]!)
      if (readable) {
        for (const [m, c] of [...MARK_CHARS].reverse()) if (i.marks.includes(m)) body = c + body + c
      }
    }
    out += body
  })
  return out
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
  const lines = s.split(/\r?\n/)
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
    const indent = Math.floor(/^ */.exec(line)![0].length / 2)
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
  // 1 行目は書式に関わらずタイトル (メモ.app は 1 行目をタイトルとみなす)
  const first = rest[0]
  out.push(`#+title: ${first ? renderInline(first.inlines) : ""}`, ...preserved)
  rest = rest.slice(1)

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
      out.push(plainText(p)) // 生のまま。装飾は解釈しない
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

// git の衝突マーカー: 行頭の <<<<<<< → ======= → >>>>>>> がこの順に揃ったときだけ (区切り線だけの行は普通の文章)
function hasConflictMarkers(s: string): boolean {
  const marks = [/^<{7}( |$)/, /^={7}( |$)/, /^>{7}( |$)/]
  let next = 0
  for (const line of s.split(/\r?\n/)) {
    if (marks[next]!.test(line) && ++next === marks.length) return true
  }
  return false
}

export const orgFormat: LocalFormat = {
  ext: ".org",
  parse,
  render,
  hasConflictMarkers,
}
