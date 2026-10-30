import { para, plainText, text, normalizeDoc, normalizeInlines, type Inline, type Mark, type Paragraph } from "../doc"
import { hasConflictMarkers, isBoldLine, splitLines, stripBold } from "./common"
import type { LocalFormat, Parsed } from "./format"

// --- 行内の変換 ---
// 普通の文章 (パス、snake_case、算術式など) を装飾と誤認しないよう、* _ ~~ の装飾は保守的に認める。
// <u></u> は文章と紛れないので位置を問わず認める

// 前後の文字を見て認める区切り。長いものから試す
const FLANKED: ReadonlyArray<readonly [string, Mark]> = [["**", "bold"], ["~~", "strike"], ["*", "italic"], ["_", "italic"]]
const U_OPEN = "<u>"
const U_CLOSE = "</u>"

// リンク先は空白を含まず、括弧は 1 段だけ釣り合ったものを許す (https://w.org/Foo_(bar))
const DEST = String.raw`(?:[^\s()]|\([^\s()]*\))+`
const LINK = new RegExp(String.raw`^\[([^\[\]]*)\]\((${DEST})\)`)
const DEST_ONLY = new RegExp(`^${DEST}$`)
const IMAGE = /^!\[[^\[\]]*\]\(\.notes-media\/([^\s()]+)\.jpg\)/
const ATTACH = /^notes-attachment:(.+)$/
const URL_RE = /^https?:\/\/(?:[^\s<>"\[\]()]|\([^\s<>"\[\]()]*\))+/
// 裸の URL の末尾の句読点や装飾の記号は URL に含めない (GFM と同じ)
const URL_TAIL = /[?!.,:;*_~'"]+$/

const isSpace = (c: string) => /\s/.test(c)
const isNonAscii = (c: string) => c.charCodeAt(0) > 127

// 開き記号の直前に置いてよい文字 (undefined は行頭)。> は </u> の閉じ
function canPrecede(c: string | undefined): boolean {
  return c === undefined || isSpace(c) || "-({'\">".includes(c) || isNonAscii(c)
}
// 閉じ記号の直後に置いてよい文字 (undefined は行末)。< は <u> の開き
function canFollow(c: string | undefined): boolean {
  return c === undefined || isSpace(c) || "-.,:;!?'\")}]<".includes(c) || isNonAscii(c)
}

// 記号の直後 (位置 n) が識別子・パスの続きか。英数字、または後ろに更に続く ._~-
function insideIdentifier(s: string, n: number): boolean {
  const c = s[n] ?? ""
  if (/[A-Za-z0-9]/.test(c)) return true
  return /[._~-]/.test(c) && /[A-Za-z0-9._~-]/.test(s[n + 1] ?? "")
}

function matchUrl(s: string): string | undefined {
  const m = URL_RE.exec(s)
  return m ? m[0].replace(URL_TAIL, "") : undefined
}

// 位置 i の区切り d が開きとして成立するとき、閉じの位置を返す
function findClose(s: string, i: number, d: string): number | undefined {
  const c = d[0]!
  const n = d.length
  // 同じ記号の連なりの途中・より長い連なりは別の区切り
  if (!canPrecede(s[i - 1]) || s[i - 1] === c || s[i + n] === c) return undefined
  const first = s[i + n]
  if (first === undefined || isSpace(first)) return undefined
  // 中身に自分の記号を含む範囲は装飾にしない。最初の同じ記号だけが閉じ候補
  const j = s.indexOf(c, i + n + 1)
  if (j < 0 || !s.startsWith(d, j) || s[j + n] === c || isSpace(s[j - 1]!) || !canFollow(s[j + n])) return undefined
  // 行内に単語の途中の同じ記号 (foo_bar の _ など。連なりは除く) があれば、識別子や式とみなして装飾にしない
  for (let k = 1; k < s.length; k++) {
    if (s[k] !== c || (k >= i && k < i + n) || (k >= j && k < j + n) || s[k - 1] === c || s[k + 1] === c) continue
    const p = s[k - 1]!
    if (!isSpace(p) && p.charCodeAt(0) <= 127 && !"-({'\"".includes(p) && insideIdentifier(s, k + 1)) return undefined
  }
  return j
}

function withMark(inlines: Inline[], mark: Mark): Inline[] {
  return inlines.map((x) => (x.kind === "text" ? text(x.text, [...x.marks, mark], x.link) : x))
}

export function parseInline(s: string): Inline[] {
  const out: Inline[] = []
  let buf = ""
  const flush = () => { if (buf !== "") { out.push(text(buf)); buf = "" } }
  let i = 0
  outer: while (i < s.length) {
    const rest = s.slice(i)
    const img = IMAGE.exec(rest)
    if (img) {
      flush()
      out.push({ kind: "attachment", id: img[1]!, image: true })
      i += img[0].length
      continue
    }
    const link = LINK.exec(rest)
    if (link) {
      flush()
      const label = link[1]!
      const dest = link[2]!
      const att = ATTACH.exec(dest)
      if (att) out.push({ kind: "attachment", id: att[1]!, image: false })
      else out.push(text(label === "" ? dest : label, [], dest))
      i += link[0].length
      continue
    }
    const url = matchUrl(rest)
    if (url) {
      flush()
      out.push(text(url, [], url))
      i += url.length
      continue
    }
    if (rest.startsWith(U_OPEN)) {
      const close = s.indexOf(U_CLOSE, i + U_OPEN.length)
      if (close > i + U_OPEN.length) {
        flush()
        out.push(...withMark(parseInline(s.slice(i + U_OPEN.length, close)), "underline"))
        i = close + U_CLOSE.length
        continue
      }
    }
    for (const [d, mark] of FLANKED) {
      if (!rest.startsWith(d)) continue
      const close = findClose(s, i, d)
      if (close === undefined) continue
      flush()
      out.push(...withMark(parseInline(s.slice(i + d.length, close)), mark))
      i = close + d.length
      continue outer
    }
    buf += s[i]
    i++
  }
  flush()
  return normalizeInlines(out)
}

// 裸のまま描いても、後ろの文字 after と合わせて同じ URL に読み戻せるか
function bareUrlOk(link: string, after: string | undefined): boolean {
  return /^https?:\/\//.test(link) && matchUrl(link + (after ?? "")) === link
}

function renderPlain(i: Inline, after?: string): string {
  if (i.kind === "attachment") return i.image ? `![](.notes-media/${i.id}.jpg)` : `[attachment](notes-attachment:${i.id})`
  if (i.link === undefined) return i.text
  if (i.text === i.link && bareUrlOk(i.link, after)) return i.link
  // ラベルの [] や空白・括弧の崩れたリンク先は [label](dest) で書けないので、文字だけ残す
  if (/[\[\]]/.test(i.text) || !DEST_ONLY.test(i.link)) return i.text
  return `[${i.text}](${i.link})`
}

// 描いたときの先頭の文字 (隣の inline の装飾の判断に使う。装飾の記号までは見ない)
function firstChar(i: Inline | undefined): string | undefined {
  if (!i) return undefined
  if (i.kind === "text" && i.marks.includes("underline")) return "<"
  return renderPlain(i)[0]
}

export function renderInline(xs: Inline[]): string {
  const items = normalizeInlines(xs)
  let out = ""
  items.forEach((i, k) => {
    const after = firstChar(items[k + 1])
    let body = renderPlain(i, after)
    if (i.kind === "text" && i.marks.length > 0) {
      const has = (m: Mark) => i.marks.includes(m)
      // 中身に自分の記号を含む marks は parse が装飾と読まないので付けない
      const italic = has("bold") ? "_" : "*"
      const flank: Array<readonly [Mark, string]> = []
      if (has("bold") && !i.text.includes("*")) flank.push(["bold", "**"])
      if (has("italic") && !i.text.includes(italic)) flank.push(["italic", italic])
      if (has("strike") && !i.text.includes("~")) flank.push(["strike", "~~"])
      const underline = has("underline") && !i.text.includes(U_OPEN) && !i.text.includes(U_CLOSE)
      // <u> の内側なら前後の文字に関わらず読める
      const readable = !isSpace(body[0]!) && !isSpace(body[body.length - 1]!) &&
        (underline || (canPrecede(out[out.length - 1]) && canFollow(after)))
      if (readable) for (const [, d] of [...flank].reverse()) body = d + body + d
      if (underline) body = U_OPEN + body + U_CLOSE
    }
    out += body
  })
  return out
}

// --- 段落 ---

const FENCE = /^```/
const QUOTE = /^>(?: (.*))?$/
const TITLE = /^#(?: (.*))?$/
const HEADING = /^(#+)(?: (.*))?$/
const LIST = /^( *)(?:([-+])|(\d+)[.)])\s+(?:\[( |X|x)\]\s+)?(.*)$/
const OPT_IN = /^notes_sync:\s*yes\s*$/i

function parse(src: string, fallbackTitle: string): Parsed {
  let lines = splitLines(src)
  let preserved: string[] = []
  // 先頭の --- から次の --- までが front matter。閉じが無ければ普通の行
  if (lines[0]?.trimEnd() === "---") {
    const end = lines.findIndex((l, k) => k > 0 && l.trimEnd() === "---")
    if (end > 0) {
      preserved = lines.slice(1, end)
      lines = lines.slice(end + 1)
    }
  }
  const optIn = preserved.some((l) => OPT_IN.test(l))
  let titleInlines: Inline[] | undefined
  const t = lines[0] === undefined ? null : TITLE.exec(lines[0])
  if (t) {
    titleInlines = parseInline(t[1] ?? "")
    lines = lines.slice(1)
  }

  const body: Paragraph[] = []
  // 開いているフェンス。quote はフェンスを開いた行が引用だったか
  let fence: { quote: boolean } | undefined
  for (const line of lines) {
    if (fence) {
      const q = fence.quote ? QUOTE.exec(line) : null
      const l = q ? q[1] ?? "" : line
      if (FENCE.test(l)) { fence = undefined; continue }
      body.push(para({ kind: "mono" }, l === "" ? [] : [text(l)], { quote: fence.quote }))
      continue
    }
    const q = QUOTE.exec(line)
    const quote = q !== null
    const l = q ? q[1] ?? "" : line
    if (FENCE.test(l)) { fence = { quote }; continue } // 言語指定は読み捨てる

    const h = HEADING.exec(l)
    if (h) {
      const n = h[1]!.length
      const inlines = parseInline(h[2] ?? "")
      if (n <= 2) body.push(para({ kind: "heading" }, inlines, { quote }))
      else if (n === 3) body.push(para({ kind: "subheading" }, inlines, { quote }))
      else body.push(para({ kind: "body" }, inlines.map((i) => (i.kind === "text" ? text(i.text, ["bold"], i.link) : i)), { quote }))
      continue
    }
    const li = LIST.exec(l)
    if (li) {
      const indent = Math.floor(li[1]!.length / 2)
      const style: Paragraph["style"] = li[4] !== undefined
        ? { kind: "check", done: li[4] !== " " }
        : li[2] === "-" ? { kind: "list", list: "bullet" }
        : li[2] === "+" ? { kind: "list", list: "dash" }
        : { kind: "list", list: "number" }
      body.push(para(style, parseInline(li[5]!), { indent, quote }))
      continue
    }
    const indent = Math.floor(/^ */.exec(l)![0].length / 2)
    body.push(para({ kind: "body" }, parseInline(l.slice(indent * 2)), { indent, quote }))
  }

  const title = para({ kind: "title" }, titleInlines ?? (fallbackTitle === "" ? [] : [text(fallbackTitle)]))
  return { doc: normalizeDoc({ paragraphs: [title, ...body] }), optIn, preserved }
}

// 中身が空なら記号だけ (行末の空白を残さない)
const marked = (mark: string, s: string) => (s === "" ? mark : `${mark} ${s}`)

function render(doc: { paragraphs: Paragraph[] }, preserved: string[]): string {
  const out: string[] = []
  if (preserved.length > 0) out.push("---", ...preserved, "---")
  // 1 行目は書式に関わらずタイトル (メモ.app は 1 行目をタイトルとみなす)
  const first = doc.paragraphs[0]
  out.push(marked("#", first ? renderInline(first.inlines) : ""))

  // 開いているフェンスの引用の有無 (undefined は閉じている)
  let fenceQuote: boolean | undefined
  // 同じ indent で連続する number 項目の番号。リスト以外の段落で全て戻る
  let counters: number[] = []
  const push = (quote: boolean, s: string) => out.push(quote ? marked(">", s) : s)
  const closeFence = () => { if (fenceQuote !== undefined) { push(fenceQuote, "```"); fenceQuote = undefined } }

  for (const p of doc.paragraphs.slice(1)) {
    if (p.style.kind === "mono") {
      if (fenceQuote !== p.quote) { closeFence(); push(p.quote, "```"); fenceQuote = p.quote }
      push(p.quote, plainText(p)) // 生のまま。装飾は解釈しない
      counters = []
      continue
    }
    closeFence()

    const pad = "  ".repeat(p.indent)
    const body = renderInline(p.inlines)
    const s = p.style
    if (s.kind === "list" || s.kind === "check") {
      counters.length = p.indent + 1
      let marker: string
      if (s.kind === "check") marker = s.done ? "- [x] " : "- [ ] "
      else if (s.list === "bullet") marker = "- "
      else if (s.list === "dash") marker = "+ "
      else {
        counters[p.indent] = (counters[p.indent] ?? 0) + 1
        marker = `${counters[p.indent]}. `
      }
      if (s.kind === "check" || s.list !== "number") counters[p.indent] = 0
      push(p.quote, pad + marker + body)
      continue
    }
    counters = []
    if (s.kind === "title" || s.kind === "heading") push(p.quote, marked("##", body))
    else if (s.kind === "subheading") push(p.quote, marked("###", body))
    else if (!p.inlines.some((i) => i.kind === "attachment") && isBoldLine(p)) {
      push(p.quote, marked("####", renderInline(stripBold(p.inlines))))
    } else {
      push(p.quote, body === "" ? "" : pad + body)
    }
  }
  closeFence()
  return out.join("\n") + "\n"
}

export const markdownFormat: LocalFormat = {
  ext: ".md",
  optInLine: "notes_sync: yes",
  parse,
  render,
  hasConflictMarkers,
}
