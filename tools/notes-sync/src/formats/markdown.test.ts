import { describe, expect, test } from "bun:test"
import { markdownFormat } from "./markdown"
import { orgFormat } from "./org"
import { docEquals, para, text, type Doc } from "../doc"

const roundTrip = (s: string) => {
  const r = markdownFormat.parse(s, "fallback")
  return markdownFormat.render(r.doc, r.preserved)
}

describe("markdown 段落: 往復で一字一句戻る", () => {
  const cases: Record<string, string> = {
    "front matter とタイトル": "---\nnotes_sync: yes\ntags: [a, b]\n---\n# 買い物\n\n本文\n",
    "front matter 無し": "# t\n本文\n",
    "見出し": "# t\n## 見出し\n### 小見出し\n#### 太字行\n本文\n",
    "リスト": "# t\n- a\n  - b\n+ c\n1. x\n2. y\n  1. z\n- [ ] todo\n- [x] done\n",
    "フェンス": "# t\n```\n  const a = 1\n*not bold*\n\n```\n後\n",
    "quote": "# t\n> 引用\n>\n> - 引用内リスト\n> ## 見出し\n後\n",
    "quote 内のフェンス": "# t\n> ```\n> x\n>\n> ```\n",
    "インデント": "# t\n  一段\n    二段\n",
    "添付": "# t\n[attachment](notes-attachment:A10B17CA)\n![](.notes-media/IMG-1.jpg)\n",
    "行内": "# t\n**b** *i* <u>u</u> ~~s~~ [ぐーぐる](https://g.co) https://a.b/c\n",
    "空のタイトル": "#\n本文\n",
  }
  for (const [name, s] of Object.entries(cases)) test(name, () => expect(roundTrip(s)).toBe(s))
})

describe("markdown 段落: parse の結果", () => {
  test("タイトルが無ければ fallback、front matter の opt-in を読む", () => {
    const r = markdownFormat.parse("---\nNOTES_SYNC: Yes\n---\n本文\n", "memo")
    expect(r.optIn).toBe(true)
    expect(r.preserved).toEqual(["NOTES_SYNC: Yes"])
    expect(r.doc.paragraphs).toEqual([para({ kind: "title" }, [text("memo")]), para({ kind: "body" }, [text("本文")])])
  })
  test("opt-in していないファイル", () => {
    expect(markdownFormat.parse("# x\n", "x").optIn).toBe(false)
    expect(markdownFormat.parse("---\nnotes_sync: no\n---\n# x\n", "x").optIn).toBe(false)
  })
  test("front matter の外の notes_sync: yes は opt-in ではない", () => {
    const r = markdownFormat.parse("# x\nnotes_sync: yes\n", "x")
    expect(r.optIn).toBe(false)
    expect(r.preserved).toEqual([])
  })
  test("閉じの無い --- は front matter ではない", () => {
    const r = markdownFormat.parse("---\nnotes_sync: yes\n", "f")
    expect(r.optIn).toBe(false)
    expect(r.doc.paragraphs.map((p) => p.style.kind)).toEqual(["title", "body", "body"])
  })
  test("タイトル行の後の # も見出し", () => {
    const r = markdownFormat.parse("# t\n# 二つ目\n", "t")
    expect(r.doc.paragraphs[1]).toEqual(para({ kind: "heading" }, [text("二つ目")]))
  })
  test("#### 以上は太字の body (描くと #### に揃う)", () => {
    const r = markdownFormat.parse("# t\n##### 深い\n", "t")
    expect(r.doc.paragraphs[1]).toEqual(para({ kind: "body" }, [text("深い", ["bold"])]))
    expect(markdownFormat.render(r.doc, [])).toBe("# t\n#### 深い\n")
  })
  test("# の後に空白が無ければ見出しではない (ハッシュタグ)", () => {
    const r = markdownFormat.parse("# t\n#tag\n", "t")
    expect(r.doc.paragraphs[1]).toEqual(para({ kind: "body" }, [text("#tag")]))
  })
  test("check と indent と quote、1) と X", () => {
    const r = markdownFormat.parse("# t\n>   - [X] 済\n3) n\n", "t")
    expect(r.doc.paragraphs[1]).toEqual(para({ kind: "check", done: true }, [text("済")], { indent: 1, quote: true }))
    expect(r.doc.paragraphs[2]).toEqual(para({ kind: "list", list: "number" }, [text("n")]))
  })
  test("フェンスの言語指定は読み捨てる、中身は装飾を解釈しない", () => {
    const r = markdownFormat.parse("# t\n```ts\n**a** - b\n```\n", "t")
    expect(r.doc.paragraphs[1]).toEqual(para({ kind: "mono" }, [text("**a** - b")]))
    expect(markdownFormat.render(r.doc, [])).toBe("# t\n```\n**a** - b\n```\n")
  })
  test("閉じていないフェンスは末尾まで mono", () => {
    const r = markdownFormat.parse("# t\n```\na\n", "t")
    expect(r.doc.paragraphs[1]).toEqual(para({ kind: "mono" }, [text("a")]))
  })
  test("> の後に空白の無い行は引用ではない", () => {
    const r = markdownFormat.parse("# t\n>_<\n", "t")
    expect(r.doc.paragraphs[1]).toEqual(para({ kind: "body" }, [text(">_<")]))
  })
  test("本文中のタイトルスタイルは見出しとして描く", () => {
    const d: Doc = { paragraphs: [para({ kind: "title" }, [text("t")]), para({ kind: "title" }, [text("途中")])] }
    expect(markdownFormat.render(d, [])).toBe("# t\n## 途中\n")
  })
  test("quote が切り替わるとフェンスを閉じる", () => {
    const d: Doc = { paragraphs: [
      para({ kind: "title" }, [text("t")]),
      para({ kind: "mono" }, [text("a")], { quote: true }),
      para({ kind: "mono" }, [text("b")]),
    ] }
    expect(markdownFormat.render(d, [])).toBe("# t\n> ```\n> a\n> ```\n```\nb\n```\n")
  })
  test("任意の Doc で parse(render(d)) が d に戻る (表現可能な範囲)", () => {
    const d: Doc = { paragraphs: [
      para({ kind: "title" }, [text("t")]),
      para({ kind: "heading" }, [text("h")]),
      para({ kind: "subheading" }, [text("s", ["italic"])]),
      para({ kind: "mono" }, [text("  x = 1")]),
      para({ kind: "mono" }, []),
      para({ kind: "list", list: "number" }, [text("n")], { indent: 1 }),
      para({ kind: "list", list: "dash" }, [text("d")], { quote: true }),
      para({ kind: "check", done: false }, [text("c", ["strike"])], { indent: 2 }),
      para({ kind: "body" }, [], { quote: true }),
      para({ kind: "body" }, [text("強調", ["bold"])]),
      para({ kind: "body" }, [text("x", ["bold"]), { kind: "attachment", id: "IMG", image: true }]),
      para({ kind: "body" }, [{ kind: "attachment", id: "T", image: false }]),
      para({ kind: "body" }, [text("a "), text("bi", ["bold", "italic"]), text(" "), text("all", ["bold", "italic", "underline", "strike"])], { indent: 1 }),
    ] }
    expect(docEquals(markdownFormat.parse(markdownFormat.render(d, []), "t").doc, d)).toBe(true)
  })
})

describe("markdown 段落: render", () => {
  test("preserved が空なら front matter を出さない、あれば先頭に挟んで出す", () => {
    const d: Doc = { paragraphs: [para({ kind: "title" }, [text("t")]), para({ kind: "body" }, [text("y")])] }
    expect(markdownFormat.render(d, [])).toBe("# t\ny\n")
    expect(markdownFormat.render(d, [markdownFormat.optInLine])).toBe("---\nnotes_sync: yes\n---\n# t\ny\n")
  })
  test("1 行目は body でもタイトルとして描く、空の Doc でも # を出す", () => {
    const d: Doc = { paragraphs: [para({ kind: "body" }, [text("X")]), para({ kind: "body" }, [text("y")])] }
    expect(markdownFormat.render(d, ["a: b"])).toBe("---\na: b\n---\n# X\ny\n")
    expect(markdownFormat.render({ paragraphs: [] }, [])).toBe("#\n")
  })
  test("太字に添付を含む行は #### にしない", () => {
    const d: Doc = { paragraphs: [para({ kind: "title" }, [text("t")]), para({ kind: "body" }, [text("x", ["bold"]), { kind: "attachment", id: "I", image: true }])] }
    expect(markdownFormat.render(d, [])).toBe("# t\n**x**![](.notes-media/I.jpg)\n")
  })
  test("mono は生の文字列で描く", () => {
    const d: Doc = { paragraphs: [para({ kind: "title" }, [text("t")]), para({ kind: "mono" }, [text("*a*", ["bold"])])] }
    expect(markdownFormat.render(d, [])).toBe("# t\n```\n*a*\n```\n")
  })
  test("CRLF は LF と同じ doc になる", () => {
    const a = markdownFormat.parse("---\r\nnotes_sync: yes\r\n---\r\n# t\r\n## h\r\n本文\r\n", "f")
    const b = markdownFormat.parse("---\nnotes_sync: yes\n---\n# t\n## h\n本文\n", "f")
    expect(docEquals(a.doc, b.doc)).toBe(true)
    expect(a.optIn).toBe(true)
  })
  test("タブと全角スペースの先頭はそのまま往復する", () => {
    for (const s of ["# t\n\tfoo\n", "# t\n　段落\n"]) expect(roundTrip(s)).toBe(s)
  })
})

test("opt-in 行", () => {
  expect(markdownFormat.optInLine).toBe("notes_sync: yes")
  expect(orgFormat.optInLine).toBe("#+notes_sync: yes")
  expect(markdownFormat.parse(markdownFormat.render({ paragraphs: [] }, [markdownFormat.optInLine]), "x").optIn).toBe(true)
})

test("衝突マーカー (org と同じ判定)", () => {
  expect(markdownFormat.hasConflictMarkers("a\n<<<<<<< local\nb\n=======\nc\n>>>>>>> notes\n")).toBe(true)
  expect(markdownFormat.hasConflictMarkers("見出し\n=======\n本文\n")).toBe(false)
})
