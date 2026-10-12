import { describe, expect, test } from "bun:test"
import { orgFormat } from "./org"
import { docEquals, para, text, type Doc } from "../doc"

const roundTrip = (s: string) => orgFormat.render(orgFormat.parse(s, "fallback").doc, orgFormat.parse(s, "fallback").preserved)

describe("org 段落: 往復で一字一句戻る", () => {
  const cases: Record<string, string> = {
    "タイトルと制御行": "#+title: 買い物\n#+notes_sync: yes\n\n本文\n",
    "見出し": "#+title: t\n* 見出し\n** 小見出し\n*** 太字行\n本文\n",
    "リスト": "#+title: t\n- a\n  - b\n+ c\n1. x\n2. y\n  1. z\n- [ ] todo\n- [X] done\n",
    "example": "#+title: t\n#+begin_example\n  const a = 1\n*not bold*\n#+end_example\n",
    "quote": "#+title: t\n#+begin_quote\n引用\n- 引用内リスト\n#+end_quote\n後\n",
    "インデント": "#+title: t\n  一段\n    二段\n",
    "添付": "#+title: t\n#+notes_attachment: A10B17CA\n",
  }
  for (const [name, s] of Object.entries(cases)) test(name, () => expect(roundTrip(s)).toBe(s))
})

describe("org 段落: parse の結果", () => {
  test("タイトルが無ければ fallback、opt-in を読む", () => {
    const r = orgFormat.parse("#+NOTES_SYNC: Yes\n本文\n", "memo")
    expect(r.optIn).toBe(true)
    expect(r.preserved).toEqual(["#+NOTES_SYNC: Yes"])
    expect(r.doc.paragraphs[0]).toEqual(para({ kind: "title" }, [text("memo")]))
  })
  test("opt-in していないファイル", () => {
    expect(orgFormat.parse("#+title: x\n", "x").optIn).toBe(false)
  })
  test("*** は太字の body", () => {
    const r = orgFormat.parse("#+title: t\n*** 強調\n", "t")
    expect(r.doc.paragraphs[1]).toEqual(para({ kind: "body" }, [text("強調", ["bold"])]))
  })
  test("**** も太字の body (描くと *** に揃う)", () => {
    const r = orgFormat.parse("#+title: t\n**** 深い\n", "t")
    expect(orgFormat.render(r.doc, [])).toBe("#+title: t\n*** 深い\n")
  })
  test("check と indent と quote", () => {
    const r = orgFormat.parse("#+title: t\n#+begin_quote\n  - [X] 済\n#+end_quote\n", "t")
    expect(r.doc.paragraphs[1]).toEqual(para({ kind: "check", done: true }, [text("済")], { indent: 1, quote: true }))
  })
  test("本文中のタイトルスタイルは見出しとして描く", () => {
    const d: Doc = { paragraphs: [para({ kind: "title" }, [text("t")]), para({ kind: "title" }, [text("途中")])] }
    expect(orgFormat.render(d, [])).toBe("#+title: t\n* 途中\n")
  })
  test("任意の Doc で parse(render(d)) が d に戻る (表現可能な範囲)", () => {
    const d: Doc = { paragraphs: [
      para({ kind: "title" }, [text("t")]),
      para({ kind: "heading" }, [text("h")]),
      para({ kind: "mono" }, [text("  x = 1")]),
      para({ kind: "list", list: "number" }, [text("n")], { indent: 1 }),
      para({ kind: "body" }, [], { quote: true }),
      para({ kind: "body" }, [{ kind: "attachment", id: "T", image: false }]),
    ] }
    expect(docEquals(orgFormat.parse(orgFormat.render(d, []), "t").doc, d)).toBe(true)
  })
})

test("衝突マーカー", () => {
  expect(orgFormat.hasConflictMarkers("a\n<<<<<<< org\nb\n=======\nc\n>>>>>>> notes\n")).toBe(true)
  expect(orgFormat.hasConflictMarkers("a <<<<<<< b\n")).toBe(false)
})
