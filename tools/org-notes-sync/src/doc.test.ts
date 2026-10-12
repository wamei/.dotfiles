import { describe, expect, test } from "bun:test"
import { docEquals, docTitle, imageIds, normalizeDoc, para, plainText, text } from "./doc"

describe("normalizeDoc", () => {
  test("同じ装飾の隣接テキストをまとめ、空テキストを捨て、marks を MARKS 順に並べる", () => {
    const d = normalizeDoc({ paragraphs: [para({ kind: "body" }, [
      text("a", ["italic", "bold"]), text("", ["bold"]), text("b", ["bold", "italic"]), text("c"),
    ])] })
    expect(d.paragraphs[0]!.inlines).toEqual([text("ab", ["bold", "italic"]), text("c", [])])
  })
  test("link が違えばまとめない", () => {
    const d = normalizeDoc({ paragraphs: [para({ kind: "body" }, [text("a", [], "https://x"), text("b")])] })
    expect(d.paragraphs[0]!.inlines.length).toBe(2)
  })
})

describe("docEquals", () => {
  test("正規化後の構造で比べる", () => {
    const a = { paragraphs: [para({ kind: "body" }, [text("a"), text("b")])] }
    const b = { paragraphs: [para({ kind: "body" }, [text("ab")])] }
    expect(docEquals(a, b)).toBe(true)
  })
  test("indent / quote / style の違いを区別する", () => {
    const a = { paragraphs: [para({ kind: "body" }, [text("a")])] }
    expect(docEquals(a, { paragraphs: [para({ kind: "body" }, [text("a")], { indent: 1 })] })).toBe(false)
    expect(docEquals(a, { paragraphs: [para({ kind: "body" }, [text("a")], { quote: true })] })).toBe(false)
    expect(docEquals(a, { paragraphs: [para({ kind: "check", done: false }, [text("a")])] })).toBe(false)
  })
})

test("plainText / docTitle / imageIds", () => {
  const d = { paragraphs: [
    para({ kind: "title" }, [text("題"), text("名", ["bold"])]),
    para({ kind: "body" }, [{ kind: "attachment", id: "IMG", image: true }, { kind: "attachment", id: "TBL", image: false }]),
  ] }
  expect(docTitle(d)).toBe("題名")
  expect(plainText(d.paragraphs[1]!)).toBe("")
  expect(imageIds(d)).toEqual(["IMG"])
  expect(docTitle({ paragraphs: [] })).toBe("")
})
