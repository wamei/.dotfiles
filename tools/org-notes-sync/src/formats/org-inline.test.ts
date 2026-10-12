import { describe, expect, test } from "bun:test"
import { parseInline, renderInline } from "./org"
import { text } from "../doc"

describe("parseInline", () => {
  test("装飾", () => {
    expect(parseInline("a *b* /c/ _d_ +e+")).toEqual([
      text("a "), text("b", ["bold"]), text(" "), text("c", ["italic"]), text(" "),
      text("d", ["underline"]), text(" "), text("e", ["strike"]),
    ])
  })
  test("日本語の中でも装飾を認める", () => {
    expect(parseInline("これは*太字*です")).toEqual([text("これは"), text("太字", ["bold"]), text("です")])
  })
  test("入れ子", () => {
    expect(parseInline("*/x/*")).toEqual([text("x", ["bold", "italic"])])
  })
  test("リンクと裸 URL", () => {
    expect(parseInline("[[https://g.co][ぐーぐる]] https://a.b/c")).toEqual([
      text("ぐーぐる", [], "https://g.co"), text(" "), text("https://a.b/c", [], "https://a.b/c"),
    ])
  })
  test("ラベルの無いリンク", () => {
    expect(parseInline("[[https://g.co]]")).toEqual([text("https://g.co", [], "https://g.co")])
  })
  test("添付", () => {
    expect(parseInline("[[file:.notes-media/AB-12.jpg]]")).toEqual([{ kind: "attachment", id: "AB-12", image: true }])
    expect(parseInline("[[notes-attachment:T1]]")).toEqual([{ kind: "attachment", id: "T1", image: false }])
  })
  test.each([
    "sslcert=../cert/Combined.crt.pem",
    "foo_bar_baz",
    "a + b + c",
    "2 * 3 * 4",
    "C++ と a/b/c",
    "* 行頭の星 *",
  ])("普通の文章は装飾にしない: %s", (s) => {
    expect(parseInline(s)).toEqual([text(s)])
  })
})

describe("renderInline", () => {
  test("往復", () => {
    for (const s of [
      "a *b* /c/ _d_ +e+", "これは*太字*です", "[[https://g.co][ぐーぐる]]", "https://a.b/c", "*/x/*",
      "[[file:.notes-media/AB-12.jpg]]", "[[notes-attachment:T1]]",
    ]) {
      expect(renderInline(parseInline(s))).toBe(s)
    }
  })
  test("装飾として読めない位置では marks を落とす", () => {
    expect(renderInline([text("ab"), text("cd", ["bold"]), text("ef")])).toBe("abcdef")
    expect(renderInline([text(" x ", ["bold"])])).toBe(" x ")
  })
  test("添付の直前では marks を落とす", () => {
    expect(renderInline([text("a", ["bold"]), { kind: "attachment", id: "X", image: true }])).toBe("a[[file:.notes-media/X.jpg]]")
  })
})
