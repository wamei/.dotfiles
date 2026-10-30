import { describe, expect, test } from "bun:test"
import { parseInline, renderInline } from "./markdown"
import { text } from "../doc"

describe("parseInline", () => {
  test("装飾", () => {
    expect(parseInline("a **b** *c* _c2_ <u>d</u> ~~e~~")).toEqual([
      text("a "), text("b", ["bold"]), text(" "), text("c", ["italic"]), text(" "), text("c2", ["italic"]), text(" "),
      text("d", ["underline"]), text(" "), text("e", ["strike"]),
    ])
  })
  test("日本語の中でも装飾を認める", () => {
    expect(parseInline("これは**太字**です")).toEqual([text("これは"), text("太字", ["bold"]), text("です")])
  })
  test("入れ子", () => {
    expect(parseInline("**_x_**")).toEqual([text("x", ["bold", "italic"])])
    expect(parseInline("<u>~~x~~</u>")).toEqual([text("x", ["underline", "strike"])])
  })
  test("<u> は単語の途中でも認める", () => {
    expect(parseInline("ab<u>cd</u>ef")).toEqual([text("ab"), text("cd", ["underline"]), text("ef")])
  })
  test("リンクと裸 URL", () => {
    expect(parseInline("[ぐーぐる](https://g.co) https://a.b/c")).toEqual([
      text("ぐーぐる", [], "https://g.co"), text(" "), text("https://a.b/c", [], "https://a.b/c"),
    ])
  })
  test("ラベルが空のリンクは URL をラベルにする", () => {
    expect(parseInline("[](https://g.co)")).toEqual([text("https://g.co", [], "https://g.co")])
  })
  test("URL の括弧", () => {
    expect(parseInline("(see https://a.b/c)")).toEqual([text("(see "), text("https://a.b/c", [], "https://a.b/c"), text(")")])
    expect(parseInline("https://w.org/Foo_(bar)")).toEqual([text("https://w.org/Foo_(bar)", [], "https://w.org/Foo_(bar)")])
    expect(parseInline("[w](https://w.org/Foo_(bar))")).toEqual([text("w", [], "https://w.org/Foo_(bar)")])
  })
  test("URL の中の _ や * は装飾にしない", () => {
    expect(parseInline("https://a.b/_x_/y z")).toEqual([text("https://a.b/_x_/y", [], "https://a.b/_x_/y"), text(" z")])
    // 末尾の記号は URL に含めない (_https://a.b_ の斜体や文末の句点)
    expect(parseInline("_https://a.b_.")).toEqual([text("https://a.b", ["italic"], "https://a.b"), text(".")])
  })
  test("複数の装飾を含む行 (閉じの後ろが句読点でも全て認める)", () => {
    expect(parseInline("**a**, **b**, *c*")).toEqual([
      text("a", ["bold"]), text(", "), text("b", ["bold"]), text(", "), text("c", ["italic"]),
    ])
    expect(parseInline("*a* and *b*.")).toEqual([text("a", ["italic"]), text(" and "), text("b", ["italic"]), text(".")])
  })
  test("添付", () => {
    expect(parseInline("![](.notes-media/AB-12.jpg)")).toEqual([{ kind: "attachment", id: "AB-12", image: true }])
    expect(parseInline("[attachment](notes-attachment:T1)")).toEqual([{ kind: "attachment", id: "T1", image: false }])
  })
  test.each([
    "sslcert=../cert/Combined.crt.pem",
    "foo_bar_baz",
    "_private and _x_y",
    "a + b + c",
    "2 * 3 * 4",
    "2*3*4",
    "a ** b ** c",
    "C++ と a/b/c",
    "* 行頭の星 *",
    "~/.config/ と ~/x~",
    "see /etc/nginx/ for cfg",
    "__init__.py",
    "a < b > c",
    "[x] と [y]",
  ])("普通の文章は装飾にしない: %s", (s) => {
    expect(parseInline(s)).toEqual([text(s)])
  })
})

describe("renderInline", () => {
  test("往復", () => {
    for (const s of [
      "a **b** *c* <u>d</u> ~~e~~", "これは**太字**です", "[ぐーぐる](https://g.co)", "https://a.b/c", "**_x_**",
      "![](.notes-media/AB-12.jpg)", "[attachment](notes-attachment:T1)", "[./notes.md](./notes.md)", "ab<u>cd</u>ef",
    ]) {
      expect(renderInline(parseInline(s))).toBe(s)
    }
  })
  test("italic は単独なら *、bold と重なるときは _", () => {
    expect(renderInline([text("x", ["italic"])])).toBe("*x*")
    expect(renderInline([text("x", ["bold", "italic"])])).toBe("**_x_**")
  })
  test("装飾として読めない位置では */~ の marks を落とす (<u> は残す)", () => {
    expect(renderInline([text("ab"), text("cd", ["bold"]), text("ef")])).toBe("abcdef")
    expect(renderInline([text(" x ", ["bold"])])).toBe(" x ")
    // <u> の内側なら前後に関わらず読めるので残す
    expect(renderInline([text("ab"), text("cd", ["bold", "underline"]), text("ef")])).toBe("ab<u>**cd**</u>ef")
  })
  test("中身に自分の記号を含む marks は落とす", () => {
    expect(renderInline([text("a "), text("2*3", ["italic"]), text(" b")])).toBe("a 2*3 b")
    expect(renderInline([text("a "), text("x~~y", ["strike"]), text(" b")])).toBe("a x~~y b")
    expect(renderInline([text("a "), text("x</u>", ["underline"])])).toBe("a x</u>")
  })
  test("添付の直前でも読める位置なら marks を付ける", () => {
    const xs = [text("a", ["bold"]), { kind: "attachment", id: "X", image: true } as const]
    expect(renderInline(xs)).toBe("**a**![](.notes-media/X.jpg)")
    expect(parseInline(renderInline(xs))).toEqual(xs)
    expect(renderInline([text("a", ["bold"]), { kind: "attachment", id: "X", image: false }])).toBe("a[attachment](notes-attachment:X)")
  })
  test("後ろに文字が続く URL はラベル付きで描く", () => {
    expect(renderInline([text("https://a.b", [], "https://a.b"), text("x")])).toBe("[https://a.b](https://a.b)x")
  })
  test("ラベルの [] や URL の空白はそのまま描けないので文字だけ残す", () => {
    expect(parseInline(renderInline([text("a]b", [], "https://g.co")]))).toEqual([text("a]b")])
  })
})
