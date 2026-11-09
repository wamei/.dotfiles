import { expect, test } from "bun:test"
import { parseIdentities } from "./identities"

test("TOML の [[identity]] を読み、~ をホームにする", () => {
  const ids = parseIdentities(
    `
[[identity]]
id = "me"
dirs = ["~/projects"]
gh = "me"

[[identity]]
id = "client"
dirs = ["~/projects/github.com.client/", "/srv/client"]
git = { name = "client-admin", email = "admin@client.test" }
gh = "client-admin"
ssh = [{ host = "github.com.client", hostname = "github.com", key = "~/.ssh/client" }]
chrome = "client"
`,
    "/Users/u",
  )
  expect(ids).toEqual([
    { id: "me", dirs: ["/Users/u/projects"], gh: "me", ssh: [] },
    {
      id: "client",
      dirs: ["/Users/u/projects/github.com.client", "/srv/client"],
      git: { name: "client-admin", email: "admin@client.test" },
      gh: "client-admin",
      ssh: [{ host: "github.com.client", hostname: "github.com", key: "/Users/u/.ssh/client" }],
      chrome: "client",
    },
  ])
})

test("書き間違いは id を添えてエラーにする", () => {
  expect(() => parseIdentities(`[[identity]]\nid = "x"\n`, "/h")).toThrow(/x: dirs/)
  expect(() => parseIdentities(`[[identity]]\nid = "x"\ndirs = ["/a"]\ngit = { name = "n" }\n`, "/h")).toThrow(/x: git.email/)
  expect(() => parseIdentities(`[[identity]]\nid = "x"\ndirs = ["/a"]\ngithub = "y"\n`, "/h")).toThrow(/x: 知らないキー github/)
  expect(() => parseIdentities(`[[identity]]\nid = "x"\ndirs = ["/a"]\n[[identity]]\nid = "x"\ndirs = ["/b"]\n`, "/h")).toThrow(/x: id が重複/)
  expect(() => parseIdentities(`[[identity]]\nid = "x"\ndirs = ["a"]\n`, "/h")).toThrow(/x: dirs は絶対パスか ~/)
})

test("dirs のワイルドカードはそのまま残す (~ だけ展開する)", () => {
  const ids = parseIdentities(`[[identity]]\nid = "mc"\ndirs = ["~/projects/*/mc*/"]\nchrome = "mc"\n`, "/h")
  expect(ids[0]?.dirs).toEqual(["/h/projects/*/mc*"])
})

test("titles は窓のタイトルに含まれる文字列の配列で、chrome と一緒に書く", () => {
  const ids = parseIdentities(`[[identity]]\nid = "mc"\ndirs = ["/a"]\nchrome = "mc"\ntitles = ["Example Corporation Group"]\n`, "/h")
  expect(ids[0]?.titles).toEqual(["Example Corporation Group"])
  expect(() => parseIdentities(`[[identity]]\nid = "x"\ndirs = ["/a"]\ntitles = ["Org"]\n`, "/h")).toThrow(/x: titles は chrome/)
  expect(() => parseIdentities(`[[identity]]\nid = "x"\ndirs = ["/a"]\nchrome = "x"\ntitles = "Org"\n`, "/h")).toThrow(/x: titles は窓のタイトルに含まれる文字列の配列/)
})

test("emails は主催者のメールアドレス (@ で始めるとドメイン) の配列で、chrome と一緒に書く", () => {
  const ids = parseIdentities(`[[identity]]\nid = "mc"\ndirs = ["/a"]\nchrome = "mc"\nemails = ["@example.com", "Boss@Other.Example"]\n`, "/h")
  // 比べるときに大文字小文字を無視するので、小文字にそろえておく
  expect(ids[0]?.emails).toEqual(["@example.com", "boss@other.example"])
  expect(() => parseIdentities(`[[identity]]\nid = "x"\ndirs = ["/a"]\nemails = ["@example.com"]\n`, "/h")).toThrow(/x: emails は chrome/)
  expect(() => parseIdentities(`[[identity]]\nid = "x"\ndirs = ["/a"]\nchrome = "x"\nemails = ["example.com"]\n`, "/h")).toThrow(/x: emails は/)
  expect(() => parseIdentities(`[[identity]]\nid = "x"\ndirs = ["/a"]\nchrome = "x"\nemails = "@example.com"\n`, "/h")).toThrow(/x: emails は/)
})

test("chrome のリンクの振り分けだけの身元は dirs を省ける。git / gh は dirs が要る", () => {
  const ids = parseIdentities(`[[identity]]\nid = "other"\nchrome = "w"\nemails = ["@*", "@*.example.com"]\n`, "/h")
  expect(ids[0]).toEqual({ id: "other", dirs: [], ssh: [], chrome: "w", emails: ["@*", "@*.example.com"] })
  expect(() => parseIdentities(`[[identity]]\nid = "x"\ngh = "x"\n`, "/h")).toThrow(/x: dirs/)
  expect(() => parseIdentities(`[[identity]]\nid = "x"\nchrome = "x"\n`, "/h")).toThrow(/x: dirs/)
})
