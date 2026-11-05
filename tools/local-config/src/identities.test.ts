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
