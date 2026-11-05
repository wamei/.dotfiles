import { expect, test } from "bun:test"
import type { Identity } from "./identities"
import { applyChromeDirs, renderGhUsersByDir, renderGitconfigLocal, renderGitIdentity, renderSshGithub } from "./render"

const me: Identity = { id: "me", dirs: ["/Users/u/projects"], gh: "me", ssh: [{ host: "github.com", hostname: "github.com", key: "/Users/u/.ssh/id_ed25519" }] }
const client: Identity = {
  id: "client",
  dirs: ["/Users/u/projects/github.com.client"],
  git: { name: "client-admin", email: "admin@client.test" },
  gh: "client-admin",
  ssh: [{ host: "github.com.client", hostname: "github.com", key: "/Users/u/.ssh/client" }],
  chrome: "client",
}
const wide: Identity = { id: "wide", dirs: ["/Users/u/work"], git: { name: "w", email: "w@x.test" }, ssh: [] }
const gen = "/Users/u/.config/local-config"

test("gitconfig.local は git を持つ身元のディレクトリごとに includeIf を並べる", () => {
  expect(renderGitconfigLocal([me, client], gen)).toBe(
    [
      "# local-config が生成する。手で編集しない (.dotfiles-local の identities.toml を直して local-config apply)",
      "",
      '[includeIf "gitdir:/Users/u/projects/github.com.client/"]',
      "\tpath = /Users/u/.config/local-config/git/client.gitconfig",
      "",
    ].join("\n"),
  )
})

test("includeIf は浅いディレクトリから並べて、深いほうが後から勝つ", () => {
  const deep: Identity = { ...client, dirs: ["/Users/u/work/client"] }
  const out = renderGitconfigLocal([deep, wide], gen)
  expect(out.indexOf("gitdir:/Users/u/work/\"")).toBeLessThan(out.indexOf("gitdir:/Users/u/work/client/"))
})

test("身元ごとの gitconfig は user を書く", () => {
  expect(renderGitIdentity(client)).toBe(
    [
      "# local-config が生成する。手で編集しない (.dotfiles-local の identities.toml を直して local-config apply)",
      "[user]",
      "\tname = client-admin",
      "\temail = admin@client.test",
      "",
    ].join("\n"),
  )
})

test("gh の対応表は gh を持つ身元のディレクトリを 1 行ずつ書く", () => {
  expect(renderGhUsersByDir([me, client, wide])).toBe(
    [
      "# local-config が生成する。手で編集しない (.dotfiles-local の identities.toml を直して local-config apply)",
      "# dotfiles の tools/gh-account/gh が読む。cwd を含むもののうち最も深いものが勝つ",
      "/Users/u/projects  me",
      "/Users/u/projects/github.com.client  client-admin",
      "",
    ].join("\n"),
  )
})

test("ssh は GitHub の別名ホストをまとめて 443 番に逃がす Match と、Host ブロックを書く", () => {
  expect(renderSshGithub([me, client])).toBe(
    [
      "# local-config が生成する。手で編集しない (.dotfiles-local の identities.toml を直して local-config apply)",
      "",
      "# 22 番で github.com に届かない (オフィスの規制など) ときは 443 番 (ssh.github.com) に向ける。",
      "# Host ブロックより先に置くこと (先に決まった値が勝つので Port 22 を上書きできる)。",
      'Match originalhost github.com,github.com.client exec "! nc -z -G 1 github.com 22 >/dev/null 2>&1"',
      "  HostName ssh.github.com",
      "  Port 443",
      "",
      "Host github.com",
      "  HostName github.com",
      "  Port 22",
      "  User git",
      "  AddKeysToAgent yes",
      "  UseKeychain yes",
      "  IdentityFile /Users/u/.ssh/id_ed25519",
      "  TCPKeepAlive yes",
      "  IdentitiesOnly yes",
      "",
      "Host github.com.client",
      "  HostName github.com",
      "  Port 22",
      "  User git",
      "  AddKeysToAgent yes",
      "  UseKeychain yes",
      "  IdentityFile /Users/u/.ssh/client",
      "  TCPKeepAlive yes",
      "  IdentitiesOnly yes",
      "",
    ].join("\n"),
  )
})

test("GitHub 以外のホストは Match に入れない", () => {
  const bb: Identity = { id: "bb", dirs: ["/a"], ssh: [{ host: "bitbucket.org", hostname: "bitbucket.org", key: "/k" }] }
  const out = renderSshGithub([me, bb])
  expect(out).toContain("Match originalhost github.com exec")
  expect(out).toContain("Host bitbucket.org\n  HostName bitbucket.org\n  Port 22")
})

test("chrome-profiles の設定は chrome を指す身元の dirs で置き換え、指されないプロファイルの dirs は消す", () => {
  const config = {
    profiles: [
      { id: "work", name: "Work", profileDir: "Default", dirs: ["/old"] },
      { id: "client", name: "Client", profileDir: "Profile 2", picture: "~/p.png" },
    ],
  }
  const { config: out, missing } = applyChromeDirs(config, [me, client, { ...wide, chrome: "nope" }])
  expect(out).toEqual({
    profiles: [
      { id: "work", name: "Work", profileDir: "Default" },
      { id: "client", name: "Client", profileDir: "Profile 2", picture: "~/p.png", dirs: ["/Users/u/projects/github.com.client"] },
    ],
  })
  // このマシンの Chrome に無いプロファイルは知らせる
  expect(missing).toEqual(["nope"])
  // 元の設定は書き換えない
  expect(config.profiles[0]).toHaveProperty("dirs")
})

test("chrome-profiles の設定の titles も chrome を指す身元から差し替える", () => {
  const config = { profiles: [{ id: "client", name: "Client", profileDir: "Profile 2", titles: ["Old Org"] }, { id: "work", name: "Work", profileDir: "Default", titles: ["Gone"] }] }
  const { config: out } = applyChromeDirs(config, [{ ...client, titles: ["Client Corporation"] }])
  expect(out.profiles).toEqual([
    { id: "client", name: "Client", profileDir: "Profile 2", dirs: ["/Users/u/projects/github.com.client"], titles: ["Client Corporation"] },
    { id: "work", name: "Work", profileDir: "Default" },
  ])
})
