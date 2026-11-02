// 一時 HOME で CLI を通しで動かす
import { expect, test } from "bun:test"
import fs from "node:fs"
import os from "node:os"
import path from "node:path"

const cli = path.join(import.meta.dir, "local-config.ts")

const TOML = `
[[identity]]
id = "me"
dirs = ["~/projects"]
gh = "me"
ssh = [{ host = "github.com", hostname = "github.com", key = "~/.ssh/id_ed25519" }]

[[identity]]
id = "client"
dirs = ["~/projects/github.com.client"]
git = { name = "client-admin", email = "admin@client.test" }
gh = "client-admin"
chrome = "client"
`

function setup(): { home: string; src: string } {
  const home = fs.mkdtempSync(path.join(os.tmpdir(), "local-config-"))
  const src = path.join(home, ".dotfiles-local")
  fs.mkdirSync(src)
  fs.writeFileSync(path.join(src, "identities.toml"), TOML)
  return { home, src }
}

const run = (home: string, src: string, ...args: string[]) =>
  Bun.spawnSync(["bun", cli, ...args], { env: { ...process.env, HOME: home, LOCAL_CONFIG_DIR: src } })

const read = (home: string, rel: string) => fs.readFileSync(path.join(home, rel), "utf8")

test("apply は各ツールの設定を書き、2 回目は何も変えない", () => {
  const { home, src } = setup()
  fs.mkdirSync(path.join(home, ".config/chrome-profiles"), { recursive: true })
  fs.writeFileSync(path.join(home, ".config/chrome-profiles/config.json"), JSON.stringify({ profiles: [{ id: "client", name: "Client", profileDir: "Profile 2" }] }))

  const r = run(home, src, "apply")
  expect(r.exitCode).toBe(0)
  expect(read(home, ".gitconfig.local")).toContain(`gitdir:${home}/projects/github.com.client/`)
  expect(read(home, ".config/local-config/git/client.gitconfig")).toContain("name = client-admin")
  expect(read(home, ".config/gh/users-by-dir")).toContain(`${home}/projects/github.com.client  client-admin`)
  expect(read(home, ".config/local-config/ssh/github")).toContain("Host github.com\n")
  expect(JSON.parse(read(home, ".config/chrome-profiles/config.json")).profiles[0].dirs).toEqual([`${home}/projects/github.com.client`])

  const again = run(home, src, "apply")
  expect(again.stdout.toString()).not.toContain("書きました")
})

test("手で書いたファイルは .pre-local-config に退避してから上書きする", () => {
  const { home, src } = setup()
  fs.writeFileSync(path.join(home, ".gitconfig.local"), "# hand written\n")
  expect(run(home, src, "apply").exitCode).toBe(0)
  expect(read(home, ".gitconfig.local.pre-local-config")).toBe("# hand written\n")
  // 生成したものを上書きするときは退避しない
  fs.rmSync(path.join(home, ".gitconfig.local.pre-local-config"))
  fs.writeFileSync(path.join(src, "identities.toml"), TOML.replace("github.com.client", "github.com.client2"))
  const r = run(home, src, "apply")
  expect(r.stdout.toString()).toContain("書きました: ~/.gitconfig.local")
  expect(fs.existsSync(path.join(home, ".gitconfig.local.pre-local-config"))).toBe(false)
})

test("消えた身元の gitconfig は消す", () => {
  const { home, src } = setup()
  run(home, src, "apply")
  fs.writeFileSync(path.join(src, "identities.toml"), TOML.replace(/git = \{[^}]*\}\n/, ""))
  run(home, src, "apply")
  expect(fs.existsSync(path.join(home, ".config/local-config/git/client.gitconfig"))).toBe(false)
})

test("--dry-run は書かずに、変わるファイルを並べる", () => {
  const { home, src } = setup()
  const r = run(home, src, "apply", "--dry-run")
  expect(r.exitCode).toBe(0)
  expect(r.stdout.toString()).toContain(".gitconfig.local")
  expect(fs.existsSync(path.join(home, ".gitconfig.local"))).toBe(false)
})

test("identities.toml が無ければ失敗する", () => {
  const { home } = setup()
  const r = run(home, path.join(home, "nowhere"), "apply")
  expect(r.exitCode).not.toBe(0)
  expect(r.stderr.toString()).toContain("identities.toml")
})
