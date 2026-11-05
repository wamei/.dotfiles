// identities から各ツールの設定を組み立てる。ファイルの読み書きは cli 側
import type { Identity } from "./identities"

const HEADER = "# local-config が生成する。手で編集しない (.dotfiles-local の identities.toml を直して local-config apply)"

const depth = (dir: string): number => dir.split("/").length

// git は後に読んだ includeIf が勝つので、浅いディレクトリから並べて深いほうを優先させる
export function renderGitconfigLocal(ids: Identity[], genDir: string): string {
  const entries = ids
    .filter((i) => i.git)
    .flatMap((i) => i.dirs.map((dir) => ({ dir, id: i.id })))
    .sort((a, b) => depth(a.dir) - depth(b.dir))
  return [
    HEADER,
    "",
    ...entries.flatMap(({ dir, id }) => [`[includeIf "gitdir:${dir}/"]`, `\tpath = ${genDir}/git/${id}.gitconfig`, ""]),
  ].join("\n")
}

export function renderGitIdentity(i: Identity): string {
  if (!i.git) throw new Error(`${i.id}: git がありません`)
  return [HEADER, "[user]", `\tname = ${i.git.name}`, `\temail = ${i.git.email}`, ""].join("\n")
}

export function renderGhUsersByDir(ids: Identity[]): string {
  return [
    HEADER,
    "# dotfiles の tools/gh-account/gh が読む。cwd を含むもののうち最も深いものが勝つ",
    ...ids.filter((i) => i.gh).flatMap((i) => i.dirs.map((dir) => `${dir}  ${i.gh}`)),
    "",
  ].join("\n")
}

export function renderSshGithub(ids: Identity[]): string {
  const hosts = ids.flatMap((i) => i.ssh)
  const github = hosts.filter((h) => h.hostname === "github.com").map((h) => h.host)
  const match = github.length
    ? [
        "# 22 番で github.com に届かない (オフィスの規制など) ときは 443 番 (ssh.github.com) に向ける。",
        "# Host ブロックより先に置くこと (先に決まった値が勝つので Port 22 を上書きできる)。",
        `Match originalhost ${github.join(",")} exec "! nc -z -G 1 github.com 22 >/dev/null 2>&1"`,
        "  HostName ssh.github.com",
        "  Port 443",
        "",
      ]
    : []
  const blocks = hosts.flatMap((h) => [
    `Host ${h.host}`,
    `  HostName ${h.hostname}`,
    "  Port 22",
    "  User git",
    "  AddKeysToAgent yes",
    "  UseKeychain yes",
    `  IdentityFile ${h.key}`,
    "  TCPKeepAlive yes",
    "  IdentitiesOnly yes",
    "",
  ])
  return [HEADER, "", ...match, ...blocks].join("\n")
}

type ChromeProfile = { id: string; dirs?: string[]; titles?: string[] } & Record<string, unknown>
type ChromeConfig = { profiles: ChromeProfile[] } & Record<string, unknown>

// chrome-profiles の config.json (プロファイルの対応はマシンの Chrome ごとに違うので生成しない) の dirs と titles だけを差し替える
export function applyChromeDirs(config: ChromeConfig, ids: Identity[]): { config: ChromeConfig; missing: string[] } {
  const known = new Set(config.profiles.map((p) => p.id))
  const profiles = config.profiles.map((p) => {
    const { dirs: _d, titles: _t, ...rest } = p
    const mine = ids.filter((i) => i.chrome === p.id)
    const dirs = mine.flatMap((i) => i.dirs)
    const titles = mine.flatMap((i) => i.titles ?? [])
    return { ...rest, ...(dirs.length ? { dirs } : {}), ...(titles.length ? { titles } : {}) }
  })
  const missing = ids.flatMap((i) => (i.chrome && !known.has(i.chrome) ? [i.chrome] : []))
  return { config: { ...config, profiles }, missing }
}
