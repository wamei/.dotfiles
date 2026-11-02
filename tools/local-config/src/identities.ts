// dotfiles-local の identities.toml (このマシンでどのディレクトリを誰として扱うか) を読む。
// 身元 1 つにつき、git の user・gh のアカウント・ssh の別名ホスト・Chrome のプロファイルを持つ
import path from "node:path"

export type SshHost = { host: string; hostname: string; key: string }
export type Identity = {
  id: string
  // この身元で扱うディレクトリ (末尾の / は落とす)
  dirs: string[]
  // 無ければ公開側 .gitconfig の user のまま
  git?: { name: string; email: string }
  // gh のアカウント (tools/gh-account/gh が切り替える)
  gh?: string
  ssh: SshHost[]
  // chrome-profiles の id (Emacs のリンクをこのプロファイルで開く)
  chrome?: string
}

const KEYS = new Set(["id", "dirs", "git", "gh", "ssh", "chrome"])

type Raw = Record<string, unknown>

const str = (v: unknown): v is string => typeof v === "string" && v !== ""

export function parseIdentities(toml: string, home: string): Identity[] {
  const expand = (p: string): string => p.replace(/^~(?=\/|$)/, home)
  const doc = Bun.TOML.parse(toml) as { identity?: Raw[] }
  const seen = new Set<string>()
  return (doc.identity ?? []).map((raw, i) => {
    const id = str(raw.id) ? raw.id : `#${i + 1}`
    const fail = (msg: string): never => {
      throw new Error(`${id}: ${msg}`)
    }
    if (!str(raw.id)) fail("id がありません")
    if (seen.has(id)) fail("id が重複しています")
    seen.add(id)
    for (const k of Object.keys(raw)) if (!KEYS.has(k)) fail(`知らないキー ${k}`)

    if (!Array.isArray(raw.dirs) || raw.dirs.length === 0 || !raw.dirs.every(str)) fail("dirs に 1 つ以上のディレクトリを書いてください")
    const dirs = (raw.dirs as string[]).map((d) => {
      if (!d.startsWith("/") && !d.startsWith("~")) fail(`dirs は絶対パスか ~ で書いてください: ${d}`)
      return path.resolve(expand(d))
    })

    const out: Identity = { id, dirs, ssh: [] }
    if (raw.git !== undefined) {
      const g = raw.git as Raw
      if (!str(g.name)) fail("git.name がありません")
      if (!str(g.email)) fail("git.email がありません")
      out.git = { name: g.name as string, email: g.email as string }
    }
    if (raw.gh !== undefined) {
      if (!str(raw.gh)) fail("gh はアカウント名の文字列にしてください")
      out.gh = raw.gh as string
    }
    if (raw.ssh !== undefined) {
      if (!Array.isArray(raw.ssh)) fail("ssh は配列にしてください")
      out.ssh = (raw.ssh as Raw[]).map((s, j) => {
        for (const k of ["host", "hostname", "key"]) if (!str(s[k])) fail(`ssh[${j}].${k} がありません`)
        return { host: s.host as string, hostname: s.hostname as string, key: expand(s.key as string) }
      })
    }
    if (raw.chrome !== undefined) {
      if (!str(raw.chrome)) fail("chrome は chrome-profiles の id にしてください")
      out.chrome = raw.chrome as string
    }
    return out
  })
}
