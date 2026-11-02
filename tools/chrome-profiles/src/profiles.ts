import path from "node:path"

// Chrome の Local State (データフォルダ直下の JSON) からプロファイルを読み、複写先に合わせて剪定する
// picture: アイコンに重ねる画像 (省略時は Chrome が取り込んだ Google アカウントの写真)
// dirs: open --dir でこのプロファイルに振り分けるディレクトリ (Emacs のタブのプロジェクトなど)
export type Profile = { id: string; name: string; profileDir: string; picture?: string; dirs?: string[] }

type Json = Record<string, unknown>
type ProfileSection = {
  info_cache?: Record<string, { name?: string }>
  profiles_order?: string[]
  last_used?: string
  last_active_profiles?: string[]
}

const section = (localState: unknown): ProfileSection => ((localState as Json).profile ?? {}) as ProfileSection

const slug = (s: string): string =>
  s.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "")

export function profilesFromLocalState(localState: unknown): Profile[] {
  const { info_cache = {}, profiles_order = [] } = section(localState)
  const dirs = [...profiles_order.filter((d) => d in info_cache), ...Object.keys(info_cache).filter((d) => !profiles_order.includes(d))]
  const used = new Set<string>()
  return dirs.map((profileDir) => {
    const name = info_cache[profileDir]?.name ?? profileDir
    // 日本語だけの名前は slug にならないので、ディレクトリ名から作る
    let id = slug(name) || slug(profileDir)
    if (used.has(id)) id = `${id}-${slug(profileDir)}`
    used.add(id)
    return { id, name, profileDir }
  })
}

// 担当のプロファイルだけを残す。他のプロファイルのディレクトリは呼び出し側で消す
export function pruneLocalState(localState: unknown, keep: string): unknown {
  const profile = section(localState)
  const entry = profile.info_cache?.[keep]
  if (!entry) throw new Error(`Local State にプロファイル ${keep} がありません`)
  return {
    ...(localState as Json),
    profile: { ...profile, info_cache: { [keep]: entry }, profiles_order: [keep], last_used: keep, last_active_profiles: [keep] },
  }
}

// dir を中に含む dirs を持つプロファイル。先に書いたものを優先する
export function profileForDir(profiles: Profile[], dir: string, home: string): Profile | undefined {
  const inside = (base: string): boolean => {
    const rel = path.relative(path.resolve(base.replace(/^~(?=\/|$)/, home)), path.resolve(dir))
    return rel === "" || (!rel.startsWith("..") && !path.isAbsolute(rel))
  }
  return profiles.find((p) => p.dirs?.some(inside))
}
