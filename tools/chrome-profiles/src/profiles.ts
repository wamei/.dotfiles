import path from "node:path"

// Chrome の Local State (データフォルダ直下の JSON) からプロファイルを読み、複写先に合わせて剪定する
// picture: アイコンに重ねる画像 (省略時は Chrome が取り込んだ Google アカウントの写真)
// dirs: open --dir でこのプロファイルに振り分けるディレクトリ (Emacs のタブのプロジェクトなど)
// titles: open --title でこのプロファイルに振り分ける、リンクを押したアプリの窓のタイトルに含まれる文字列 (Teams の組織名など)
// emails: open --email でこのプロファイルに振り分ける、予定の主催者のメールアドレス (@ で始めるとドメイン、* も書ける。カレンダー.app のリンク)
export type Profile = {
  id: string
  name: string
  profileDir: string
  picture?: string
  dirs?: string[]
  titles?: string[]
  emails?: string[]
}

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
  // * と ? は git の includeIf "gitdir:" と同じく / をまたがない
  const inside = (base: string): boolean => {
    const glob = path.resolve(base.replace(/^~(?=\/|$)/, home))
    const re = glob.replace(/[.+^${}()|[\]\\*?]/g, (c) => (c === "*" ? "[^/]*" : c === "?" ? "[^/]" : `\\${c}`))
    return new RegExp(`^${re}(?:/.*)?$`).test(path.resolve(dir))
  }
  return profiles.find((p) => p.dirs?.some(inside))
}

// 窓のタイトルに titles の文字列を含むプロファイル。先に書いたものを優先する
export function profileForWindowTitle(profiles: Profile[], title: string): Profile | undefined {
  if (title === "") return undefined
  return profiles.find((p) => p.titles?.some((s) => s !== "" && title.includes(s)))
}

// 主催者のメールアドレスが emails に当たるプロファイル。@ で始まればドメイン、そうでなければアドレスそのものと比べる。
// * は @ をまたがずに何にでも当たる ("@*.example.com" はサブドメイン、"@*" はどれにも当たらなかったときの受け皿)。
// 複数に当たれば、より詳しい (* 以外の文字が多い) ものが勝つ。同じなら先に書いたもの。大文字小文字は区別しない
export function profileForEmail(profiles: Profile[], email: string): Profile | undefined {
  const e = email.toLowerCase()
  if (e === "") return undefined
  const matches = (pattern: string): boolean => {
    const s = pattern.toLowerCase()
    const re = s.replace(/[.+^${}()|[\]\\?*]/g, (c) => (c === "*" ? "[^@]*" : `\\${c}`))
    return new RegExp(s.startsWith("@") ? `^[^@]*${re}$` : `^${re}$`).test(e)
  }
  let best: { profile: Profile; score: number } | undefined
  for (const profile of profiles) {
    for (const s of profile.emails ?? []) {
      const score = s.replaceAll("*", "").length
      if (matches(s) && (!best || score > best.score)) best = { profile, score }
    }
  }
  return best?.profile
}
