import { expect, test } from "bun:test"
import { profileForDir, profileForWindowTitle, profilesFromLocalState, pruneLocalState } from "./profiles"

const localState = {
  browser: { enabled_labs_experiments: [] },
  profile: {
    info_cache: {
      Default: { name: "Work", user_name: "a@example.com" },
      "Profile 1": { name: "wamei" },
      "Profile 2": { name: "Client" },
      "Profile 3": { name: "テスト用" },
    },
    profiles_order: ["Default", "Profile 1", "Profile 2", "Profile 3"],
    last_used: "Default",
    last_active_profiles: ["Default", "Profile 2"],
    profile_counts_reported: 4,
  },
}

test("Local State のプロファイルを profiles_order の順に設定へ写す", () => {
  expect(profilesFromLocalState(localState)).toEqual([
    { id: "work", name: "Work", profileDir: "Default" },
    { id: "wamei", name: "wamei", profileDir: "Profile 1" },
    { id: "client", name: "Client", profileDir: "Profile 2" },
    // 名前から slug を作れなければディレクトリ名から作る
    { id: "profile-3", name: "テスト用", profileDir: "Profile 3" },
  ])
})

test("slug が重なったら後のほうにディレクトリ名を足す", () => {
  const ls = {
    profile: {
      info_cache: { Default: { name: "Work" }, "Profile 1": { name: "work" } },
      profiles_order: ["Default", "Profile 1"],
    },
  }
  expect(profilesFromLocalState(ls).map((p) => p.id)).toEqual(["work", "work-profile-1"])
})

test("profiles_order に無いプロファイルも末尾に拾う", () => {
  const ls = { profile: { info_cache: { Default: { name: "A" }, "Profile 9": { name: "B" } }, profiles_order: ["Default"] } }
  expect(profilesFromLocalState(ls).map((p) => p.profileDir)).toEqual(["Default", "Profile 9"])
})

test("剪定すると担当のプロファイルだけが残り、それが最後に使ったものになる", () => {
  const pruned = pruneLocalState(localState, "Profile 2") as typeof localState
  expect(Object.keys(pruned.profile.info_cache)).toEqual(["Profile 2"])
  expect(pruned.profile.profiles_order).toEqual(["Profile 2"])
  expect(pruned.profile.last_used).toBe("Profile 2")
  expect(pruned.profile.last_active_profiles).toEqual(["Profile 2"])
  // 関係ない値はそのまま
  expect(pruned.profile.profile_counts_reported).toBe(4)
  expect(pruned.browser).toEqual(localState.browser)
  // 元は変えない
  expect(Object.keys(localState.profile.info_cache)).toHaveLength(4)
})

test("無いプロファイルで剪定しようとしたら失敗する", () => {
  expect(() => pruneLocalState(localState, "Profile 7")).toThrow("Profile 7")
})

test("ディレクトリの中にあるプロファイルを選ぶ。~ はホームにする", () => {
  const ps = [
    { id: "work", name: "Work", profileDir: "Default" },
    { id: "client", name: "Client", profileDir: "Profile 2", dirs: ["~/projects/client/", "/srv/client"] },
  ]
  expect(profileForDir(ps, "/Users/u/projects/client/app/src", "/Users/u")?.id).toBe("client")
  expect(profileForDir(ps, "/srv/client", "/Users/u")?.id).toBe("client")
  // 名前が前方一致するだけの別ディレクトリは当たらない
  expect(profileForDir(ps, "/Users/u/projects/client-old", "/Users/u")).toBeUndefined()
  expect(profileForDir(ps, "/Users/u/projects/other", "/Users/u")).toBeUndefined()
})

test("dirs の * と ? は / をまたがない 1 段の中で当たる", () => {
  const ps = [{ id: "mc", name: "MC", profileDir: "Profile 5", dirs: ["~/projects/github.com/*/mc*", "~/projects/github.com/*/quirre?"] }]
  expect(profileForDir(ps, "/Users/u/projects/github.com/org/mc-gpt", "/Users/u")?.id).toBe("mc")
  expect(profileForDir(ps, "/Users/u/projects/github.com/org/mcwf-web/src/app", "/Users/u")?.id).toBe("mc")
  expect(profileForDir(ps, "/Users/u/projects/github.com/other/quirrel", "/Users/u")?.id).toBe("mc")
  // * は / をまたがないので、1 段深い所の mc は当たらない
  expect(profileForDir(ps, "/Users/u/projects/github.com/org/sub/mc-gpt", "/Users/u")).toBeUndefined()
  expect(profileForDir(ps, "/Users/u/projects/github.com/org/xmc", "/Users/u")).toBeUndefined()
  expect(profileForDir(ps, "/Users/u/projects/github.com/org", "/Users/u")).toBeUndefined()
})

test("窓のタイトルに titles の文字列を含むプロファイルを選ぶ。先に書いたものを優先する", () => {
  const ps = [
    { id: "work", name: "Work", profileDir: "Default" },
    { id: "mc", name: "MC", profileDir: "Profile 5", titles: ["Example Corporation Group"] },
    { id: "other", name: "Other", profileDir: "Profile 6", titles: ["Example"] },
  ]
  expect(profileForWindowTitle(ps, "チャット | 山田 太郎 | Example Corporation Group(Example CI) | a@example.com")?.id).toBe("mc")
  expect(profileForWindowTitle(ps, "チャット | Example | Microsoft Teams")?.id).toBe("other")
  expect(profileForWindowTitle(ps, "チャット | Another Org | Microsoft Teams")).toBeUndefined()
  // タイトルを読めなかったときは空文字が来る
  expect(profileForWindowTitle(ps, "")).toBeUndefined()
})

test("Teams 以外のアプリの窓のタイトルでも同じように選ぶ", () => {
  const ps = [{ id: "mc", name: "MC", profileDir: "Profile 5", titles: ["Example Workspace"] }]
  expect(profileForWindowTitle(ps, "general (チャンネル) - Example Workspace - Slack")?.id).toBe("mc")
})
