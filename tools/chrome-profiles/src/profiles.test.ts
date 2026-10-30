import { expect, test } from "bun:test"
import { profilesFromLocalState, pruneLocalState } from "./profiles"

const localState = {
  browser: { enabled_labs_experiments: [] },
  profile: {
    info_cache: {
      Default: { name: "RIT", user_name: "a@example.com" },
      "Profile 1": { name: "wamei" },
      "Profile 2": { name: "LINKA" },
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
    { id: "rit", name: "RIT", profileDir: "Default" },
    { id: "wamei", name: "wamei", profileDir: "Profile 1" },
    { id: "linka", name: "LINKA", profileDir: "Profile 2" },
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
