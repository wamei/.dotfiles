import { expect, test } from "bun:test"
import { classifyLaunch, launchArgs, redirectArgs } from "./launch"
import { pathsFor } from "./paths"

const paths = pathsFor("/Users/u")
const profiles = [
  { id: "rit", name: "RIT", profileDir: "Default" },
  { id: "linka", name: "LINKA", profileDir: "Profile 2" },
]
const linka = profiles[1]!

test("複製の場所とデータフォルダ", () => {
  expect(paths.app(linka)).toBe("/Users/u/Applications/Chrome LINKA.app")
  expect(paths.dataDir(linka)).toBe("/Users/u/Library/Application Support/Chrome Profiles/linka")
})

test("起動の引数はデータフォルダとプロファイルを指定し、URL を後ろに並べる", () => {
  expect(launchArgs(paths, linka, ["https://example.com/a b"])).toEqual([
    "-na", "/Users/u/Applications/Chrome LINKA.app", "--args",
    "--user-data-dir=/Users/u/Library/Application Support/Chrome Profiles/linka",
    "--profile-directory=Profile 2",
    "https://example.com/a b",
  ])
})

test("複製が引数なしで起動されたら止めて起動し直す", () => {
  const exe = "/Users/u/Applications/Chrome LINKA.app/Contents/MacOS/Google Chrome"
  expect(classifyLaunch(paths, profiles, "/Users/u/Applications/Chrome LINKA.app", [exe])).toEqual({ action: "relaunch", id: "linka" })
  // macOS が付ける -psn_ だけのときも同じ
  expect(classifyLaunch(paths, profiles, "/Users/u/Applications/Chrome LINKA.app", [exe, "-psn_0_12345"])).toEqual({ action: "relaunch", id: "linka" })
})

test("データフォルダの指定があれば放っておく", () => {
  const argv = ["x", "--user-data-dir=/Users/u/Library/Application Support/Chrome Profiles/linka", "--profile-directory=Profile 2"]
  expect(classifyLaunch(paths, profiles, "/Users/u/Applications/Chrome LINKA.app", argv)).toEqual({ action: "ignore" })
  // 別のデータフォルダ (検証用など) を指定した起動も邪魔しない
  expect(classifyLaunch(paths, profiles, "/Users/u/Applications/Chrome LINKA.app", ["x", "--user-data-dir", "/tmp/t"])).toEqual({ action: "ignore" })
})

test("元の Chrome のデータフォルダで複製が起動されたら、元の Chrome に回す", () => {
  // PWA の起動用アプリなどは識別子で Chrome を探すので、同じ識別子の複製が選ばれることがある
  const exe = "/Users/u/Applications/Chrome LINKA.app/Contents/MacOS/Google Chrome"
  const argv = [exe, "--user-data-dir=/Users/u/Library/Application Support/Google/Chrome", "--no-startup-window"]
  expect(classifyLaunch(paths, profiles, "/Users/u/Applications/Chrome LINKA.app", argv)).toEqual({
    action: "redirect", args: ["--user-data-dir=/Users/u/Library/Application Support/Google/Chrome", "--no-startup-window"],
  })
  // 末尾の / や --user-data-dir <値> の形でも同じ
  expect(classifyLaunch(paths, profiles, "/Users/u/Applications/Chrome LINKA.app", [exe, "--user-data-dir", "/Users/u/Library/Application Support/Google/Chrome/"])).toEqual({
    action: "redirect", args: ["--user-data-dir", "/Users/u/Library/Application Support/Google/Chrome/"],
  })
})

test("元の Chrome に回す引数は open の -na で元の Chrome に渡す", () => {
  expect(redirectArgs(paths, ["--no-startup-window"])).toEqual(["-na", "/Applications/Google Chrome.app", "--args", "--no-startup-window"])
})

test("設定に無いアプリは放っておく", () => {
  expect(classifyLaunch(paths, profiles, "/Applications/Google Chrome.app", ["x"])).toEqual({ action: "ignore" })
  expect(classifyLaunch(paths, profiles, "/Users/u/Applications/Chrome Other.app", ["x"])).toEqual({ action: "ignore" })
})
