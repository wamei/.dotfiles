import { expect, test } from "bun:test"
import fs from "node:fs"
import os from "node:os"
import path from "node:path"
import { finickyModule } from "./finicky"
import { pathsFor } from "./paths"

const paths = pathsFor("/Users/u")
const profiles = [
  { id: "rit", name: "RIT", profileDir: "Default" },
  { id: "linka", name: "LINKA", profileDir: "Profile 2" },
]

// 生成した JS を実際に import して、Finicky に渡る値を確かめる
async function load(): Promise<Record<string, (url: URL) => unknown>> {
  const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), "cp-finicky-")), "finicky.js")
  fs.writeFileSync(file, finickyModule(paths, profiles))
  return (await import(file)).chromeProfiles
}

test("プロファイルごとに、URL を受けて Finicky の browser を返す関数を出す", async () => {
  const cp = await load()
  expect(Object.keys(cp)).toEqual(["rit", "linka"])
  // Finicky は args に --args があれば自分では付けず、open -a <name> <args...> を実行する。
  // -n が無いと、複製が動いているとき引数ごと URL が捨てられる
  expect(cp.linka!(new URL("https://example.com/a?b=c"))).toEqual({
    name: "/Users/u/Applications/Chrome LINKA.app",
    appType: "path",
    args: [
      "-n", "--args",
      "--user-data-dir=/Users/u/Library/Application Support/Chrome Profiles/linka",
      "--profile-directory=Profile 2",
      "https://example.com/a?b=c",
    ],
  })
})

test("名前やパスに引用符があっても壊れない", async () => {
  const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), "cp-finicky-")), "finicky.js")
  fs.writeFileSync(file, finickyModule(paths, [{ id: "q", name: `a"b\`c`, profileDir: "Profile 1" }]))
  const cp = (await import(file)).chromeProfiles
  expect(cp.q(new URL("https://x.test/")).name).toBe("/Users/u/Applications/Chrome a\"b`c.app")
})
