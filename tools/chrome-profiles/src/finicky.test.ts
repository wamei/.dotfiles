import { expect, test } from "bun:test"
import fs from "node:fs"
import os from "node:os"
import path from "node:path"
import { finickyModule } from "./finicky"
import { pathsFor } from "./paths"

const paths = pathsFor("/Users/u")
const profiles = [
  { id: "work", name: "Work", profileDir: "Default" },
  { id: "client", name: "Client", profileDir: "Profile 2" },
]

// 生成した JS を実際に import して、Finicky に渡る値を確かめる
async function load(): Promise<Record<string, (url: URL) => unknown>> {
  const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), "cp-finicky-")), "finicky.js")
  fs.writeFileSync(file, finickyModule(paths, profiles))
  return (await import(file)).chromeProfiles
}

test("プロファイルごとに、URL を受けて Finicky の browser を返す関数を出す", async () => {
  const cp = await load()
  expect(Object.keys(cp)).toEqual(["work", "client"])
  // Finicky は args に --args があれば自分では付けず URL も足さず、open -a <name> <args...> を実行する。
  // URL を --args より前に置くと、動いている複製にはそのまま URL が届き (新しいプロセスを起動しない)、
  // 止まっていれば引数付きで起動して URL を開く。-n を付けると毎回プロセスが増えて Dock にちらつく
  expect(cp.client!(new URL("https://example.com/a?b=c"))).toEqual({
    name: "/Users/u/Applications/Chrome Client.app",
    appType: "path",
    args: [
      "https://example.com/a?b=c",
      "--args",
      "--user-data-dir=/Users/u/Library/Application Support/Chrome Profiles/client",
      "--profile-directory=Profile 2",
    ],
  })
})

test("名前やパスに引用符があっても壊れない", async () => {
  const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), "cp-finicky-")), "finicky.js")
  fs.writeFileSync(file, finickyModule(paths, [{ id: "q", name: `a"b\`c`, profileDir: "Profile 1" }]))
  const cp = (await import(file)).chromeProfiles
  expect(cp.q(new URL("https://x.test/")).name).toBe("/Users/u/Applications/Chrome a\"b`c.app")
})

test("窓のタイトルで振り分けるリンクは、タイトルを読む補助アプリに URL ごと渡す", async () => {
  const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), "cp-finicky-")), "finicky.js")
  fs.writeFileSync(file, finickyModule(paths, profiles))
  const { windowRouter } = await import(file)
  // Finicky はリンクを受けると自分を前面に出すので、補助アプリからは押したアプリが分からない。
  // Finicky が知っている opener の bundleId を引数で渡す。-n で毎回起動するので、引数は必ず届く
  expect(windowRouter(new URL("https://example.com/a"), { opener: { name: "Microsoft Teams", bundleId: "com.microsoft.teams2", path: "/Applications/Microsoft Teams.app" } })).toEqual({
    name: "/Users/u/.local/share/chrome-profiles/chrome-profiles-window.app",
    appType: "path",
    args: ["-n", "https://example.com/a", "--args", "--opener", "com.microsoft.teams2"],
  })
  // opener が無ければ空で渡す (補助アプリは既定のブラウザに戻す)
  expect(windowRouter(new URL("https://example.com/a"), { opener: null }).args).toEqual(["-n", "https://example.com/a", "--args", "--opener", ""])
})
