// Finicky の設定から import する JS を作る。プロファイルごとに「URL を受けて Finicky の browser を返す関数」を出す
import { openArgs } from "./launch"
import type { Paths } from "./paths"
import type { Profile } from "./profiles"

export function finickyModule(paths: Paths, profiles: Profile[]): string {
  const entries = profiles.map((p) => {
    // openArgs は open -a <複製> <URL...> --args ...。Finicky は open -a <name> を自分で付けるので URL より後ろを渡す。
    // args に --args が入っていると Finicky は URL を付け足さないので、URL を --args の前に自分で置く
    const [, app, ...rest] = openArgs(paths, p, [])
    return `  ${JSON.stringify(p.id)}: (url) => ({ name: ${JSON.stringify(app)}, appType: "path", args: [url.href].concat(${JSON.stringify(rest)}) }),`
  })
  return [
    "// chrome-profiles が生成する。手で編集しない (chrome-profiles build のたびに作り直される)",
    "// 使い方: import { chromeProfiles } from \"../chrome-profiles/finicky.js\" して browser: chromeProfiles.<id>",
    "export const chromeProfiles = {",
    ...entries,
    "}",
    "",
  ].join("\n")
}
