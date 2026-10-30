// 複製の起動。データフォルダは起動引数でしか渡せないので、引数なしで起動された複製は止めて起動し直す
import path from "node:path"
import type { Paths } from "./paths"
import type { Profile } from "./profiles"

// open(1) に渡す引数。-n が無いと、動いている複製があるとき引数が捨てられる
export const launchArgs = (paths: Paths, p: Profile, urls: string[]): string[] => [
  "-na", paths.app(p), "--args",
  `--user-data-dir=${paths.dataDir(p)}`,
  `--profile-directory=${p.profileDir}`,
  ...urls,
]

export const redirectArgs = (paths: Paths, args: string[]): string[] => ["-na", paths.chromeApp, "--args", ...args]

// ignore: そのまま動かす / relaunch: 止めてその複製のデータフォルダで起動し直す
// redirect: 止めて同じ引数で元の Chrome を起動する
export type LaunchVerdict = { action: "ignore" } | { action: "relaunch"; id: string } | { action: "redirect"; args: string[] }

const userDataDir = (argv: string[]): string | undefined => {
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i] as string
    if (a.startsWith("--user-data-dir=")) return a.slice("--user-data-dir=".length)
    if (a === "--user-data-dir") return argv[i + 1] ?? ""
  }
  return undefined
}

export function classifyLaunch(paths: Paths, profiles: Profile[], bundlePath: string, argv: string[]): LaunchVerdict {
  const p = profiles.find((x) => paths.app(x) === bundlePath)
  if (!p) return { action: "ignore" }
  const dir = userDataDir(argv)
  if (dir === undefined) return { action: "relaunch", id: p.id }
  // 識別子が元と同じなので、PWA の起動用アプリなどが元の Chrome のつもりで複製を起動することがある
  if (path.resolve(dir) === path.resolve(paths.sourceDataDir)) return { action: "redirect", args: argv.slice(1) }
  // 自分のデータフォルダか、検証用などの別のデータフォルダ
  return { action: "ignore" }
}
