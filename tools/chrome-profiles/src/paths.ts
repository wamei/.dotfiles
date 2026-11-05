import path from "node:path"
import type { Profile } from "./profiles"

export type Paths = ReturnType<typeof pathsFor>

export const pathsFor = (home: string) => ({
  chromeApp: "/Applications/Google Chrome.app",
  sourceDataDir: path.join(home, "Library/Application Support/Google/Chrome"),
  config: path.join(home, ".config/chrome-profiles/config.json"),
  finicky: path.join(home, ".config/chrome-profiles/finicky.js"),
  state: path.join(home, ".local/state/chrome-profiles"),
  // Teams などから開かれたリンクを、そのときの前面の窓のタイトルを添えて open --title に渡す補助アプリ
  windowApp: path.join(home, ".local/share/chrome-profiles/chrome-profiles-window.app"),
  app: (p: Profile) => path.join(home, "Applications", `Chrome ${p.name}.app`),
  dataDir: (p: Profile) => path.join(home, "Library/Application Support/Chrome Profiles", p.id),
})
