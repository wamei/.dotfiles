// 複製を作る/作り直すかの判定。何を元に作ったかは build.json に残す
export type BuildStamp = { chromeVersion: string; pictureHash: string; iconVersion: number }

// create: 新しく作る / rebuild: 中身を差し替えてアイコンも付け直す / icon: アイコンだけ付け直す
// defer: 動いているので次の機会に回す / skip: 何もしない
export type BuildAction = "create" | "rebuild" | "icon" | "defer" | "skip"

// hasIcon: 複製に付けたアイコンが残っているか (Google の更新が複製を上書きすると消える)
export function planBuild(o: { exists: boolean; running: boolean; hasIcon?: boolean; recorded: BuildStamp | undefined; current: BuildStamp }): BuildAction {
  if (!o.exists) return "create"
  const r = o.recorded
  const iconLost = o.hasIcon === false
  if (!r || r.chromeVersion !== o.current.chromeVersion) return o.running ? (iconLost ? "icon" : "defer") : "rebuild"
  if (iconLost || r.pictureHash !== o.current.pictureHash || r.iconVersion !== o.current.iconVersion) return "icon"
  return "skip"
}

export function compareVersions(a: string, b: string): number {
  const x = a.split(".").map(Number)
  const y = b.split(".").map(Number)
  for (let i = 0; i < Math.max(x.length, y.length); i++) {
    const d = (x[i] ?? 0) - (y[i] ?? 0)
    if (d !== 0) return d
  }
  return 0
}

export type Bundle = { path: string; version: string }

// 複製を動かしていると、Chrome が自分 (複製) を Google の更新に登録し直すので、更新は元の Chrome ではなく複製に入る。
// 一番新しいものから写せば、どの複製も版が戻らない (戻すとプロファイルが新しい版で開かれていて Chrome が開けない)
export function pickSource(source: Bundle, clones: Bundle[]): Bundle {
  return clones.reduce((best, c) => (compareVersions(c.version, best.version) > 0 ? c : best), source)
}
