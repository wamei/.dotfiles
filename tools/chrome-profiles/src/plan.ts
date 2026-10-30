// 複製を作る/作り直すかの判定。何を元に作ったかは build.json に残す
export type BuildStamp = { chromeVersion: string; pictureHash: string; iconVersion: number }

// create: 新しく作る / rebuild: 中身を差し替えてアイコンも付け直す / icon: アイコンだけ付け直す
// defer: 動いているので次の機会に回す / skip: 何もしない
export type BuildAction = "create" | "rebuild" | "icon" | "defer" | "skip"

export function planBuild(o: { exists: boolean; running: boolean; recorded: BuildStamp | undefined; current: BuildStamp }): BuildAction {
  if (!o.exists) return "create"
  const r = o.recorded
  if (!r || r.chromeVersion !== o.current.chromeVersion) return o.running ? "defer" : "rebuild"
  if (r.pictureHash !== o.current.pictureHash || r.iconVersion !== o.current.iconVersion) return "icon"
  return "skip"
}
