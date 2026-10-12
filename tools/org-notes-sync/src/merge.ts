import { diff3Merge } from "node-diff3"

export type MergeResult = { conflict: boolean; text: string }

// 末尾の改行を 1 つ外して行配列にする (空文字は空配列)
const toLines = (text: string): string[] => (text === "" ? [] : text.replace(/\n$/, "").split("\n"))

// ours = org、theirs = メモ。行単位の 3-way マージで、衝突は git 形式のマーカーにする
export function merge3(base: string, ours: string, theirs: string): MergeResult {
  const regions = diff3Merge(toLines(ours), toLines(base), toLines(theirs), { excludeFalseConflicts: true })
  const lines: string[] = []
  let conflict = false
  for (const region of regions) {
    if (region.ok) {
      lines.push(...region.ok)
    } else if (region.conflict) {
      conflict = true
      lines.push("<<<<<<< org", ...region.conflict.a, "=======", ...region.conflict.b, ">>>>>>> notes")
    }
  }
  return { conflict, text: lines.length === 0 ? "" : `${lines.join("\n")}\n` }
}
