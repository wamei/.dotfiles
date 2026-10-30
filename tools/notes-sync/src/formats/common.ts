// 書式をまたいで使う段落まわりの小物
import { text, type Inline, type Paragraph } from "../doc"

export function splitLines(s: string): string[] {
  const lines = s.split(/\r?\n/)
  // 末尾の改行で終わる最後の空行は段落にしない
  if (lines[lines.length - 1] === "") lines.pop()
  return lines
}

export function isBoldLine(p: Paragraph): boolean {
  return p.inlines.length > 0 && p.inlines.every((i) => i.kind === "text" && i.marks.includes("bold"))
}

export function stripBold(inlines: Inline[]): Inline[] {
  return inlines.map((i) => (i.kind === "text" ? text(i.text, i.marks.filter((m) => m !== "bold"), i.link) : i))
}

// git の衝突マーカー: 行頭の <<<<<<< → ======= → >>>>>>> がこの順に揃ったときだけ (区切り線だけの行は普通の文章)
export function hasConflictMarkers(s: string): boolean {
  const marks = [/^<{7}( |$)/, /^={7}( |$)/, /^>{7}( |$)/]
  let next = 0
  for (const line of s.split(/\r?\n/)) {
    if (marks[next]!.test(line) && ++next === marks.length) return true
  }
  return false
}
