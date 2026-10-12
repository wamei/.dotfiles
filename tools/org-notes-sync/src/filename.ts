// メモのタイトルから、ローカルに作るファイル名を決める

export function noteFileName(title: string, taken: Set<string>, ext: string): string {
  const cleaned = title.replace(/[\/\\:*?"<>|\x00-\x1f]/g, "-").trim().replace(/^\./, "-")
  const name = cleaned === "" ? "untitled" : cleaned
  if (!taken.has(`${name}${ext}`)) return `${name}${ext}`
  for (let n = 2; ; n++) {
    const candidate = `${name}-${n}${ext}`
    if (!taken.has(candidate)) return candidate
  }
}
