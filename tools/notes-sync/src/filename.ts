// メモのタイトルから、ローカルに作るファイル名を決める

// APFS は大文字小文字を区別せず、NFC と NFD を同じ名前として扱うので、比較はこの形で行う
export function fileNameKey(name: string): string {
  return name.normalize("NFC").toLowerCase()
}

export function noteFileName(title: string, taken: Set<string>, ext: string): string {
  const keys = new Set([...taken].map(fileNameKey))
  const isTaken = (name: string) => keys.has(fileNameKey(name))
  const cleaned = title.replace(/[\/\\:*?"<>|\x00-\x1f]/g, "-").trim().replace(/^\./, "-")
  const name = cleaned === "" ? "untitled" : cleaned
  if (!isTaken(`${name}${ext}`)) return `${name}${ext}`
  for (let n = 2; ; n++) {
    const candidate = `${name}-${n}${ext}`
    if (!isTaken(candidate)) return candidate
  }
}
