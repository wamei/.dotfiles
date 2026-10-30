import type { Doc } from "../doc"

export type Parsed = { doc: Doc; optIn: boolean; preserved: string[] }

// ローカルファイルの書式の契約 (Markdown が本番、org は移行で読むために残す)
export type LocalFormat = {
  ext: string
  optInLine: string // 新しく作るファイルに書く opt-in の行 (render の preserved に渡す)
  parse(text: string, fallbackTitle: string): Parsed
  render(doc: Doc, preserved: string[]): string
  hasConflictMarkers(text: string): boolean
}
