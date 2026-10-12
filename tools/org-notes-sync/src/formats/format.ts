import type { Doc } from "../doc"

export type Parsed = { doc: Doc; optIn: boolean; preserved: string[] }

// ローカルファイルの書式の契約。org 以外の書式を足すときもこれを実装する
export interface LocalFormat {
  ext: string
  parse(text: string, fallbackTitle: string): Parsed
  render(doc: Doc, preserved: string[]): string
  hasConflictMarkers(text: string): boolean
}
