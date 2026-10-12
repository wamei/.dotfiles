// notes-bridge の出力の型。
import type { Doc } from "./doc"

// list の 1 件。パスワードでロックされたメモは中身を読まず locked だけを返す
export type BridgeNote = { id: string; uri: string; doc: Doc } | { id: string; uri: string; locked: true }
