#!/usr/bin/env bun
// ~/org の org メモを ~/notes の Markdown に移す (一度きり)。終了コード: 0 ok / 1 中止
// 元の org ファイルは残す。state は移さない (新しいフォルダで組み直す)
import os from "node:os"
import path from "node:path"
import { parseArgs } from "node:util"
import { applyMigration, planMigration } from "../src/migrate"

const home = os.homedir()
const { values } = parseArgs({
  options: {
    "org-dir": { type: "string", default: path.join(home, "org") },
    dir: { type: "string", default: path.join(home, "notes") },
    "dry-run": { type: "boolean", default: false },
  },
  strict: true,
})

const plan = planMigration(values["org-dir"], values.dir)
for (const f of plan.files) console.log(`${f.optIn ? "sync " : "     "} ${f.from} → ${f.to}`)
for (const m of plan.moves) console.log(`move  ${m.from} → ${m.to}`)
for (const e of plan.errors) console.error(`error ${e}`)
console.log(`${plan.files.length} 件 (opt-in ${plan.files.filter((f) => f.optIn).length} 件)、エラー ${plan.errors.length} 件`)
if (plan.errors.length > 0) {
  console.error("移行を中止しました。何も書いていません")
  process.exit(1)
}
if (values["dry-run"]) {
  console.log("--dry-run なので書いていません")
  process.exit(0)
}
applyMigration(plan)
console.log("移行しました")
