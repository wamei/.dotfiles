import { expect, test } from "bun:test"
import { planBuild } from "./plan"

const current = { chromeVersion: "156.0.8078.80", pictureHash: "aaa", iconVersion: 1 }

test("複製が無ければ作る", () => {
  expect(planBuild({ exists: false, running: false, recorded: undefined, current })).toBe("create")
})

test("記録と同じなら何もしない", () => {
  expect(planBuild({ exists: true, running: false, recorded: { ...current }, current })).toBe("skip")
})

test("Chrome の版が変わったら作り直す", () => {
  expect(planBuild({ exists: true, running: false, recorded: { ...current, chromeVersion: "155.0.1.2" }, current })).toBe("rebuild")
})

test("写真かアイコンの描き方が変わったらアイコンだけ付け直す", () => {
  expect(planBuild({ exists: true, running: false, recorded: { ...current, pictureHash: "bbb" }, current })).toBe("icon")
  expect(planBuild({ exists: true, running: false, recorded: { ...current, iconVersion: 0 }, current })).toBe("icon")
})

test("記録が無い複製は作り直す", () => {
  expect(planBuild({ exists: true, running: false, recorded: undefined, current })).toBe("rebuild")
})

test("動いている複製は中身を差し替えず後に回す。アイコンだけなら動いていても付け直す", () => {
  expect(planBuild({ exists: true, running: true, recorded: { ...current, chromeVersion: "155.0.1.2" }, current })).toBe("defer")
  expect(planBuild({ exists: true, running: true, recorded: undefined, current })).toBe("defer")
  expect(planBuild({ exists: true, running: true, recorded: { ...current, pictureHash: "bbb" }, current })).toBe("icon")
})
