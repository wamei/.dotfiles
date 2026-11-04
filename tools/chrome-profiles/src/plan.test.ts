import { expect, test } from "bun:test"
import { compareVersions, pickSource, planBuild } from "./plan"

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

test("Chrome の更新でアイコンが消えていたら付け直す。動いていても付け直す", () => {
  expect(planBuild({ exists: true, running: false, hasIcon: false, recorded: { ...current }, current })).toBe("icon")
  expect(planBuild({ exists: true, running: true, hasIcon: false, recorded: { ...current, chromeVersion: "155.0.1.2" }, current })).toBe("icon")
})

test("版を数字で比べる", () => {
  expect(compareVersions("157.0.8103.38", "156.0.8078.80")).toBeGreaterThan(0)
  expect(compareVersions("156.0.10.0", "156.0.9.0")).toBeGreaterThan(0)
  expect(compareVersions("156.0.8078.80", "156.0.8078.80")).toBe(0)
  expect(compareVersions("155.1", "156.0")).toBeLessThan(0)
})

test("複写元は元の Chrome と複製のうち一番新しいもの。同じ版なら元の Chrome", () => {
  const src = { path: "/Applications/Google Chrome.app", version: "156.0.8078.80" }
  const rit = { path: "/x/Chrome RIT.app", version: "157.0.8103.38" }
  const old = { path: "/x/Chrome wamei.app", version: "156.0.8078.80" }
  expect(pickSource(src, [old, rit])).toEqual(rit)
  expect(pickSource(src, [old, { ...rit, version: src.version }])).toEqual(src)
  expect(pickSource(src, [{ ...old, version: "155.0.0.0" }])).toEqual(src)
})
