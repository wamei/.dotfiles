import { expect, test } from "bun:test"
import { merge3 } from "./merge"

test("別々の行の変更は両方残る", () => {
  expect(merge3("a\nb\nc\n", "A\nb\nc\n", "a\nb\nC\n")).toEqual({ conflict: false, text: "A\nb\nC\n" })
})
test("同じ変更は衝突にしない", () => {
  expect(merge3("a\n", "x\n", "x\n")).toEqual({ conflict: false, text: "x\n" })
})
test("同じ行の別々の変更は衝突マーカー", () => {
  expect(merge3("a\nb\n", "a\nORG\n", "a\nNOTES\n")).toEqual({
    conflict: true,
    text: "a\n<<<<<<< org\nORG\n=======\nNOTES\n>>>>>>> notes\n",
  })
})
test("base が空 (state 喪失時の採用) でも動く", () => {
  expect(merge3("", "x\n", "x\n")).toEqual({ conflict: false, text: "x\n" })
  expect(merge3("", "x\n", "y\n").conflict).toBe(true)
})

test("重複行があっても ours の挿入行が落ちない", () => {
  expect(merge3("a\nb\nc\nc\n", "a\nb\nb\nc\nc\n", "b\nb\nc\nc\n")).toEqual({
    conflict: false,
    text: "b\nb\nb\nc\nc\n",
  })
})
test("重複行の片側だけの削除は素直に反映される", () => {
  expect(merge3("a\nc\nc\nc\n", "a\nc\nc\nc\n", "a\nc\nc\n")).toEqual({ conflict: false, text: "a\nc\nc\n" })
})
test("片側が全行を消したときはもう一方が無変更なら空になる", () => {
  expect(merge3("a\nb\n", "a\nb\n", "")).toEqual({ conflict: false, text: "" })
  expect(merge3("a\nb\n", "", "a\nb\n")).toEqual({ conflict: false, text: "" })
})
test("改行だけの入力", () => {
  expect(merge3("\n", "\n", "\n")).toEqual({ conflict: false, text: "\n" })
  expect(merge3("\n", "\nx\n", "\n")).toEqual({ conflict: false, text: "\nx\n" })
})
