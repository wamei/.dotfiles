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
