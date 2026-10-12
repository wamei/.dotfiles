import { expect, test } from "bun:test"
import { noteFileName } from "./filename"
test("ファイル名", () => {
  expect(noteFileName("買い物", new Set(), ".org")).toBe("買い物.org")
  expect(noteFileName("a/b: c?", new Set(), ".org")).toBe("a-b- c-.org")
  expect(noteFileName(".hidden", new Set(), ".org")).toBe("-hidden.org")
  expect(noteFileName("  ", new Set(), ".org")).toBe("untitled.org")
  expect(noteFileName("x", new Set(["x.org", "x-2.org"]), ".org")).toBe("x-3.org")
})
