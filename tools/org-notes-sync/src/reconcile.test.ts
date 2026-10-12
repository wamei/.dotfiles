import { describe, expect, test } from "bun:test"
import { canon, reconcile, rehydrate, type NoteSnap, type OrgFile, type Pair } from "./reconcile"
import { orgFormat as f } from "./formats/org"
import { EMPTY_DOC, para, text, type Doc } from "./doc"
const text_ = text

const doc = (title: string, ...lines: string[]): Doc => ({ paragraphs: [
  para({ kind: "title" }, [text(title)]), ...lines.map((l) => para({ kind: "body" }, l ? [text(l)] : [])),
] })
const orgText = (d: Doc) => f.render(d, ["#+notes_sync: yes"])
const org = (file: string, d: Doc, extra = ""): OrgFile => ({ file, text: orgText(d) + extra, mtimeMs: 1 })
const note = (id: string, d: Doc): NoteSnap => ({ id, uri: `x-coredata://${id}`, doc: d })
const run = (pairs: Pair[], orgFiles: OrgFile[], notes: NoteSnap[] | null, force = false) =>
  reconcile({ format: f, pairs, orgFiles, notes, force })

describe("ペア済み", () => {
  const base = doc("t", "a")
  const pair: Pair = { file: "t.org", noteId: "N", base }
  test("どちらも変化なし → noop", () => {
    expect(run([pair], [org("t.org", base)], [note("N", base)])).toEqual({ ok: true, actions: [{ kind: "noop", file: "t.org", noteId: "N" }] })
  })
  test("org だけ変更 → pushNotes", () => {
    const p = run([pair], [org("t.org", doc("t", "a", "b"))], [note("N", base)])
    expect(p.ok && p.actions[0]?.kind).toBe("pushNotes")
  })
  test("メモだけ変更 → pullOrg (制御行を保つ)", () => {
    const p = run([pair], [org("t.org", base)], [note("N", doc("t", "a", "iphone"))])
    expect(p.ok && p.actions[0]).toMatchObject({ kind: "pullOrg", text: "#+title: t\n#+notes_sync: yes\na\niphone\n" })
  })
  test("メモだけ変更 → pullOrg は org の触っていない行を書き換えない", () => {
    const text = "#+TITLE: t\n#+notes_sync: yes\n1) x\na\n"
    const b = f.parse(text, "t").doc
    const p = run([{ ...pair, base: b }], [{ file: "t.org", text, mtimeMs: 1 }], [note("N", { paragraphs: [...b.paragraphs, para({ kind: "body" }, [text_("iphone")])] })])
    expect(p.ok && p.actions[0]).toMatchObject({ kind: "pullOrg", text: "#+TITLE: t\n#+notes_sync: yes\n1) x\na\niphone\n" })
  })
  test("メモだけ変更でも org との行マージが衝突したらメモの描画で置き換える", () => {
    const text = "#+TITLE: t\n#+notes_sync: yes\n1) x\n"
    const b = f.parse(text, "t").doc
    const n: Doc = { paragraphs: [b.paragraphs[0]!, para({ kind: "list", list: "number" }, [text_("y")])] }
    const p = run([{ ...pair, base: b }], [{ file: "t.org", text, mtimeMs: 1 }], [note("N", n)])
    expect(p.ok && p.actions[0]).toMatchObject({ kind: "pullOrg", text: f.render(n, ["#+notes_sync: yes"]) })
  })
  test("両方の別々の行 → both", () => {
    const b3 = doc("t", "a", "b", "c")
    const p = run([{ ...pair, base: b3 }], [org("t.org", doc("t", "A", "b", "c"))], [note("N", doc("t", "a", "b", "C"))])
    expect(p.ok && p.actions[0]).toMatchObject({ kind: "both", text: "#+title: t\n#+notes_sync: yes\nA\nb\nC\n" })
  })
  test("同じ行 → conflict、base はメモ側", () => {
    const n = doc("t", "N")
    const p = run([pair], [org("t.org", doc("t", "O"))], [note("N", n)])
    expect(p.ok && p.actions[0]).toMatchObject({ kind: "conflict", base: n })
  })
  test("同じ行の conflict は衝突マーカー入りの本文", () => {
    const p = run([pair], [org("t.org", doc("t", "O"))], [note("N", doc("t", "N"))])
    if (!p.ok || p.actions[0]?.kind !== "conflict") throw new Error("expected conflict")
    expect(p.actions[0].text).toContain("<<<<<<< org")
  })
  test("両方変わっても org がメモの org 表現と同じなら pullOrg (org の本文のまま)", () => {
    const n: Doc = { paragraphs: [para({ kind: "title" }, [text("t")]), para({ kind: "body" }, [text("*b*")])] }
    const o = org("t.org", n)
    const p = run([pair], [o], [note("N", n)])
    expect(p.ok && p.actions[0]).toEqual({ kind: "pullOrg", file: "t.org", noteId: "N", text: o.text, base: n })
  })
  test("衝突マーカーが残っている → blocked", () => {
    const p = run([pair], [org("t.org", base, "<<<<<<< org\nx\n=======\ny\n>>>>>>> notes\n")], [note("N", doc("t", "z"))])
    expect(p.ok && p.actions[0]?.kind).toBe("blocked")
  })
  test("org 削除 → deleteNote / opt-in 解除も同じ", () => {
    expect(run([pair], [], [note("N", base)])).toMatchObject({ ok: true, actions: [{ kind: "deleteNote", uri: "x-coredata://N" }] })
    const optOut: OrgFile = { file: "t.org", text: f.render(base, []), mtimeMs: 1 }
    expect(run([pair], [optOut], [note("N", base)])).toMatchObject({ ok: true, actions: [{ kind: "deleteNote" }] })
  })
  test("メモ削除 → trashOrg、両方無い → drop", () => {
    const other = note("X", doc("other"))
    expect(run([pair], [org("t.org", base)], [other])).toMatchObject({ ok: true, actions: [{ kind: "trashOrg" }, { kind: "createOrg" }] })
    expect(run([pair], [], [other])).toMatchObject({ ok: true, actions: [{ kind: "drop" }, { kind: "createOrg" }] })
  })
})

describe("ペア無し", () => {
  test("opt-in の org → createNote、opt-in でない org は無視", () => {
    const p = run([], [org("a.org", doc("a")), { file: "b.org", text: "#+title: b\n", mtimeMs: 1 }], [])
    expect(p).toMatchObject({ ok: true, actions: [{ kind: "createNote", file: "a.org" }] })
  })
  test("メモ → createOrg (名前の衝突を避ける)", () => {
    const p = run([], [{ file: "買い物.org", text: "#+title: 買い物\n", mtimeMs: 1 }], [note("N", doc("買い物", "卵"))])
    expect(p).toMatchObject({ ok: true, actions: [{ kind: "createOrg", file: "買い物-2.org", noteId: "N", text: "#+title: 買い物\n#+notes_sync: yes\n卵\n" }] })
  })
  test("同じタイトルなら採用 (同じ内容は pullOrg、違えばマージ)", () => {
    expect(run([], [org("a.org", doc("a", "x"))], [note("N", doc("a", "x"))])).toMatchObject({ ok: true, actions: [{ kind: "pullOrg", noteId: "N" }] })
    expect(run([], [org("a.org", doc("a", "x"))], [note("N", doc("a", "y"))])).toMatchObject({ ok: true, actions: [{ kind: "conflict", noteId: "N" }] })
  })
})

describe("ペア無しの org に衝突マーカー", () => {
  test("skipped にして、同じタイトルのメモも採用しない", () => {
    const o = org("a.org", doc("a", "x"), "<<<<<<< org\nx\n=======\ny\n>>>>>>> notes\n")
    expect(run([], [o], [note("N", doc("a", "y"))])).toMatchObject({ ok: true, actions: [
      { kind: "skipped", file: "a.org", reason: "conflict-markers" },
      { kind: "createOrg", file: "a-2.org", noteId: "N" },
    ] })
  })
})

describe("全体停止 (Review Focus 4)", () => {
  const pairs = ["a", "b", "c", "d"].map((x): Pair => ({ file: `${x}.org`, noteId: x, base: doc(x) }))
  const orgs = pairs.map((p) => org(p.file, p.base))
  test("フォルダが無い / 0 件", () => {
    expect(run(pairs, orgs, null)).toEqual({ ok: false, reason: "folder-missing" })
    expect(run(pairs, orgs, [])).toEqual({ ok: false, reason: "notes-empty" })
  })
  test("大量削除は --force が要る", () => {
    const notes = [note("a", doc("a"))]
    expect(run(pairs, orgs, notes)).toEqual({ ok: false, reason: "mass-delete" })
    expect(run(pairs, orgs, notes, true).ok).toBe(true)
  })
})

describe("表現差でループしない (Review Focus 2, 3)", () => {
  // 1 周目で createOrg、書いた org をそのまま読んだ 2 周目は noop になること
  const expectNoopAfterCreateOrg = (d: Doc) => {
    const p1 = run([], [], [note("N", d)])
    if (!p1.ok || p1.actions[0]?.kind !== "createOrg") throw new Error("expected createOrg")
    const a = p1.actions[0]
    const p2 = run([{ file: a.file, noteId: "N", base: a.base }], [{ file: a.file, text: a.text, mtimeMs: 1 }], [note("N", d)])
    expect(p2).toEqual({ ok: true, actions: [{ kind: "noop", file: a.file, noteId: "N" }] })
  }
  test("org で表せない装飾を持つメモは、org を書いた後に変化なしと判定される", () => {
    const fancy: Doc = { paragraphs: [para({ kind: "title" }, [text("t")]), para({ kind: "body" }, [text("ab"), text("cd", ["bold"]), text("ef")])] }
    expectNoopAfterCreateOrg(fancy)
  })
  test("org の記法を文字どおり含むメモも、org を書いた後に変化なしと判定される", () => {
    const literal: Doc = { paragraphs: [para({ kind: "title" }, [text("t")]), para({ kind: "body" }, [text("*b*")])] }
    expect(canon(f, literal)).not.toEqual(literal) // org では太字として読まれる
    expectNoopAfterCreateOrg(literal)
  })
  test("org で 1 行足しても、他の段落のメモ側の書式は残る", () => {
    const fancy: Doc = { paragraphs: [para({ kind: "title" }, [text("t")]), para({ kind: "body" }, [text("ab"), text("cd", ["bold"]), text("ef")])] }
    const edited = f.render(canon(f, fancy), ["#+notes_sync: yes"]) + "追記\n"
    const p = run([{ file: "t.org", noteId: "N", base: fancy }], [{ file: "t.org", text: edited, mtimeMs: 1 }], [note("N", fancy)])
    expect(p.ok && p.actions[0]).toMatchObject({ kind: "pushNotes", doc: { paragraphs: [fancy.paragraphs[0], fancy.paragraphs[1], para({ kind: "body" }, [text("追記")])] } })
  })
  test("rehydrate は段落数がずれたら何もしない", () => {
    const t = doc("t", "x")
    expect(rehydrate(t, EMPTY_DOC, f)).toEqual(t)
  })
})

describe("org のリネーム", () => {
  const base = doc("t", "a")
  const pair: Pair = { file: "t.org", noteId: "N", base }
  test("同じ内容のペア無し org があれば rename して、そのペアとして判断する", () => {
    expect(run([pair], [org("u.org", base)], [note("N", base)])).toEqual({ ok: true, actions: [
      { kind: "rename", from: "t.org", to: "u.org", noteId: "N" },
      { kind: "noop", file: "u.org", noteId: "N" },
    ] })
  })
  test("リネーム先でも iPhone の編集は pullOrg になる", () => {
    const p = run([pair], [org("u.org", base)], [note("N", doc("t", "a", "iphone"))])
    expect(p).toMatchObject({ ok: true, actions: [{ kind: "rename", to: "u.org" }, { kind: "pullOrg", file: "u.org", text: "#+title: t\n#+notes_sync: yes\na\niphone\n" }] })
  })
  test("内容が変わっていてもタイトルが同じ候補が 1 つだけなら rename (内容一致を優先)", () => {
    expect(run([pair], [org("u.org", doc("t", "b"))], [note("N", base)])).toMatchObject({ ok: true, actions: [{ kind: "rename", to: "u.org" }, { kind: "pushNotes", file: "u.org" }] })
    const p = run([pair], [org("u.org", doc("t", "b")), org("v.org", base), org("w.org", doc("t", "c"))], [note("N", base)])
    expect(p).toMatchObject({ ok: true, actions: [{ kind: "rename", to: "v.org" }, { kind: "noop", file: "v.org" }, { kind: "createNote", file: "u.org" }, { kind: "createNote", file: "w.org" }] })
  })
  test("タイトルだけ同じ候補が複数 / opt-in でない / opt-in 解除なら rename しない", () => {
    expect(run([pair], [org("u.org", doc("t", "b")), org("v.org", doc("t", "c"))], [note("N", base)]))
      .toMatchObject({ ok: true, actions: [{ kind: "deleteNote" }, { kind: "createNote" }, { kind: "createNote" }] })
    expect(run([pair], [{ file: "u.org", text: f.render(base, []), mtimeMs: 1 }], [note("N", base)]))
      .toMatchObject({ ok: true, actions: [{ kind: "deleteNote" }] })
    const optOut: OrgFile = { file: "t.org", text: f.render(base, []), mtimeMs: 1 }
    expect(run([pair], [optOut, org("u.org", base)], [note("N", base)]))
      .toMatchObject({ ok: true, actions: [{ kind: "deleteNote" }, { kind: "createNote", file: "u.org" }] })
  })
})
