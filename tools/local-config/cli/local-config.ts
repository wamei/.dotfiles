#!/usr/bin/env bun
// マシンだけにある設定 (.dotfiles-local の identities.toml) から、各ツールの設定を書き出す
//   local-config apply [--dry-run]
// 定義の場所は $LOCAL_CONFIG_DIR (既定は ~/projects/github.com/wamei/.dotfiles-local)
import fs from "node:fs"
import os from "node:os"
import path from "node:path"
import { parseIdentities } from "../src/identities"
import { applyChromeDirs, renderGhUsersByDir, renderGitconfigLocal, renderGitIdentity, renderSshGithub } from "../src/render"

const home = os.homedir()
const src = process.env.LOCAL_CONFIG_DIR ?? path.join(home, "projects/github.com/wamei/.dotfiles-local")
const gen = path.join(home, ".config/local-config")
const GENERATED = "# local-config が生成する"

const fail = (s: string): never => {
  console.error(`local-config: ${s}`)
  process.exit(1)
}

const tilde = (p: string): string => (p.startsWith(`${home}/`) ? `~${p.slice(home.length)}` : p)

// 中身が同じなら書かない。生成したものでないファイルは初回だけ退避する
function put(file: string, content: string, dryRun: boolean): void {
  const old = fs.existsSync(file) ? fs.readFileSync(file, "utf8") : undefined
  if (old === content) return
  if (dryRun) {
    console.log(`変わります: ${tilde(file)}`)
    return
  }
  fs.mkdirSync(path.dirname(file), { recursive: true })
  if (old !== undefined && !old.startsWith(GENERATED) && !file.endsWith(".json")) {
    fs.copyFileSync(file, `${file}.pre-local-config`)
    console.log(`退避しました: ${tilde(file)}.pre-local-config`)
  }
  fs.writeFileSync(file, content)
  console.log(`書きました: ${tilde(file)}`)
}

function apply(args: string[]): void {
  const dryRun = args.includes("--dry-run")
  const file = path.join(src, "identities.toml")
  if (!fs.existsSync(file)) fail(`${file} がありません (LOCAL_CONFIG_DIR で場所を変えられます)`)
  let ids
  try {
    ids = parseIdentities(fs.readFileSync(file, "utf8"), home)
  } catch (e) {
    return fail(`${tilde(file)}: ${(e as Error).message}`)
  }

  put(path.join(home, ".gitconfig.local"), renderGitconfigLocal(ids, gen), dryRun)
  const gitDir = path.join(gen, "git")
  const wanted = new Set<string>()
  for (const i of ids.filter((x) => x.git)) {
    const f = path.join(gitDir, `${i.id}.gitconfig`)
    wanted.add(f)
    put(f, renderGitIdentity(i), dryRun)
  }
  if (fs.existsSync(gitDir)) {
    for (const name of fs.readdirSync(gitDir)) {
      const f = path.join(gitDir, name)
      if (wanted.has(f)) continue
      if (dryRun) console.log(`消します: ${tilde(f)}`)
      else {
        fs.rmSync(f)
        console.log(`消しました: ${tilde(f)}`)
      }
    }
  }
  put(path.join(home, ".config/gh/users-by-dir"), renderGhUsersByDir(ids), dryRun)
  put(path.join(gen, "ssh/github"), renderSshGithub(ids), dryRun)

  // chrome-profiles を入れていないマシンでは飛ばす
  const chrome = path.join(home, ".config/chrome-profiles/config.json")
  if (fs.existsSync(chrome)) {
    const { config, missing } = applyChromeDirs(JSON.parse(fs.readFileSync(chrome, "utf8")), ids)
    for (const id of missing) console.error(`local-config: chrome-profiles に ${id} がありません (このマシンでは振り分けません)`)
    put(chrome, `${JSON.stringify(config, null, 2)}\n`, dryRun)
  }
}

const [cmd, ...rest] = process.argv.slice(2)
switch (cmd) {
  case "apply": apply(rest); break
  default: fail("usage: local-config apply [--dry-run]")
}
