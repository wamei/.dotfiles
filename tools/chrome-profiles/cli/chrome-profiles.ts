// Chrome のプロファイルごとに、Dock で別アプリになる Chrome の複製を作って動かす
//   chrome-profiles init [--force]          元の Local State から設定を作る
//   chrome-profiles seed <id>...            元のデータフォルダを複写して担当外のプロファイルを消す (Chrome を終了して使う)
//   chrome-profiles build [--quiet] [--settle 秒]   複製を作る/作り直す
//   chrome-profiles launch <id> [url...]    複製をデータフォルダ付きで起動する
//   chrome-profiles classify <bundle> -- <argv...>  監視から呼ばれる。ignore / relaunch <id> / redirect を出す
//   chrome-profiles redirect <bundle> -- <argv...>  複製に来た起動を、同じ引数で元の Chrome に回す
//   chrome-profiles list
import { createHash } from "node:crypto"
import fs from "node:fs"
import os from "node:os"
import path from "node:path"
import { classifyLaunch, launchArgs, redirectArgs } from "../src/launch"
import { type Paths, pathsFor } from "../src/paths"
import { type BuildStamp, planBuild } from "../src/plan"
import { type Profile, profilesFromLocalState, pruneLocalState } from "../src/profiles"

// アイコンの描き方を変えたら上げる (全部の複製のアイコンを付け直す)
const ICON_VERSION = 1

const paths = pathsFor(os.homedir())
let quiet = false
const info = (s: string) => quiet || console.log(s)
const fail = (s: string): never => {
  console.error(s)
  process.exit(1)
}

const readJson = (file: string): unknown => JSON.parse(fs.readFileSync(file, "utf8"))
const writeJson = (file: string, v: unknown) => {
  fs.mkdirSync(path.dirname(file), { recursive: true })
  fs.writeFileSync(`${file}.tmp`, `${JSON.stringify(v, null, 2)}\n`)
  fs.renameSync(`${file}.tmp`, file)
}

function sh(cmd: string[], opts: { allowFail?: boolean } = {}): string {
  const r = Bun.spawnSync(cmd, { stderr: "pipe" })
  if (r.exitCode !== 0 && !opts.allowFail) fail(`${cmd.join(" ")} が失敗しました (${r.exitCode}): ${r.stderr.toString().trim()}`)
  return r.exitCode === 0 ? r.stdout.toString().trim() : ""
}

const loadProfiles = (): Profile[] => {
  if (!fs.existsSync(paths.config)) fail(`設定がありません。先に chrome-profiles init を実行してください: ${paths.config}`)
  return (readJson(paths.config) as { profiles: Profile[] }).profiles
}
const byId = (profiles: Profile[], id: string): Profile =>
  profiles.find((p) => p.id === id) ?? fail(`設定に無いプロファイルです: ${id} (${profiles.map((p) => p.id).join(", ")})`)

// 実行ファイルのパスで動いているかを見る。複製はどれも識別子が com.google.Chrome なので名前では区別できない
const runningUnder = (appPath: string): boolean =>
  sh(["ps", "-axo", "command="]).split("\n").some((l) => l.startsWith(`${appPath}/Contents/MacOS/`))

// 同じ Swift のバイナリ置き場から探す (bun で直接動かしているときは swift/ の下)
const helper = (name: string): string => {
  const candidates = [path.join(path.dirname(process.execPath), name), path.join(import.meta.dir, "../swift", name)]
  return candidates.find((c) => fs.existsSync(c)) ?? fail(`${name} がありません: ${candidates.join(", ")}`)
}

function cmdInit(args: string[]) {
  if (fs.existsSync(paths.config) && !args.includes("--force")) fail(`設定が既にあります (上書きは --force): ${paths.config}`)
  const profiles = profilesFromLocalState(readJson(path.join(paths.sourceDataDir, "Local State")))
  writeJson(paths.config, { profiles })
  for (const p of profiles) info(`${p.id}\t${p.name}\t${p.profileDir}`)
  info(`書きました: ${paths.config} (要らないプロファイルは消してください)`)
}

function cmdSeed(ids: string[]) {
  const profiles = loadProfiles()
  const targets = (ids.length ? ids : profiles.map((p) => p.id)).map((id) => byId(profiles, id))
  // 動いている間に複写すると DB が壊れた状態で写るおそれがある
  if (runningUnder(paths.chromeApp)) fail("Chrome を終了してから実行してください")
  for (const p of targets) {
    const dest = paths.dataDir(p)
    if (fs.existsSync(dest)) {
      info(`複写済みなので飛ばします: ${dest}`)
      continue
    }
    const tmp = `${dest}.seeding`
    fs.rmSync(tmp, { recursive: true, force: true })
    fs.mkdirSync(tmp, { recursive: true })
    // ログインの引き継ぎに要るのは Local State とプロファイルのディレクトリだけ (Cookie の鍵はキーチェーンにある)。
    // 翻訳モデルや Widevine などの部品は起動後に Chrome が取り直すので写さない (丸ごとだとファイルが多く数分かかる)。
    // APFS のクローンなので、書き換えるまではディスクを食わない
    for (const name of ["Local State", "First Run", p.profileDir]) {
      if (fs.existsSync(path.join(paths.sourceDataDir, name))) sh(["cp", "-c", "-R", path.join(paths.sourceDataDir, name), path.join(tmp, name)])
    }
    writeJson(path.join(tmp, "Local State"), pruneLocalState(readJson(path.join(tmp, "Local State")), p.profileDir))
    // 写している間に Chrome が起動されたら、中途半端な写しになっているので捨てる
    if (runningUnder(paths.chromeApp)) {
      fs.rmSync(tmp, { recursive: true, force: true })
      fail("複写の途中で Chrome が起動されたので取りやめました。Chrome を終了してからやり直してください")
    }
    fs.renameSync(tmp, dest)
    info(`複写しました: ${p.name} -> ${dest}`)
  }
}

const sha256 = (file: string): string =>
  fs.existsSync(file) ? createHash("sha256").update(fs.readFileSync(file)).digest("hex") : ""

// 写真は複写先のものを優先する (その Chrome でアカウントの写真が変われば追従する)
const pictureOf = (p: Profile): string | undefined =>
  [paths.dataDir(p), paths.sourceDataDir]
    .map((d) => path.join(d, p.profileDir, "Google Profile Picture.png"))
    .find((f) => fs.existsSync(f))

function setIcon(p: Profile) {
  sh([helper("chrome-profiles-icon"), paths.chromeApp, pictureOf(p) ?? "-", paths.app(p)])
}

// 中身の差し替えは Contents だけ入れ替える。.app のディレクトリ自体は残すので Dock の固定が外れない
function replaceContents(p: Profile) {
  const app = paths.app(p)
  const staged = `${app}.staged`
  fs.rmSync(staged, { recursive: true, force: true })
  sh(["cp", "-c", "-R", paths.chromeApp, staged])
  // 更新の途中で写すと中身が揃っていない。署名で確かめて、だめなら次の機会に回す
  if (Bun.spawnSync(["codesign", "--verify", staged], { stderr: "pipe" }).exitCode !== 0) {
    fs.rmSync(staged, { recursive: true, force: true })
    throw new Error(`複製の署名を確かめられません (Chrome の更新中かもしれません): ${staged}`)
  }
  if (!fs.existsSync(app)) {
    fs.renameSync(staged, app)
  } else {
    const old = `${app}/Contents.old`
    fs.rmSync(old, { recursive: true, force: true })
    fs.renameSync(`${app}/Contents`, old)
    fs.renameSync(`${staged}/Contents`, `${app}/Contents`)
    fs.rmSync(old, { recursive: true, force: true })
    fs.rmSync(staged, { recursive: true, force: true })
  }
  sh(["/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister", "-f", app], { allowFail: true })
}

async function cmdBuild(args: string[]) {
  quiet = args.includes("--quiet")
  const settle = Number(args[args.indexOf("--settle") + 1] ?? 0)
  if (args.includes("--settle") && settle > 0) await Bun.sleep(settle * 1000)
  const profiles = loadProfiles()
  const lock = path.join(paths.state, "build.lock")
  fs.mkdirSync(paths.state, { recursive: true })
  try {
    fs.writeFileSync(lock, String(process.pid), { flag: "wx" })
  } catch {
    const pid = Number(fs.readFileSync(lock, "utf8"))
    let alive = false
    try {
      process.kill(pid, 0)
      alive = true
    } catch {}
    if (alive) return info("別の build が動いているので終わります")
    fs.writeFileSync(lock, String(process.pid))
  }
  try {
    const stampsFile = path.join(paths.state, "build.json")
    const stamps = (fs.existsSync(stampsFile) ? readJson(stampsFile) : {}) as Record<string, BuildStamp>
    const chromeVersion = sh(["plutil", "-extract", "CFBundleShortVersionString", "raw", `${paths.chromeApp}/Contents/Info.plist`])
    let failed = false
    for (const p of profiles) {
      const app = paths.app(p)
      const picture = pictureOf(p)
      const current: BuildStamp = { chromeVersion, pictureHash: picture ? sha256(picture) : "", iconVersion: ICON_VERSION }
      const action = planBuild({ exists: fs.existsSync(app), running: runningUnder(app), recorded: stamps[p.id], current })
      try {
        if (action === "create" || action === "rebuild") replaceContents(p)
        if (action === "create" || action === "rebuild" || action === "icon") {
          setIcon(p)
          // アイコンだけ付け直したときは中身の版を変えない
          stamps[p.id] = action === "icon" ? { ...current, chromeVersion: stamps[p.id]?.chromeVersion ?? "" } : current
          writeJson(stampsFile, stamps)
        }
        if (action !== "skip") info(`${p.name}: ${action}`)
      } catch (e) {
        failed = true
        console.error(`${p.name}: ${(e as Error).message}`)
      }
    }
    if (failed) process.exitCode = 1
  } finally {
    fs.rmSync(lock, { force: true })
  }
}

function cmdLaunch([id, ...urls]: string[]) {
  if (!id) fail("usage: chrome-profiles launch <id> [url...]")
  const p = byId(loadProfiles(), id as string)
  if (!fs.existsSync(paths.dataDir(p))) fail(`データフォルダがありません。先に chrome-profiles seed ${p.id} を実行してください`)
  if (!fs.existsSync(paths.app(p))) fail(`複製がありません。先に chrome-profiles build を実行してください`)
  sh(["open", ...launchArgs(paths, p, urls)])
}

function cmdClassify(args: string[]) {
  const sep = args.indexOf("--")
  const bundle = args[0]
  if (!bundle || sep !== 1) fail("usage: chrome-profiles classify <bundle> -- <argv...>")
  const v = classifyLaunch(paths, loadProfiles(), bundle as string, args.slice(sep + 1))
  console.log(v.action === "relaunch" ? `relaunch ${v.id}` : v.action)
}

function cmdRedirect(args: string[]) {
  const sep = args.indexOf("--")
  const bundle = args[0]
  if (!bundle || sep !== 1) fail("usage: chrome-profiles redirect <bundle> -- <argv...>")
  const v = classifyLaunch(paths, loadProfiles(), bundle as string, args.slice(sep + 1))
  if (v.action !== "redirect") fail(`元の Chrome に回す起動ではありません: ${v.action}`)
  else sh(["open", ...redirectArgs(paths, v.args)])
}

function cmdList(p: Paths = paths) {
  for (const x of loadProfiles()) {
    const seeded = fs.existsSync(p.dataDir(x)) ? "seeded" : "-"
    const built = fs.existsSync(p.app(x)) ? "built" : "-"
    console.log(`${x.id}\t${x.name}\t${x.profileDir}\t${seeded}\t${built}`)
  }
}

const [cmd, ...rest] = process.argv.slice(2)
switch (cmd) {
  case "init": cmdInit(rest); break
  case "seed": cmdSeed(rest); break
  case "build": await cmdBuild(rest); break
  case "launch": cmdLaunch(rest); break
  case "classify": cmdClassify(rest); break
  case "redirect": cmdRedirect(rest); break
  case "list": cmdList(); break
  default: fail("usage: chrome-profiles init|seed|build|launch|classify|redirect|list")
}
