// 複製の Chrome の起動を見張る (LaunchAgent で常駐)
//   chrome-profiles-watch <chrome-profiles CLI>
// 引数なしで起動された複製 (Dock のクリックなど) は、元のデータフォルダを見て元の Chrome に処理を渡してしまう。
// 渡す前に SIGSTOP で止め、CLI の classify に聞いて、起動し直すものは SIGKILL して launch を呼ぶ。
// 識別子が元と同じなので、PWA の起動用アプリなどが元の Chrome のつもりで複製を起動することもある。
// 元のデータフォルダを指定した起動は SIGKILL して redirect で元の Chrome に回す。
// 複製が終了したら build を呼ぶ (動いていて作り直せなかった分を拾う)
import AppKit

let cli = CommandLine.arguments.count > 1
  ? CommandLine.arguments[1]
  : FileManager.default.homeDirectoryForCurrentUser.appendingPathComponent(".local/bin/chrome-profiles").path
let appsDir = FileManager.default.homeDirectoryForCurrentUser.appendingPathComponent("Applications").standardizedFileURL.path

func log(_ s: String) {
  print("\(ISO8601DateFormatter().string(from: Date())) \(s)")
  fflush(stdout)
}

// ~/Applications/Chrome *.app で中身が Chrome のもの。設定に載っているかは CLI が判定する
func isCandidate(_ app: NSRunningApplication) -> Bool {
  guard app.bundleIdentifier == "com.google.Chrome", let url = app.bundleURL?.standardizedFileURL else { return false }
  return url.deletingLastPathComponent().path == appsDir && url.lastPathComponent.hasPrefix("Chrome ")
}

// KERN_PROCARGS2: argc (int32) のあと実行ファイルのパス、NUL の詰め物、argv が並ぶ
func argv(of pid: pid_t) -> [String]? {
  var mib: [Int32] = [CTL_KERN, KERN_PROCARGS2, pid]
  var size = 0
  guard sysctl(&mib, 3, nil, &size, nil, 0) == 0 else { return nil }
  var buf = [UInt8](repeating: 0, count: size)
  guard sysctl(&mib, 3, &buf, &size, nil, 0) == 0, size > 4 else { return nil }
  let argc = Int(buf.withUnsafeBytes { $0.load(as: Int32.self) })
  var i = 4
  while i < size && buf[i] != 0 { i += 1 }
  while i < size && buf[i] == 0 { i += 1 }
  var out: [String] = []
  while out.count < argc && i < size {
    let start = i
    while i < size && buf[i] != 0 { i += 1 }
    out.append(String(decoding: buf[start..<i], as: UTF8.self))
    i += 1
  }
  return out
}

@discardableResult
func run(_ args: [String]) -> String {
  let p = Process()
  p.executableURL = URL(fileURLWithPath: cli)
  p.arguments = args
  let pipe = Pipe()
  p.standardOutput = pipe
  do { try p.run() } catch {
    log("CLI を起動できません: \(cli): \(error)")
    return ""
  }
  let data = pipe.fileHandleForReading.readDataToEndOfFile()
  p.waitUntilExit()
  return String(decoding: data, as: UTF8.self).trimmingCharacters(in: .whitespacesAndNewlines)
}

let center = NSWorkspace.shared.notificationCenter
center.addObserver(forName: NSWorkspace.didLaunchApplicationNotification, object: nil, queue: .main) { n in
  guard let app = n.userInfo?[NSWorkspace.applicationUserInfoKey] as? NSRunningApplication, isCandidate(app),
        let bundle = app.bundleURL?.standardizedFileURL.path else { return }
  let pid = app.processIdentifier
  // 判定の間に元の Chrome へ処理を渡されないよう、先に止める
  kill(pid, SIGSTOP)
  guard let av = argv(of: pid) else {
    kill(pid, SIGCONT)
    return
  }
  let verdict = run(["classify", bundle, "--"] + av)
  if verdict.hasPrefix("relaunch ") {
    let id = String(verdict.dropFirst("relaunch ".count))
    kill(pid, SIGKILL)
    log("引数なしの起動を止めて起動し直す: \(bundle) -> \(id)")
    run(["launch", id])
  } else if verdict == "redirect" {
    kill(pid, SIGKILL)
    log("元の Chrome に回す: \(bundle) \(av.dropFirst().joined(separator: " "))")
    run(["redirect", bundle, "--"] + av)
  } else {
    kill(pid, SIGCONT)
    log("そのまま動かす: \(bundle) pid=\(pid)")
  }
}
center.addObserver(forName: NSWorkspace.didTerminateApplicationNotification, object: nil, queue: .main) { n in
  guard let app = n.userInfo?[NSWorkspace.applicationUserInfoKey] as? NSRunningApplication, isCandidate(app) else { return }
  // 起動し直しのために止めた直後にも来るが、build は記録と同じなら何もしないので気にしない
  DispatchQueue.global().async { run(["build", "--quiet"]) }
}
log("watching \(appsDir)/Chrome *.app")
RunLoop.main.run()
