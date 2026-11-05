// リンクを受けて、リンクを押したアプリ (Finicky が --opener で渡す) の窓のタイトルを添えて chrome-profiles open --title に渡す。
// Finicky が Teams などから来たリンクを open -n -a <このアプリ> <URL> --args --opener <bundleId> で回してくる。
// Finicky には開いたアプリしか届かず、どの組織 (ワークスペース) の画面で押したかが分からないので、窓のタイトルを読む。
// タイトルを読むにはこのアプリに「アクセシビリティ」の許可が要る。無ければ空のタイトルで渡す (既定のブラウザで開く)
import AppKit
import ApplicationServices

let home = FileManager.default.homeDirectoryForCurrentUser.path
let cli = "\(home)/.local/bin/chrome-profiles"
let logFile = "\(home)/.local/state/chrome-profiles/window.log"

func log(_ s: String) {
  let line = "\(ISO8601DateFormatter().string(from: Date())) \(s)\n"
  if let h = FileHandle(forWritingAtPath: logFile) {
    h.seekToEndOfFile()
    h.write(line.data(using: .utf8)!)
    h.closeFile()
  } else {
    FileManager.default.createFile(atPath: logFile, contents: line.data(using: .utf8))
  }
}

func attribute(_ element: AXUIElement, _ name: String) -> CFTypeRef? {
  var value: CFTypeRef?
  return AXUIElementCopyAttributeValue(element, name as CFString, &value) == .success ? value : nil
}

// Finicky が --opener <bundleId> で渡す、リンクを押したアプリ。
// Finicky はリンクを受けると自分を前面に出すので、前面のアプリを見ても押したアプリは分からない
let openerId: String = {
  let args = CommandLine.arguments
  guard let i = args.firstIndex(of: "--opener"), i + 1 < args.count else { return "" }
  return args[i + 1]
}()

// リンクを押したアプリの、前面の窓のタイトル (押した直後なので、その画面の窓)
func openerWindowTitle() -> (app: String, title: String) {
  let appId = openerId
  guard !appId.isEmpty, let opener = NSRunningApplication.runningApplications(withBundleIdentifier: appId).first else { return (appId, "") }
  // 初回は許可を求めるダイアログを出す (出ないこともあるので、そのときはシステム設定で足す)
  guard AXIsProcessTrustedWithOptions([kAXTrustedCheckOptionPrompt.takeUnretainedValue() as String: true] as CFDictionary) else {
    log("アクセシビリティの許可が無いので窓のタイトルを読めません")
    return (appId, "")
  }
  let app = AXUIElementCreateApplication(opener.processIdentifier)
  for name in [kAXFocusedWindowAttribute, kAXMainWindowAttribute] {
    if let window = attribute(app, name), let title = attribute(window as! AXUIElement, kAXTitleAttribute) as? String {
      return (appId, title)
    }
  }
  return (appId, "")
}

func route(_ urls: [URL]) {
  guard !urls.isEmpty else { return }
  let (appId, title) = openerWindowTitle()
  let p = Process()
  p.executableURL = URL(fileURLWithPath: cli)
  p.arguments = ["open", "--title", title] + urls.map(\.absoluteString)
  do {
    try p.run()
    p.waitUntilExit()
    log("\(p.terminationStatus == 0 ? "開きました" : "失敗しました (\(p.terminationStatus))"): app=\(appId) title=\(title)")
  } catch {
    log("CLI を起動できません: \(cli): \(error)")
  }
}

final class Delegate: NSObject, NSApplicationDelegate {
  var quit: DispatchWorkItem?

  func application(_ application: NSApplication, open urls: [URL]) {
    route(urls)
    scheduleQuit()
  }

  func applicationDidFinishLaunching(_ notification: Notification) {
    // URL を受け取らずに起動されたときも残らないようにする
    scheduleQuit()
  }

  // 続けてリンクを押されたときのために、最後のリンクから少し待ってから終わる
  func scheduleQuit() {
    quit?.cancel()
    let item = DispatchWorkItem { NSApp.terminate(nil) }
    quit = item
    DispatchQueue.main.asyncAfter(deadline: .now() + 3, execute: item)
  }
}

let app = NSApplication.shared
let delegate = Delegate()
app.delegate = delegate
app.setActivationPolicy(.prohibited)
app.run()
