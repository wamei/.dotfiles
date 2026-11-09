// リンクを受けて、リンクを押したアプリ (Finicky が --opener で渡す) の窓のタイトルを添えて chrome-profiles open --title に渡す。
// Finicky が Teams などから来たリンクを open -n -a <このアプリ> <URL> --args --opener <bundleId> で回してくる。
// Finicky には開いたアプリしか届かず、どの組織 (ワークスペース) の画面で押したかが分からないので、窓のタイトルを読む。
// タイトルを読むにはこのアプリに「アクセシビリティ」の許可が要る。無ければ空のタイトルで渡す (既定のブラウザで開く)
// カレンダー.app から来たリンクは、窓のタイトルではなく、そのリンクを持つ予定の主催者のメールアドレスを
// chrome-profiles open --email に渡す。予定を読むには「カレンダー」の許可が要る (初回にダイアログが出る)
import AppKit
import ApplicationServices
import EventKit

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

let calendarAppId = "com.apple.iCal"
let eventStore = EKEventStore()

// 予定の参加者のメールアドレス。url は mailto: のことが多い。そうでなければ (Exchange など) emailAddress を見る
func emailOf(_ participant: EKParticipant) -> String {
  if participant.url.scheme?.lowercased() == "mailto" {
    let s = participant.url.absoluteString.dropFirst("mailto:".count)
    return String(s).removingPercentEncoding ?? String(s)
  }
  let key = "emailAddress"
  if participant.responds(to: NSSelectorFromString(key)), let s = participant.value(forKey: key) as? String { return s }
  return ""
}

// url を URL・メモ・場所のどれかに含む予定のうち、今に一番近いものの主催者。
// カレンダー.app は押された予定を教えてくれないので、リンクから予定を引き当てる
func organizerEmail(for url: URL) -> String {
  let now = Date()
  let day: TimeInterval = 24 * 60 * 60
  let predicate = eventStore.predicateForEvents(withStart: now - 60 * day, end: now + 180 * day, calendars: nil)
  let target = url.absoluteString
  let needles = Set([target, target.removingPercentEncoding ?? target])
  let hits = eventStore.events(matching: predicate).filter { e in
    [e.url?.absoluteString, e.notes, e.location].compactMap { $0 }.contains { text in needles.contains { text.contains($0) } }
  }
  guard let event = hits.min(by: { abs($0.startDate.timeIntervalSince(now)) < abs($1.startDate.timeIntervalSince(now)) }) else {
    log("リンクを持つ予定が見つかりません")
    return ""
  }
  guard let organizer = event.organizer else {
    log("予定に主催者がありません")
    return ""
  }
  return emailOf(organizer)
}

// カレンダーの許可を確かめてから done を呼ぶ。初回は許可を求めるダイアログを出す
// (Dock に出ないアプリのままだとダイアログが前に出ないことがあるので、そのときだけ前面に出る)
func withCalendarAccess(_ done: @escaping (Bool) -> Void) {
  switch EKEventStore.authorizationStatus(for: .event) {
  case .fullAccess: done(true)
  case .notDetermined:
    NSApp.setActivationPolicy(.accessory)
    NSApp.activate(ignoringOtherApps: true)
    eventStore.requestFullAccessToEvents { granted, error in
      if let error { log("カレンダーの許可を求められません: \(error)") }
      DispatchQueue.main.async { done(granted) }
    }
  default: done(false)
  }
}

func run(_ args: [String], _ urls: [URL], _ detail: String) {
  let p = Process()
  p.executableURL = URL(fileURLWithPath: cli)
  p.arguments = ["open"] + args + urls.map(\.absoluteString)
  do {
    try p.run()
    p.waitUntilExit()
    log("\(p.terminationStatus == 0 ? "開きました" : "失敗しました (\(p.terminationStatus))"): app=\(openerId) \(detail)")
  } catch {
    log("CLI を起動できません: \(cli): \(error)")
  }
}

func route(_ urls: [URL], done: @escaping () -> Void) {
  guard !urls.isEmpty else { return done() }
  if openerId == calendarAppId, let first = urls.first {
    withCalendarAccess { granted in
      if !granted { log("カレンダーの許可が無いので予定の主催者を読めません") }
      let email = granted ? organizerEmail(for: first) : ""
      run(["--email", email], urls, "email=\(email)")
      done()
    }
    return
  }
  let (_, title) = openerWindowTitle()
  run(["--title", title], urls, "title=\(title)")
  done()
}

final class Delegate: NSObject, NSApplicationDelegate {
  var quit: DispatchWorkItem?

  func application(_ application: NSApplication, open urls: [URL]) {
    quit?.cancel()
    // カレンダーの許可のダイアログに答えるまで待つことがあるので、終わってから終了を予約する
    route(urls) { self.scheduleQuit() }
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
