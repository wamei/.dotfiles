// Chrome のアイコンの右上にプロファイル写真を丸く重ね、複製のカスタムアイコンにする
//   chrome-profiles-icon <Chrome.app> <写真 or -> <複製.app> [確認用 PNG]
// 写真が - なら重ねずに Chrome のアイコンをそのまま付ける
import AppKit

let args = CommandLine.arguments
guard args.count == 4 || args.count == 5 else {
  FileHandle.standardError.write("usage: chrome-profiles-icon <Chrome.app> <picture|-> <target.app> [preview.png]\n".data(using: .utf8)!)
  exit(2)
}
let (chromeApp, picturePath, target) = (args[1], args[2], args[3])

let size: CGFloat = 1024
let base = NSWorkspace.shared.icon(forFile: chromeApp)
let picture: NSImage? = picturePath == "-" ? nil : NSImage(contentsOfFile: picturePath)
if picturePath != "-" && picture == nil {
  FileHandle.standardError.write("写真を読めません: \(picturePath)\n".data(using: .utf8)!)
  exit(1)
}

guard let rep = NSBitmapImageRep(
  bitmapDataPlanes: nil, pixelsWide: Int(size), pixelsHigh: Int(size), bitsPerSample: 8, samplesPerPixel: 4,
  hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)
else { exit(1) }
NSGraphicsContext.saveGraphicsState()
NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)
NSGraphicsContext.current?.imageInterpolation = .high
base.draw(in: NSRect(x: 0, y: 0, width: size, height: size))
if let picture {
  // macOS のアイコンは四辺に 1 割ほどの余白があるので、本体の右上の角に掛かる位置に置く
  let d: CGFloat = 470, ring: CGFloat = 22
  let circle = NSRect(x: size - d - 40, y: size - d - 40, width: d, height: d)
  NSGraphicsContext.saveGraphicsState()
  let shadow = NSShadow()
  shadow.shadowColor = NSColor.black.withAlphaComponent(0.35)
  shadow.shadowBlurRadius = 24
  shadow.shadowOffset = NSSize(width: 0, height: -8)
  shadow.set()
  NSColor.white.setFill()
  NSBezierPath(ovalIn: circle).fill()
  NSGraphicsContext.restoreGraphicsState()
  let inner = circle.insetBy(dx: ring, dy: ring)
  NSGraphicsContext.saveGraphicsState()
  NSBezierPath(ovalIn: inner).addClip()
  picture.draw(in: inner, from: .zero, operation: .sourceOver, fraction: 1)
  NSGraphicsContext.restoreGraphicsState()
}
NSGraphicsContext.restoreGraphicsState()

let image = NSImage(size: NSSize(width: size, height: size))
image.addRepresentation(rep)
if args.count == 5, let png = rep.representation(using: .png, properties: [:]) {
  try png.write(to: URL(fileURLWithPath: args[4]))
}
guard NSWorkspace.shared.setIcon(image, forFile: target, options: []) else {
  FileHandle.standardError.write("アイコンを付けられません: \(target)\n".data(using: .utf8)!)
  exit(1)
}
