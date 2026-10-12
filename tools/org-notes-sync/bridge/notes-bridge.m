// notes-bridge: NotesShared.framework (非公開) を dlopen し、メモ.app の DB を JSON で読み出す小さな CLI。
//
// 使い方 (出力は stdout に JSON 1 つ。失敗時は stderr にメッセージ):
//   notes-bridge [--store <dir>] selfcheck                      → {"ok":true}
//   notes-bridge [--store <dir>] list --folder <name>           → {"folderMissing":false,"notes":[{"id","uri","doc"} | {"id","uri","locked":true}]}
//   notes-bridge [--store <dir>] read --id <id>                 → {"doc":Doc}
//   notes-bridge [--store <dir>] identify --uri <x-coredata>    → {"id":"…"}
//   notes-bridge [--store <dir>] export-media --attachment <id> --to <path.jpg> → {"written":true}
// --store はどこに置いてもよい (サブコマンドの後ろでも効く)。知らないオプション・重複・空の値は終了コード 1。
// 終了コード: 3 = selfcheck 失敗 (欠けた名前を stderr に並べる)、4 = DB を開けない、1 = その他。
//
// 実物で確かめた事実 (2026-10-13, macOS 26, DB のコピーに対して class_copyMethodList と
// NSManagedObjectModel の entitiesByName で調べた):
// - ICAccount: Core Data 属性 accountType (NSNumber) と name。iCloud アカウントは accountType == 1、
//   name == "iCloud"。-isPrimaryiCloudAccount はコピーでは NO を返すので使わない。
// - ICFolder: 属性 title / markedForDeletion / folderType、関係 account / parent。
//   「最近削除した項目」も ICFolder (title "Recently Deleted"、-isTrashFolder == YES)。
// - ICNote: 属性 identifier (UUID 文字列) / title / markedForDeletion / isPasswordProtected
//   (ロックされたメモ。中身は暗号化されているので読まない)、関係 folder / account / attachments。
//   -attributedString が段落と装飾を返す。objectID.URIRepresentation が x-coredata://…/ICNote/p<N>。
// - ICAttachment: 属性 identifier / typeUTI、関係 media (ICMedia、表 com.apple.notes.table は nil)。
//   ICMedia: 属性 filename、-mediaURL が実体ファイルの file URL
//   (~/Library/Group Containers/group.com.apple.notes/Accounts/<アカウントID>/Media/<メディアID>/<…>/<filename>)。
//   mediaURL は setApplicationDocumentsURL: に従わず常に本物のグループコンテナを指すので、
//   --store のときは差し替える前の +applicationDocumentsURL (= グループコンテナ) からの相対パスを
//   store に付け替える。+applicationDataContainerURL は ~/Library/Containers/com.apple.Notes/Data で別物。
// - AppleScript や HTML 経由で作られたメモは 1 段落目が style=3 (body) のことがある
//   (書式は ICTTFont で付く)。ここでは TTStyle をそのまま Doc にする。
// - attributedString の属性キー:
//   TTStyle = ICTTParagraphStyle (-style: unsigned int、-indent / -blockQuoteLevel: NSUInteger、
//   -todo: ICTTTodo (-done: BOOL))、TTHints (1 bold, 2 italic)、TTUnderline、TTStrikethrough、
//   TTEmphasis (ハイライト。Doc には持たない)、NSLink (NSString / NSURL)、
//   NSAttachment = ICTTAttachment (-attachmentUTI / -attachmentIdentifier)、文字は U+FFFC。
#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>
#import <UniformTypeIdentifiers/UniformTypeIdentifiers.h>
#import <objc/message.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <unistd.h>

#define SEL_(x) NSSelectorFromString(@x)

static const char *kFramework = "/System/Library/PrivateFrameworks/NotesShared.framework/NotesShared";
static const NSInteger kICloudAccountType = 1;

// ---- 出力と失敗 ----

static void emit(id obj) {
  NSError *e = nil;
  NSData *d = [NSJSONSerialization dataWithJSONObject:obj options:NSJSONWritingWithoutEscapingSlashes error:&e];
  if (!d) { fprintf(stderr, "JSON にできない: %s\n", e.description.UTF8String); exit(1); }
  fwrite(d.bytes, 1, d.length, stdout);
  fputc('\n', stdout);
  fflush(stdout);
}

static void fail(int code, NSString *msg) {
  fprintf(stderr, "notes-bridge: %s\n", msg.UTF8String);
  exit(code);
}

// ---- selfcheck: 使うクラスとセレクタ ----

// 各要素は { クラス名, +クラスメソッド... , -インスタンスメソッド... }
static NSArray<NSArray<NSString *> *> *requiredNames(void) {
  return @[
    @[@"ICPaths", @"+setApplicationDocumentsURL:", @"+persistentStoreURL", @"+applicationDocumentsURL"],
    @[@"ICNoteContext", @"+startSharedContextWithOptions:", @"+sharedContext", @"-managedObjectContext"],
    @[@"ICNote", @"-attributedString"],
    @[@"ICFolder", @"-isTrashFolder"],
    @[@"ICMedia", @"-mediaURL"],
    @[@"ICTTMergeableAttributedString"],
    @[@"ICTTParagraphStyle", @"-style", @"-indent", @"-blockQuoteLevel", @"-todo"],
    @[@"ICTTTodo", @"-done"],
    @[@"ICTTAttachment", @"-attachmentUTI", @"-attachmentIdentifier"],
  ];
}

static NSArray<NSString *> *missingNames(void) {
  NSMutableArray *missing = [NSMutableArray array];
  for (NSArray<NSString *> *entry in requiredNames()) {
    Class c = NSClassFromString(entry[0]);
    if (!c) { [missing addObject:entry[0]]; continue; }
    for (NSString *m in [entry subarrayWithRange:NSMakeRange(1, entry.count - 1)]) {
      SEL s = NSSelectorFromString([m substringFromIndex:1]);
      BOOL ok = [m hasPrefix:@"+"] ? [c respondsToSelector:s] : [c instancesRespondToSelector:s];
      if (!ok) [missing addObject:[NSString stringWithFormat:@"%@[%@ %@]", [m substringToIndex:1], entry[0], [m substringFromIndex:1]]];
    }
  }
  return missing;
}

static void requireNames(void) {
  NSArray *missing = missingNames();
  if (missing.count == 0) return;
  for (NSString *n in missing) fprintf(stderr, "%s\n", n.UTF8String);
  fail(3, @"NotesShared に必要なクラス・セレクタが無い");
}

// ---- DB を開く ----

// --store で差し替える前の書類の場所 (本物のグループコンテナ)
static NSString *liveDocumentsPath;

// シンボリックリンクを解いた絶対パス (/tmp → /private/tmp)。無ければ nil
static NSString *realPath(NSString *p) {
  char buf[PATH_MAX];
  if (!realpath(p.fileSystemRepresentation, buf)) return nil;
  return [[NSFileManager defaultManager] stringWithFileSystemRepresentation:buf length:strlen(buf)];
}

// --store のとき、開いた DB がすべて store の中にあることを確かめる (本物を開いていたら止める)
static void requireStoreUnder(NSManagedObjectContext *moc, NSString *store) {
  NSString *prefix = [store stringByAppendingString:@"/"];
  for (NSPersistentStore *ps in moc.persistentStoreCoordinator.persistentStores) {
    NSString *p = realPath(ps.URL.path);
    if (!p || ![p hasPrefix:prefix]) fail(4, [NSString stringWithFormat:@"--store の外の DB を開いた: %@", ps.URL.path]);
  }
}

static NSManagedObjectContext *openContextUnsafe(NSString *store) {
  Class paths = NSClassFromString(@"ICPaths");
  liveDocumentsPath = [[paths performSelector:SEL_("applicationDocumentsURL")] path];
  if (store) {
    // 無いところを指すと NotesShared が空の DB を作ってしまうので、先に確かめる
    NSString *db = [store stringByAppendingPathComponent:@"NoteStore.sqlite"];
    if (![[NSFileManager defaultManager] fileExistsAtPath:db]) fail(4, [NSString stringWithFormat:@"DB が無い: %@", db]);
    [paths performSelector:SEL_("setApplicationDocumentsURL:") withObject:[NSURL fileURLWithPath:store isDirectory:YES]];
  } else {
    NSURL *db = [paths performSelector:SEL_("persistentStoreURL")];
    if (![[NSFileManager defaultManager] isReadableFileAtPath:db.path])
      fail(4, [NSString stringWithFormat:@"DB を読めない (フルディスクアクセスが必要): %@", db.path]);
  }
  Class ctxClass = NSClassFromString(@"ICNoteContext");
  ((void (*)(id, SEL, NSUInteger))objc_msgSend)(ctxClass, SEL_("startSharedContextWithOptions:"), 0);
  id ctx = [ctxClass performSelector:SEL_("sharedContext")];
  NSManagedObjectContext *moc = [ctx performSelector:SEL_("managedObjectContext")];
  if (!moc || moc.persistentStoreCoordinator.persistentStores.count == 0) fail(4, @"DB を開けない");
  if (store) requireStoreUnder(moc, store);
  return moc;
}

// store は realPath 済みのもの
static NSManagedObjectContext *openContext(NSString *store) {
  @try {
    return openContextUnsafe(store);
  } @catch (NSException *ex) {
    fail(4, [NSString stringWithFormat:@"DB を開けない: %@: %@", ex.name, ex.reason]);
    return nil;
  }
}

static NSArray *fetch(NSManagedObjectContext *moc, NSString *entity, NSPredicate *pred, NSArray *sort) {
  NSFetchRequest *fr = [NSFetchRequest fetchRequestWithEntityName:entity];
  fr.predicate = pred;
  fr.sortDescriptors = sort;
  NSError *e = nil;
  NSArray *rs = [moc executeFetchRequest:fr error:&e];
  if (!rs) fail(4, [NSString stringWithFormat:@"%@ を読めない: %@", entity, e.localizedDescription]);
  return rs;
}

// ---- attributedString → Doc ----

static BOOL isImageUTI(NSString *uti) {
  static NSSet *known;
  if (!known) known = [NSSet setWithArray:@[@"public.jpeg", @"public.png", @"public.heic", @"public.heif", @"public.tiff", @"public.gif"]];
  if (!uti) return NO;
  if ([known containsObject:uti]) return YES;
  UTType *t = [UTType typeWithIdentifier:uti];
  return t && [t conformsToType:UTTypeImage];
}

static NSDictionary *paraStyleJSON(id ps) {
  NSUInteger code = ps ? [[ps valueForKey:@"style"] unsignedIntegerValue] : 3;
  switch (code) {
    case 0: return @{@"kind": @"title"};
    case 1: return @{@"kind": @"heading"};
    case 2: return @{@"kind": @"subheading"};
    case 4: return @{@"kind": @"mono"};
    case 100: return @{@"kind": @"list", @"list": @"bullet"};
    case 101: return @{@"kind": @"list", @"list": @"dash"};
    case 102: return @{@"kind": @"list", @"list": @"number"};
    case 103: {
      id todo = [ps valueForKey:@"todo"];
      return @{@"kind": @"check", @"done": @((BOOL)(todo && [[todo valueForKey:@"done"] boolValue]))};
    }
    default: return @{@"kind": @"body"};
  }
}

static NSArray<NSString *> *marksOf(NSDictionary *at) {
  NSMutableArray *marks = [NSMutableArray array];
  NSInteger hints = [at[@"TTHints"] integerValue];
  if (hints & 1) [marks addObject:@"bold"];
  if (hints & 2) [marks addObject:@"italic"];
  if ([at[@"TTUnderline"] integerValue] != 0) [marks addObject:@"underline"];
  if ([at[@"TTStrikethrough"] integerValue] != 0) [marks addObject:@"strike"];
  return marks;
}

static NSString *linkOf(NSDictionary *at) {
  id l = at[@"NSLink"];
  if ([l isKindOfClass:[NSURL class]]) return [l absoluteString];
  if ([l isKindOfClass:[NSString class]]) return l;
  return nil;
}

// 同じ marks / link の文字列が続けば 1 つの inline にまとめる
static void appendText(NSMutableArray *inlines, NSString *text, NSArray *marks, NSString *link) {
  if (text.length == 0) return;
  NSMutableDictionary *prev = inlines.lastObject;
  if (prev && [prev[@"kind"] isEqual:@"text"] && [prev[@"marks"] isEqual:marks] &&
      ((prev[@"link"] == nil && link == nil) || [prev[@"link"] isEqual:link])) {
    prev[@"text"] = [prev[@"text"] stringByAppendingString:text];
    return;
  }
  NSMutableDictionary *t = [@{@"kind": @"text", @"text": text, @"marks": marks} mutableCopy];
  if (link) t[@"link"] = link;
  [inlines addObject:t];
}

static NSArray *inlinesIn(NSAttributedString *as, NSRange range) {
  NSMutableArray *inlines = [NSMutableArray array];
  NSString *s = as.string;
  Class attClass = NSClassFromString(@"ICTTAttachment");
  [as enumerateAttributesInRange:range options:0 usingBlock:^(NSDictionary *at, NSRange r, BOOL *stop) {
    NSArray *marks = marksOf(at);
    NSString *link = linkOf(at);
    id att = at[@"NSAttachment"];
    if (![att isKindOfClass:attClass]) { appendText(inlines, [s substringWithRange:r], marks, link); return; }
    // 添付は U+FFFC 1 文字に 1 つ。同じ属性の連なりに他の文字が混じっても文字として残す
    NSUInteger start = r.location;
    for (NSUInteger i = r.location; i < NSMaxRange(r); i++) {
      if ([s characterAtIndex:i] != 0xFFFC) continue;
      appendText(inlines, [s substringWithRange:NSMakeRange(start, i - start)], marks, link);
      NSString *uti = [att valueForKey:@"attachmentUTI"];
      [inlines addObject:@{@"kind": @"attachment", @"id": [att valueForKey:@"attachmentIdentifier"] ?: @"", @"image": @(isImageUTI(uti))}];
      start = i + 1;
    }
    appendText(inlines, [s substringWithRange:NSMakeRange(start, NSMaxRange(r) - start)], marks, link);
  }];
  return inlines;
}

static NSDictionary *docFrom(NSAttributedString *as) {
  NSMutableArray *paragraphs = [NSMutableArray array];
  NSString *s = as.string ?: @"";
  NSUInteger len = s.length, start = 0;
  // \n で分割する。末尾の \n の後の空要素は作らない
  while (start < len) {
    NSRange nl = [s rangeOfString:@"\n" options:NSLiteralSearch range:NSMakeRange(start, len - start)];
    NSUInteger end = nl.location == NSNotFound ? len : nl.location;
    // 段落スタイルは段落の最初の文字 (空段落ならその \n) から取る
    id ps = [as attribute:@"TTStyle" atIndex:start effectiveRange:NULL];
    NSUInteger indent = ps ? [[ps valueForKey:@"indent"] unsignedIntegerValue] : 0;
    BOOL quote = ps ? [[ps valueForKey:@"blockQuoteLevel"] unsignedIntegerValue] > 0 : NO;
    [paragraphs addObject:@{
      @"style": paraStyleJSON(ps),
      @"indent": @(indent),
      @"quote": @(quote),
      @"inlines": inlinesIn(as, NSMakeRange(start, end - start)),
    }];
    start = end + 1;
  }
  return @{@"paragraphs": paragraphs};
}

static NSDictionary *docOfNote(id note) {
  NSAttributedString *as = [note performSelector:SEL_("attributedString")];
  return docFrom(as ?: [[NSAttributedString alloc] init]);
}

// ---- サブコマンド ----

static NSPredicate *notDeleted(void) {
  return [NSPredicate predicateWithFormat:@"markedForDeletion == NO"];
}

static BOOL isLocked(id note) {
  return [[note valueForKey:@"isPasswordProtected"] boolValue];
}

static void cmdList(NSManagedObjectContext *moc, NSString *folderName) {
  NSPredicate *fp = [NSPredicate predicateWithFormat:@"title == %@ AND markedForDeletion == NO AND account.accountType == %ld",
                     folderName, (long)kICloudAccountType];
  NSMutableArray *folders = [NSMutableArray array];
  for (id f in fetch(moc, @"ICFolder", fp, nil))
    if (!((BOOL (*)(id, SEL))objc_msgSend)(f, SEL_("isTrashFolder"))) [folders addObject:f];
  if (folders.count == 0) { emit(@{@"folderMissing": @YES, @"notes": @[]}); return; }
  NSPredicate *np = [NSCompoundPredicate andPredicateWithSubpredicates:@[
    notDeleted(), [NSPredicate predicateWithFormat:@"folder IN %@", folders]]];
  NSMutableArray *notes = [NSMutableArray array];
  for (id n in fetch(moc, @"ICNote", np, @[[NSSortDescriptor sortDescriptorWithKey:@"identifier" ascending:YES]])) {
    NSString *nid = [n valueForKey:@"identifier"];
    NSString *uri = [[n objectID] URIRepresentation].absoluteString;
    [notes addObject:isLocked(n) ? @{@"id": nid, @"uri": uri, @"locked": @YES}
                                 : @{@"id": nid, @"uri": uri, @"doc": docOfNote(n)}];
  }
  emit(@{@"folderMissing": @NO, @"notes": notes});
}

static void cmdRead(NSManagedObjectContext *moc, NSString *noteId) {
  NSPredicate *p = [NSCompoundPredicate andPredicateWithSubpredicates:@[
    notDeleted(), [NSPredicate predicateWithFormat:@"identifier == %@", noteId]]];
  id note = fetch(moc, @"ICNote", p, nil).firstObject;
  if (!note) fail(1, [NSString stringWithFormat:@"メモが無い: %@", noteId]);
  if (isLocked(note)) fail(1, [NSString stringWithFormat:@"ロックされたメモは読まない: %@", noteId]);
  emit(@{@"doc": docOfNote(note)});
}

static void cmdIdentify(NSManagedObjectContext *moc, NSString *uri) {
  NSURL *url = [NSURL URLWithString:uri];
  NSManagedObjectID *oid = url ? [moc.persistentStoreCoordinator managedObjectIDForURIRepresentation:url] : nil;
  NSError *e = nil;
  NSManagedObject *obj = oid ? [moc existingObjectWithID:oid error:&e] : nil;
  if (!obj || ![obj.entity.name isEqual:@"ICNote"] || [[obj valueForKey:@"markedForDeletion"] boolValue]) fail(1, [NSString stringWithFormat:@"メモが無い: %@", uri]);
  emit(@{@"id": [obj valueForKey:@"identifier"]});
}

// mediaURL は常に本物のグループコンテナを指す。--store のときはコピー側へ付け替える
static NSString *mediaPathIn(NSString *store, NSURL *mediaURL) {
  if (!store) return mediaURL.path;
  NSString *p = mediaURL.path;
  NSString *prefix = [liveDocumentsPath stringByAppendingString:@"/"];
  if (![p hasPrefix:prefix]) fail(1, [NSString stringWithFormat:@"添付の実体がグループコンテナの外にある: %@", p]);
  return [store stringByAppendingPathComponent:[p substringFromIndex:prefix.length]];
}

static void cmdExportMedia(NSManagedObjectContext *moc, NSString *store, NSString *attId, NSString *to) {
  NSPredicate *p = [NSCompoundPredicate andPredicateWithSubpredicates:@[
    notDeleted(), [NSPredicate predicateWithFormat:@"identifier == %@", attId]]];
  id att = fetch(moc, @"ICAttachment", p, nil).firstObject;
  if (!att) fail(1, [NSString stringWithFormat:@"添付が無い: %@", attId]);
  id media = [att valueForKey:@"media"];
  NSURL *url = media ? [media performSelector:SEL_("mediaURL")] : nil;
  if (!url) fail(1, [NSString stringWithFormat:@"添付に実体が無い: %@", attId]);
  NSString *src = mediaPathIn(store, url);
  NSFileManager *fm = [NSFileManager defaultManager];
  if (![fm fileExistsAtPath:src]) fail(1, [NSString stringWithFormat:@"添付の実体ファイルが無い: %@", src]);
  BOOL isDir = NO;
  if ([fm fileExistsAtPath:to isDirectory:&isDir] && isDir) fail(1, [NSString stringWithFormat:@"書き出し先がディレクトリ: %@", to]);
  // 隣の一時ファイルに書き、成功したときだけ rename で置き換える (失敗しても既存のファイルは残る)
  NSString *tmp = [[to stringByDeletingLastPathComponent] stringByAppendingPathComponent:
                   [NSString stringWithFormat:@".%@.%d.tmp.jpg", to.lastPathComponent, getpid()]];
  [fm removeItemAtPath:tmp error:nil];
  NSString *err = nil;
  NSString *uti = [att valueForKey:@"typeUTI"];
  if ([uti isEqual:@"public.jpeg"]) {
    NSError *e = nil;
    if (![fm copyItemAtPath:src toPath:tmp error:&e]) err = [NSString stringWithFormat:@"コピーできない: %@", e.localizedDescription];
  } else {
    NSTask *t = [[NSTask alloc] init];
    t.executableURL = [NSURL fileURLWithPath:@"/usr/bin/sips"];
    t.arguments = @[@"-s", @"format", @"jpeg", src, @"--out", tmp];
    t.standardOutput = [NSFileHandle fileHandleWithNullDevice];
    t.standardError = [NSFileHandle fileHandleWithNullDevice];
    NSError *e = nil;
    if (![t launchAndReturnError:&e]) err = [NSString stringWithFormat:@"sips を起動できない: %@", e.localizedDescription];
    else {
      [t waitUntilExit];
      if (t.terminationStatus != 0 || ![fm fileExistsAtPath:tmp])
        err = [NSString stringWithFormat:@"sips が JPEG にできない (%d): %@", t.terminationStatus, src];
    }
  }
  if (!err && rename(tmp.fileSystemRepresentation, to.fileSystemRepresentation) != 0)
    err = [NSString stringWithFormat:@"置き換えられない: %s", strerror(errno)];
  if (err) { [fm removeItemAtPath:tmp error:nil]; fail(1, err); }
  emit(@{@"written": @YES});
}

// ---- 引数 ----

static void usage(void) {
  fail(1, @"使い方: notes-bridge [--store <dir>] (selfcheck | list --folder <name> | read --id <id> | "
          @"identify --uri <uri> | export-media --attachment <id> --to <path>)");
}

// サブコマンドごとに受け付けるオプション (--store はどこでも受け付ける)
static NSDictionary<NSString *, NSArray<NSString *> *> *commandOptions(void) {
  return @{
    @"selfcheck": @[],
    @"list": @[@"folder"],
    @"read": @[@"id"],
    @"identify": @[@"uri"],
    @"export-media": @[@"attachment", @"to"],
  };
}

static NSString *option(NSDictionary *opts, NSString *key) {
  NSString *v = opts[key];
  if (!v) fail(1, [NSString stringWithFormat:@"--%@ が要る", key]);
  return v;
}

int main(int argc, char **argv) {
  @autoreleasepool {
    NSString *cmd = nil;
    NSMutableDictionary<NSString *, NSString *> *opts = [NSMutableDictionary dictionary];
    for (int i = 1; i < argc; i++) {
      NSString *a = @(argv[i]);
      if (![a hasPrefix:@"--"]) {
        if (cmd) fail(1, [NSString stringWithFormat:@"余分な引数: %@", a]);
        cmd = a;
        continue;
      }
      NSString *key = [a substringFromIndex:2];
      if (i + 1 >= argc) fail(1, [NSString stringWithFormat:@"%@ に値が無い", a]);
      if (opts[key]) fail(1, [NSString stringWithFormat:@"%@ が重複している", a]);
      opts[key] = @(argv[++i]);
    }
    if (!cmd) usage();
    NSArray *allowed = commandOptions()[cmd];
    if (!allowed) usage();
    for (NSString *key in opts)
      if (![key isEqual:@"store"] && ![allowed containsObject:key])
        fail(1, [NSString stringWithFormat:@"%@ は --%@ を受け付けない", cmd, key]);
    NSString *store = nil;
    if (opts[@"store"]) {
      if (opts[@"store"].length == 0) fail(1, @"--store が空");
      store = realPath(opts[@"store"]);
      if (!store) fail(4, [NSString stringWithFormat:@"--store が無い: %@", opts[@"store"]]);
    }

    if (!dlopen(kFramework, RTLD_NOW)) fail(3, [NSString stringWithFormat:@"NotesShared を読み込めない: %s", dlerror()]);
    requireNames();
    if ([cmd isEqual:@"selfcheck"]) { emit(@{@"ok": @YES}); return 0; }

    NSManagedObjectContext *moc = openContext(store);
    __block int code = 0;
    [moc performBlockAndWait:^{
      @try {
        if ([cmd isEqual:@"list"]) cmdList(moc, option(opts, @"folder"));
        else if ([cmd isEqual:@"read"]) cmdRead(moc, option(opts, @"id"));
        else if ([cmd isEqual:@"identify"]) cmdIdentify(moc, option(opts, @"uri"));
        else cmdExportMedia(moc, store, option(opts, @"attachment"), option(opts, @"to"));
      } @catch (NSException *ex) {
        fprintf(stderr, "notes-bridge: %s: %s\n", ex.name.UTF8String, ex.reason.UTF8String);
        code = 1;
      }
    }];
    return code;
  }
}
