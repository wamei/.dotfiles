// notes-bridge: NotesShared.framework (非公開) を dlopen し、メモ.app の DB を JSON で読み出す小さな CLI。
//
// 使い方 (出力は stdout に JSON 1 つ。失敗時は stderr にメッセージ):
//   notes-bridge [--store <dir>] selfcheck                      → {"ok":true}
//   notes-bridge [--store <dir>] list --folder <name>           → {"folderMissing":false,"notes":[{"id","uri","doc"} | {"id","uri","locked":true}]}
//   notes-bridge [--store <dir>] read --id <id>                 → {"doc":Doc}
//   notes-bridge [--store <dir>] identify --uri <x-coredata>    → {"id":"…"}
//   notes-bridge [--store <dir>] export-media --attachment <id> --to <path.jpg> → {"written":true}
//   notes-bridge [--store <dir>] write --id <id>  (stdin: {"doc":Doc})  → {"doc":Doc} (書いた後に読み戻したもの)
//   notes-bridge [--store <dir>] dump-attrs --id <id>  → {"paragraphs":[["TTEmphasis","TTStyle"],…]} (デバッグ用)
// --store はどこに置いてもよい (サブコマンドの後ろでも効く)。知らないオプション・重複・空の値は終了コード 1。
// 終了コード: 3 = selfcheck 失敗 (欠けた名前を stderr に並べる)、4 = DB を開けない、
//   5 = write で添付の扱いを守れない (残すべき添付を消す・動かす、今のメモに無い添付を置く)、
//   6 = write で書き換えた結果 (保存前のメモ上の Doc) が目標と違う。保存せずに止める、1 = その他。
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
//   NSLink の実物は NSURL。TTTimestamp は CRDT が付けるので触らない。
// - 書き込み (write): -[ICNote mergeableString] は ICTTMergeableAttributedString (attributedString は
//   -[ICNote attributedString] と同じオブジェクト)。replaceCharactersInRange:withString: で入れた文字は
//   直前の文字の TTStyle と新しい TTTimestamp を受け継ぐ。saveNoteData → updateChangeCountWithReason: →
//   [moc save:] でメモ一覧のタイトル (ICNote.title) も 1 段落目から自動で作り直される (専用の呼び出しは要らない)。
//   ICTTTodo: -initWithIdentifier:(NSUUID) done:(BOOL) と -todoWithDone: (同じ uuid で done だけ変えた複製)。
//   1 つのチェック段落の文字はすべて同じ uuid の todo を持つ。
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
    @[@"ICNote", @"-attributedString", @"-mergeableString", @"-saveNoteData", @"-updateChangeCountWithReason:"],
    @[@"ICFolder", @"-isTrashFolder"],
    @[@"ICMedia", @"-mediaURL"],
    @[@"ICTTMergeableAttributedString", @"-beginEditing", @"-endEditing", @"-attributedString",
      @"-replaceCharactersInRange:withString:", @"-setAttributes:range:"],
    @[@"ICTTParagraphStyle", @"-style", @"-indent", @"-blockQuoteLevel", @"-todo",
      @"-mutableCopy", @"-setStyle:", @"-setIndent:", @"-setBlockQuoteLevel:", @"-setTodo:", @"-setUuid:"],
    @[@"ICTTTodo", @"-done", @"-initWithIdentifier:done:", @"-todoWithDone:"],
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

// 段落ごとの文字範囲 (\n を含まない)。\n で分割し、末尾の \n の後の空要素は作らない
static NSArray<NSValue *> *paragraphRanges(NSString *s) {
  NSMutableArray *ranges = [NSMutableArray array];
  NSUInteger len = s.length, start = 0;
  while (start < len) {
    NSRange nl = [s rangeOfString:@"\n" options:NSLiteralSearch range:NSMakeRange(start, len - start)];
    NSUInteger end = nl.location == NSNotFound ? len : nl.location;
    [ranges addObject:[NSValue valueWithRange:NSMakeRange(start, end - start)]];
    start = end + 1;
  }
  return ranges;
}

static NSDictionary *paragraphJSON(NSAttributedString *as, NSRange r) {
  // 段落スタイルは段落の最初の文字 (空段落ならその \n) から取る
  id ps = [as attribute:@"TTStyle" atIndex:r.location effectiveRange:NULL];
  NSUInteger indent = ps ? [[ps valueForKey:@"indent"] unsignedIntegerValue] : 0;
  BOOL quote = ps ? [[ps valueForKey:@"blockQuoteLevel"] unsignedIntegerValue] > 0 : NO;
  return @{
    @"style": paraStyleJSON(ps),
    @"indent": @(indent),
    @"quote": @(quote),
    @"inlines": inlinesIn(as, r),
  };
}

static NSDictionary *docFrom(NSAttributedString *as) {
  NSMutableArray *paragraphs = [NSMutableArray array];
  for (NSValue *v in paragraphRanges(as.string ?: @"")) [paragraphs addObject:paragraphJSON(as, v.rangeValue)];
  return @{@"paragraphs": paragraphs};
}

static NSDictionary *docOfNote(id note) {
  NSAttributedString *as = [note performSelector:SEL_("attributedString")];
  return docFrom(as ?: [[NSAttributedString alloc] init]);
}

// ---- 入力の Doc ----

static void badInput(NSString *what) {
  fail(1, [NSString stringWithFormat:@"入力の Doc が壊れている: %@", what]);
}

static BOOL isBool(id v) {
  return [v isKindOfClass:[NSNumber class]] && CFGetTypeID((__bridge CFTypeRef)v) == CFBooleanGetTypeID();
}

// 段落スタイルの JSON → ICTTParagraphStyle の style 値 (paraStyleJSON の逆)
static unsigned styleCode(NSDictionary *st) {
  NSString *k = st[@"kind"];
  if ([k isEqual:@"title"]) return 0;
  if ([k isEqual:@"heading"]) return 1;
  if ([k isEqual:@"subheading"]) return 2;
  if ([k isEqual:@"body"]) return 3;
  if ([k isEqual:@"mono"]) return 4;
  if ([k isEqual:@"check"]) return 103;
  if ([k isEqual:@"list"]) {
    NSString *l = st[@"list"];
    if ([l isEqual:@"bullet"]) return 100;
    if ([l isEqual:@"dash"]) return 101;
    if ([l isEqual:@"number"]) return 102;
  }
  badInput([NSString stringWithFormat:@"知らない段落スタイル %@", st]);
  return 3;
}

static NSArray<NSString *> *knownMarks(void) { return @[@"bold", @"italic", @"underline", @"strike"]; }

// 入力の段落を docFrom と同じ形 (キーと値の型) に作り直す。比較は isEqual: で行う
static NSDictionary *canonicalParagraph(id p) {
  if (![p isKindOfClass:[NSDictionary class]]) badInput(@"段落がオブジェクトでない");
  NSDictionary *st = p[@"style"];
  if (![st isKindOfClass:[NSDictionary class]]) badInput(@"style が無い");
  unsigned code = styleCode(st);
  NSDictionary *style;
  if (code == 103) {
    if (!isBool(st[@"done"])) badInput(@"check に done が無い");
    style = @{@"kind": @"check", @"done": @([st[@"done"] boolValue])};
  } else if (code >= 100) style = @{@"kind": @"list", @"list": st[@"list"]};
  else style = @{@"kind": st[@"kind"]};
  id indent = p[@"indent"];
  if (![indent isKindOfClass:[NSNumber class]] || isBool(indent) || [indent doubleValue] < 0 ||
      [indent doubleValue] != (double)[indent unsignedIntegerValue]) badInput(@"indent が 0 以上の整数でない");
  if (!isBool(p[@"quote"])) badInput(@"quote が真偽値でない");
  NSArray *inlines = p[@"inlines"];
  if (![inlines isKindOfClass:[NSArray class]]) badInput(@"inlines が配列でない");
  NSMutableArray *out = [NSMutableArray array];
  for (NSDictionary *i in inlines) {
    if (![i isKindOfClass:[NSDictionary class]]) badInput(@"inline がオブジェクトでない");
    if ([i[@"kind"] isEqual:@"attachment"]) {
      if (![i[@"id"] isKindOfClass:[NSString class]] || !isBool(i[@"image"])) badInput(@"attachment に id / image が無い");
      [out addObject:@{@"kind": @"attachment", @"id": i[@"id"], @"image": @([i[@"image"] boolValue])}];
    } else if ([i[@"kind"] isEqual:@"text"]) {
      NSArray *marks = i[@"marks"];
      if (![i[@"text"] isKindOfClass:[NSString class]] || ![marks isKindOfClass:[NSArray class]]) badInput(@"text に text / marks が無い");
      for (id m in marks) if (![knownMarks() containsObject:m]) badInput([NSString stringWithFormat:@"知らない mark %@", m]);
      id link = i[@"link"];
      if (link && ![link isKindOfClass:[NSString class]]) badInput(@"link が文字列でない");
      // read と同じ形に正規化する: marks は既定の順で重複なし、空の text は捨て、同じ marks / link の text はつなぐ
      NSString *text = i[@"text"];
      if (text.length == 0) continue;
      NSMutableArray *ms = [NSMutableArray array];
      for (NSString *m in knownMarks()) if ([marks containsObject:m]) [ms addObject:m];
      NSMutableDictionary *prev = [out.lastObject isKindOfClass:[NSMutableDictionary class]] ? out.lastObject : nil;
      if (prev && [prev[@"marks"] isEqual:ms] && ((prev[@"link"] == nil && link == nil) || [prev[@"link"] isEqual:link])) {
        prev[@"text"] = [prev[@"text"] stringByAppendingString:text];
        continue;
      }
      NSMutableDictionary *t = [@{@"kind": @"text", @"text": text, @"marks": ms} mutableCopy];
      if (link) t[@"link"] = link;
      [out addObject:t];
    } else badInput([NSString stringWithFormat:@"知らない inline %@", i[@"kind"]]);
  }
  return @{@"style": style, @"indent": @([indent unsignedIntegerValue]), @"quote": @([p[@"quote"] boolValue]), @"inlines": out};
}

// stdin の {"doc":Doc} を読む。DB を開く前に呼ぶ (壊れた入力では何も開かない)
static NSArray<NSDictionary *> *readDocFromStdin(void) {
  NSData *data = [[NSFileHandle fileHandleWithStandardInput] readDataToEndOfFile];
  NSError *e = nil;
  NSDictionary *root = data.length ? [NSJSONSerialization JSONObjectWithData:data options:0 error:&e] : nil;
  if (![root isKindOfClass:[NSDictionary class]]) badInput(e ? e.localizedDescription : @"JSON オブジェクトでない");
  NSArray *ps = [root[@"doc"] isKindOfClass:[NSDictionary class]] ? root[@"doc"][@"paragraphs"] : nil;
  if (![ps isKindOfClass:[NSArray class]]) badInput(@"doc.paragraphs が配列でない");
  NSMutableArray *out = [NSMutableArray array];
  for (id p in ps) [out addObject:canonicalParagraph(p)];
  return out;
}

// ---- 書き込み: 段落の差分を mergeableString に当てる ----

// 文字の単位。添付 (U+FFFC) は添付 id まで比べる
typedef struct { unichar ch; __unsafe_unretained NSString *att; } Unit;

static BOOL unitEq(Unit a, Unit b) {
  return a.ch == b.ch && (a.att == b.att || [a.att isEqual:b.att]);
}

// 目標の 1 段落: 文字列と、その中の inline ごとの範囲
typedef struct {
  NSString *text;
  NSArray<NSDictionary *> *runs;  // {range, inline}
} WantText;

static WantText wantText(NSDictionary *para) {
  NSMutableString *s = [NSMutableString string];
  NSMutableArray *runs = [NSMutableArray array];
  for (NSDictionary *i in para[@"inlines"]) {
    NSString *t = [i[@"kind"] isEqual:@"attachment"] ? @"\uFFFC" : i[@"text"];
    if (t.length == 0) continue;
    [runs addObject:@{@"range": [NSValue valueWithRange:NSMakeRange(s.length, t.length)], @"inline": i}];
    [s appendString:t];
  }
  return (WantText){s, runs};
}

// 段落の等しさで LCS を取り、変わる塊 (旧 [o0,o1) → 新 [n0,n1)) を前から順に返す
static NSArray<NSArray<NSNumber *> *> *changedBlocks(NSArray *cur, NSArray *want) {
  NSUInteger n = cur.count, m = want.count;
  NSMutableData *buf = [NSMutableData dataWithLength:(n + 1) * (m + 1) * sizeof(NSUInteger)];
  NSUInteger *L = buf.mutableBytes;
#define AT(i, j) L[(i) * (m + 1) + (j)]
  for (NSInteger i = n - 1; i >= 0; i--)
    for (NSInteger j = m - 1; j >= 0; j--)
      AT(i, j) = [cur[i] isEqual:want[j]] ? AT(i + 1, j + 1) + 1 : MAX(AT(i + 1, j), AT(i, j + 1));
  NSMutableArray *blocks = [NSMutableArray array];
  NSUInteger i = 0, j = 0, bi = 0, bj = 0;
  while (i < n || j < m) {
    if (i < n && j < m && [cur[i] isEqual:want[j]]) {
      if (bi < i || bj < j) [blocks addObject:@[@(bi), @(i), @(bj), @(j)]];
      i++; j++; bi = i; bj = j;
    } else if (j < m && (i == n || AT(i, j + 1) >= AT(i + 1, j))) j++;
    else i++;
  }
  if (bi < n || bj < m) [blocks addObject:@[@(bi), @(n), @(bj), @(m)]];
#undef AT
  return blocks;
}

// 塊 1 つの書き込み計画
@interface ONSEdit : NSObject
@property NSArray *repls;           // {range (書き換え前の絶対位置), with} を前から順に。後ろから当てる
@property NSUInteger start;         // 塊の新しい文字列が始まる位置 (置き換えをすべて当てた後の位置)
@property NSArray *paras;           // {offset, length, hasNL, runs, style(ICTTParagraphStyle)}
@end
@implementation ONSEdit
@end

static void attachmentFail(NSString *msg) { fail(5, msg); }

static NSDictionary *marksAttrs(NSDictionary *inl) {
  NSMutableDictionary *d = [NSMutableDictionary dictionary];
  NSArray *marks = inl[@"marks"];
  NSInteger hints = ([marks containsObject:@"bold"] ? 1 : 0) | ([marks containsObject:@"italic"] ? 2 : 0);
  if (hints) d[@"TTHints"] = @(hints);
  if ([marks containsObject:@"underline"]) d[@"TTUnderline"] = @1;
  if ([marks containsObject:@"strike"]) d[@"TTStrikethrough"] = @1;
  NSString *link = inl[@"link"];
  if (link) d[@"NSLink"] = [NSURL URLWithString:link] ?: link;
  return d;
}

// 段落ごとに上書きするキー。これ以外 (TTEmphasis / TTTimestamp など) は残す
static NSArray<NSString *> *controlledKeys(void) {
  return @[@"TTStyle", @"TTHints", @"TTUnderline", @"TTStrikethrough", @"NSLink"];
}

// base を元に、目標の段落スタイルを持つ ICTTParagraphStyle を作る
static id makeParagraphStyle(id base, NSDictionary *para, BOOL fresh) {
  id ps = base ? [base mutableCopy] : [[NSClassFromString(@"ICTTParagraphStyle") alloc] init];
  unsigned code = styleCode(para[@"style"]);
  ((void (*)(id, SEL, unsigned))objc_msgSend)(ps, SEL_("setStyle:"), code);
  ((void (*)(id, SEL, NSUInteger))objc_msgSend)(ps, SEL_("setIndent:"), [para[@"indent"] unsignedIntegerValue]);
  NSUInteger level = base ? [[base valueForKey:@"blockQuoteLevel"] unsignedIntegerValue] : 0;
  ((void (*)(id, SEL, NSUInteger))objc_msgSend)(ps, SEL_("setBlockQuoteLevel:"), [para[@"quote"] boolValue] ? MAX(level, (NSUInteger)1) : 0);
  if (fresh || !base) [ps performSelector:SEL_("setUuid:") withObject:[NSUUID UUID]];
  id todo = nil;
  if (code == 103) {
    BOOL done = [para[@"style"][@"done"] boolValue];
    id old = fresh || !base ? nil : [base valueForKey:@"todo"];
    todo = old ? ((id (*)(id, SEL, BOOL))objc_msgSend)(old, SEL_("todoWithDone:"), done)
               : ((id (*)(id, SEL, id, BOOL))objc_msgSend)([NSClassFromString(@"ICTTTodo") alloc], SEL_("initWithIdentifier:done:"), [NSUUID UUID], done);
  }
  [ps performSelector:SEL_("setTodo:") withObject:todo];
  return ps;
}

// 置き換え範囲の両端を合成文字列 (サロゲートペア・ZWJ 列・結合文字) の境界まで広げる。
// 旧は s の [of, of+ol)、新は neu の [nf, nf+nl)。pre / suf (共通の先頭・末尾の長さ) を縮める
static void snapToComposed(NSString *s, NSUInteger of, NSUInteger ol, NSString *neu, NSUInteger nf, NSUInteger nl,
                           NSUInteger *pre, NSUInteger *suf) {
  BOOL changed = YES;
  while (changed) {
    changed = NO;
    NSUInteger p = *pre;
    if (p > 0 && p < ol) p = MIN(p, MAX([s rangeOfComposedCharacterSequenceAtIndex:of + p].location, of) - of);
    if (p > 0 && p < nl) p = MIN(p, MAX([neu rangeOfComposedCharacterSequenceAtIndex:nf + p].location, nf) - nf);
    NSUInteger q = *suf;
    if (q > 0 && q < ol) {
      NSUInteger end = of + ol - q;
      NSRange r = [s rangeOfComposedCharacterSequenceAtIndex:end];
      if (r.location < end) q = MIN(q, of + ol - MIN(NSMaxRange(r), of + ol));
    }
    if (q > 0 && q < nl) {
      NSUInteger end = nf + nl - q;
      NSRange r = [neu rangeOfComposedCharacterSequenceAtIndex:end];
      if (r.location < end) q = MIN(q, nf + nl - MIN(NSMaxRange(r), nf + nl));
    }
    if (p != *pre || q != *suf) { *pre = p; *suf = q; changed = YES; }
  }
}

static NSArray<ONSEdit *> *planEdits(NSAttributedString *as, NSArray *cur, NSArray<NSValue *> *ranges, NSArray *want) {
  NSString *s = as.string ?: @"";
  NSUInteger len = s.length;
  // 今のメモの添付 (U+FFFC の位置 → id)
  NSMutableDictionary<NSNumber *, NSString *> *attAt = [NSMutableDictionary dictionary];
  Class attClass = NSClassFromString(@"ICTTAttachment");
  [as enumerateAttribute:@"NSAttachment" inRange:NSMakeRange(0, len) options:0 usingBlock:^(id att, NSRange r, BOOL *stop) {
    if (![att isKindOfClass:attClass]) return;
    for (NSUInteger i = r.location; i < NSMaxRange(r); i++)
      if ([s characterAtIndex:i] == 0xFFFC) attAt[@(i)] = [att valueForKey:@"attachmentIdentifier"] ?: @"";
  }];
  NSSet *curAtts = [NSSet setWithArray:attAt.allValues];
  // 目標の添付は今のメモにあるものだけ、それぞれ 1 回まで (bridge は添付を作らない)
  NSMutableSet *wantAtts = [NSMutableSet set];
  for (NSDictionary *p in want)
    for (NSDictionary *i in p[@"inlines"]) {
      if (![i[@"kind"] isEqual:@"attachment"]) continue;
      if (![curAtts containsObject:i[@"id"]]) attachmentFail([NSString stringWithFormat:@"今のメモに無い添付は置けない: %@", i[@"id"]]);
      if ([wantAtts containsObject:i[@"id"]]) attachmentFail([NSString stringWithFormat:@"同じ添付を 2 回置けない: %@", i[@"id"]]);
      [wantAtts addObject:i[@"id"]];
    }
  BOOL trailingNL = len > 0 && [s characterAtIndex:len - 1] == '\n';
  // 最後の段落が空なら、その \n が無いと読み戻したときに段落が消える
  BOOL lastEmpty = want.count > 0 && wantText(want.lastObject).text.length == 0;
  id anyStyle = len ? [as attribute:@"TTStyle" atIndex:0 effectiveRange:NULL] : nil;

  NSMutableArray<ONSEdit *> *edits = [NSMutableArray array];
  for (NSArray<NSNumber *> *b in changedBlocks(cur, want)) {
    NSUInteger o0 = b[0].unsignedIntegerValue, o1 = b[1].unsignedIntegerValue;
    NSUInteger n0 = b[2].unsignedIntegerValue, n1 = b[3].unsignedIntegerValue;
    // 旧の範囲は各段落とその後ろの \n。新しい文字列も段落ごとに \n を付ける
    NSUInteger from, to;
    BOOL leadNL = NO, tailNL = YES;
    if (o1 < cur.count) {
      from = ranges[o0].rangeValue.location;
      to = ranges[o1].rangeValue.location;
    } else if (o0 < cur.count) {
      from = ranges[o0].rangeValue.location;
      to = len;
      tailNL = trailingNL || lastEmpty;
    } else {
      // 末尾への追加。最後の段落が \n で終わっていなければ前に \n を足す
      from = to = len;
      leadNL = len > 0 && !trailingNL;
      tailNL = trailingNL || lastEmpty;
    }
    NSMutableString *neu = [NSMutableString string];
    NSMutableArray<NSString *> *newAtts = [NSMutableArray array];  // neu の各文字の添付 id (無ければ "")
    if (leadNL) { [neu appendString:@"\n"]; [newAtts addObject:@""]; }
    NSMutableArray *paras = [NSMutableArray array];
    for (NSUInteger j = n0; j < n1; j++) {
      WantText wt = wantText(want[j]);
      NSUInteger offset = neu.length;
      [neu appendString:wt.text];
      for (NSUInteger k = 0; k < wt.text.length; k++) [newAtts addObject:@""];
      for (NSDictionary *run in wt.runs)
        if ([run[@"inline"][@"kind"] isEqual:@"attachment"]) newAtts[offset + [run[@"range"] rangeValue].location] = run[@"inline"][@"id"];
      BOOL hasNL = j + 1 < n1 || tailNL;
      if (hasNL) { [neu appendString:@"\n"]; [newAtts addObject:@""]; }
      // 旧の段落と順に組にする。組の無い段落は新しい段落 (uuid を新しくする)
      NSUInteger oi = o0 + (j - n0);
      BOOL fresh = oi >= o1;
      id base = fresh ? nil : [as attribute:@"TTStyle" atIndex:ranges[oi].rangeValue.location effectiveRange:NULL];
      if (!base) {
        fresh = YES;
        base = from > 0 ? [as attribute:@"TTStyle" atIndex:from - 1 effectiveRange:NULL] : anyStyle;
      }
      [paras addObject:@{@"offset": @(offset), @"length": @(wt.text.length), @"hasNL": @(hasNL), @"runs": wt.runs,
                         @"style": makeParagraphStyle(base, want[j], fresh)}];
    }
    NSUInteger ol = to - from, nl = neu.length;
    // 動かさない文字 (錨) を旧と新で対応させる。錨は置き換え範囲に入れない。
    // 1. 残す添付: 塊の旧と新の両方に同じ順で現れなければならない (よそへ動かす・よそから持ってくるのは不可)
    NSMutableDictionary<NSString *, NSNumber *> *oldKept = [NSMutableDictionary dictionary];
    for (NSUInteger i = 0; i < ol; i++) {
      NSString *a = attAt[@(from + i)];
      if (a && [wantAtts containsObject:a]) oldKept[a] = @(i);
    }
    NSMutableArray<NSArray<NSNumber *> *> *anchors = [NSMutableArray array];  // [旧の位置, 新の位置]
    for (NSUInteger i = 0; i < nl; i++) {
      if (newAtts[i].length == 0) continue;
      NSNumber *o = oldKept[newAtts[i]];
      if (!o) attachmentFail([NSString stringWithFormat:@"残すべき添付を消す・動かす書き込みはできない: %@", newAtts[i]]);
      if (anchors.count && anchors.lastObject[0].unsignedIntegerValue > o.unsignedIntegerValue)
        attachmentFail([NSString stringWithFormat:@"添付の順番を入れ替える書き込みはできない: %@", newAtts[i]]);
      [anchors addObject:@[o, @(i)]];
      [oldKept removeObjectForKey:newAtts[i]];
    }
    if (oldKept.count) attachmentFail([NSString stringWithFormat:@"残すべき添付を消す・動かす書き込みはできない: %@", oldKept.allKeys.firstObject]);
    // 2. 組になった段落の終わりの \n (添付の錨と順序が食い違うものは使わない)
    NSArray *attAnchors = [anchors copy];
    for (NSUInteger k = 0; k < MIN(o1 - o0, n1 - n0); k++) {
      NSRange orng = ranges[o0 + k].rangeValue;
      NSUInteger on = NSMaxRange(orng) - from, nn = [paras[k][@"offset"] unsignedIntegerValue] + [paras[k][@"length"] unsignedIntegerValue];
      if (on >= ol || ![paras[k][@"hasNL"] boolValue]) continue;
      BOOL ok = YES;
      for (NSArray<NSNumber *> *a in attAnchors) {
        NSUInteger ao = a[0].unsignedIntegerValue, an = a[1].unsignedIntegerValue;
        if (!((ao < on && an < nn) || (ao > on && an > nn))) { ok = NO; break; }
      }
      if (ok) [anchors addObject:@[@(on), @(nn)]];
    }
    [anchors sortUsingComparator:^NSComparisonResult(NSArray *x, NSArray *y) { return [x[0] compare:y[0]]; }];
    [anchors addObject:@[@(ol), @(nl)]];  // 番兵 (塊の終わり)
    // 錨と錨の間ごとに、共通の先頭と末尾を除いた中間だけを置き換える
    Unit (^oldU)(NSUInteger) = ^Unit(NSUInteger i) { return (Unit){[s characterAtIndex:from + i], attAt[@(from + i)]}; };
    Unit (^newU)(NSUInteger) = ^Unit(NSUInteger i) {
      NSString *a = newAtts[i];
      return (Unit){[neu characterAtIndex:i], a.length ? a : nil};
    };
    NSMutableArray *repls = [NSMutableArray array];
    NSUInteger os = 0, ns = 0;
    for (NSArray<NSNumber *> *a in anchors) {
      NSUInteger oe = a[0].unsignedIntegerValue, ne = a[1].unsignedIntegerValue;
      NSUInteger sol = oe - os, snl = ne - ns, pre = 0, suf = 0;
      while (pre < sol && pre < snl && unitEq(oldU(os + pre), newU(ns + pre))) pre++;
      while (suf < sol - pre && suf < snl - pre && unitEq(oldU(oe - 1 - suf), newU(ne - 1 - suf))) suf++;
      snapToComposed(s, from + os, sol, neu, ns, snl, &pre, &suf);
      NSRange rep = NSMakeRange(from + os + pre, sol - pre - suf);
      NSString *with = [neu substringWithRange:NSMakeRange(ns + pre, snl - pre - suf)];
      if (rep.length || with.length) {
        for (NSUInteger i = rep.location; i < NSMaxRange(rep); i++) {
          NSString *att = attAt[@(i)];
          if (att && [wantAtts containsObject:att]) attachmentFail([NSString stringWithFormat:@"残すべき添付を消す・動かす書き込みはできない: %@", att]);
        }
        if ([with rangeOfString:@"￼"].location != NSNotFound) attachmentFail(@"添付 (U+FFFC) を新しく作る書き込みはできない");
        [repls addObject:@{@"range": [NSValue valueWithRange:rep], @"with": with}];
      }
      os = oe + 1;
      ns = ne + 1;
    }
    ONSEdit *e = [ONSEdit new];
    e.repls = repls;
    e.start = from;
    e.paras = paras;
    [edits addObject:e];
  }
  return edits;
}

// 段落の文字に目標の属性を付ける。既存の属性辞書のうち controlledKeys 以外は残す。
// 添付の文字 (U+FFFC) には TTStyle だけを付ける (添付とその他の属性はそのまま)
static void applyParagraphAttrs(id ms, NSUInteger start, NSDictionary *p) {
  NSAttributedString *as = [ms performSelector:SEL_("attributedString")];
  id style = p[@"style"];
  NSMutableArray *pieces = [NSMutableArray array];  // {range, add} (add が nil なら TTStyle だけ)
  for (NSDictionary *run in p[@"runs"]) {
    NSDictionary *inl = run[@"inline"];
    NSRange r = [run[@"range"] rangeValue];
    NSValue *range = [NSValue valueWithRange:NSMakeRange(start + r.location, r.length)];
    [pieces addObject:[inl[@"kind"] isEqual:@"attachment"] ? @{@"range": range} : @{@"range": range, @"add": marksAttrs(inl)}];
  }
  NSUInteger len = [p[@"length"] unsignedIntegerValue];
  // 空段落でも \n にスタイルを付ける
  if ([p[@"hasNL"] boolValue]) [pieces addObject:@{@"range": [NSValue valueWithRange:NSMakeRange(start + len, 1)], @"add": @{}}];
  for (NSDictionary *piece in pieces) {
    NSRange range = [piece[@"range"] rangeValue];
    NSDictionary *add = piece[@"add"];
    // 既存の属性の区切りごとに分けて付ける。変わらない区切りには触れない
    NSMutableArray *parts = [NSMutableArray array];
    [as enumerateAttributesInRange:range options:0 usingBlock:^(NSDictionary *at, NSRange r, BOOL *stop) {
      NSMutableDictionary *d = [at mutableCopy];
      if (add) {
        [d removeObjectsForKeys:controlledKeys()];
        [d addEntriesFromDictionary:add];
      }
      d[@"TTStyle"] = style;
      if (![d isEqual:at]) [parts addObject:@[[NSValue valueWithRange:r], d]];
    }];
    for (NSArray *part in parts)
      ((void (*)(id, SEL, id, NSRange))objc_msgSend)(ms, SEL_("setAttributes:range:"), part[1], [part[0] rangeValue]);
  }
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

static id noteById(NSManagedObjectContext *moc, NSString *noteId) {
  NSPredicate *p = [NSCompoundPredicate andPredicateWithSubpredicates:@[
    notDeleted(), [NSPredicate predicateWithFormat:@"identifier == %@", noteId]]];
  id note = fetch(moc, @"ICNote", p, nil).firstObject;
  if (!note) fail(1, [NSString stringWithFormat:@"メモが無い: %@", noteId]);
  if (isLocked(note)) fail(1, [NSString stringWithFormat:@"ロックされたメモは読み書きしない: %@", noteId]);
  return note;
}

static void cmdWrite(NSManagedObjectContext *moc, NSString *noteId, NSArray *want) {
  id note = noteById(moc, noteId);
  id ms = [note performSelector:SEL_("mergeableString")];
  if (!ms) fail(1, [NSString stringWithFormat:@"メモの本文を開けない: %@", noteId]);
  // attributedString は書き換えると変わるので、計画は複製に対して立てる
  NSAttributedString *as = [[ms performSelector:SEL_("attributedString")] copy] ?: [[NSAttributedString alloc] init];
  NSArray<NSValue *> *ranges = paragraphRanges(as.string);
  NSMutableArray *cur = [NSMutableArray array];
  for (NSValue *v in ranges) [cur addObject:paragraphJSON(as, v.rangeValue)];
  // 添付を守れない塊が 1 つでもあれば、何も書かずに終了コード 5
  NSArray<ONSEdit *> *edits = planEdits(as, cur, ranges, want);
  if (edits.count > 0) {
    // 後ろの塊から当てれば、前の塊の位置は変わらない
    for (ONSEdit *e in edits.reverseObjectEnumerator) {
      [ms performSelector:SEL_("beginEditing")];
      for (NSDictionary *r in e.repls.reverseObjectEnumerator)
        ((void (*)(id, SEL, NSRange, id))objc_msgSend)(ms, SEL_("replaceCharactersInRange:withString:"), [r[@"range"] rangeValue], r[@"with"]);
      for (NSDictionary *p in e.paras) applyParagraphAttrs(ms, e.start + [p[@"offset"] unsignedIntegerValue], p);
      [ms performSelector:SEL_("endEditing")];
    }
    // 保存の前に、書き換えた結果が目標どおりか確かめる。違えば保存せずに終了コード 6
    NSArray *got = docFrom([ms performSelector:SEL_("attributedString")])[@"paragraphs"];
    if (![got isEqual:want]) {
      NSUInteger i = 0;
      while (i < got.count && i < want.count && [got[i] isEqual:want[i]]) i++;
      fail(6, [NSString stringWithFormat:@"書き換えた結果が目標と違うので保存しない: 段落 %lu (結果 %lu 段落、目標 %lu 段落)",
                                         (unsigned long)i, (unsigned long)got.count, (unsigned long)want.count]);
    }
    if (!((BOOL (*)(id, SEL))objc_msgSend)(note, SEL_("saveNoteData"))) fail(1, @"saveNoteData に失敗した");
    [note performSelector:SEL_("updateChangeCountWithReason:") withObject:@"org-notes-sync"];
    NSError *err = nil;
    if (![moc save:&err]) fail(1, [NSString stringWithFormat:@"保存できない: %@", err.localizedDescription]);
  }
  emit(@{@"doc": docOfNote(note)});
}

static void cmdDumpAttrs(NSManagedObjectContext *moc, NSString *noteId) {
  NSAttributedString *as = [noteById(moc, noteId) performSelector:SEL_("attributedString")] ?: [[NSAttributedString alloc] init];
  NSMutableArray *out = [NSMutableArray array];
  for (NSValue *v in paragraphRanges(as.string)) {
    NSRange r = v.rangeValue;
    // 空段落はその \n の属性を見る
    if (r.length == 0) r.length = 1;
    NSMutableSet *keys = [NSMutableSet set];
    [as enumerateAttributesInRange:r options:0 usingBlock:^(NSDictionary *at, NSRange rr, BOOL *stop) {
      [keys addObjectsFromArray:at.allKeys];
    }];
    [out addObject:[keys.allObjects sortedArrayUsingSelector:@selector(compare:)]];
  }
  emit(@{@"paragraphs": out});
}

static void cmdRead(NSManagedObjectContext *moc, NSString *noteId) {
  emit(@{@"doc": docOfNote(noteById(moc, noteId))});
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
          @"identify --uri <uri> | export-media --attachment <id> --to <path> | write --id <id> | dump-attrs --id <id>)");
}

// サブコマンドごとに受け付けるオプション (--store はどこでも受け付ける)
static NSDictionary<NSString *, NSArray<NSString *> *> *commandOptions(void) {
  return @{
    @"selfcheck": @[],
    @"list": @[@"folder"],
    @"read": @[@"id"],
    @"identify": @[@"uri"],
    @"export-media": @[@"attachment", @"to"],
    @"write": @[@"id"],
    @"dump-attrs": @[@"id"],
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

    // write の入力は DB を開く前に読んで確かめる
    NSArray *want = [cmd isEqual:@"write"] ? readDocFromStdin() : nil;
    NSManagedObjectContext *moc = openContext(store);
    __block int code = 0;
    [moc performBlockAndWait:^{
      @try {
        if ([cmd isEqual:@"list"]) cmdList(moc, option(opts, @"folder"));
        else if ([cmd isEqual:@"read"]) cmdRead(moc, option(opts, @"id"));
        else if ([cmd isEqual:@"identify"]) cmdIdentify(moc, option(opts, @"uri"));
        else if ([cmd isEqual:@"write"]) cmdWrite(moc, option(opts, @"id"), want);
        else if ([cmd isEqual:@"dump-attrs"]) cmdDumpAttrs(moc, option(opts, @"id"));
        else cmdExportMedia(moc, store, option(opts, @"attachment"), option(opts, @"to"));
      } @catch (NSException *ex) {
        fprintf(stderr, "notes-bridge: %s: %s\n", ex.name.UTF8String, ex.reason.UTF8String);
        code = 1;
      }
    }];
    return code;
  }
}
