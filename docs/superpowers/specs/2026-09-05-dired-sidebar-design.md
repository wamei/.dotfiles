# dired ベースのプロジェクトサイドバー (treemacs の置き換え)

日付: 2026-09-05

## 目的

treemacs と dired で操作体系が二重になっているのを解消する。treemacs をやめ、
dired + dired-subtree の上にプロジェクトサイドバーを作り、dired 側にも
treemacs にあった機能 (git 状態の色分け、変更の自動検知、macOS の open、
プレビュー、右クリックメニュー、ドラッグ & ドロップ) を足す。
ファイル操作は全部 dired のコマンドで行えるようにし、サイドバーは
「見た目と follow を整えた dired バッファ」に留める。

## 前提と制約

- Emacs 31.1 (macOS, NS ビルド)。dired には右クリックメニュー
  (`dired-context-menu`)、D&D (`dired-mouse-drag-files` /
  `dired-dnd-handle-file`)、macOS の open (`dired-do-open`) が組み込みで
  ある。足りないのは dired-subtree で展開した行の扱いだけ。
- dired-subtree (MELPA) は revert 後に展開状態を自分で復元する
  (`dired-subtree--after-readin` が `dired-after-readin-hook` に載っている)。
  一方で親を閉じる (`dired-subtree-remove`) と子孫の overlay をまとめて捨てるので、
  子孫の展開状態は消える。
- nerd-icons-dired は `dired-after-readin-hook` でしかアイコンを付け直さないため、
  subtree 展開行にアイコンが付かない。
- macOS の drop ハンドラ `ns-drag-n-drop` は drop 位置に point を移してから
  `dnd-handle-multiple-urls` を呼ぶ。`dnd-begin-drag-files` は
  ALLOW-SAME-FRAME で同一フレーム内への drop を許す。
- `dired-mouse-drag` (down-mouse-1) は、動かさずに離したときは mouse-1 イベントを
  押し戻す。mouse-1 / double-mouse-1 に独自の挙動を載せても D&D と両立する。
- タブ = プロジェクトの対応は project-tabs.el が担う。サイドバーはタブごとに
  独立したツリー (そのタブのプロジェクト) を持つ。
- D&D は Emacs 内の dired 同士を想定する。Finder との drop / drag は対象外だが、
  Finder からの drop も同じハンドラを通るので (macOS では action が常に `private`)
  既定では移動になる。
- Finder で個別に「表示」する機能は入れない (`dired-do-open` で足りる)。
- 既存の .el (project-tabs.el, desktop-side-windows.el) と同じ流儀で、
  ロジックは別ファイル + ert テスト、数行の設定は init.el に直接書く。

## 全体構成

```
init.el
  ├ (leaf dired)          context-menu / dired-mouse-drag-files / auto-revert / C-c o
  ├ (leaf dired-subtree)  TAB toggle、背景色 off、nerd-icons の付け直し
  ├ (leaf dired-tree)        dired-mode-hook
  ├ (leaf dired-git-status)  dired-mode-hook
  ├ (leaf project-sidebar)   C-x C-n、display-buffer-alist、desktop restorer
  └ (leaf desktop)           restorer と除外の差し替え

dired-git-status.el   git status --porcelain → path→state → 行に face (純関数 + 非同期取得)
dired-tree.el         展開記憶 / 展開ディレクトリの file-notify / path まで展開 / drop 先
project-sidebar.el    プロジェクトごとの sidebar バッファ / side window / header / follow / マウス
project-tabs.el       treemacs ガードを削除 (タブ名の決定・固定はそのまま)
```

依存: project-sidebar → dired-tree → dired-subtree。dired-git-status は
dired-subtree に依存しない (subtree 行は hook で拾う)。

## 1. init.el に直接書く設定

`dired-tree` / `dired-git-status` / `project-sidebar` の 3 leaf はどれも
`:after` を付けず、`:preface` で対象の .el を `load` してから `:hook` を
登録する (`:after` にすると `:hook` の登録が `eval-after-load` に包まれ、
読み込む順が変わったときに黙って効かなくなるため)。

dired ブロック:

- `context-menu-mode` を有効化。`context-menu-functions` に
  `wamei/dired-context-menu-extras` を足す。dired 組み込みの
  Find / Open / Open With に加えて、Copy path / Copy / Rename / Delete /
  New file / New directory、ディレクトリ行では Expand / Collapse を出す。
  右クリックした行に point を移してからメニュー項目を組み立てる
  (各コマンドが point のファイルに効くようにするため)。Copy path は
  `dired-copy-filename-as-kill` を引数 0 (絶対パス) で呼ぶ。
- `dired-mouse-drag-files` は `t`。macOS では drop は常に action `private` で届き、
  修飾キー (Shift / Control / Meta) は受け手に伝わらない。`private` は
  `wamei/dired-tree-drop-action` (既定 `move`) に読み替えるので、既定は Finder と
  同じ「移動」になる。Finder からの drop も同じく移動になる。copy したいときは
  `wamei/dired-tree-drop-action` を `copy` にするか、dired の `C` (`dired-do-copy`)
  を使う。
- `C-c o` に `dired-do-open` (macOS の `open`)。
- dired バッファで `auto-revert-mode` (file-notify 経由、`auto-revert-verbose` nil)。

dired-subtree ブロック:

- `TAB` = `dired-subtree-toggle`、`<backtab>` = `dired-subtree-cycle`。
- `dired-subtree-use-backgrounds` nil。line-prefix のインデントだけにする。
- `dired-subtree-after-insert-hook` に `nerd-icons-dired--refresh` 相当を登録して
  展開行にもアイコンを付ける (revert 時の復元も `dired-subtree-insert` を通るので
  これ一箇所で足りる)。

## 2. dired-git-status.el

dired バッファのファイル名に git の状態で色を付ける。treemacs-git-mode の代替。
minor mode `wamei/dired-git-status-mode` を dired-mode-hook で有効化する。
プロジェクト外 (project-current が nil、または `.git` が無い)、リモート
(TRAMP) のディレクトリでは何もしない。

### 状態の取得

- ルートは `project-current` の root。ルート単位で
  `git status --porcelain=v1 -z --untracked-files=all` を `make-process` で
  非同期に実行する。ignored は取らない (重いので。treemacs の deferred も同じ)。
- 同じルートを見る dired バッファが複数あっても git は 1 回だけ叩く。
  結果は `wamei/dired-git-status--cache` (root → table) に置き、完了時にそのルートの
  全 dired バッファへ描画を配る。実行中に再要求が来たら完了後にもう 1 回だけ走らせる。

### 純関数

- `wamei/dired-git-status--parse (output root)`: porcelain の出力を
  `path → state` の hash に変換する。state は
  `modified` / `added` / `untracked` / `renamed` / `conflict` の 5 つ。
  リネームは新旧 2 パスが来るので新パスに `renamed`。
- `wamei/dired-git-status--propagate (table root)`: 変更を含むディレクトリに
  `modified` を付けて返す (`conflict` が含まれるなら `conflict`)。
  treemacs と同じく、閉じたディレクトリでも中に変更があれば色が付く。

### 描画

- `dired-after-readin-hook` と `dired-subtree-after-insert-hook` で
  `wamei/dired-git-status--decorate` を呼ぶ。バッファの各行 (subtree 行含む) について
  `dired-get-filename` で絶対パスを取り、table にあればファイル名領域に overlay
  (`face` と `wamei/dired-git-status-overlay t`) を張る。既存 overlay は張り直す前に消す。
- face: `wamei/dired-git-status-modified` (黄)、`-added` (緑)、`-untracked` (シアン。dired-rainbow の実行ファイルの緑と見分けるため)、
  `-renamed` (青)、`-conflict` (赤)。treemacs-git-*-face に寄せた初期値。

### 更新契機

- バッファ表示 (mode 有効化) と revert。
- `magit-post-refresh-hook` (`with-eval-after-load 'magit`)。treemacs-magit の代替。
  表示中の dired バッファのルートを集めて `wamei/dired-git-status--refresh-all-visible`
  でまとめて再取得する。
- dired-tree.el の file-notify から `wamei/dired-tree-refresh-hook` 経由で
  `wamei/dired-git-status-refresh` が呼ばれる。
- 手動 `wamei/dired-git-status-refresh` (interactive)。

## 3. dired-tree.el

dired-subtree を使う dired バッファに共通する「木」の振る舞い。
minor mode `wamei/dired-tree-mode` を dired-mode-hook で有効化する。

### 展開記憶

- バッファローカルな集合 `wamei/dired-tree--expanded` (ディレクトリの絶対パス、
  末尾 `/` なしに正規化) を真実の状態として持つ。overlay は表示の都合。
- 展開時 (`dired-subtree-after-insert-hook`): 挿入されたディレクトリ
  (`dired-subtree--get-ov` の `dired-subtree-name`) を集合に入れる。
  挿入範囲内の子ディレクトリ行のうち集合に入っているものを順に
  `dired-subtree-insert` する。hook が再帰するので深い階層も戻る。
- 閉じるとき (`dired-subtree-remove` の :before advice): 閉じるディレクトリ自身
  だけを集合から外す。子孫は残す。
- 純関数: `wamei/dired-tree--expanded-add` / `-remove` / `-children-to-reopen
  (expanded dir children)`。

### 展開ディレクトリの監視

- いま見えている展開ディレクトリ (overlay 由来。記憶集合ではない) を
  file-notify で監視する。`after-insert-hook` で追加、`after-remove-hook` と
  バッファ kill で解除。top ディレクトリは auto-revert が見るので対象外。
- 通知は `wamei/dired-tree-revert-delay` (既定 300ms) だけ debounce してから
  `wamei/dired-tree-revert` する。subtree が展開を復元し、point は下記の
  「カーソル保持」で戻す。revert が終わったら `wamei/dired-tree-refresh-hook` を
  実行する (dired-git-status はこの hook に `wamei/dired-git-status-refresh` を
  掛けて色を再取得する)。debounce 中の revert は `condition-case` で囲み、
  失敗してもエラーメッセージを出すだけでバッファを壊さない。
- 予約中の revert タイマーと監視は、mode 無効化とバッファ kill の両方で
  まとめて解除する (`wamei/dired-tree--remove-all-watches`)。

### path まで展開

- `wamei/dired-tree-expand-to (file)`: root からの祖先ディレクトリを順に
  `dired-utils-goto-line` → 未展開なら `dired-subtree-insert`、最後に FILE の行
  (末尾 `/` を落として正規化するので、FILE がディレクトリでもその行に乗るだけで
  展開はしない) へ移動して非 nil を返す。途中の行が見つからない、または FILE が
  バッファのルート外なら nil を返し、point は呼び出し前の位置に戻す。
- 純関数 `wamei/dired-tree--ancestors (root file)`: root の直下から file の親までの
  ディレクトリ列を返す。

### drop 先の決定

- `wamei/dired-tree-drop-directory-at-point`: point の行がディレクトリならそれ、
  ファイルならその行の親 (subtree overlay の `dired-subtree-name`、無ければ
  `dired-current-directory`)。空行や見出し行なら `dired-current-directory`。
- `wamei/dired-tree-dnd-handle-file (uris action)`: `dired-dnd-handle-file` は使わず、
  自前で運ぶ。ACTION が `private` / `copy` なら `wamei/dired-tree-drop-action`
  (既定 `move`) に読み替え、`link` はそのまま渡す。macOS では drop は常に `private`
  で届き、修飾キー (Shift / Control / Meta) は受け手に伝わらない (`ns-drag-n-drop`
  が action を渡さない)。dired 既定では `private` は copy 扱いになるが、Finder と
  同じく既定は移動にする。Finder からの drop も同じく移動になる。copy したいときは
  `wamei/dired-tree-drop-action` を `copy` にするか dired の `C` を使う。
- URIS は URI 1 本の文字列でもリストでもよい。シンボルに `dnd-multiple-handler`
  プロパティを付けてあるので、`dnd-handle-multiple-urls` は複数ファイルの drop を
  リストで 1 回だけ渡してくる (プロパティが無いと URI ごとに呼ばれ、そのたびに
  revert が走って 2 つめ以降の落下先が point から取り直されてずれる)。落下先は
  最初に 1 回だけ決め、全部運んでから revert と `wamei/dired-tree-refresh-hook` を
  1 回だけ回す (何も動かなければ revert もしない)。上書き確認はファイルごとに出す。
- 移動先が移動元と同じ (自分自身への drop)、または移動元ディレクトリの中なら、
  `rename-file` に渡さず message を出して飛ばす
  (`wamei/dired-tree--drop-into-self-p`)。移動先に既存ファイルがあれば `y-or-n-p`
  で上書き確認する。実行後は `wamei/dired-tree-revert` でカーソル位置を保って
  revert し、`wamei/dired-tree-refresh-hook` を呼ぶ。dired-mode バッファの
  `dnd-protocol-alist` (buffer-local) の先頭に
  `("^file:" . wamei/dired-tree-dnd-handle-file)` を置く。
- 純関数 `wamei/dired-tree--drop-target (directory-p file parent top)` /
  `wamei/dired-tree--resolve-action (action)`。

### カーソル保持

- revert の前後でカーソル行のファイル名を控えて戻す。dired 標準の復元は
  subtree 行では効かないので `dired-utils-goto-line` を使う。
- sidebar は選択されていない window に出る (`wamei/project-sidebar--reveal` は
  window-point だけを動かす) ので、buffer point と window-point はずれる。
  dired 標準の `dired-restore-positions` も window ごとの復元を持つが、subtree 行
  では `dired-goto-file` が効かず行番号にフォールバックするため、行数が変わる
  revert で別の行に飛ぶ。`get-buffer-window-list` の各 window についても
  window-point の行のファイル名を控え、revert 後に `set-window-point` で戻す。

## 4. project-sidebar.el

### バッファ

- プロジェクトルートごとに 1 つ、名前は `" *sidebar: <project-name>*"`
  (先頭空白で一覧から隠す)。ルートを開いた dired バッファに
  minor mode `wamei/project-sidebar-mode` を付けたもの。dired なので
  dired-git-status / dired-tree / 右クリック / D&D / nerd-icons はそのまま効く。
- ルート判定は `project-current` が返す root (無ければ dir) を `file-truename`
  で実体に解決してから使う (`wamei/project-sidebar--root-for` /
  `wamei/project-sidebar-buffer`)。symlink 越しに同じディレクトリを開いても
  同じ sidebar バッファに寄せるため。
- sidebar バッファは通常の dired 一覧には出さない。`dired-buffers` からは
  自分のエントリだけを外す (`dired-unadvertise` は同じディレクトリの通常 dired
  まで外してしまうため使わない)。
- 見た目: `dired-hide-details-mode`、`.` `..` は `dired-omit-mode` で隠し
  (`dired-omit-verbose` は nil にして「Omitted N lines」を出さない)。
  先頭のディレクトリ見出し行 (total 行は `dired-hide-details-mode` が隠す) は
  専用の invisibility シンボル `wamei/project-sidebar-header` を付けた overlay
  で隠す (`dired-hide-details-mode` が `buffer-invisibility-spec` をリストにする
  ため、`t` ではなく専用シンボルを使う)。このシンボルの
  `buffer-invisibility-spec` への登録は mode 有効化時に 1 回だけ行う
  (`add-to-invisibility-spec` は非冪等で、revert のたびに呼ぶと重複が積もる)。
  mode-line は `hide-mode-line-mode` を `wamei/project-sidebar-mode-hook` で
  有効にする。
- header-line にプロジェクト名を `wamei/project-sidebar-root` face (太字、1.3 倍) で表示。
- タブとの関係: 各タブの window 構成が左 side window に「そのタブのプロジェクトの
  sidebar バッファ」を持つ。タブ切替は window 構成の復元なので特別な連携は不要。

### 表示とトグル (C-x C-n)

- `display-buffer-alist` に `display-buffer-in-side-window`、`side left`、
  `slot 0`、`window-width 35`、`dedicated t`、`window-parameters`
  `(no-other-window . t) (no-delete-other-windows . t)` を登録する
  (端末パネルと同じ)。
- `wamei/project-sidebar-toggle` は端末パネル (`wamei/term-toggle`) と同じ 4 態:
  非表示 → 開いてフォーカス / 表示中で未フォーカス → フォーカス /
  フォーカス中 → 元の window へ戻る / `C-u` → 閉じる。`q` は window を閉じる (`C-u C-x C-n` と同じ。戻るだけなら `C-x C-n`)。
- 出すプロジェクトは、選択 window (side window なら直近の通常 window) の
  バッファの `project-current`。プロジェクト外なら `default-directory` をルートにする。
  この「基準 window」の判定は project-tabs.el の `wamei/project-tabs--name-window`
  と同じなので、それを公開関数 `wamei/project-tabs-main-window` にして共用する。

### follow

- `window-buffer-change-functions` と `window-selection-change-functions`
  (frame 単位) から、`run-at-time 0` で次のコマンド境界に回して
  `wamei/project-sidebar--follow (frame)` を呼ぶ (project-tabs の pin-name と同じ方式)。
- 選択 window が side window、バッファにファイルが無い、sidebar が表示されていない、
  のいずれかなら何もしない。
- リモート (TRAMP) のファイルには追従しない。`file-truename` が TRAMP 接続を
  試みてブロックするのを避けるため、`file-remote-p` で先に弾く。
- ファイルが表示中 sidebar のルート配下なら `wamei/dired-tree-expand-to` で祖先を展開して
  その行へ移動し、`set-window-point` で window の点を動かす (フォーカスは奪わない)。
- ファイルがルート外 (別プロジェクトのファイル) なら、side window のバッファを
  そのファイルのプロジェクトの sidebar (無ければ作る) に切り替えてから follow する。
  切り替え中は `set-window-dedicated-p` を一旦外してバッファを差し替え、
  成功しても失敗しても `unwind-protect` で必ず dedicated を戻す。
- 純関数 `wamei/project-sidebar--follow-target (file shown-root file-root)`:
  `same` / `switch` / `none` を返す。

### 現在行の強調

- 本文で開いているファイル (follow 先) やカーソル行を、`hl-line` を継承した行背景
  (`wamei/project-sidebar-current-row`) と左フリンジの三角マーク
  (`wamei/project-sidebar-current-fringe`、プロジェクト名と同じ色) で示す。
  `global-hl-line-mode` は選択 window にしか出ないので、非選択の sidebar でも残る
  overlay をバッファに 1 つ持つ (`wamei/project-sidebar--row-overlay`)。
- 更新契機: follow (`--reveal`)、sidebar 内のカーソル移動 (`post-command-hook`)、
  revert 後 (`wamei/dired-tree-refresh-hook`、dired-tree が window point を戻した後)。
- 非選択の sidebar では現在行 overlay が位置を示すので、中空カーソルは出さない
  (`cursor-in-non-selected-windows` をバッファローカルに nil)。

### マウスとキー (`wamei/project-sidebar-mode-map`)

- `mouse-1`: その行へ point を移す。ファイルならメイン window
  (`wamei/project-tabs-main-window`) に `find-file-noselect` したバッファを
  `set-window-buffer` で出す。フォーカスは sidebar に残す (= プレビュー)。
  ディレクトリなら選択のみ。
- `double-mouse-1`: ファイルならメイン window で開いてそちらへフォーカス。
  ディレクトリなら `dired-subtree-toggle`。
- `down-mouse-1` は dired 既定 (D&D) を活かす。
- `RET`: ファイルはメイン window で開いてフォーカス、ディレクトリは toggle。
  `TAB` は toggle (dired-subtree ブロックの束縛)。`C-c o` は `dired-do-open`。
- それ以外は dired のキーがそのまま使える (C / R / D / + / m / u など)。
- `mouse-1-click-follows-link` は nil にする (dired のファイル名リンクを
  mouse-1 が横取りしないように)。

### desktop 連携

- `wamei/desktop-side-restorers` の treemacs エントリを
  `("\\` \\*sidebar: " . wamei/desktop--restore-sidebar)` に差し替える。
  desktop-side-windows が SPEC の :directory を `default-directory` に束縛して
  呼ぶので、その directory のプロジェクトで `wamei/project-sidebar-show` し、
  `wamei/desktop-side-resize` で幅を合わせる。
- sidebar は dired バッファなので desktop がそのまま保存してしまう。
  `desktop-buffers-not-to-save` に `"\\` \\*sidebar: "` を足して除外し、
  復元は restorer に任せる。`desktop-modes-not-to-save` の treemacs-mode は削除。

## 5. treemacs の削除と移行

- init.el: treemacs / treemacs-nerd-icons / treemacs-magit / treemacs-tab-bar の
  4 leaf を削除。`hide-mode-line-mode` 等の hook を treemacs-mode-hook →
  `wamei/project-sidebar-mode-hook` に付け替える。treemacs に言及したコメント
  (端末パネル、claude パネル、desktop、magit) は文言だけ直す。
- project-tabs.el: `wamei/treemacs--find-file-node-guard`、declare-function、
  コメントの「3. treemacs のガード」を削除。テスト 3 本と `(require 'treemacs)`
  も削除。pin-name を `run-at-time 0` で遅らせる理由が treemacs-tab-bar だったので、
  「window change 関数の中では tab-rename しない」という一般論にコメントを書き換える
  (遅延自体は残す)。`wamei/project-tabs--name-window` を
  `wamei/project-tabs-main-window` として公開する (旧名は alias で残す)。
- magit 連携: treemacs-magit の代わりに `magit-post-refresh-hook` →
  `wamei/dired-git-status--refresh-all-visible` (表示中の dired バッファの
  ルートをまとめて再取得する)。
- elpa の treemacs 4 ディレクトリは最後に削除する (`:ensure` で再生成できる)。

## 6. テストと検証

- 3 モジュール (dired-tree / dired-git-status / project-sidebar) それぞれに
  `*-test.el` (ert) を既存 (project-tabs-test.el) と同じ形で置き、batch で回す。
  テスト本数は dired-tree 21、dired-git-status 12、project-sidebar 17
  (既存の project-tabs 8、desktop-side-windows 20 と合わせて batch を回す)。
  対象は主に純関数:
  - dired-git-status: porcelain のパース (M / A / ?? / R / UU、`-z` 区切り、
    リネームの新旧)、親への伝播、conflict の優先。
  - dired-tree: 展開記憶の追加・削除・子孫抽出、祖先列の計算、drop 先の決定と
    action の読み替え、正規化 (末尾 `/`)。
  - project-sidebar: follow 先の判定、バッファ名。
- GUI でしか確かめられないもの (subtree 行のアイコン、同一フレーム内 D&D、
  右クリックメニュー、クリック / ダブルクリック、follow の見え方、desktop 復元)
  は起動中の Emacs に emacsclient で流して確認する。
- 実装の最初に spike を 1 つ行った: macOS で `dired-mouse-drag-files` の
  同一フレーム内 drop が動くかを素の dired 2 窓で確認した。同一フレーム内の
  drop は動作し、想定どおり `private` (dired 既定では copy 扱い) で届いた。
  この結果を受けて、drop 先の決定 (3 章) では `private` / `copy` を
  `wamei/dired-tree-drop-action` (既定 move) に読み替える設計にした。

## 実装順

spike (D&D の同一フレーム内 drop の確認) → init.el の dired / dired-subtree
設定 (context-menu、D&D、auto-revert、subtree キー、nerd-icons の付け直し) →
dired-tree.el (展開記憶 → 監視 → expand-to → drop 先) → dired-git-status.el →
project-sidebar.el (バッファ → 表示 / トグル → follow → マウス → desktop) →
treemacs の削除と project-tabs.el の整理、desktop の差し替え → elpa から
treemacs を削除、の順で実装した。
