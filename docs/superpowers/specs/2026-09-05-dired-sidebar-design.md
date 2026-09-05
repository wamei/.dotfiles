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
- D&D は Emacs 内の dired 同士に限る。Finder との drop / drag は対象外。
- Finder で個別に「表示」する機能は入れない (`dired-do-open` で足りる)。
- 既存の .el (project-tabs.el, desktop-side-windows.el) と同じ流儀で、
  ロジックは別ファイル + ert テスト、数行の設定は init.el に直接書く。

## 全体構成

```
init.el
  ├ (leaf dired)          context-menu / dired-mouse-drag-files / auto-revert / C-c o
  ├ (leaf dired-subtree)  TAB toggle、背景色 off、nerd-icons の付け直し
  ├ (leaf dired-git-status)  dired-mode-hook
  ├ (leaf dired-tree)        dired-mode-hook
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

dired ブロック:

- `context-menu-mode` を有効化。`context-menu-functions` に
  `wamei/dired-context-menu-extras` を足す。dired 組み込みの
  Find / Open / Open With に加えて、Copy path / Copy / Rename / Delete /
  New file / New directory、ディレクトリ行では Expand / Collapse を出す。
- `dired-mouse-drag-files` は `t` (既定 action は copy、Shift で move、
  Control で copy、Meta で link。Finder と同じ感覚)。
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
minor mode `dired-git-status-mode` を dired-mode-hook で有効化する。
プロジェクト外 (project-current が nil、または `.git` が無い) では何もしない。

### 状態の取得

- ルートは `project-current` の root。ルート単位で
  `git status --porcelain=v1 -z --untracked-files=all` を `make-process` で
  非同期に実行する。ignored は取らない (重いので。treemacs の deferred も同じ)。
- 同じルートを見る dired バッファが複数あっても git は 1 回だけ叩く。
  結果は `dired-git-status--cache` (root → table) に置き、完了時にそのルートの
  全 dired バッファへ描画を配る。実行中に再要求が来たら完了後にもう 1 回だけ走らせる。

### 純関数

- `dired-git-status--parse (output root)`: porcelain の出力を
  `path → state` の hash に変換する。state は
  `modified` / `added` / `untracked` / `renamed` / `conflict` の 5 つ。
  リネームは新旧 2 パスが来るので新パスに `renamed`。
- `dired-git-status--propagate (table root)`: 変更を含むディレクトリに
  `modified` を付けて返す (`conflict` が含まれるなら `conflict`)。
  treemacs と同じく、閉じたディレクトリでも中に変更があれば色が付く。

### 描画

- `dired-after-readin-hook` と `dired-subtree-after-insert-hook` で
  `dired-git-status--decorate` を呼ぶ。バッファの各行 (subtree 行含む) について
  `dired-get-filename` で絶対パスを取り、table にあればファイル名領域に overlay
  (`face` と `dired-git-status-overlay t`) を張る。既存 overlay は張り直す前に消す。
- face: `dired-git-status-modified` (黄)、`-added` (緑)、`-untracked` (緑、やや薄い)、
  `-renamed` (青)、`-conflict` (赤)。treemacs-git-*-face に寄せた初期値。

### 更新契機

- バッファ表示 (mode 有効化) と revert。
- `magit-post-refresh-hook` (`with-eval-after-load 'magit`)。treemacs-magit の代替。
- dired-tree.el の file-notify から `dired-git-status-refresh` が呼ばれる。
- 手動 `dired-git-status-refresh` (interactive)。

## 3. dired-tree.el

dired-subtree を使う dired バッファに共通する「木」の振る舞い。
minor mode `dired-tree-mode` を dired-mode-hook で有効化する。

### 展開記憶

- バッファローカルな集合 `dired-tree--expanded` (ディレクトリの絶対パス、
  末尾 `/` なしに正規化) を真実の状態として持つ。overlay は表示の都合。
- 展開時 (`dired-subtree-after-insert-hook`): 挿入されたディレクトリ
  (`dired-subtree--get-ov` の `dired-subtree-name`) を集合に入れる。
  挿入範囲内の子ディレクトリ行のうち集合に入っているものを順に
  `dired-subtree-insert` する。hook が再帰するので深い階層も戻る。
- 閉じるとき (`dired-subtree-remove` の :before advice): 閉じるディレクトリ自身
  だけを集合から外す。子孫は残す。
- 純関数: `dired-tree--expanded-add` / `-remove` / `-children-to-reopen
  (expanded dir children)`。

### 展開ディレクトリの監視

- いま見えている展開ディレクトリ (overlay 由来。記憶集合ではない) を
  file-notify で監視する。`after-insert-hook` で追加、`after-remove-hook` と
  バッファ kill で解除。top ディレクトリは auto-revert が見るので対象外。
- 通知は 300ms debounce して `revert-buffer`。subtree が展開を復元し、
  point は下記の「カーソル保持」で戻す。あわせて `dired-git-status-refresh`。

### path まで展開

- `dired-tree-expand-to (file)`: root からの祖先ディレクトリを順に
  `dired-utils-goto-line` → 未展開なら `dired-subtree-insert`、最後に file の行へ
  移動して非 nil を返す。file がバッファのルート外なら nil。
- 純関数 `dired-tree--ancestors (root file)`: root の直下から file の親までの
  ディレクトリ列を返す。

### drop 先の決定

- `dired-tree-drop-directory-at-point`: point の行がディレクトリならそれ、
  ファイルならその行の親 (subtree overlay の `dired-subtree-name`、無ければ
  `dired-current-directory`)。空行や見出し行なら `dired-current-directory`。
- `dired-tree-dnd-handle-file (uri action)`: 上の drop 先を `dired-current-directory`
  の戻り値として見せた状態で `dired-dnd-handle-file` を呼ぶ薄い包み。
  dired-mode バッファの `dnd-protocol-alist` (buffer-local) の先頭に
  `("^file:" . dired-tree-dnd-handle-file)` を置く。
- 純関数 `dired-tree--drop-target (line-kind file-at-point parent-dir top-dir)`。

### カーソル保持

- revert の前後でカーソル行のファイル名を控えて戻す。dired 標準の復元は
  subtree 行では効かないので `dired-utils-goto-line` を使う。

## 4. project-sidebar.el

### バッファ

- プロジェクトルートごとに 1 つ、名前は `" *sidebar: <project-name>*"`
  (先頭空白で一覧から隠す)。ルートを開いた dired バッファに
  minor mode `project-sidebar-mode` を付けたもの。dired なので
  dired-git-status / dired-tree / 右クリック / D&D / nerd-icons はそのまま効く。
- 見た目: `dired-hide-details-mode`、`.` `..` は `dired-omit-mode` で隠し、
  先頭のディレクトリ見出し行と total 行は `invisible` overlay で隠す。
  mode-line は hide-mode-line-mode (いまの treemacs-mode-hook の扱いを移す)。
- header-line にプロジェクト名を `project-sidebar-root` face (太字、1.3 倍) で表示。
- タブとの関係: 各タブの window 構成が左 side window に「そのタブのプロジェクトの
  sidebar バッファ」を持つ。タブ切替は window 構成の復元なので特別な連携は不要。

### 表示とトグル (C-x C-n)

- `display-buffer-alist` に `display-buffer-in-side-window`、`side left`、
  `slot 0`、`window-width 35`、`dedicated t`、`window-parameters`
  `(no-other-window . t) (no-delete-other-windows . t)` を登録する
  (端末パネルと同じ)。
- `project-sidebar-toggle` は端末パネル (`wamei/term-toggle`) と同じ 4 態:
  非表示 → 開いてフォーカス / 表示中で未フォーカス → フォーカス /
  フォーカス中 → 元の window へ戻る / `C-u` → 閉じる。`q` は「元の window へ戻る」。
- 出すプロジェクトは、選択 window (side window なら直近の通常 window) の
  バッファの `project-current`。プロジェクト外なら `default-directory` をルートにする。
  この「基準 window」の判定は project-tabs.el の `wamei/project-tabs--name-window`
  と同じなので、それを公開関数 `wamei/project-tabs-main-window` にして共用する。

### follow

- `window-buffer-change-functions` と `window-selection-change-functions`
  (frame 単位) から、`run-at-time 0` で次のコマンド境界に回して
  `project-sidebar--follow (frame)` を呼ぶ (project-tabs の pin-name と同じ方式)。
- 選択 window が side window、バッファにファイルが無い、sidebar が表示されていない、
  のいずれかなら何もしない。
- ファイルが表示中 sidebar のルート配下なら `dired-tree-expand-to` で祖先を展開して
  その行へ移動し、`set-window-point` で window の点を動かす (フォーカスは奪わない)。
- ファイルがルート外 (別プロジェクトのファイル) なら、side window のバッファを
  そのファイルのプロジェクトの sidebar (無ければ作る) に切り替えてから follow する。
- 純関数 `project-sidebar--follow-target (file shown-root file-root)`:
  `same` / `switch` / `none` を返す。

### マウスとキー (`project-sidebar-mode-map`)

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

### desktop 連携

- `wamei/desktop-side-restorers` の treemacs エントリを
  `("\\` \\*sidebar: " . wamei/desktop--restore-sidebar)` に差し替える。
  desktop-side-windows が SPEC の :directory を `default-directory` に束縛して
  呼ぶので、その directory のプロジェクトで `project-sidebar-show` し、
  `wamei/desktop-side-resize` で幅を合わせる。
- sidebar は dired バッファなので desktop がそのまま保存してしまう。
  `desktop-buffers-not-to-save` に `"\\` \\*sidebar: "` を足して除外し、
  復元は restorer に任せる。`desktop-modes-not-to-save` の treemacs-mode は削除。

## 5. treemacs の削除と移行

- init.el: treemacs / treemacs-nerd-icons / treemacs-magit / treemacs-tab-bar の
  4 leaf を削除。`hide-mode-line-mode` 等の hook を treemacs-mode-hook →
  project-sidebar-mode-hook に付け替える。treemacs に言及したコメント
  (端末パネル、claude パネル、desktop、magit) は文言だけ直す。
- project-tabs.el: `wamei/treemacs--find-file-node-guard`、declare-function、
  コメントの「3. treemacs のガード」を削除。テスト 3 本と `(require 'treemacs)`
  も削除。pin-name を `run-at-time 0` で遅らせる理由が treemacs-tab-bar だったので、
  「window change 関数の中では tab-rename しない」という一般論にコメントを書き換える
  (遅延自体は残す)。`wamei/project-tabs--name-window` を
  `wamei/project-tabs-main-window` として公開する (旧名は alias で残す)。
- magit 連携: treemacs-magit の代わりに `magit-post-refresh-hook` →
  `dired-git-status-refresh`。
- elpa の treemacs 4 ディレクトリは最後に削除する (`:ensure` で再生成できる)。

## 6. テストと検証

- 3 モジュールそれぞれに `*-test.el` (ert) を既存 (project-tabs-test.el) と
  同じ形で置き、batch で回す。対象は純関数:
  - dired-git-status: porcelain のパース (M / A / ?? / R / UU、`-z` 区切り、
    リネームの新旧)、親への伝播、conflict の優先。
  - dired-tree: 展開記憶の追加・削除・子孫抽出、祖先列の計算、drop 先の決定、
    正規化 (末尾 `/`)。
  - project-sidebar: follow 先の判定、バッファ名。
- GUI でしか確かめられないもの (subtree 行のアイコン、同一フレーム内 D&D、
  右クリックメニュー、クリック / ダブルクリック、follow の見え方、desktop 復元)
  は起動中の Emacs に emacsclient で流して確認する。
- 実装の最初に spike を 1 つ: macOS で `dired-mouse-drag-files` の同一フレーム内
  drop が動くかを素の dired 2 窓で確認する。動かなければ D&D の設計だけ見直す。

## 実装順

1. spike: D&D の同一フレーム内 drop。
2. init.el の dired / dired-subtree 設定 (context-menu、D&D、auto-revert、
   subtree キー、nerd-icons の付け直し)。
3. dired-tree.el (展開記憶 → 監視 → expand-to → drop 先)。
4. dired-git-status.el。
5. project-sidebar.el (バッファ → 表示 / トグル → follow → マウス → desktop)。
6. treemacs の削除と project-tabs.el の整理、desktop の差し替え。
7. elpa から treemacs を削除。
