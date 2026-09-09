# org メモ (プロジェクト別 + 全体) と、プロジェクトタブの初期画面

日付: 2026-09-09

## 目的

プロジェクトごとの覚書と、プロジェクトに紐づかない全体の覚書を org で持つ。
保存を意識せずに書けること (自動保存) と、Emacs を再起動しても同じ状態で
戻ること (自動復元) を満たす。

あわせて、プロジェクトタブを開いた直後の画面を「root の dired」から
「dired サイドバー + そのプロジェクトのメモ」に変える。タブを開いた時点で
「前回の続き」が目に入る状態にするのが狙い。

## 前提と制約

- Emacs 31.1 (macOS, NS ビルド)。`auto-save-visited-mode` /
  `auto-save-visited-predicate` / `auto-save-visited-interval` は標準にある。
- タブ 1 つ = プロジェクト 1 つで運用している (`project-tabs.el`)。タブ名は
  `wamei/tab-bar-tab-name-project` が「本文 window のバッファが属する
  プロジェクト名」から決め、`wamei/project-tabs-pin-name` が最初の 1 回で
  固定する。
- サイドバーは `project-sidebar.el` の左 side window。端末パネルは下、
  claude パネルは右の side window。
- `project-switch-project` は `project-switch-commands` がシンボルなら
  そのコマンドを `call-interactively` する。このとき呼び出し元バッファに
  `project-current-directory-override` がバッファローカルで設定されている
  (`default-directory` は変わらない)。
- init.el は `~/.emacs.d/init.el` への symlink なので、追加モジュールは
  `(file-truename user-init-file)` の隣から load する。

## 決めたこと

- メモの実体は `~/org/` 配下のフラットな org ファイル。
  - プロジェクトメモ: `~/org/<project-name>.org`
  - 全体メモ: `~/org/global.org`
- 自動保存は「アイドル + メモから離れるとき」。
- タブを開いた直後は「左に dired サイドバー、本文 window にプロジェクトメモ」。
- 全体メモはプレフィックス引数付きの同じキーで開く。
- org 側の設定はメモに必要な分だけ (agenda / capture は入れない)。

## 構成

新規 `~/.dotfiles/.emacs.d/project-memo.el` (+ `project-memo-test.el`)。
init.el 側は `leaf org` (新規・最小) と `leaf project-memo` (load と keybind)、
および `leaf project` の `project-switch-commands` の差し替え。

### 1. パス解決

```elisp
(defcustom wamei/project-memo-directory "~/org/")
(defcustom wamei/project-memo-global-name "global.org")
```

- `wamei/project-memo-file (project)` … `~/org/<project-name>.org`。
  名前は `project-name` (タブに出ている名前と同じ) を使い、`/` は `-` に潰す。
- `wamei/project-memo-global-file ()` … `~/org/global.org`。
- ディレクトリが無ければ作る。ファイルが無い場合はバッファだけ作り、
  `#+title: <name>` の 1 行を入れる。ファイルは最初の保存で生まれる。
- 同名の repo が複数あると同じメモを共有する。フラット構成を選んだ帰結として
  受け入れる (「どこにある repo でも扱えること」を優先した)。

### 2. メモバッファとプロジェクトの結びつけ

メモバッファが訪れているのは `~/org/foo.org` でプロジェクト外なので、
そのまま本文 window に出すと `wamei/tab-bar-tab-name-project` の判定が外れ、
タブ名が `foo.org` に化ける (`wamei/project-tabs-pin-name` も走らない)。

プロジェクトメモのバッファには `project-current-directory-override` を
バッファローカルで対象プロジェクトの root に設定する。`project-current` が
標準で見る変数なので、タブ名・`project-find-file`・`consult-project-buffer`
が揃ってそのプロジェクト基準になる。将来 `~/org` 自体を git repo にしても
`~/org` のプロジェクトとは判定されない。

あわせて `default-directory` も同じ root に向ける。override はバッファ
ローカルで `project-current` 越しにしか見えないので、`default-directory` を
生で読む利用者 (`wamei/project-sidebar-toggle` の「sidebar が出ていない」枝、
`dired-jump` など) には届かないため。

全体メモには設定しない (プロジェクト無しのまま)。

`wamei/project-tabs--use-tab-root` の advice は
`project-current-directory-override` が設定済みならそれを勝たせるので、
タブの root と同じ答えになり衝突しない。

### 3. 表示コマンド

| キー | 動作 |
| --- | --- |
| `C-x C-m` | プロジェクトメモを本文 window に出す / 出ていれば元に戻る |
| `C-u C-x C-m` | 全体メモを本文 window に出す / 出ていれば元に戻る |

- 出す先は `wamei/project-tabs-main-window`。サイドバーや端末パネルに
  フォーカスがあっても本文 window に出す (既存モジュールと同じ流儀)。
- その window が既に目的のメモを表示していれば元のバッファに戻る。戻り先は
  メモを出すときに window パラメータへ退避し、無ければ
  `switch-to-prev-buffer`。
- タブにプロジェクトが紐づいていない (`*scratch*` のタブなど) 場合は、
  プレフィックス無しでも全体メモを開く。

### 4. 自動保存

- **アイドル**: このモジュール専用の `run-with-idle-timer` (繰り返し) で
  `wamei/project-memo-save-all` を回す。間隔は
  `wamei/project-memo-autosave-idle-interval` (既定 5 秒)。他のファイルは
  従来どおり (`#file#` への auto-save のみで、実ファイルは手動保存)。
  対象の判定はバッファローカルのフラグでなくパスで行うので、desktop 復元で
  開き直されたメモにもそのまま効く。

  `auto-save-visited-mode` + `auto-save-visited-predicate` は採らない。
  あれは `save-some-buffers` 経由で、`buffer-save-without-query` が非 nil の
  バッファを述語より先に無条件で保存する (files.el)。magit の
  save-repository-buffers に `Y` と答えるとそのフラグが立つので、以後その
  ソースファイルが書きかけのまま毎回ディスクへ書かれてしまう。
  `save-some-buffers-functions` も走るので abbrev ファイルまで書かれる。
  自前のタイマーなら「メモ以外は書かない」が述語頼みでなく構造で保証される。
- **離れるとき**: 変更のあるメモバッファを `save-buffer` する関数を
  `window-selection-change-functions` / `after-focus-change-function` /
  `kill-emacs-hook` に足す。`save-silently` を束縛してエコーエリアを汚さない。
- `before-save-hook` に org へ効くものが無いことを実装時に確認する
  (`project-formatter` は biome / prettier なので対象外の想定)。

### 5. 自動復元

メモは通常のファイルバッファなので desktop がそのまま保存・復元する
(`desktop-restore-eager` 10、それ以降はアイドル時)。専用の復元処理は持たない。
サイドバーは side window なので既存の `desktop-side-windows.el` の
`wamei/desktop--restore-sidebar` が受け持つ。

### 6. プロジェクトタブの初期画面

`project-switch-commands` を `#'project-dired` から
`#'wamei/project-memo-switch-setup` に差し替える。

`wamei/project-memo-switch-setup` (interactive) の中身:

1. `project-current` から root を取る (呼び出し元の
   `project-current-directory-override` が効いている)
2. `delete-other-windows`
3. 本文 window にそのプロジェクトのメモを出す
4. `wamei/project-sidebar-show` で左サイドバーを出す (フォーカスは本文のまま)

効くのは `C-x C-p` で新しいタブを作ったときと `C-x t p`
(`project-other-tab-command`)。既存タブへの切り替え
(`wamei/project-switch-project-in-tab` が `tab-bar-select-tab` する経路) は
レイアウトを触らない。

root の dired は開かなくなる。必要なときは既存の `C-x C-j`
(`dired-toggle-current-or-project-directory`) を使う。

### 7. org の設定 (最小)

`leaf org` を新規に置き、`org-directory` と、`~/org/*.org` を開いたときの
見た目 (indent と折り返し) だけを設定する。agenda / capture / TODO 運用は
入れない。必要になったら別の leaf で足す。

## テスト

`project-memo-test.el` を既存の `*-test.el` と同じ形式で置く
(`emacs -Q --batch -l project-memo-test.el -f ert-run-tests-batch-and-exit`)。
一時ディレクトリを transient プロジェクトにするフィクスチャは
`project-sidebar-test.el` の流儀に合わせる。

- パス解決: プロジェクト → `~/org/<name>.org`、`/` を含む名前の正規化、全体メモ
- 新規メモバッファ: `#+title:` が入る、プロジェクトメモには
  `project-current-directory-override` が設定され `project-current` が
  そのプロジェクトを返す、全体メモには設定されない
- トグル: 本文 window にメモが出る / もう一度で元のバッファに戻る /
  プロジェクト外ではプレフィックス無しでも全体メモ
- 自動保存: 保存関数が変更のあるメモだけを保存する (他のファイルは触らない)、
  modtime がずれたメモは飛ばす、1 つの保存が失敗しても外へ飛ばさない、
  setup がアイドルタイマーを 1 つだけ作る (2 回呼んでも増えない)
- `wamei/project-memo-switch-setup` 後の window 構成: 左に sidebar、
  本文にメモ、フォーカスは本文

TDD (Red → Green → Refactoring) で進める。

## やらないこと

- org-agenda / org-capture / TODO ワークフロー
- メモの検索 UI (既存の `consult-ripgrep` などで足りる)
- 同名プロジェクトの衝突回避 (フラット構成を優先した結果として受け入れる)
- 既存タブへ切り替えたときのレイアウト再構成
