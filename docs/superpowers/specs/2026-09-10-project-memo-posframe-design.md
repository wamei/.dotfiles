# メモを画面中央の posframe で開く

日付: 2026-09-10

前提となる設計: `docs/superpowers/specs/2026-09-09-project-memo-design.md`

## 目的

メモを本文 window に出すと、そのとき見ていたコードが隠れる。メモは「手を止めて
数行書いて戻る」使い方が主なので、既定の出し方を画面中央の posframe に変える。
本文 window に出す経路も残し、腰を据えて書くときはそちらを使う。

## 前提と制約

- Emacs 31.1 (macOS, NS ビルド)。GUI と `emacs -nw` の両方で使う。
- posframe は導入済みで、`vertico-posframe` を tty child frame 込みで運用している
  (`init.el` の `leaf vertico-posframe`)。判定は
  `(or (display-graphic-p) (featurep 'tty-child-frames))` で、これは
  `posframe-workable-p` の中身と同じ。
- child frame ポップアップの背景と枠は `wamei/popup-body` /
  `wamei/popup-border` に集約されている (`init.el` の `leaf *popup-appearance`)。
  tty の罫線枠は同ブロックが display table の box グリフを設定して描いている。
- `posframe-workable-p` は `noninteractive` で必ず nil を返す。batch では
  posframe は出せない。
- タブ 1 つ = プロジェクト 1 つ (`project-tabs.el`)。タブ名は
  `wamei/tab-bar-tab-name-project` が「本文 window のバッファのプロジェクト名」
  から決める。
- posframe に `:accept-focus t` を渡さないと、posframe 自身が
  `posframe--redirect-posframe-focus` でフォーカスを親フレームへ送り返す。
  編集させるので必須。

## 決めたこと

- `C-x C-m` の既定の表示先を posframe にする。`C-u` を付けると本文 window。
- 全体メモは `C-x C-S-m` (Shift 付き) に分ける。
- posframe はフォーカスが外れたら閉じる。`ESC` / `C-g` では閉じない。
- posframe にメモ以外のバッファが入ったら、閉じて親フレームで開き直す。
- プロジェクトタブを開いた直後 (`C-x C-p` / `C-x t p`) は今までどおり本文 window。

## キーとコマンド

| キー | コマンド | 表示先 | 対象 |
| --- | --- | --- | --- |
| `C-x C-m` | `wamei/project-memo-toggle` | posframe | プロジェクトメモ |
| `C-u C-x C-m` | 同上 | 本文 window | プロジェクトメモ |
| `C-x C-S-m` | `wamei/project-memo-toggle-global` | posframe | 全体メモ |
| `C-u C-x C-S-m` | 同上 | 本文 window | 全体メモ |

2 つのコマンドは共有の内部関数を対象 (プロジェクト / 全体) と表示先で呼び分ける
だけにする。prefix 引数の意味が「全体メモ」から「本文 window」に変わる点が、
既存コマンドに対する非互換な変更になる。

`C-x C-M` は Emacs では `C-x C-m` と同一のキー列 (`(kbd "C-x C-M")` と
`(kbd "C-x C-m")` は `equal`)。Shift を区別するには `C-x C-S-m` と書く必要が
あり、これは GUI では確実に効くが、端末では terminal が modifyOtherKeys で
送る場合にだけ届く。実装時に次の 2 段階で確認する。

1. Emacs 側: tmux + `emacs -Q -nw` に modifyOtherKeys の raw シーケンスを
   流し込み、`C-S-m` として受け取るかを確認する
2. 端末側: Ghostty が実際に送るかは `C-h k` を 1 回押してもらって確認する

2 が通らなければ tty 用の代替キーを別途決める。GUI 側の設計は変えない。

### 両方の表示先が絡むとき

- posframe が出ている状態で `C-u` 付き (本文 window) を押したら、posframe を
  閉じてから本文 window に出す。表示先は常に 1 つに保つ。
- 本文 window に既にそのメモが出ている状態で posframe を開くのは許す。同じ
  バッファを 2 つの window に出すのは Emacs として正常で、point は window ごとに
  独立する。特別扱いを入れない。

## posframe の表示

`posframe-show` に渡すもの:

- `:poshandler #'posframe-poshandler-frame-center` — フレーム中央。モードラインや
  端末パネルの有無で位置がずれない (vertico-posframe と同じ選択)
- `:width` / `:height` — 親フレームに対する比率の defcustom
  (`wamei/project-memo-posframe-width-ratio` / `-height-ratio`、既定 0.6) から
  計算する。`:min-width` / `:min-height` も持たせ、極端に小さいフレームで
  潰れないようにする
- `:border-width 1` と `:border-color` — `wamei/popup-border` の `:background` から
  取る。corfu / eldoc-box / vertico-posframe と枠幅も色も揃う
- `:background-color` — `wamei/popup-body` の `:background` から取る
- `:accept-focus t` — これが無いと編集できない (前述)
- `:cursor` — 編集するのでカーソルを出す。既定の posframe は隠す

表示後に `select-frame-set-input-focus` でフォーカスを移す。`posframe-show` は
フレームを返すので、それをモジュールの変数に持って以降の判定に使う。

`posframe-workable-p` が nil のときは posframe を出さず、本文 window に
フォールバックする (`C-u` を付けたのと同じ動き)。batch、`emacs_basic_display`、
child frame 非対応環境でコマンドが壊れないようにするため。

## posframe を閉じる条件

次の 3 つ。`ESC` と `C-g` では閉じない (どちらも org の編集中に使うため)。

1. `C-x C-m` / `C-x C-S-m` をもう一度押す (トグル)
2. Emacs 内の別の window / frame / タブへフォーカスが移る
3. posframe にメモ以外のバッファが入る

いずれの経路でも、隠す前に必ずそのメモを保存する。

2 と 3 は `post-command-hook` 1 つで見る。

```
posframe が生きている
├─ selected-frame が posframe のフレームでない
│    → 保存して隠す
└─ posframe の window がメモバッファ以外を映している
     → 保存して隠し、そのバッファを親の本文 window に出してフォーカスを渡す
```

3 を事後に引き取る形にしたのは、禁止キーの一覧を持たずに済むため。
`find-file` のように `display-buffer` を通らないコマンドも、`magit-status` の
ように通るコマンドも、同じ 1 か所で拾える。`post-command-hook` は再描画の前に
走るので、別バッファが posframe に見える瞬間は基本的に出ない。

代替案として「child frame の window を dedicated にして `display-buffer-alist`
で親へ流す」を検討したが、`switch-to-buffer` は `display-buffer` を通らないため
`switch-to-buffer-in-dedicated-window` をグローバルに変える必要があり、sidebar・
端末パネル・claude パネル (いずれも dedicated) の挙動まで変えてしまう。採らない。

## child frame と tab-bar

posframe にフォーカスがある間、`selected-frame` は子フレームになる。このまま
だと 2 つ壊れる。

- `wamei/project-tabs-current-root` は `(frame-parameter frame 'tabs)` を読む。
  子フレームに tabs は無いので nil を返し、対象プロジェクトを見失う。
- `wamei/project-tabs-main-window` は `(selected-window)` を返す。子フレームの
  window は side window ではないのでそのまま採られ、`wamei/tab-bar-tab-name-project`
  がメモバッファ名をタブ名にしてしまう (タブ名が `foo.org` に化ける)。
  `get-mru-window` の ALL-FRAMES も nil なので、子フレーム内しか探さない。

`project-tabs.el` に `parent-frame` を遡って最上位のフレームを返すヘルパを足し、
`wamei/project-tabs-current-root` と `wamei/project-tabs-main-window` を
そこ経由にする。`wamei/project-tabs-set-root` も `tab-bar--current-tab-find` を
通すので同じ扱いにする。

sibling モジュールへの変更になるが、posframe を入れる以上避けられず、
変更は「どのフレームを見るか」に閉じている。

## 自動保存

既存の仕組み (このモジュール専用の idle timer と、離脱時の
`wamei/project-memo-save-all`) はそのまま。posframe を隠す経路にも保存を挟むので、
「閉じた時点で必ずファイルに入っている」が成立する。

## 変えないもの

- パス解決、メモバッファの生成、`project-current-directory-override`、
  メモバッファの `default-directory`
- 本文 window 経路のトグルと戻り先の退避 (`wamei/project-memo-back`、
  `wamei/project-memo--restore`)。`C-u` 経路として残る
- `wamei/project-memo-switch-setup` (タブを開いた直後の画面)
- 自動保存の仕組み

## テスト

`posframe-workable-p` が `noninteractive` で nil を返すため、batch では posframe は
出せない。4 層に分ける。

1. **純粋なロジック** — 対象メモの決定、表示先の決定、`post-command` の状態遷移の
   判断関数。通常の ert。
2. **posframe の呼び出し** — `posframe-show` / `posframe-hide` を `cl-letf` で
   スタブし、正しいバッファ・poshandler・サイズ・`:accept-focus t` で呼ばれるか、
   隠すときに保存するかを検証する。
3. **フォールバック** — batch はそのまま「posframe が使えない環境」なので、
   `C-x C-m` が本文 window に出ることを実環境として検証できる。
4. **タブ名** — 子フレームを選択した状態で `wamei/tab-bar-tab-name-project` が
   プロジェクト名を返すことを ert で直接確かめる。batch でも `make-frame` で
   child frame は作れないため、`selected-frame` と `parent-frame` を
   スタブして判定関数の側を検証する。

実表示は隔離 daemon に GUI フレームを作って確認する。過去に踏んだ罠として、
`sit-for` は即返るので `sleep-for` を使う、幾何は `posframe--frame` の
frame-parameter から読む、`screencapture` は権限で落ちるので使わない。

## やらないこと

- posframe を出したまま他の window で作業する運用 (フォーカスが外れたら閉じる)
- `org-capture` 風の「確定して閉じる」操作
- posframe 内でのキーの制限 (許可リスト / 禁止リストの維持はしない)
- `wamei/project-sidebar--follow` の手当て (前設計からの既知の制限のまま)
