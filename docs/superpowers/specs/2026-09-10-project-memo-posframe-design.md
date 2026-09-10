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
- 全体メモは `C-x m` に分ける。
- posframe はフォーカスが外れたら閉じる。`ESC` / `C-g` では閉じない。
- posframe の window は dedicated にしない。小窓にフォーカスがある状態で
  `C-x C-f` や `magit-status` を実行したら、その小窓の中に開く。
- プロジェクトタブを開いた直後 (`C-x C-p` / `C-x t p`) は今までどおり本文 window。

## キーとコマンド

| キー | コマンド | 表示先 | 対象 |
| --- | --- | --- | --- |
| `C-x C-m` | `wamei/project-memo-toggle` | posframe | プロジェクトメモ |
| `C-u C-x C-m` | 同上 | 本文 window | プロジェクトメモ |
| `C-x m` | `wamei/project-memo-toggle-global` | posframe | 全体メモ |
| `C-u C-x m` | 同上 | 本文 window | 全体メモ |

2 つのコマンドは共有の内部関数を対象 (プロジェクト / 全体) と表示先で呼び分ける
だけにする。prefix 引数の意味が「全体メモ」から「本文 window」に変わる点が、
既存コマンドに対する非互換な変更になる。

全体メモは当初 `C-x C-S-m` (Shift 付き) を検討したが、実装して確認した結果
存在しないキーだった。`C-x C-M` は Emacs では `C-x C-m` と同一のキー列 (`(kbd
"C-x C-M")` と `(kbd "C-x C-m")` は `equal`) なので Shift を明示する
`C-x C-S-m` と書いたが:

- 端末: modifyOtherKeys の decode 表 (`input-decode-map`) は Return/Tab/記号
  だけを収録し文字キーの entry を持たないため、端末が CSI 27;6;109~ を
  正しく送っても Emacs 側が `C-S-m` として認識できない (tmux + 手動注入で
  確認済み、GOT メッセージが出ず生シーケンスの断片が self-insert された)。
- GUI (Emacs.app): 実測で `C-x <return>` として届き
  (`local-function-key-map` の翻訳)、結局どこにもバインドが無い状態だった。

存在しないキーを設定に残す理由は無いので、素の Emacs では `compose-mail`
だった `C-x m` を (`C-x C-m` が mule-keymap を奪うのと同じ理由で) 意図的に
奪って割り当てた。保険にしていた `C-q m` も、本体の `C-x C-S-m` が機能しない
以上残す理由が無いので一緒に外した。

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
- `:window-point` — メモバッファの現在の point を渡す。`posframe-show` は
  `:window-point` を渡さないと毎回 point 0 に飛ばす
  (`(window-point (or window-point 0))`) ので、渡さないと閉じて開き直す
  たびにカーソルが先頭へ戻ってしまう。

表示後に `select-frame-set-input-focus` でフォーカスを移す。`posframe-show` は
フレームを返すので、それをモジュールの変数に持って以降の判定に使う。

`posframe-show` は child frame の root window を強い dedicated にする
(posframe.el)。show の直後にそれを `(set-window-dedicated-p (frame-selected-window
frame) nil)` で解除する。狙いは「メモの小窓にフォーカスがある状態で
`C-x C-f` や `magit-status` を叩いたら、その小窓の中に開く」こと (下の
「posframe を閉じる条件」参照)。本文 window は一切触らない。

`posframe-workable-p` が nil のときは posframe を出さず、本文 window に
フォールバックする (`C-u` を付けたのと同じ動き)。batch、`emacs_basic_display`、
child frame 非対応環境でコマンドが壊れないようにするため。

## posframe を閉じる条件

次の 2 つ。`ESC` と `C-g` では閉じない (どちらも org の編集中に使うため)。
いずれの経路でも、隠す前に必ずそのメモを保存する。

1. `C-x C-m` / `C-x m` をもう一度押す (トグル)
2. Emacs 内の別の window / frame / タブへフォーカスが移る

2 は `post-command-hook` で見る (`selected-frame` が posframe のフレームで
なくなったら保存して隠す)。

dedicated を外した (前節) ので、小窓の中で `find-file` した別のファイルや
`magit-status` が開いたバッファは、`display-buffer` を経由するかどうかに
関わらずその小窓自身の中に表示される。「メモ以外のバッファが入ったら本文
window へ引き渡す」という当初の案 (handoff) は、これによって前提ごと無く
なったので採らない — 小窓に何が映っていても、フォーカスが小窓に留まって
いる限り閉じない。閉じるかどうかの判断は、次の「トグルの規則」に移した。

代替案として「child frame の window を dedicated にして `display-buffer-alist`
で親へ流す」を検討したが、`switch-to-buffer` は `display-buffer` を通らないため
`switch-to-buffer-in-dedicated-window` をグローバルに変える必要があり、sidebar・
端末パネル・claude パネル (いずれも dedicated) の挙動まで変えてしまう。採らない。

### トグルの規則

dedicated を外したので、小窓に「要求と違うバッファ」が映っているのは異常
ではなく正常な状態になった。トグル (`wamei/project-memo-toggle` /
`wamei/project-memo-toggle-global`) は、いま小窓に映っているバッファ
(`wamei/project-memo--posframe-buffer-shown`、内部の追跡変数ではなく実際に
映っているものを見る) で分岐する。

- 映っているのが要求されたメモ本人 → 閉じる
- それ以外 (別のメモ、または小窓の中で開いたファイル) → 閉じずに要求された
  メモを出す

小窓の中でファイルを開いた後に `C-x C-m` を押すとメモに戻り、もう一度押すと
閉じる、という往復になる。本文 window 表示 (`C-u`) の経路は変わらない。

#### この規則が実機で踏んだ posframe.el 側の 2 つの罠

batch (67 本) と GUI プローブは通ったが、tmux + `emacs -nw` の実機確認で
初めて再現した。どちらも「小窓の中でファイルを開いた後、同じ対象をもう一度
求める」動線 (上のトグルの規則そのもの) でしか起こらない。

1. **親フレームが小窓自身になる** — `posframe-show` は `:parent-frame` を
   渡す口が無く、常に `(selected-window)` の frame を親にする。小窓の中で
   作業した後にフォーカスがそこに残ったまま `posframe-show` を呼ぶと、
   親が小窓自身になる循環した `parent-frame` チェーンを作ろうとして
   \"Circular specification of 'parent-frame'\" で落ちる。
   `wamei/project-memo-posframe-show` は `wamei/project-tabs-base-frame'
   (project-tabs.el) で最上位の実フレームまで遡ってから `posframe-show`
   を呼ぶことで避けている。
2. **フレームを使い回すと窓の中身が変わらない** — `posframe-show` は
   フレームを使い回すとき (生死と直前の引数が一致するとき)
   `set-window-buffer` を呼ばない。窓が強い dedicated で BUFFER 以外を
   映せない、という前提の上の最適化らしい。dedicated を外した今、小窓に
   別のバッファが映ったまま同じ対象を求め直すと、フレームは使い回されるが
   窓の中身は前のバッファのままになる。`wamei/project-memo-posframe-show`
   は `posframe-show` の返り値任せにせず、そのあと自分で
   `set-window-buffer' / `set-window-point` をやり直している。

### 隠すときの入力フォーカス

`wamei/project-memo-posframe-hide` は、選択がまだ posframe の window に残って
いたら本文 window へ戻すが、`select-window` はウィンドウシステムの入力
フォーカスまでは動かさない。child frame が OS レベルの入力フォーカスを
持ったまま隠れると、親フレームのカーソルが非アクティブ表示 (見えない) に
なる。戻す先のフレームに `select-frame-set-input-focus` してから
`select-window` する。「選択が隠したフレームに残っているときだけ戻す」と
いう既存のガードは変えない。

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
   判断関数。通常の ert。トグルの分岐 (小窓に映っているのが要求されたメモ
   本人か、別のメモか、非メモのバッファか) もここに含む。
2. **posframe の呼び出し** — `posframe-show` / `posframe-hide` を `cl-letf` で
   スタブし、正しいバッファ・poshandler・サイズ・`:accept-focus t`・
   `:window-point` で呼ばれるか、dedicated を解除するか、隠すときに保存し
   `select-frame-set-input-focus` するかを検証する。
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
