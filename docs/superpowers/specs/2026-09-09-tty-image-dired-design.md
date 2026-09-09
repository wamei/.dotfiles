# tty で image-dired のサムネイル一覧を出す (Phase 2)

tty の Emacs で `image-dired` を使ったときに、kitty graphics protocol の Unicode placeholder でサムネイルをグリッド表示する。

## 目的

Phase 1 (`docs/superpowers/specs/2026-09-09-tty-image-mode-design.md`、実装済み) で、tty でも画像ファイルを 1 枚ずつ開けるようになった。残っているのが `image-dired` のサムネイル一覧である。

Emacs の `image-dired` には `display-images-p` のガードが 1 つも無く、`insert-image` をそのまま呼ぶ。tty では**エラーも出ず空白が並ぶ**だけになる。Phase 1 で入れた `image-mode` への advice は `image-mode` を通る経路にしか効かず、サムネイルの挿入は別経路なので届かない。

## 前提と制約

Emacs 31.1 (aarch64-apple-darwin25)、Ghostty 1.3.1、tmux 3.7 (`allow-passthrough on`)。

Phase 1 で `.emacs.d/kitty-graphics.el` が完成している。これは「端末に画像を置き placeholder 文字列を返す」だけの層で、バッファも frame も dired も知らない。本 Phase はその 2 つ目の呼び手ではなく **3 つ目**にあたる (既存は `dired-image-preview-kitty` と `tty-image-mode`)。

```elisp
(wamei/kitty-graphics-available-p)                            ; tty かつ端末が対応なら非 nil
(wamei/kitty-graphics-cell-size)                              ; セルの (幅 . 高さ) ピクセル
(wamei/kitty-graphics-image-size FILE)                        ; (幅 . 高さ) ピクセル or nil
(wamei/kitty-graphics-cell-count IMAGE-PX CELL-PX MAX-CELLS)  ; (桁 . 行)
(wamei/kitty-graphics-put FILE COLS ROWS &optional SIZE)      ; 画像 ID or nil
(wamei/kitty-graphics-delete ID)
(wamei/kitty-graphics-placeholder-line ID COLS ROW &optional COLOR)
(wamei/kitty-graphics-placeholder-string ID COLS ROWS)
```

### 調査で分かった、設計を決めた 2 つの事実

**点ベースのコマンドは再利用できる。** `m` / `u` / `d` (マーク・削除フラグ)、`.` (dired 追従)、`TAB` (dired へ移動) は、point 上の `original-file-name` / `associated-dired-buffer` テキストプロパティを読むだけで動く。矩形全体に同じプロパティを載せればそのまま使える。

**走査するコマンドは再利用できない。** `image-dired-forward-image` は「1 文字進んで、`image-dired-thumbnail` プロパティが無い間さらに進む」という走査なので、矩形の中では 1 セル右に動いて止まる。`image-dired-do-flagged-delete` と `image-dired--with-marked` は `forward-char 2` で「1 サムネ = 1 文字 + 空白 1 つ」を前提にしている。

**入口は 1 点。** `M-x image-dired` (= `image-dired-show-all-from-dir`) は `dired` → `dired-mark-files-regexp` → `image-dired-display-thumbs` と進む。dired からの呼び出しも `image-dired-display-thumbs` を通る。

### スコープ

- 合格ラインは「一覧 + 開く + dired 連動」。タグ (`t t` / `t r`)、回転 (`L` / `R`)、外部表示、line-up 方式の切り替えは対象外
- 対象は tty で起動した Emacs のみ (Phase 1 と同じく init で `(display-graphic-p)` を一度だけ見る)

## 構成

新規 `.emacs.d/tty-image-dired.el` (prefix `wamei/tty-image-dired-`) 1 ファイル。既存には手を入れない (init.el の leaf 追加を除く)。

```
kitty-graphics.el                 (Phase 1、無変更)
├── dired-image-preview-kitty.el  (既存)
├── tty-image-mode.el             (Phase 1)
└── tty-image-dired.el            (本 Phase)
```

### モードは image-dired-thumbnail-mode から派生させる

```elisp
(define-derived-mode wamei/tty-image-dired-mode image-dired-thumbnail-mode "TtyImageDired" ...)
```

派生させるのが要点である。image-dired の多くのコマンドは冒頭に `(unless (derived-mode-p 'image-dired-thumbnail-mode) (user-error ...))` のガードを持つので、派生していればそれを通過でき、点ベースのコマンドが組み込みのまま動く。

モード本体では Phase 1 と同じく `display-line-numbers-mode` を切り (桁を食われると placeholder の桁数が合わなくなる**機能要件**)、`truncate-lines` を立て、`cursor-type` を nil にする。

### グリッドの構築

**サムネイル 1 枚 = 固定の箱。** 桁・行とも一定の箱を並べる。箱を固定しないとグリッドが不揃いになり、移動の添字計算が成立しない。

既定の箱は `image-dired-thumb-size` とセルのピクセル数から算出する — 桁数は `(ceiling image-dired-thumb-size セル幅)`、行数は `(ceiling image-dired-thumb-size セル高さ)`。defcustom `wamei/tty-image-dired-box-size` で `(桁 . 行)` を直接指定して上書きできる (nil なら算出)。

箱と箱の間は **1 桁**空ける。段の高さは **箱の行数 + キャプション 1 行**。

**画像は箱の中に縦横比を保って置く。** kitty は `c` x `r` の矩形に合わせて拡縮するので、箱に無理やり合わせると歪む。サムネイルの実寸から `wamei/kitty-graphics-cell-count` でセル数を出し、余りは空白で埋める。

**箱の下にキャプション行を 1 行置く。** ファイル名を箱幅に切り詰めて表示する。理由は 2 つある。tty ではどのサムネイルにいるかが分かりにくいこと、そして **placeholder のセルには face を当てられない**こと (前景色が画像 ID そのものなので、face を重ねると画像が壊れる)。キャプションは通常のテキストなので、選択中は `highlight` face、dired 側でマークされていれば頭に `*`、という表現ができる。

**バッファは段単位で組み立てる。** image-dired は「1 枚 = 1 文字」を順に挿入するが、矩形は複数行にまたがる。グリッドの 1 段ぶん (箱の行数 + キャプション 1 行) をまとめて作り、段の数だけ繰り返す。

**テキストプロパティは箱とキャプションの全体に載せる。** image-dired と同じ `image-dired-thumbnail` / `original-file-name` / `associated-dired-buffer` / `tags` / `comment` / `mouse-face` を付ける。これが派生させたことの見返りである。

### 画像 ID と可視範囲

画像 ID は 256 色端末で前景色 `color-N` として表すため **1〜255** に限られ、このプールは `dired-image-preview` (カーソルを止めるたびに 1 つ消費) と `tty-image-mode` (バッファを開いている間ずっと保持) と共有である。写真が 500 枚あるディレクトリを全部 placeholder にすると破綻する。

**表示中のサムネイルだけを端末に送る。** バッファは最初、全部の箱を空白とキャプションだけで組み立てる。window に見えている段の箱にだけ ID を採番して転送し、placeholder の文字を書き込む。見えなくなった箱は placeholder を空白に戻して `wamei/kitty-graphics-delete` で解放する。

可視のサムネイル数は「window の行数 ÷ 段の高さ × 段あたりの枚数」で、実用上 20〜40 枚。255 に対して十分な余裕がある。転送量も画面ぶんに収まる。

きっかけは `window-configuration-change-hook` (Phase 1 と同じ) に加えて、スクロールを拾う `window-scroll-functions`。Phase 1 の「変わっていなければ何もしない」と同じ考え方で、可視範囲が変わったときだけ差分を送受する。

placeholder を書き換えるときはテキストプロパティを載せ直すこと (プロパティが消えると点ベースのコマンドが動かなくなる)。

### サムネイルの実体

置き場所は `image-dired-thumb-name` が返すパスをそのまま使い、無いか元ファイルより古ければ **sips で同期的に作る**。GUI の image-dired とキャッシュを共有できる。

組み込みの `image-dired-create-thumb` は使わない。あれは非同期でプロセスを起こすので「ファイルができるまで箱は空白、できたら描き直す」という非同期の面倒を持ち込む。sips は Phase 1 で既に同期呼び出しの経路があり、キャッシュが効けば 2 回目以降は起動しない。組み込みが `convert` で作る PNG と厳密には同じ画像にならないが、サムネイルなので実害はない。

### 移動

バッファローカルに「グリッド順のサムネイル一覧 (ファイルと箱の開始位置)」と「1 段あたりの枚数」を持ち、添字で動かす。

| キー | 動き |
|---|---|
| `f` / `b` | 添字 ±1 |
| `n` / `p` | 添字 ± 段あたりの枚数 |
| `a` / `e` | 段の先頭 / 末尾 |

端では巡回せずメッセージ (Phase 1 の `n`/`p`、および組み込みの "At last image" に合わせる)。移動のたびに前のキャプションの選択 face を外し、新しい方に付ける。添字の計算は純粋関数として切り出し、batch でテストする。

### 上書きするコマンド

| キー | 理由 |
|---|---|
| `f` `b` `n` `p` `a` `e` | 組み込みは 1 文字ずつ走査するので矩形の中で止まる |
| `x` (`image-dired-do-flagged-delete`) | 組み込みの走査ループが「1 サムネ = 1 文字」前提。自分の一覧を使って書き直す |
| `RET` | 組み込みは `image-dired-image-mode` (`image-mode` 派生) を使うが、それを起こすと Phase 1 の advice と噛み合わない。`find-file` して `auto-mode-alist` → `image-mode` → advice → `wamei/tty-image-mode` の経路に載せる |

加えて `image-dired--thumb-update-marks` を差し替える。組み込みはサムネイルの枠の見た目でマークを表すので、こちらはキャプションの `*` を更新する形にする。これが無いと `m` / `u` / `d` を押しても画面が変わらない。

`m` `u` `d` `U` `.` `TAB` は派生のまま何もせずに動く。

### 入口の配線

`image-dired-display-thumbs` への `:around` advice 1 点。`M-x image-dired` も dired からの呼び出しもここを通る。

`:override` ではなく `:around` にするのは、端末が kitty graphics に非対応のときに**元の実装をそのまま呼ぶ**ため (「失敗時の振る舞い」を参照)。Phase 1 の `image-mode` は元を呼ぶと `error` になるので `:override` + `image-mode-as-text` だったが、`image-dired-display-thumbs` は元を呼んでも無害 (空白が並ぶだけ) なので、今日と同じ挙動に落とせる方がよい。

init.el に leaf を 1 つ足す。Phase 1 と同じく `:if (not (display-graphic-p))`、`:preface` で `(file-name-directory (file-truename user-init-file))` を基準に `load`、`:config` で setup 関数を呼ぶ。

## 失敗時の振る舞い

`error` を投げない (tty で `debug-on-error t` のときにデバッガへ落ちると操作不能になる)。

| 状況 | 振る舞い |
|---|---|
| 端末が kitty graphics 非対応 | メッセージを出して元の `image-dired-display-thumbs` に流す (空白が並ぶだけで害はない) |
| sips がサムネイルを作れない / 大きさを測れない | その 1 枚だけ箱を空白のままにし、キャプションは出す。他の枚数の表示は続ける |
| 端末への転送に失敗 | その 1 枚だけ空白のまま。同じ可視範囲では再試行しない |
| ディレクトリに画像が無い | 組み込みと同じくメッセージだけ |

## テスト

**batch** (`emacs -Q --batch -l tty-image-dired-test.el -f ert-run-tests-batch-and-exit`):

- 段あたりの枚数の算出 — `(max 1 (/ (1- (window-body-width)) (1+ 箱の桁数)))`。Phase 1 の I4 と同じく、tty には fringe が無いので最終桁が truncation glyph に取られるぶん 1 桁引く
- 添字の移動 (`f`/`b`/`n`/`p`/`a`/`e`、端で止まる、1 枚しかない、最終段が半端)
- 可視範囲の算出 (window の先頭行と行数から、見えている段 → サムネイルの添字の範囲)
- キャプションの組み立て (切り詰め、選択、マーク)
- advice の分岐 (`wamei/kitty-graphics-available-p` を mock)
- 差分の送受 (可視範囲が変わったときだけ `put` / `delete` が呼ばれること、変わらなければ呼ばれないこと)

**実端末** (Ghostty + tmux、`emacs -Q -nw` に module を明示 load。素の `emacs -nw` は master の init を読むので変更が効かない):

- グリッドが出ること (`find-composition` で 1 セル = 1 グリフの合成を確認)
- スクロールで送受が入れ替わり、端末に置かれている ID の数が可視ぶんに収まること
- `m` がキャプションに反映され、dired 側にもマークが付くこと
- `RET` で `wamei/tty-image-mode` に入ること

## 受け入れ条件

1. tty で `M-x image-dired` するとサムネイルがグリッドで並ぶ
2. `f` / `b` / `n` / `p` で選択が動き、キャプションで今どれを選んでいるか分かる
3. スクロールしても画像が付いてきて、画面外のぶんは端末から解放される (ID が 255 に達しない)
4. `m` でマークするとキャプションに `*` が付き、dired 側にもマークが付く
5. `RET` で `wamei/tty-image-mode` が開く
6. GUI の image-dired と、Phase 1 の挙動が変わらない

## 非目標

- タグ (`t t` / `t r`)、回転 (`L` / `R`)、外部表示、line-up 方式の切り替え
- `image-dired-dired-toggle-marked-thumbs` (dired バッファの中にサムネイルを差し込む機能)
- GUI frame と tty frame の混在 (Phase 1 と同じ。別途対応する)
- サムネイルの非同期生成
