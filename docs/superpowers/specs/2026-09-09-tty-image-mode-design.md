# tty の Emacs で画像ファイルを開く (Phase 1)

tty の Emacs で画像ファイルを開いたときに、kitty graphics protocol の Unicode placeholder で画像を表示する。あわせて image-mode の行番号を消す。

## 目的

Ghostty (直下 / tmux 内) で動かしている `emacs -nw` では、画像ファイルを開くと `image-mode` が入口で失敗する。

```elisp
;; image-mode.el:656
(unless (display-images-p)
  (error "Display does not support images"))
```

GUI と同じ操作感で画像を見られるようにする。`image-dired` のサムネイル一覧は Phase 2 に分ける (本 spec の対象外)。

## 前提と制約

Emacs 31.1 (aarch64-apple-darwin25)、Ghostty 1.3.1、tmux 3.7 (`allow-passthrough on`)。

`dired-image-preview-kitty.el` (commit 1d33503) で、tty の Emacs から Ghostty へ画像を出す手段は実証済み:

- 方式は kitty graphics protocol の **Unicode placeholder (`U=1`)**。画像を `a=T,U=1,f=100,i=ID,c=COLS,r=ROWS` で送り、`U+10EEEE` + 行の結合文字 + 桁の結合文字を**前景色 = 画像 ID** で並べたセルに端末が描く
- 座標計算も tmux の pane 補正も要らない。Emacs の redisplay が placeholder を置いたり消したりするのがそのまま画像の表示・消去になる
- 結合文字は `compose-string` で 1 グリフに合成しないと tty Emacs が 1 桁ずつ描き、画像が 3 セルおきの縦縞になる
- 端末の色数が 256 のとき (tmux 内は `TERM=xterm-256color`) 画像 ID は前景色 `color-N` で表すため **1〜255** に限られる

### スコープの制約

- **対象は tty で起動した Emacs のみ**。GUI で起動した Emacs に `emacsclient -nw` で tty frame を足す使い方は対象外 (既存 `dired-image-preview-kitty` と同じく、init での `(display-graphic-p)` で一度だけ判定する)
- 合格ラインは「見える + 追従」。拡大縮小・スクロール・回転は GUI のみ

## 構成

3 つのファイルに分ける。既存の `dired-image-preview-kitty.el` から描画層を切り出し、新しいモードと共有する。

```
kitty-graphics.el          端末に画像を置き placeholder 文字列を返す (バッファも frame も知らない)
├── dired-image-preview-kitty.el   child frame に出す (既存、薄くなる)
└── tty-image-mode.el              画像ファイルを開くバッファ (新規)
```

### kitty-graphics.el

prefix は `wamei/kitty-graphics-`。公開インターフェース:

| 関数 | 役割 |
|---|---|
| `(…-available-p)` | tty かつ端末が `a=q` に OK を返すか。端末ごとにキャッシュ |
| `(…-cell-size)` | セルの `(幅 . 高さ)` ピクセル。`CSI 16 t`、訊けなければ `(8 . 16)` |
| `(…-image-size FILE)` | sips で測った `(幅 . 高さ)` ピクセル |
| `(…-cell-count IMAGE-PX CELL-PX MAX-CELLS)` | 縦横比を保って収めた `(桁 . 行)`。純粋関数。拡大はしない |
| `(…-put FILE COLS ROWS)` | PNG 化して転送し画像 ID を返す。失敗なら nil |
| `(…-delete ID)` | 端末側の配置とデータを解放 |
| `(…-placeholder-line ID COLS ROW)` | 1 行ぶんの propertized 文字列 (結合文字を合成済み、前景色 = ID) |

既存 `dired-image-preview-kitty.el` から**中身を変えずに移す**内部関数: `--wrap` `--chunks` `--transmit-sequences` `--delete-sequence` `--query-sequence` `--query-ok-p` `--parse-cell-size` `--cell-count` `--diacritic` `--color` `--placeholder-line` `--next-id` `--prepare-png` `--image-size` `--parse-sips-size` `--resize-args` `--send` `--read-response` `--query-terminal`、および定数 `--chunk-size` `--placeholder` `--diacritics`。

defcustom 2 つは `wamei/kitty-graphics-max-pixels` / `wamei/kitty-graphics-response-timeout` に改名する。個人設定なので互換 alias は置かない。

`dired-image-preview-kitty.el` に残るのは child frame 固有の `--frame-position` `--make-frame` `--fill-buffer` `--max-cells` と `show` / `hide` / `setup`。388 行から 150 行程度に縮む。

**ID の所有権**: `…-put` が返した ID の解放は呼び手の責任とする。Phase 2 でグリッドに何十枚も同時に出すため、所有者を明確にしておく。255 枚を超える同時表示は想定しない (巡回して古いものを上書きする)。

### tty-image-mode.el

prefix は `wamei/tty-image-`。`special-mode` から派生した major mode `wamei/tty-image-mode`。

**入口**: `auto-mode-alist` は触らず、tty のときだけ `image-mode` に `:override` の advice を置いて自分のモードへ回す。files.el の拡張子エントリ (`\\.png\\'` → `image-mode` 等)、`M-x image-mode`、dired の RET、bookmark 復元がすべてこの 1 点を通るので、配線が 1 箇所で済む。

advice を置くのは tty で起動したときだけ (init.el の leaf が `:if (not (display-graphic-p))`)。advice の中で `(wamei/kitty-graphics-available-p)` を見て、真なら `wamei/tty-image-mode`、偽なら `image-mode-as-text` に落とす (「失敗時の振る舞い」を参照)。

**バッファの持ち方**: image-mode と同じく、ファイルの生データはバッファに残したまま `(point-min)`〜`(point-max)` に `display` テキストプロパティとして placeholder 文字列 (行ぶんを改行で連結) を被せ、`set-buffer-modified-p nil` で戻す。`erase-buffer` して書き換える案はファイル訪問バッファを壊しうるので採らない。

**サイズと追従**:

- 上限は `(window-body-width)` × `(window-body-height)`。縦横比は `…-cell-count` が保つ
- 再描画のきっかけは `window-size-change-functions` (frame のリサイズ) と buffer-local な `window-configuration-change-hook` (別 window に出た / 分割された)
- 最後に描いた `(桁 . 行)` を buffer-local に覚え、**変わったときだけ**古い ID を `…-delete` して再送する。判定は純粋関数に切り出す
- `kill-buffer-hook` で `…-delete`

**キー** (`special-mode-map` の上に):

| キー | 動作 |
|---|---|
| `n` | 同じディレクトリの次の画像へ (`find-alternate-file`) |
| `p` | 同じディレクトリの前の画像へ |
| `g` | 再描画 |
| `q` | `quit-window` (special-mode 由来) |

次/前は `image-mode` の `image-next-file` に相乗りせず、`image-file-name-regexp` でディレクトリを絞る自前関数にする。image-mode の内部状態に依存しないので batch でテストできる。

**バッファ設定**: `truncate-lines t`、`cursor-type nil`、`display-line-numbers-mode 0`。行番号は見た目の問題ではなく、桁を取られると placeholder の桁数が合わなくなるので**機能要件**。

### 行番号 (GUI 側)

GUI の `image-mode` で行番号を消すのは見た目の設定なので、init.el の `hide-mode-line` leaf にある hook リスト (init.el:343) に `image-mode-hook` を足す。

```elisp
((dired-mode-hook ghostel-mode-hook wamei/term-list-mode-hook image-mode-hook)
 . (lambda() (display-line-numbers-mode 0)))
```

### init.el の配線

`kitty-graphics` を読む leaf を `dired-image-preview-kitty` より前に置く。既存と同じく `:if (not (display-graphic-p))` と `:preface` の `load` (init.el は symlink なので実体の隣から読む)。その後ろに `tty-image-mode` の leaf を追加する。

## 失敗時の振る舞い

`error` を投げない。tty で `debug-on-error t` のときにデバッガへ落ちると操作不能になるため (memory: `kitty-graphics-placeholder-emacs-tty` で踏んだ罠)。

| 状況 | 振る舞い |
|---|---|
| 端末が kitty graphics 非対応 | メッセージを出して `image-mode-as-text` に落ちる (元の `image-mode` を呼ぶと `error` になるため) |
| sips が大きさを測れない (画像でない / 空ファイル / sips の失敗) | メッセージを出して `image-mode-as-text` に落ちる |
| 端末への転送に失敗 | メッセージを出す。モードには入ったままで、同じ大きさでは再試行しない (`g` で再試行できる) |

`image-mode-as-text` は `major-mode-restore` に `'(image-mode image-mode-as-text)` を渡すので、`auto-mode-alist` から `image-mode` を外した状態で `normal-mode` を呼ぶ。advice は `image-mode` に掛かっているため、ここから advice へ戻る再帰は起きない。

`…-available-p` は `terminal-parameter` にキャッシュ済みなので、find-file のたびに端末へ問い合わせることはない。

## テスト

**batch** (`emacs -Q --batch -l <file>-test.el -f ert-run-tests-batch-and-exit`):

- `kitty-graphics-test.el` — 既存 `dired-image-preview-kitty-test.el` から純粋関数のテストを移設。エスケープシーケンス生成、`--parse-cell-size`、`--cell-count`、placeholder の合成 (`find-composition` で 1 セル = 1 グリフを確認)
- `tty-image-mode-test.el` — 再描画要否の判定、次/前のファイル選択 (`image-file-name-regexp` で絞る / 端で止まる / 1 枚しかない)、advice の分岐 (`…-available-p` を mock して、真なら tty モード / 偽なら `image-mode-as-text`)
- `dired-image-preview-kitty-test.el` — 移設したぶんを削り、残りが緑であることでリファクタを担保

**実端末**: tmux の中で `emacs -nw` に実 init を読ませて画像を開き、`tmux capture-pane` で placeholder 行が **1 セル = 1 桁**に合成されていることを機械的に確認する。起動前に隔離 dir の desktop を消す (GUI で開いた画像バッファが残っていると tty で image-mode がエラーになる)。目視は最後の確認だけ。

## 受け入れ条件

1. tmux + Ghostty の `emacs -nw` で PNG / JPEG を開くと画像が出る
2. window の分割・frame のリサイズに追従して描き直る
3. `n` / `p` で同じディレクトリの次 / 前の画像へ移る
4. `q` で閉じると端末に画像が残らない
5. GUI Emacs は従来どおり `image-mode` で開き、行番号だけが消える
6. 既存の `dired-image-preview` の挙動が GUI / tty とも変わらない

## Phase 1 の非目標

- `image-dired` のサムネイル一覧 (Phase 2)
- `+` / `-` の拡大縮小、画像が window より大きいときのスクロール、回転
- アニメーション GIF
- `C-c C-c` によるテキスト表示との往復
- GUI frame と tty frame の混在 (同一 Emacs に両方ぶら下げる使い方)
