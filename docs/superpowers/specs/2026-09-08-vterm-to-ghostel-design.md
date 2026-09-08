# vterm から ghostel への移行

Emacs の端末を emacs-libvterm から ghostel (libghostty-vt) に完全移行し、vterm を消す。

## 目的

端末パネル・Claude Code パネル・desktop 復元の土台を vterm から ghostel に移す。移行の主目的は機能追加ではなく、**libvterm の不足を埋めるために書いた自前レイヤーの削除**である。

- `term-faint.el` (libvterm に SGR 2 が無いので入力ストリームを書き換えて色に変換)
- `vterm--get-color` の advice (既定色セルにも色を貼るので auto-dim が透けない)
- `vterm--filter` の advice (shell 起動時の `stty` が pty サイズを上書きする)
- `wamei/term-input-mouse-mode` (vterm がホイールを pty へ渡さない)
- `vterm-timer-delay` を 0 に束縛する回避 (`vterm-send-string` が応答を待ってブロックする)
- `.zshrc` の `_wamei_vterm_prompt_mark` (vterm 独自の OSC 51;A でプロンプト位置を印す)

いずれも ghostel では本体が担当する。移行後は「vterm の代わりに ghostel を使う」設定と、ghostel にも無い 3 つの自前機能 (端末パネル、kill-ring 連携、スクロールバック復元) だけが残る。

## 前提と制約

Emacs 31.1 (aarch64-apple-darwin25)、ghostel 20260902.1753 (MELPA) + ネイティブモジュール 0.53.0 (プリビルド)。

### Phase 0 の実測結果

隔離 daemon (`--init-directory` に本設定を symlink、`elpa` は per-package symlink で本番を汚さない) + tmux の tty frame で実測した。**`ghostel--redraw-now` は render window が無いと描画しないので、frame の無い daemon では何も観測できない** (`ghostel.el:4628`)。

| # | 項目 | 実測 | 帰結 |
|---|---|---|---|
| 1 | faint (SGR 2) | `\e[2mFAINT` → `(:foreground "#8d8d8d")`、`\e[2;31m` → `#8b3d3d` | `term-faint.el` を削除 |
| 2 | 既定色セルの face | 装飾なしの文字は face プロパティが **nil** | `vterm--get-color` advice を削除。term-restore の「default 色も貼られる」対策も削除 |
| 3 | face の持ち方 | `face` プロパティに plist (vterm は `font-lock-face`) | term-restore の読み側をプロパティ名 1 箇所だけ変更 |
| 4 | pty サイズ | window 幅に即追従。80 桁の床なし、`stty` による上書きなし。`window-max-chars-per-line` 基準 | サイズ同期 advice と `vterm-min-window-width` 相当が不要 |
| 5 | pty 幅が window より 5 桁狭い例 | 原因は `display-line-numbers` が有効なまま (行番号が桁を食う) | `display-line-numbers-mode 0` を `ghostel-mode-hook` に移すのは**必須** |
| 6 | OSC 133 | `.zshrc` を無改変で `ghostel-prompt` プロパティが 9 span 付く | `_wamei_vterm_prompt_mark` を削除 |
| 7 | `INSIDE_EMACS` | 子プロセスで `ghostel` | `.zshrc` の `*vterm*` 判定を `*ghostel*` に |
| 8 | ホイール | `emulation-mode-map-alists` 経由で `ghostel--scroll-intercept-up/down` が端末へ転送 (`ghostel.el:1050-1116`) | `wamei/term-input-mouse-mode` を削除 |
| 9 | C-k / C-y | C-k は `ghostel--send-event` (kill-ring 連携なし)、C-y は `ghostel-yank` を内蔵 | kill-line は残す、yank 束縛は削除 |
| 10 | s-v (Cmd+V) | ghostel のマップで未束縛。global の `yank` が勝つ | s-v の束縛は自分で持つ |
| 11 | `buffer-display-table` | ghostel-mode では **nil** (vterm は自前で用意していた) | 背の高いグリフ置換のコードはそのまま動く。コメントの前提だけ修正 |
| 12 | claude-code-ide | ghostel backend あり・推奨。`ghostel-exec` + title tracking 無効化 + `kill-buffer-on-exit` nil + sentinel チェーン | `claude-code-ide--configure-vterm-buffer` は **vterm ブランチ専用**。ここに掛けていた advice 2 本は使えない |
| 13 | `ghostel-pre-spawn-hook` | spawn 直前にホストバッファで走り、`process-environment` が動的束縛されている (`ghostel.el:441-451, 4092`) | スクロールバック復元の環境変数注入に使える |
| 14 | desktop 連携 | `ghostel-mode` が `desktop-save-buffer` を設定し、ハンドラは load 時に登録済み (`ghostel.el:864, 5029`)。復元は dir + identity のみ | 端末の生成・復元は `ghostel-desktop.el` に委譲できる |
| 15 | `ghostel-keymap-exceptions` の照合 | キーマップ構築時に候補キー文字列を `member` で除外する方式 (`ghostel.el:1173-1232`)。文字列は `key-description` 形式 | vterm の `"<C-tab>"` ではなく **`"C-<tab>"`**。`C-S-z` と `C-S-<iso-lefttab>` はそもそも束縛されないので例外に入れる必要がない |
| 16 | `ghostel-create` | `(&optional NAME DISPLAY IDENTITY)`。DISPLAY を渡さなければ表示しない (`ghostel.el:5345`) | term-panel の `save-window-excursion` が不要 |
| 17 | `desktop-restore-eager` | 本設定は 10 (`init.el:1083`) | ghostel-desktop 任せだと 11 番目以降の端末が lazy 復元になり、side window 復元より遅れ得る (§4 で対策) |

### 決めたこと

- vterm は完全に消す (パッケージも設定も)。バックエンド抽象は挟まない。
- キー入力は **ghostel 既定に寄せ、自前レイヤーを削る**。`ghostel-mode-map` を作り込まず、`ghostel-keymap-exceptions` で「Emacs 側に残すキー」だけ宣言する。
- desktop 復元は **`ghostel-desktop.el` を入れ物にし、スクロールバックだけ自前で足す**。
- 端末バッファ名 `*term: <project>[ N]*` は維持する。`display-buffer-alist`・`desktop-side-windows` のディスパッチ表・既存テストの期待値がそのまま通る。

### 操作の変化 (既定に寄せる帰結)

| キー | 現状 (vterm) | 移行後 (ghostel 既定) |
|---|---|---|
| `C-c` | 単発で SIGINT (`vterm--self-insert` に置換) | prefix。SIGINT は **`C-c C-c`** |
| `C-c C-t` | (C-c を潰していたので使えない) | copy mode |
| `C-c C-l` | 同上 | line mode (行を Emacs で編集して RET で一括送信) |
| `C-y` / `M-y` | `vterm-yank` / `vterm-yank-pop` を明示束縛 | `ghostel-yank` / `ghostel-yank-pop` が既定で入っている |
| `C-q` | Emacs 側の prefix (`C-q t c` 等) | 既定は `ghostel-send-next-key`。**例外に入れて Emacs 側を維持する** (`ghostel-send-next-key` は使えなくなる) |
| `C-h` | `vterm-send-backspace` を明示束縛 | 既定で例外 (Emacs help)。init.el 冒頭の `keyboard-translate` で DEL になり `ghostel--send-event` が送る |
| ホイール | 自前で SGR 1006 を生成 | 本体が転送 |
| マウスクリック | vterm は point 移動に横取り | 子プログラムへ転送 (`down-mouse-1` は `ghostel-mouse-press-or-copy-mode`) |

`C-c` が prefix になる点だけ体感が変わる。単発 SIGINT を維持したい場合は `ghostel-keymap-exceptions` から `"C-c"` を外して `C-c` を `ghostel-send-C-c` に束縛すればよいが、それは自前レイヤーを戻すことになるので既定に従う。

## 全体構成

| ファイル | 帰結 |
|---|---|
| `term-faint.el` / `term-faint-test.el` | **削除** |
| `term-input.el` | mouse-mode を削除。kill-line と「C-v を端末へ送る」だけ残す |
| `term-panel.el` | 起動を `ghostel-create` に、title を `ghostel-title` に。サイズ同期 advice を削除。バッファ名と `display-buffer-alist` は不変 |
| `term-restore.el` | 端末の生成・復元を `ghostel-desktop.el` に委譲。スクロールバック書き出しと環境変数注入だけ残す |
| `claude-panel.el` | title の advice を削除し `ghostel-title` を遅延参照。s-v をバッファローカルの合成キーマップへ |
| `claude-grid.el` | 変更なし (vterm API 依存ゼロ) |
| `claude-usage.el` | 変更なし (hook 名は init.el 側) |
| `init.el` | `leaf vterm` → `leaf ghostel`。advice 3 本と `define-key` 6 本を削除 |
| `.zshrc` | OSC 51;A を削除、`INSIDE_EMACS` 判定を変更 |

## 1. init.el

### leaf ghostel

`leaf vterm` を置き換える。`:bind` (C-z / C-S-z / C-q t c / C-tab 系) はグローバル束縛なので現状のまま。

```elisp
(leaf ghostel
  :doc "フレーム下部に固定する端末パネル"
  :ensure t
  :bind (...)                           ; 現状のまま
  ;; 既定 ("C-c" "C-x" "C-u" "C-h" "M-x" "M-:" "C-\\") に、Emacs 側で処理したい
  ;; キーを足す。:set でキーマップを作り直す defcustom なので customize 経由
  ;; (leaf の :custom) で設定する。文字列は key-description 形式。
  ;; C-S-z と C-S-<iso-lefttab> は ghostel が束縛しないので挙げる必要がない。
  ;; ghostel-module-directory は、パッケージディレクトリ内 (既定) だと package の
  ;; 更新でロード中のモジュールが消えるので elpa の外に置く。
  :custom `((ghostel-keymap-exceptions
             . '("C-c" "C-x" "C-u" "C-h" "M-x" "M-:" "C-\\"
                 "C-g" "C-l" "M-o" "M-w"   ; keyboard-quit / recenter / other-window / kill-ring-save
                 "C-q" "C-z"               ; 端末パネルの prefix とトグル
                 "C-<tab>" "C-S-<tab>"))   ; wamei/term-next / -previous
            (ghostel-max-scrollback . 10000)
            (ghostel-module-directory . ,(locate-user-emacs-file "ghostel/")))
  :preface
  (load ... "term-input") (load ... "term-panel")   ; term-faint の load は削除
  (defconst wamei/term-glyph-substitutions ...)     ; 現状のまま
  (defun wamei/term--substitute-tall-glyphs () ...) ; コメントの前提だけ修正
  :init
  (wamei/term-panel-setup)
  :config
  (define-key ghostel-semi-char-mode-map (kbd "C-k") #'wamei/term-input-kill-line)
  (define-key ghostel-semi-char-mode-map (kbd "s-v") #'ghostel-yank)
  (add-hook 'ghostel-mode-hook #'wamei/term--substitute-tall-glyphs)
  (add-hook 'ghostel-mode-hook #'wamei/term--setup-buffer))
```

削除するもの:

- `wamei/vterm-omit-default-background` と `vterm--get-color` の advice (実測 2: 既定色セルに face が付かない)
- `vterm--set-title` の advice (`ghostel-title` が buffer-local で入る)
- `vterm--filter` の advice (実測 4: `stty` 上書きが無い)
- `wamei/term-faint-enable` と `term-faint.el` の load
- `vterm-min-window-width`
- `define-key` の C-c / C-h / C-y / M-y (既定に寄せる)

`vterm-keymap-exceptions` に付いていた「C-c と C-h を外すとキーマップ構築が `starts with non-prefix key` で落ちる」というワークアラウンドのコメントは、ghostel の既定が両方を例外に含んでいるので不要。移植せず削除する。

`ghostel-module-directory` を elpa の外に置くのは公式 docstring の推奨に従う。Phase 0 では既定 (パッケージディレクトリ) で 0.53.0 の取得・ロードに成功しているので、動作条件ではなく運用上の予防。

### hide-mode-line

`vterm-mode-hook` → `ghostel-mode-hook`。`display-line-numbers-mode 0` の hook も同じ (実測 5 のとおり pty 幅に直接効くので落とせない)。

```elisp
:hook ((ghostel-mode-hook) . hide-mode-line-mode)
      ((dired-mode-hook ghostel-mode-hook wamei/term-list-mode-hook)
       . (lambda () (display-line-numbers-mode 0)))
```

### claude-code-ide / docker

- `claude-code-ide-terminal-backend` を `'ghostel` に。
- `claude-code-ide-prevent-reflow-glitch` の指定を削除 (vterm 専用の workaround。ghostel ブランチでは `('ghostel nil)`)。
- `docker-terminal-backend` を `'ghostel` に (`docker-container-ghostel` が同梱されている)。
- `claude-code-ide--configure-vterm-buffer` に掛けていた advice 2 本を削除。mouse-mode は不要になり、s-v は claude-panel.el 側のバッファローカルキーマップに移す (§5)。

### desktop

`init.el:1019` の side window ディスパッチ表 (`\`\*term: ` / `\`\*terminals: ` / `\`\*claude-code\[`) は不変。

ghostel-desktop は desktop の per-buffer 機構で `desktop-read` 中に端末を復元する。side window の復元 (`desktop-after-read-hook`) より前に走るので順序は問題ないが、`desktop-restore-eager` が 10 なので 11 番目以降のバッファは idle 復元に回る。`desktop-restore-eager` は変えず、§4 の「取りこぼしの補完」で端末だけ確実に先に作る。

## 2. term-panel.el

役割 (プロジェクトごとの端末バッファ、下部 side window、端末一覧) は変えない。

| 現状 | 移行後 |
|---|---|
| `wamei/term--create` が `(vterm BUFFER-NAME)` を呼ぶ (`:191`) | `(let ((default-directory dir)) (ghostel-create name))`。第 2 引数 DISPLAY を渡さなければ表示しないので、`save-window-excursion` は不要になる (表示は従来どおり `display-buffer` + `display-buffer-alist` に任せる) |
| `vterm-shell` を `boundp` で参照 (`:17, :249`) | `ghostel-shell` |
| `wamei/term--record-title` + advice (`:237-245`) | 削除。`wamei/term--title-of` は `(buffer-local-value 'ghostel-title buffer)` を読む |
| `wamei/term--sync-size-on-first-output` (`:162-186`) | 削除 |
| `wamei/term--setup-buffer` (`:152-160`) | そのまま (`cursor-in-non-selected-windows` 等の汎用設定のみ) |
| バッファ名生成・正規表現・`display-buffer-alist` (`:106-113, :456-480`) | 不変 |

`wamei/term--title` (buffer-local) は消える。端末一覧の「最後に実行したコマンド」表示は `ghostel-title` から取る。`.zshrc` の `_wamei_set_terminal_title` (preexec で OSC 0) はそのまま動く。

## 3. term-input.el

3 機能のうち 1 つを削除、2 つを残す。

- **`wamei/term-input-kill-line`**: 残す。C-k は ghostel でも端末へ素通しで kill-ring に入らない (実測 9)。送信を `(ghostel-send-key "k" "ctrl")` に変更。行末の空白を落とす処理はそのまま。
- **`wamei/term-input-mouse-mode`**: **削除** (実測 8)。`mwheel` の require、SGR 生成、`vterm-copy-mode` 分岐、`vterm-timer-delay` の 0 束縛、`vterm--term` ガード回避の `with-current-buffer` も一緒に消える。
- **`wamei/term-input-paste-mode`**: minor mode をやめ、`wamei/term-input-send-C-v` という 1 コマンドにする。中身は `(ghostel-send-key "v" "ctrl")`。Claude Code は C-v を受けると自分で osascript を叩いてクリップボードの画像を読むので、Emacs 側で画像を運ぶ必要はない。束縛は claude-panel.el のバッファローカルキーマップで行う (§5)。シェルでは C-v が quoted-insert になるため、端末パネル全体には掛けない。

`declare-function` / `defvar` は `ghostel-send-key` の 1 つだけになる。

## 4. term-restore.el

「入れ物は `ghostel-desktop.el`、スクロールバックだけ自前」に組み替える。

### 委譲する部分 (削除)

`ghostel-mode` が `desktop-save-buffer` に `ghostel-desktop-save-buffer` を設定し、`desktop-buffer-mode-handlers` には load 時にハンドラが登録される。バッファ名・`default-directory`・identity の保存と、`desktop-read` 中の端末再生成はここに任せる。

削除する: `wamei/term-restore--parse-name`、`--buffer-name` (バッファ名の生成・解析)、`--entry` / `--prune` の plist 構造、`WAMEI_TERM_RESTORE` を `process-environment` に積んで `vterm` を呼ぶ起動経路 (`:232-251`)。`wamei/term-restore-all` は「保存された端末を全部作り直す」から「取りこぼしだけ作る」に縮む (下記 4)。

これで `*term: <project>[ N]*` の生成・解析が term-panel.el と二重管理になっている問題も解消する (term-panel.el 側の 1 箇所に一本化)。

### 残す部分

```elisp
(defvar wamei/term-restore-saved nil
  "((BUFFER-NAME DIRECTORY SCROLLBACK-FILE TITLE) ...)。
`desktop-globals-to-save' 経由で desktop ファイルに書く。")
```

1. **保存** (`desktop-save-hook`): `*term: ` で始まる `ghostel-mode` バッファごとに、スクロールバック末尾 `wamei/term-restore-scrollback-lines` 行を SGR エスケープに写してファイルへ書き、1 エントリを積む。現存しないバッファのファイルは消す (`--prune` 相当)。
2. **注入** (`ghostel-pre-spawn-hook`): ホストバッファで走り `process-environment` が動的束縛されているので、`(buffer-name)` でエントリを引いて `(setenv "WAMEI_TERM_RESTORE" file)` する。復元経路が ghostel-desktop であっても、手動で開いた端末であっても同じフックを通る。
3. **再生** (`.zshrc`): 最初のプロンプトの前に `cat` する現状の仕組みをそのまま使う。
4. **取りこぼしの補完** (`desktop-after-read-hook`、深さ -10): エントリのうちバッファが live でないものだけ `ghostel-create` で作り、`ghostel-title` を戻す。

4 が必要なのは `desktop-restore-eager` が 10 のため (実測 17)。desktop の per-buffer 復元は eager 分を超えると idle 復元に回るので、端末が `desktop-side-windows` の復元 (同じ `desktop-after-read-hook`) に間に合わない場合がある。現状は `wamei/term-restore-all` が端末を必ず先に作ることでこれを保証していた。ghostel-desktop が eager に復元していれば 4 は no-op になり、`desktop-restore-eager` を `t` にすれば常に no-op になる。ここを削ると「端末が 11 番目以降に来た desktop でだけパネルが空で復元される」という再現しにくい壊れ方をするので残す。

### face → SGR の変換

| 現状 | 移行後 |
|---|---|
| `font-lock-face` プロパティを読む (`:89-127`) | `face` プロパティを読む (実測 3) |
| 「default 色のセルにも色が貼られる」ので同色は出力しない (`:91-93, :100-106`) | **削除**。装飾なしセルは face が nil (実測 2) |
| 色は `#rrggbb` なので自前パース (`:70-88`) | 不変 (ghostel も `#rrggbb`) |
| `vterm-prompt` プロパティで末尾のプロンプト行を判定 (`:143-151`) | `ghostel-prompt` プロパティ。OSC 133 由来なので付く位置が違う可能性があり、実装時に実測して合わせる |
| 画面下端までの空行を落とす (`:52-56`) | 不変 |

`:title` の保存・復元は現状 `wamei/term--title` に戻していた。移行後は `ghostel-title` が端末の報告で埋まる変数なので、復元直後は空になる (最初のコマンドで埋まる)。**parity を保つため、エントリの TITLE を `desktop-after-read-hook` で `ghostel-title` に戻す** (端末の起動時にバッファ状態のリセットで `ghostel-title` は nil に落ちる — `ghostel.el:5194` — ので、復元は spawn 後でなければならない)。一覧の「最後に実行したコマンド」表示だけの小機能なので、不要なら落としてよい。

## 5. claude-panel.el

- `wamei/claude-panel--record-title` と `vterm--set-title` の advice (`:90-95, :238-239`) を **削除**。`wamei/claude-panel--tab-name` が `(buffer-local-value 'ghostel-title buffer)` を `--clean-title` に通して使う。タブ名は redisplay ごとに評価されるので `force-mode-line-update` は不要になり、buffer-local 状態が 1 つ減る。
  - claude-code-ide は Claude バッファで title tracking (バッファ名の自動リネーム) を切るが、`ghostel--set-title` は `ghostel-title` を無条件に設定するので値は取れる (`ghostel.el:3447`)。
- `use-local-map` + `make-composed-keymap` でバッファローカルにキーを重ねる作り (`:132-136`) はそのまま。ここに `s-v` → `wamei/term-input-send-C-v` を足す。これで init.el の `claude-code-ide--configure-vterm-buffer` advice が不要になる。
- `with-eval-after-load 'vterm` → 不要 (advice が無くなる)。

## 6. .zshrc

- `_wamei_vterm_prompt_mark` の定義 (`:246, :255`) と PROMPT 内の 3 箇所の呼び出し (`:139, :142, :144`) を削除。ghostel は OSC 133 のシェル統合を自動注入する (実測 6)。
- `WAMEI_TERM_RESTORE` を cat するブロックの条件を `[[ $INSIDE_EMACS == *ghostel* ]]` に (実測 7)。
- `_wamei_set_terminal_title` (preexec の OSC 0) は不変。

## 7. テストと検証

### batch テスト

全テストは `emacs -Q --batch -l <file>-test.el -f ert-run-tests-batch-and-exit`。実 ghostel は起動しない (ネイティブモジュールを batch で読まない)。

| テスト | 変更 |
|---|---|
| `term-faint-test.el` | 削除 |
| `term-panel-test.el` | fake の対象を `vterm` → `ghostel-create` に。pty サイズ同期のテスト (`:228-262`) を削除 |
| `term-input-test.el` | スタブを `ghostel-send-key` / `ghostel-yank` に。`vterm-timer-delay` を検証している箇所 (`:44, :165-192`) を削除。mouse-mode のテストを削除 |
| `term-restore-test.el` | propertize を `font-lock-face` → `face` に。default 色のケースを削除。端末生成の fake (`--with-fake-vterm`) を 2 つのテストに置き換え: (1) pre-spawn hook が `WAMEI_TERM_RESTORE` を setenv する、(2) 取りこぼしの補完が live なバッファを作り直さず、欠けているものだけ作る |
| `claude-panel-test.el` | 偽 `provide 'vterm` + advice 存在確認 (`:176-181`) を削除し、`ghostel-title` からタブ名が出ることのテストに置き換え |
| `claude-grid-test.el` / `claude-usage-test.el` | 変更なし |

現状 `term-faint.el` の advice 本体 (`--filter` の carry 状態) と init.el の advice 群はテストが無い。移行で advice ごと消えるため、移行前にテストを足す必要はない。

### 隔離 daemon での目視

Phase 0 と同じ手順 (`emacs --daemon=... --init-directory=<scratch>`、`elpa` は per-package symlink、tmux で tty frame)。**render window が無いと ghostel は描画しないので、frame を付けてから確認する。**

1. `C-z` で端末パネルが開く。`stty size` が window の実幅・実高と一致する
2. 端末を 2 つ以上開いて一覧 (slot 1) が出た状態で、幅 80 未満でも zsh-autocomplete の候補が重ならない
3. Claude Code (`claude-code-ide`) の TUI で faint の薄字が薄く出る。ホイールで TUI 内スクロールが効く
4. Claude パネルで Cmd+V が画像を渡す。タブ名が会話名になる
5. `C-c C-c` で SIGINT、`C-k` で kill-ring に入る、`C-y` で貼れる
6. desktop: 端末 2 つ + Claude パネルを開いて Emacs を落とし、再起動で本数・ディレクトリ・スクロールバックが戻る
7. GUI frame (`emacsclient -c`) で `⏺` `⏵` `⧉` を含む行の行高が揺れない

### 移行後に消えるものの確認

`grep -rn vterm ~/.dotfiles` の結果が、履歴的な記述 (memory / docs) 以外に残らないこと。`~/.emacs.d/elpa` から vterm を削除し、`vterm-module.so` のビルド成果物も消えることを確認する。

## 実装順

1. Phase 0 の隔離環境で ghostel を入れ、上の実測を再現できる状態にする **(完了)**
2. `term-input.el`: mouse-mode 削除、送信 API 差し替え、テスト更新
3. `term-panel.el`: `ghostel-create` / `ghostel-title`、サイズ同期削除、テスト更新
4. `term-restore.el`: ghostel-desktop への委譲、`ghostel-pre-spawn-hook`、face 変換、テスト更新
5. `claude-panel.el`: title の遅延参照、s-v の束縛、テスト更新
6. `init.el`: `leaf ghostel`、hook 名、backend 指定、advice と define-key の削除、`term-faint.el` の load 削除
7. `.zshrc`: OSC 51;A 削除、`INSIDE_EMACS` 判定
8. `term-faint.el` / `term-faint-test.el` を削除、vterm をアンインストール
9. 隔離 daemon + GUI frame で目視確認、全 batch テスト実行

2-5 は互いに独立しているので並列に進められる。6 は 2-5 の後。
