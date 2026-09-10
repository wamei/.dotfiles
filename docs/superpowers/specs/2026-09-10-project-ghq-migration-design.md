# ~/projects 配下のプロジェクトを ghq レイアウトへ移し、Claude セッションを追随させる

- 日付: 2026-09-10
- 対象: `~/projects` 直下の 43 ディレクトリ、および移動に追随すべきホーム配下の状態ファイル
- 先行 spec: `docs/superpowers/specs/2026-09-09-mise-bootstrap-migration-design.md` の「対象外」節で別 spec とされた作業

## 背景

先行 spec で `ghq.root = ~/projects` を設定し、dotfiles 自身を
`~/projects/github.com/wamei/.dotfiles` へ移した。新しく clone するものは ghq
レイアウトに載るが、**既存の 43 ディレクトリは `~/projects` 直下に残ったまま**である。

さらに、その dotfiles の移動で問題が 1 つ露呈した。**Claude Code のセッション履歴が
追随していない。** `~/.claude/projects/-Users-wamei--dotfiles` に旧 `~/.dotfiles`
時代のセッションが 122 本取り残されており、新パス側には移動後の 6 本しかない。
`claude --resume` はカレントディレクトリ由来のスラッグのディレクトリしか見ないため、
過去 122 本は事実上失われている。

同じことを 43 件で繰り返すわけにはいかない。

## 目的と成功条件

1. remote を持つ 26 件が `~/projects/<host>/<owner>/<repo>` に載っている
2. remote を持たない 16 件が `~/projects/local/<name>` に集まっている
3. 移動後のディレクトリで `claude --resume` が**移動前のセッションを列挙する**
4. 取り残されている dotfiles の 122 本が復旧している
5. 移動と追随が**再利用可能なコマンド**になっていて、将来 `local/` から ghq へ
   上げるときにも同じものが使える

## 方針

**使い捨てスクリプトにしない。** 今回の発端は「前回の移動で追随の手当てがなかったこと」
なので、手当てを道具として残さなければ次の移動で同じ穴に落ちる。

## 移行マッピング

`ghq migrate --dry-run` を全 43 件に対して実行して確定させた（推測ではない）。

### ghq レイアウトへ: 26 件

| 移行先プレフィクス | 件数 | 対象 |
|---|---|---|
| `github.com/RIT-Inc-Dev/` | 9 | lt_scheduler, mc-data-catalog, mc-gpt, mcwf-api, mcwf-web, quirrel, rit-dev-book, rither, SDXFW_TEMPLATE |
| `github.com/Marubeni-BeecoProgram/` | 2 | BeecoV2, ImportApps |
| `github.com/fumiyagi/` | 2 | GPT-portal-meeting-assist, GPT-portal-review |
| `github.com/wamei/` | 1 | dominion-card-generator |
| `github.com/mc-digital/` | 1 | mc-it-service-mc-gpt |
| `github.com/` (外部 repo) | 2 | alexa-remote-control (thorsten-gehrig), aws-kms-sign-csr (g-a-d) |
| `gitlab.com/beeco1/backend/` | 5 | farmnote-client, farmnote-common, farmnote-momo, farmnote-report, notification |
| `gitlab.com/beeco1/web/` | 1 | farmnote-fi |
| `bitbucket.org/rit-hamada/` | 2 | misetaro-backend, misetaro-frontend |
| `github.com.linka/linka-admin/` | 1 | ietateru |

内訳: github.com 17 / gitlab.com 6 / bitbucket.org 2 / github.com.linka 1 = 26 件。

### `~/projects/local/` へ: 16 件

remote 無し 12 件: aws-sagemaker, clock, dr-hama.jp,
GPT-portal-early_deploy_mcproto2_20250312_1, hama-reminder, mc-application-review,
mc-research, memotan, memotan_knowledge, proxy, sync-dashboard, waminder

非 git 4 件: alexa-reminder, aws_cli_test, mc-research-server, pytest

後から remote を作れば `gh repo create --source=.` と `ghq migrate` の 2 コマンドで
ghq レイアウトへ上げられる（`ghq migrate` を実機で確認済み。remote が無い場合は
`failed to get remote URL: no remotes found`、非 git は
`failed to detect VCS backend` で終了コード 1）。

### 削除: 1 件

`ImportApps_2` は `ImportApps` と同じ remote (Marubeni-BeecoProgram/ImportApps) を
持ち ghq パスが衝突する。状態を確認済み: clean、unpushed 0、stash 0、
最終コミット 2026-02-28 (`047bce7`)。本体の `ImportApps` の方が新しい (2026-07-09)。
削除してよいとの判断を得ている。削除直前にもう一度状態を表示してから消す。

## 調査で確定した事実 (再調査不要)

### Claude Code のスラッグ規則

バイナリ (`claude` 2.1.259) から抽出した実装:

```js
function k(e){ return e.replace(/[^a-zA-Z0-9]/g, "-") }
function sC(e){ let n = k(e); if (n.length <= 200) return n;
                return `${n.slice(0,200)}-${be(e)}` }   // be = base36(32bit hash)
```

- 英数字以外の**1 文字が `-` 1 個**に置換される。`/` `.` `_` すべて
- 大文字小文字は保存 (`SDXFW_TEMPLATE` → `SDXFW-TEMPLATE`)
- 連続 `-` は畳まれない。先頭の `/` も `-` になるので必ず `-` 始まり
- 200 文字超のみハッシュ接尾辞が付く

`~/.claude/projects/` の全 18 ディレクトリと各 jsonl の `cwd` の突合で全件一致を確認済み。

**この変換は非可逆で多対一である。** `mc-data-catalog` / `mc_data_catalog` /
`mc.data.catalog` は同じスラッグになる。したがって**新スラッグは必ず実パスから
再計算する**。旧スラッグ文字列を加工して新スラッグを作ってはならない。

### 移動時に書き換えが必要な Claude 側の 5 箇所

| 対象 | 内容 |
|---|---|
| `~/.claude/projects/<slug>/` | ディレクトリ名。`--resume` / `--continue` はこれだけを見る |
| 配下の `*.jsonl` | `cwd` (ほぼ全行)、`persistedOutputPath` (旧スラッグを含む)。表示だけでなく `attach` / `respawn` / worktree 解決に効く |
| `~/.claude.json` の `.projects` | 絶対パスがキー。21 件。移さないと trust dialog 再表示、`allowedTools` と MCP 許可がリセット |
| `~/.claude.json` の `.githubRepoPaths` | `owner/repo` → 絶対パス配列。12 エントリ / 14 パス |
| `~/.claude/history.jsonl` | 各行の `project` が絶対パス。約 1900 行。↑キーのプロンプト履歴がディレクトリ単位で引かれる |

公式の移行手段は存在しない (`claude project` にあるのは `purge` のみ)。

書き換え不要と確認したもの: `shell-snapshots/`、`session-env/`、`sessions/`、`ide/`、
`tasks/`、`jobs/`、`~/.claude/settings.json`、`<project>/.claude/settings.local.json`
(ディレクトリごと移動するため)、スクラッチパッド (`/private/tmp/claude-501/<slug>/`、
一時領域なので再生成される)。

`~/.claude/file-history/` は編集対象ファイルの絶対パスのハッシュをキーに持つため、
移動後に古いセッションから rewind / checkpoint 復元はできない。**これは諦める。**

### 追随対象のセッション

移動対象のうちセッションを持つのは 14 ディレクトリ / 95 本。
これに取り残されている dotfiles の 122 本を加えて **計 217 本**。

`~/.claude/projects/-Users-wamei-projects-apple-reminder-sync` (1 本) は実体
ディレクトリが既に無い孤児。**対象外とし放置する。**

### 絶対パス参照の実態

`.env` 44 ファイル、`docker-compose*` / `Dockerfile` 30 ファイル超、`Makefile`、
`.vscode/`、`.git/config` は**すべて絶対パス参照ゼロ**だった (bind mount も全部相対)。
懸念していたほど壊れない。

明確に壊れるのは以下:

| 対象 | 内容 | 手当て |
|---|---|---|
| BeecoV2 の git worktree | `.git/worktrees/pr1054-test-fix/gitdir` と `.claude/worktrees/pr1054-test-fix/.git` が双方向に絶対パス | `ghq migrate` が移動後に `git worktree repair` を自動実行する (dry-run で確認済み)。手当て不要 |
| ietateru の git hooks | `pre-push` / `prepare-commit-msg` が lefthook バイナリを絶対パス起動。hook が沈黙して無効化される | `npx lefthook install` で再生成 |
| aws-kms-sign-csr の direnv | allow がパスのハッシュ | 移動後に `direnv allow` |
| `*.code-workspace` 3 本 | `ImportApps` / `ImportApps_2` / `BeecoV2`。参照先 `../BeecoProgram/...` は**移動前から既に存在せず壊れている** | 今回の移動とは独立の既存の壊れ。触らない |

挙動が劣化するもの (自動で手当てする):

- `.claude/settings.local.json` 5 本の permission allowlist —
  BeecoV2 57 / farmnote-momo 13 / rither 11 / farmnote-report 4 / mc-gpt 1 エントリ
- Emacs の状態ファイル 7 本 — `recentf` / `places` / `.emacs.desktop` /
  `.emacs.desktop-nw` / `projects` / `.cache/treemacs-persist` / `history`、計 250 行超。
  BeecoV2 / ietateru / mc-gpt / mcwf-web / mcwf-api に集中

再生成で直るので触らないもの: ビルド生成物 約 2000 ファイル
(`.next` / `.open-next` / `.build` / `.wrangler` / `target` / `.nuxt` / `.chrome-profile`)、
`SDXFW_TEMPLATE/.git/hooks/*` 19 本 (husky の生成コメントのみ)、
`.venv/pyvenv.cfg` 2 件 (`command =` 行の記録のみ)、`~/.docker/buildx/refs/` 102 ファイル、
VSCode / Antigravity の workspaceStorage・History・state.vscdb、
zsh の `chpwd-recent-dirs`、`recently-used.xbel`。

移動は同一ボリュームの rename なので、ファイル数は移動速度に影響しない。
**ビルド生成物を事前に消す必要はない。**

## 設計

### コマンド 2 本に分ける

分割の理由は 1 つ。**`~/.claude.json` は Claude Code が起動中ずっと書き戻している**
(`~/.claude/backups/` に数分おきのバックアップが溜まっている)。起動中に書き換えても
上書きで消える。一方でディレクトリの移動そのものは普段の作業中にできるべきである。
両者を 1 本にすると、移動のたびに Claude を落とす必要が出る。

#### `project-move [--dry-run] [--to <path>] <dir>...`

Claude が動いていても安全な作業だけを担当する。

1. **事前確認** — 未コミット変更 / unpushed / stash を表示する (消さない、見せるだけ)
2. **移動** — remote あり → `ghq migrate -y` (移動先は ghq が決める)。
   remote 無し・非 git → `~/projects/local/<name>` へ mv。`--to` で明示指定も可
3. **手当て**
   - `.claude/settings.local.json` の旧絶対パスを新パスへ (JSON をパースして置換)
   - git hooks に旧絶対パスが残っていれば**警告と再生成コマンドの提示のみ**。
     lefthook / husky は再インストールが正解なので自動実行しない
   - `.envrc` があれば `direnv allow` が要る旨を表示
   - Emacs の状態ファイル 7 本を置換。**Emacs 起動中なら拒否して警告する**
     (保存時に上書きされるため)
4. **移動ログ追記** — `~/.local/state/project-move/moves.tsv` に
   `timestamp<TAB>old<TAB>new`

#### `claude-state-move [--dry-run] [--from-log | <old> <new>]`

Claude を落としてから叩く。移動ログを読んで未処理分をまとめて処理できる。

1. **Claude プロセスが生きていたら拒否する** (`--force` は用意しない)
2. `~/.claude.json` と `~/.claude/projects/` をバックアップ
3. `~/.claude/projects/<旧slug>` → `<新slug>` に mv。移動先が既にあればマージする
   (ファイル名が UUID なので実質衝突しない)
4. 配下の `*.jsonl` の旧パス文字列と旧スラッグ文字列を置換
5. `~/.claude.json` の `.projects` キーをリネーム、`.githubRepoPaths` の値を置換
6. `~/.claude/history.jsonl` の `project` を置換

テスト可能性のため、`~/.claude` と `~/.claude.json` の位置は
`--claude-home` / `--claude-json` で差し替えられる。`CLAUDE_CONFIG_DIR` は
バイナリに存在するが `~/.claude.json` がその配下に移るかは未確認なので、
env のセマンティクスには依存しない。

### 200 文字超は実装せず落とす

スラッグの 200 文字超のケース `slice(0,200) + "-" + base36(hash(fullPath))` の
`hash` の実装を再現できていない。推測で実装すると静かに誤ったスラッグを作るので、
**200 文字を超えるパスが来たらエラーで停止する**。現在の 43 件と全移行先は
200 文字未満なので実害はなく、将来引っかかったときに気づける。

### 配置と実装言語

- ソースは `tools/project-move/` に置く。実装は TypeScript、実行は bun
  (`[tools]` に既にあり、CLAUDE.md のタスクランナー指定でもある)
- `~/.claude.json` (21 プロジェクトキー) と `history.jsonl` (約 1900 行) への手術は、
  sed ではなく本物の JSON パーサで行う
- `mise.toml` の `[dotfiles]` に `~/bin/project-move` と `~/bin/claude-state-move` を
  足して symlink する
- `mise.toml` に `[tasks.test]` (`bun test`) を足す

**symlink 経由の import 解決は検証項目とする。** bun は realpath 基準で相対 import を
解決するはずだが、`~/bin/project-move` から共有モジュールを引けることを
テストで証明する (目視で済ませない)。

## テスト戦略

副作用のない純関数と I/O を分け、TDD (Red → Green) で進める。

| 単位 | Red で使うデータ |
|---|---|
| `slug(path)` | **実データのゴールデン**。`~/.claude/projects/` の 18 ディレクトリと各 jsonl の `cwd` の対応表を一度吸い出して fixture に固め、全件一致で Green |
| `rewriteClaudeJson(json, moves)` | `.projects` のキーリネーム、移動先キーが既存だった場合のマージ、`.githubRepoPaths` の配列内置換と重複整理、無関係キーが無傷であること |
| `rewriteJsonlLine(line, moves)` | `cwd` / `persistedOutputPath` (旧スラッグを含む) / 本文中の絶対パス |
| `planMove(dir)` | tmpdir に `git init` + `remote add` した偽リポジトリで、remote あり / remote 無し / 非 git の 3 分岐 |

統合テストは tmpdir に偽の `~/.claude` ツリーを組み、`--claude-home` /
`--claude-json` を差して実行し結果を assert する。**本物のホームには一切触らない。**

### dry-run を唯一の検証面にする

先行 spec で「`[dotfiles]` の未知キーは警告なしに無視されるので目視レビューは検証に
ならない」と学んだ。同じ姿勢を取り、`--dry-run` に以下を出させる。

- 各ディレクトリの移動元 → 移動先
- 書き換わる状態ファイルと**変更行数** (Claude 5 箇所、Emacs 7 本、`settings.local.json`)
- 自動化せず手作業に回すもの (lefthook / husky の再生成、`direnv allow`)
- 衝突と警告 (スラッグ衝突、移動先が既に存在、Emacs / Claude が起動中)

### 実機検証は canary 1 件で往復させる

fixture テストが通っても「本物の `claude --resume` が引けるか」は別問題である。
**`dominion-card-generator`** (自分の repo、セッション 2 本、ghq パス) を canary とし、
`cd ~/projects/github.com/wamei/dominion-card-generator && claude --resume` が
移動前の 2 本を列挙することを確認してから残りに広げる。

**`claude-state-move` は Claude 全停止が前提なので、この確認は Claude 自身では
実行できない。** 用意するのは貼れるコマンド列と dry-run 出力までで、実行と確認は
人間の手番になる。

## 実行順序

1. 道具を作る (TDD)。ホームには触らない
2. canary 1 件 — `project-move` → Claude 停止 → `claude-state-move` →
   `claude --resume` 確認
3. 残り 42 件
4. dotfiles の 122 本を復旧
   (`claude-state-move /Users/wamei/.dotfiles /Users/wamei/projects/github.com/wamei/.dotfiles`)。
   移動先には既に 6 本あるのでマージ経路の実適用例になる
5. `ImportApps_2` を削除

## 切り戻し

- 移動: `moves.tsv` の old / new を逆に mv
- Claude 状態: `claude-state-move` が実行前に取ったバックアップから戻す

## リスク・未確定

1. **git worktree の正規化ロジックが未検証。** Claude のバイナリに
   `canonicalWcRootForProject()` があり、worktree の場合はメイン worktree ルートへ
   スラッグを寄せる可能性がある。実データでは
   `BeecoV2/.claude-worktrees/pr1054-test-fix` が独立ディレクトリを持っているので
   条件は不明。BeecoV2 は移行後に挙動を確認する
2. **`~/.claude.json` の同時書き込み競合。** 全 Claude プロセス停止中に行う。
   コマンド側でプロセス検査して拒否するが、検査をすり抜ける起動形態は残りうる
3. **jsonl の全文置換の巻き込み。** 会話本文にも旧パス文字列が大量に含まれる。
   `persistedOutputPath` との整合を取るため全置換する方針だが、
   「昔このパスだった」という歴史的記述も書き換わる。実害はほぼ無いと判断する
4. **スラッグ衝突。** 現状の 18 件では衝突なしを確認済み。移行後の
   43 件分についても dry-run で衝突検査する

## 対象外

- 各プロジェクトのランタイム定義 (`.nvmrc` 3 件、`.python-version` 1 件) を
  mise の `mise.toml` へ寄せる作業。別 spec とする
- VSCode / Antigravity / Claude Desktop の状態ファイルの追随。
  再度開けば直るものであり、自動化の価値が薄い
- `~/.claude/file-history/` の追随 (原理的に諦める)
- `~/.claude/projects/-Users-wamei-projects-apple-reminder-sync` (実体が無い孤児)
