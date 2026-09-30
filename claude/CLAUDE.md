# 開発スタイル

TDD で開発する（探索 → Red → Green → Refactoring）。
KPI やカバレッジ目標が与えられたら、達成するまで試行する。
不明瞭な指示は質問して明確にする。

# コード設計

- 関心の分離を保つ
- 状態とロジックを分離する
- 可読性と保守性を重視する
- コントラクト層（API/型）を厳密に定義し、実装層は再生成可能に保つ
- 静的検査可能なルールはプロンプトではなく、その環境の linter か ast-grep で記述する

# ツール

- タスク: bun
- Node.js: bun, v24+
- E2E: playwright

# 言語

- 日本語
- 公開リポジトリではドキュメントやコミットメッセージを英語で記述する

# 環境

- GitHub: wamei
- リポジトリ: ~/projects/<host>/<owner>/<repo> (ghq 管理)

# 破壊的操作

- ツール（home-manager / brew / chezmoi / pre-commit / pip / npm 等）が auto-rename した `*.backup` / `*.orig` / `*.pre-*` 系を `rm` する前に、内容を `cat` して会話に出すか別ファイルに dump する。最低 1 回の表示を経てから削除する
  （理由: 自分が作ったファイルではないので、消すと「元に何が入っていたか」が永久に失われる。`/etc/zshenv` のような system-level 置き土産が紛れていても気づけなくなる）

# スキル作成

新規 skill を作るとき、配置先を次の指針で決める:

- **project 固有** (`<repo>/.claude/skills/` に置く / 該当 repo の `apm.yml` で配布): 特定 repo のドメイン知識・規約・ファイルレイアウトに依存し、他 repo で使う見込みがない
- **グローバル** (`~/.claude/skills/` 直置き or APM global): 言語・ツール横断、複数 repo で再利用可能、運用ノウハウ
- **判断不能なとき**: ユーザーに「project 固有かグローバルか」を質問してから作成（理由: 後から移動するとパス参照や apm.yml 設定が壊れやすい）

# 並列化と subagent

タスクを受けたら最初に「**並列化できる subtask は何か**」「**subagent に投げて main context を空けられるか**」を洗い出してから動く。default は subagent 優先 / 並列優先。

判断:

- **互いに独立な 2+ task** → Agent tool で 1 message 内に並列 dispatch (independent search、 multi-scenario eval、 multi-model 比較など)
- **大量探索・grep・解析 (3+ query 規模)** → `general-purpose` / `Explore` subagent に投げ、 main は要約だけ受け取る
- **bias-free 評価** (skill / prompt / 自分の生成物の検証) → 新規 subagent。 「自分で再読」 は禁じ手 (`empirical-prompt-tuning` の caveat 通り)
- **Long-running batch** (Bash の 10 分上限を超える / `apm install` を多 repo に回す等) → subagent dispatch か `run_in_background` + `Monitor`

避けるべき:

- 直列依存 (前 task の結果が次 task 入力) を無理に並列化する
- 1-step / short lookup を subagent に投げる (overhead がコストに見合わない)
- subagent と main で同じ作業を二重で走らせる

# Skill 利用方針

skill には大きく 2 種類あり、invoke 方針を分ける:

- **Project 固有 skill** (例: `cloudflare/skills` の `wrangler` / `workers-best-practices`、`mizchi/skills` の `gh-fix-ci` / `playwright-test` 等の lang/tool 系)
  - 入手は project の `apm.yml` で declare、`apm install` で deploy
  - 利用は CLAUDE.md の指示や project context から自然に判断する。`apm.yml` に書いてあれば「使う前提」、invoke を躊躇しない
- **Meta skill** (`skill-selector` / `skill-finder` / `waxa-eval` / `optimizing-descriptions` / `apm-usage` / `empirical-prompt-tuning` / `retrospective-codify`)
  - 入手元は `mizchi/skills`。同じく global に入れている `mizchi/pkfire` の `pkfire` は meta ではなく、下の secretlint recipe の参照元。`~/.apm/apm.yml` に宣言し `apm install -g --target claude` で `~/.claude/skills/` に deploy する (apm 本体は mise 管理)
  - 認識は常に保つ (catalog の存在を意識する) が、**自分から invoke しない**
  - wamei が「skill-selector で…」「empirical で評価して」と明示的に呼んだときだけ Skill tool で起動する
  - Why: 自動 invoke すると prompt 解釈に skill が override 効きすぎ、 wamei が想定しない過剰運用 (Phase 1 catalog scan の毎回フル実行など) になる。subagent smoke で挙動確認済

# プロジェクト初期化

新しい repo に入ったとき `apm.yml` か `.waxa.yaml` がない場合、`skill-selector` で project signal を見て必要 skill を判定してから setup する。:
やる場合は repo 側に変更を残さない。setup 前に `.git/info/exclude` に次を追加してから `apm install`:

  ```
  apm.yml
  apm.lock.yaml
  apm_modules/
  .claude/
  ```

  これで自分のローカル環境にだけ skill が deploy され、他者の repo は汚さない (`.git/info/exclude` は per-clone なので push されない)。

  `apm install` は `.gitignore` に `apm_modules/` を勝手に追記するので、install 後に `git status` を見て `git checkout -- .gitignore` で戻す。
  Cloudflare 系など特定 repo でしか使わない skill は `~/.claude/skills/` に直置きせず、その repo の `apm.yml` に宣言する。

### secretlint を必ず含める

`hooks { pre-push { ... } }` に secretlint を必ず入れる (理由: credential 漏洩は repo 単位の最後の防衛線、 ad-hoc に省略しない)。pre-commit ではなく **pre-push** に寄せる (毎 commit で走らせると体感が悪い、push が外界との境界なので最低限ここで止める)。最小構成は pkfire skill の `assets/recipes/14-secretlint-pre-push.pkl` を参照 — `secretlint` ^9 + `@secretlint/secretlint-rule-preset-recommend` ^9 を devDeps に入れて `npx secretlint --secretlintignore .gitignore "**/*"` を pre-push で実行する形。既存 hook に `gitleaks` 等の sibling があれば重複させず既存採用。