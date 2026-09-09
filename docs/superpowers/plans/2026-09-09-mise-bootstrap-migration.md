# mise bootstrap 移行 実装計画

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `init.sh` の手書き `ln -sf` と `README.md` の `brew install` 手順書を、リポジトリ直下の `mise.toml` による宣言的な `mise bootstrap` へ置き換え、リポジトリ自体を ghq レイアウト (`~/projects/github.com/wamei/.dotfiles`) へ移す。

**Architecture:** リポジトリ直下に新規 `mise.toml` を置き、`[dotfiles]` (symlink)・`[bootstrap.packages]` (brew)・`[bootstrap.macos.defaults]`・`[bootstrap.user]`・`[tasks.*]` を宣言する。ランタイム定義 `[tools]` / `[settings]` は既存の `.config/mise/config.toml` に残し、brew から移せる formula をそこへ足す。`init.sh` は brew と mise を入れて `mise bootstrap` に渡すだけの seed スクリプトへ縮小する。

**Tech Stack:** mise 2026.9.1 (macos-arm64), Homebrew, zsh, TOML

**Spec:** `docs/superpowers/specs/2026-09-09-mise-bootstrap-migration-design.md`

## Global Constraints

- **mise は 2026.9.1。公開ドキュメントより古い。** ドキュメントにある `[dotfiles]` の `mode = "track"`、`[bootstrap.macos.defaults]` の配列/テーブル値、`mise bootstrap --adopt` は**この版では動かない**。使わない。
- **`[dotfiles]` のエントリ内の未知キーは警告なしに無視される。** 目視レビューは検証にならない。各タスクの検証は必ず `mise bootstrap -n` と `mise bootstrap status` の出力で行う。
- `mise bootstrap` は**必ずリポジトリ直下で実行する**。リポジトリ外からだと `[bootstrap.*]` が設定階層に入らない。
- 新規 `mise.toml` は初回に `mise trust` が必要。
- **Task 8 まで `~/.dotfiles` を動かさない。** 既存の symlink がすべて `/Users/wamei/.dotfiles/...` を指しており、途中で動かすと以降の検証が成立しない。
- 破壊的操作の前に必ず現状を控える。特に `~/.gitconfig` と `~/.gitignore` は**リポジトリ管理外の実ファイル**なので、置き換える前に内容を会話に出す (CLAUDE.md の「破壊的操作」節)。
- `brew uninstall` は**実行しない**。コマンドを提示するだけ。
- コメントは日本語。「なぜそうしたか」を書く。既存の `.config/mise/config.toml` のコメント密度に合わせる。
- TDD (Red → Green)。各タスクの最後にコミットする。
- コミットメッセージは英語 (公開リポジトリのため)。末尾に以下を付ける:

  ```
  Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_019d2gCNuSHyoPgLbsVMDz8m
  ```

## 実機で確認済みの事実 (再調査不要)

- `mise bootstrap --only` に渡せる値: `plugins` `packages` `accounts` `files` `services` `firewall` `compose` `repos` `dotfiles` `mise-shell-activate` `shell` `macos-defaults` `defaults` `macos-launchd-agents` `launchd` `linux-systemd-units` `systemd` `user` `tools` `task` `final-hook`
- tap の実 URL: `laishulu/homebrew` → `https://github.com/laishulu/homebrew-homebrew`、`d12frosted/emacs-plus` → `https://github.com/d12frosted/homebrew-emacs-plus`
- 全 aqua spec は解決確認済み (`mise latest` で latest が返る)
- `aqua:uutils/coreutils` は **multicall バイナリ `coreutils` 1 本のみ**を入れる。`ls` / `gls` / `gdate` は PATH に出ない
- `core:java@17` は 17.0.2 (2022 年で更新停止) に解決される。`temurin-17` は 17.0.20+101 で brew の 17.0.20.1 と同世代
- `~/.gitignore` の現在の中身は `**/.claude/settings.local.json` の 1 行のみ
- `~/bin/rpbcopy` は symlink ではなく実ファイル (コピー)
- `~/.config/karabiner/assets/complex_modifications/ja.json` は存在しない (未リンク)

---

## File Structure

| ファイル | 責務 |
|---|---|
| `mise.toml` (新規) | `[dotfiles]` `[bootstrap.*]` `[tasks.*]`。セットアップの宣言 |
| `.config/mise/config.toml` (変更) | `[tools]` `[settings]`。ランタイムと単体ツールの定義 |
| `.gitconfig` (変更) | `~/.gitconfig` の実体。alias・user・lfs・ghq.root を統合 |
| `.gitignore_global` (新規) | `core.excludesfile` の実体 |
| `.zshrc` (変更) | `JAVA_HOME` 2 行の削除のみ |
| `init.sh` (書き換え) | 新規マシン用 seed。brew + mise + ghq get + `mise bootstrap` |
| `README.md` (書き換え) | `mise bootstrap` を中心とした説明へ |

---

## Task 1: mise.toml の骨格と既存 symlink 9 件の宣言

既存の symlink をそのまま宣言し、**適用が no-op になること**をもって宣言が正しいことを検証する。

**Files:**
- Create: `mise.toml`

**Interfaces:**
- Produces: リポジトリ直下の `mise.toml`。以降のタスクはこのファイルにセクションを足していく。`[dotfiles]` の `source` はこのファイルのあるディレクトリ (リポジトリルート) からの相対パス。

- [ ] **Step 1: Red — 宣言前の状態を確認する**

```bash
cd ~/.dotfiles
mise bootstrap -n
```

Expected: `mise.toml` がまだ無いので `[bootstrap.*]` / `[dotfiles]` に関する出力が一切出ない。`[tools]` (グローバル config 由来) の行だけが出る。この出力を控えておく。

- [ ] **Step 2: mise.toml を作る**

```toml
# dotfiles のセットアップを mise の native bootstrap に寄せる。
#
# `mise bootstrap` が宣言を順に、冪等に適用する。実行はこのファイルのある
# ディレクトリ (リポジトリ直下) で行うこと。mise は cwd の設定階層をマージ
# するので、リポジトリ外から実行すると [bootstrap.*] が見えない。
#
#   mise bootstrap        適用
#   mise bootstrap -n     dry-run (適用せず差分だけ表示)
#   mise bootstrap status 収束状態の確認
#   mise run update       brew と mise の管理下をまとめて最新化
#
# ランタイム定義 ([tools] / [settings]) はこのファイルではなく
# .config/mise/config.toml にある。あちらは ~/.config/mise/config.toml へ
# symlink されるグローバル設定で、どのディレクトリでもツールが解決する。
# .zshrc の show_env_mise がそのパスを基準に「global 由来か」を判定している
# ため、[tools] をこちらへ移すとプロンプトの表示が壊れる。

# ---------------------------------------------------------------------------
# $HOME への symlink (旧 init.sh の ln -sf)
# source はこのファイルのあるディレクトリからの相対
# ---------------------------------------------------------------------------
[dotfiles]
"~/.inputrc" = { source = ".inputrc", mode = "symlink" }
"~/.zshenv" = { source = ".zshenv", mode = "symlink" }
"~/.zshrc" = { source = ".zshrc", mode = "symlink" }
"~/.tmux.conf" = { source = ".tmux.conf", mode = "symlink" }
"~/.config/mise/config.toml" = { source = ".config/mise/config.toml", mode = "symlink" }
"~/.config/yamllint/config" = { source = ".config/yamllint/config", mode = "symlink" }
"~/.aws/update-mfa-profile" = { source = ".aws/update-mfa-profile", mode = "symlink" }
"~/.emacs.d/init.el" = { source = ".emacs.d/init.el", mode = "symlink" }
"~/.emacs.d/early-init.el" = { source = ".emacs.d/early-init.el", mode = "symlink" }
```

- [ ] **Step 3: trust して dry-run する**

```bash
cd ~/.dotfiles
mise trust
mise bootstrap -n --only dotfiles
```

Expected: **9 件すべてが変更なし (current / ok) として報告される。** 既存の symlink はすべて正しい先を向いているので、ここで差分が出たら宣言のパスが間違っている。差分が出たエントリがあれば `source` を直してから次へ進む。

- [ ] **Step 4: Green — status で収束を確認する**

```bash
cd ~/.dotfiles
mise bootstrap status
```

Expected: `[dotfiles]` の 9 件が `current`。1 件でも `missing` / `differs` があれば宣言が誤っている。

- [ ] **Step 5: 未知キーが黙って落ちていないか確認する**

`[dotfiles]` のエントリ内の未知キーは警告なしに無視されるため、`mode` の綴りミスなどは status では気づけない。次で明示的に確認する。

```bash
cd ~/.dotfiles
mise bootstrap dotfiles status
```

Expected: 9 件それぞれについて `symlink` として扱われていることが読み取れる。`mode` が効いていないと `dotfiles.default_mode` の既定 (`symlink`) にフォールバックするため結果は同じになるが、出力にエントリが 9 件出ていることは確認する。

- [ ] **Step 6: Commit**

```bash
cd ~/.dotfiles
git add mise.toml
git commit -m "$(cat <<'EOF'
Declare the existing home symlinks in mise.toml

Move init.sh's nine ln -sf calls into a [dotfiles] block. Applying it is
a no-op on this machine, which is what verifies the declaration: every
existing symlink already points where the block says it should.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_019d2gCNuSHyoPgLbsVMDz8m
EOF
)"
```

---

## Task 2: git 設定の統合と、取りこぼしていた 4 件の symlink

`init.sh` が管理できていなかった 4 件を `[dotfiles]` に足す。`~/.gitconfig` と `~/.gitignore` はリポジトリ管理外の実ファイルなので、置き換える前に内容を保全する。

**Files:**
- Modify: `.gitconfig`
- Create: `.gitignore_global`
- Modify: `mise.toml` (`[dotfiles]` に 4 件追加)

**Interfaces:**
- Consumes: Task 1 の `mise.toml` の `[dotfiles]` ブロック
- Produces: `.gitconfig` が `~/.gitconfig` の唯一の実体になる。`[ghq] root = ~/projects` がここに入り、Task 7 の init.sh と Task 8 の移動先パスがこれに依存する。

- [ ] **Step 1: 置き換える実ファイルの内容を保全する**

CLAUDE.md の「破壊的操作」に従い、消す前に内容を会話に出す。

```bash
echo "===== ~/.gitconfig ====="; cat ~/.gitconfig
echo "===== ~/.gitignore ====="; cat ~/.gitignore
echo "===== repo .gitconfig ====="; cat ~/.dotfiles/.gitconfig
```

Expected: `~/.gitconfig` に `[user]` (name=wamei, email=wamei.cho@gmail.com) と `[color] ui` `[core] excludesfile` `[filter "lfs"]`。`~/.gitignore` に `**/.claude/settings.local.json` の 1 行。リポジトリ側 `.gitconfig` に `[color] ui` `[alias]` 5 本 `[push] default = nothing` `[core] excludesfile`。**この出力を会話に残してから次へ進む。**

- [ ] **Step 2: Red — 4 件が未管理であることを確認する**

```bash
ls -la ~/bin/rpbcopy
ls -la ~/.config/karabiner/assets/complex_modifications/ja.json
ls -la ~/.gitconfig ~/.gitignore
```

Expected: `rpbcopy` は実ファイル (symlink ではない)。`ja.json` は `No such file or directory`。`.gitconfig` と `.gitignore` は実ファイル。

- [ ] **Step 3: .gitconfig を統合する**

リポジトリの `.gitconfig` を次の内容にする。両方の設定を合わせ、`[ghq] root` を足す。

```ini
[user]
    name = wamei
    email = wamei.cho@gmail.com
[color]
    ui = auto
[alias]
    st = status
    co = checkout
    mylog = log --date=short --decorate=short --pretty=format:'%Cgreen%h %Creset%cd %Cblue%cn %Cred%d %Creset%s'
    graph = log --graph --date=short --decorate=short --pretty=format:'%Cgreen%h %Creset%cd %Cblue%cn %Cred%d %Creset%s'
    today = log --graph --date=short --decorate=short --pretty=format:'%Cgreen%h %Creset%cd %Cblue%cn %Cred%d %Creset%s' --since='18 hour ago'
[push]
    default = nothing
[core]
    excludesfile = ~/.gitignore
[ghq]
    root = ~/projects
[filter "lfs"]
    process = git-lfs filter-process
    required = true
    clean = git-lfs clean -- %f
    smudge = git-lfs smudge -- %f
```

- [ ] **Step 4: .gitignore_global を作る**

`~/.gitignore` の現在の中身をそのまま移す。リポジトリの `.gitignore` はリポジトリ自身の除外 (`.DS_Store`) なので別ファイルにする。

```gitignore
**/.claude/settings.local.json
```

- [ ] **Step 5: mise.toml の [dotfiles] に 4 件足す**

Task 1 で書いた `[dotfiles]` ブロックの末尾に追加する。

```toml
# ここから下は旧 init.sh が管理できていなかったもの。
# rpbcopy は cp でコピーされていたためリポジトリを編集しても反映されなかった。
# karabiner の ja.json はどこにもリンクされていなかった。
"~/bin/rpbcopy" = { source = "bin/rpbcopy", mode = "symlink" }
"~/.config/karabiner/assets/complex_modifications/ja.json" = { source = ".config/karabiner/assets/complex_modifications/ja.json", mode = "symlink" }
# .gitconfig は init.sh で ln -sf がコメントアウトされ、代わりに
# git config --global が 4 行走っていた。そのためリポジトリの alias 群と
# push.default = nothing が一度も有効になっていなかった。
"~/.gitconfig" = { source = ".gitconfig", mode = "symlink" }
# core.excludesfile が指す ~/.gitignore はリポジトリ管理外の実ファイルだった。
# リポジトリの .gitignore はリポジトリ自身の除外なので別ファイルにする。
"~/.gitignore" = { source = ".gitignore_global", mode = "symlink" }
```

- [ ] **Step 6: dry-run で差分が 4 件だけであることを確認する**

```bash
cd ~/.dotfiles
mise bootstrap -n --only dotfiles
```

Expected: **Task 1 の 9 件は変更なし、新規 4 件だけが差分に出る。** `~/.gitconfig` と `~/.gitignore` は実ファイルが存在するため衝突として報告される。

- [ ] **Step 7: Green — 適用する**

実ファイルとの衝突があるので `--force-dotfiles` が要る。Step 1 で内容を控えてあることを確認してから実行する。

```bash
cd ~/.dotfiles
mise bootstrap --only dotfiles --force-dotfiles
mise bootstrap status
```

Expected: 13 件すべて `current`。

- [ ] **Step 8: git 設定が壊れていないことを確認する**

```bash
ls -la ~/.gitconfig ~/.gitignore ~/bin/rpbcopy ~/.config/karabiner/assets/complex_modifications/ja.json
git config --global user.email
git config --global ghq.root
git config --global alias.st
git -C ~/.dotfiles status --short
```

Expected: 4 件すべて symlink。`user.email` が `wamei.cho@gmail.com`、`ghq.root` が `~/projects`、`alias.st` が `status`。`git status` が正常に動く (壊れた config なら失敗する)。

- [ ] **Step 9: Commit**

```bash
cd ~/.dotfiles
git add mise.toml .gitconfig .gitignore_global
git commit -m "$(cat <<'EOF'
Bring the four unmanaged dotfiles under [dotfiles]

rpbcopy was copied rather than linked, karabiner's ja.json was linked
nowhere, and ~/.gitconfig was a real file kept in sync by four
git config --global calls in init.sh -- so the repo's aliases and
push.default were never in effect. Merge the two gitconfigs, split the
global excludesfile into .gitignore_global, and declare all four.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_019d2gCNuSHyoPgLbsVMDz8m
EOF
)"
```

---

## Task 3: brew formula 13 本を mise の [tools] へ移す

**Files:**
- Modify: `.config/mise/config.toml`
- Modify: `.zshrc` (`JAVA_HOME` 2 行の削除)

**Interfaces:**
- Consumes: なし (`.config/mise/config.toml` は既存)
- Produces: `coreutils` `direnv` `gh` `git-lfs` `jq` `rg` `tmux` `pipx` `aws` `copilot` `java` `ghq` `az` `wrangler` が mise 側から PATH に出る。Task 7 の init.sh は `ghq` がここにあることを前提にする。

- [ ] **Step 1: Red — 現在どこから引かれているかを控える**

```bash
for c in coreutils direnv gh git-lfs jq rg tmux pipx aws copilot java ghq az wrangler; do printf "%-10s %s\n" "$c" "$(command -v $c || echo '(none)')"; done
```

Expected: ほぼすべてが `/opt/homebrew/bin/...`。`ghq` は `(none)`。`java` は `/usr/bin/java` (システムのスタブ) の可能性がある。**この出力を控える。**

- [ ] **Step 2: .config/mise/config.toml の [tools] に追記する**

既存の `"npm:five-server"` の行の後、`# YAML の lint` コメントの前に次のブロックを挿入する。

```toml
# brew から mise へ移した CLI。バイナリ配布があり aqua registry に載っている
# ものだけを移し、zsh プラグイン (share/ 配下を .zshrc が source する) や
# ライブラリ (libpq)、registry に無いもの (timg / wget / nkf / screen) と
# tap のソースビルド (macism) は brew に残してある (mise.toml 側で宣言)。
"aqua:uutils/coreutils" = "latest"
"aqua:direnv/direnv" = "latest"
"aqua:cli/cli" = "latest"              # gh
"aqua:git-lfs/git-lfs" = "latest"
"aqua:jqlang/jq" = "latest"
"aqua:BurntSushi/ripgrep" = "latest"
"aqua:tmux/tmux-builds" = "latest"
"aqua:pypa/pipx" = "latest"
"aqua:aws/aws-cli" = "latest"
# brew の copilot は AWS Copilot CLI (ECS/Fargate)。mise registry の
# "copilot" は GitHub Copilot CLI で別物なので、こちらを明示する。
"aqua:aws/copilot-cli" = "latest"
# ghq。リポジトリの置き場を ~/projects/<host>/<owner>/<repo> に揃える
# (root は .gitconfig の [ghq] で指定)。
"aqua:x-motemen/ghq" = "latest"
# java を裸で書くと 17 系が 17.0.2 (2022 年で更新停止の OpenJDK GA ビルド)
# に解決され、brew の openjdk@17 (17.0.20.1) から 3 年分のパッチが失われる。
# temurin を明示すると 17.0.20+101 になり同世代を保てる。
# backend 名込みで書くのは、.zshrc の show_env_mise が <backend>:<pkg> 形式を
# 「言語のバージョンではない」としてプロンプトから除くため。裸の java にすると
# 全プロンプトに java:temurin-17 が出る。
"core:java" = "temurin-17"
"pipx:azure-cli" = "latest"
# brew の formula 名は cloudflare-wrangler だが npm パッケージ名は wrangler。
# 実体の取得は下の npm.package_manager (bun)。
"npm:wrangler" = "latest"
```

- [ ] **Step 3: インストールして解決を確認する**

```bash
cd ~/.dotfiles
mise install
mise ls --current | grep -E "coreutils|direnv|cli/cli|git-lfs|jq|ripgrep|tmux|pipx|aws|copilot|ghq|java|azure|wrangler"
```

Expected: すべて `missing` でなくバージョンが付いている。

- [ ] **Step 4: Green — mise 版が PATH で勝っていることを確認する**

新しい zsh を開いて確認する (`mise activate` は precmd で PATH を差し込むため)。

```bash
zsh -ic 'for c in direnv gh git-lfs jq rg tmux pipx aws copilot ghq az wrangler; do printf "%-10s %s\n" "$c" "$(command -v $c)"; done'
```

Expected: すべて `~/.local/share/mise/installs/...` 配下。`/opt/homebrew/bin/` のままのものがあれば `[tools]` の記述順か activate の問題。

- [ ] **Step 5: coreutils の実体を確認する (重要)**

`aqua:uutils/coreutils` は **multicall バイナリ `coreutils` 1 本だけ**を入れる。brew の GNU coreutils が入れる `gls` `gdate` `gsed` などは**一切入らない**。

```bash
zsh -ic 'command -v coreutils; coreutils ls --version | head -1; command -v gdate gls gsed 2>/dev/null || echo "g-prefixed commands: none from mise"'
```

Expected: `coreutils` は mise 配下。`coreutils ls --version` が uutils のバージョンを返す。`gdate` などは brew 版が残っている間は `/opt/homebrew/bin/` から引ける。**brew の coreutils を uninstall すると `g` プレフィックス付きコマンドが失われる**ことを Step 8 の提示に明記する。

- [ ] **Step 6: java を確認して .zshrc の 2 行を削除する**

```bash
zsh -ic 'command -v java; java -version 2>&1 | head -1; echo "JAVA_HOME=$JAVA_HOME"'
```

Expected: mise 配下の java、`17.0.20`。`JAVA_HOME` は `.zshrc` の行がまだ生きているので brew を指している。

`.zshrc` から次の 2 行を削除する。

```
export PATH="/opt/homebrew/opt/openjdk@17/bin:$PATH"
export JAVA_HOME="/opt/homebrew/opt/openjdk@17"
```

削除後に確認する。

```bash
zsh -ic 'command -v java; echo "JAVA_HOME=$JAVA_HOME"'
```

Expected: `java` は mise 配下のまま。`JAVA_HOME` が mise の java インストール先を指している (mise が activate 時に設定する)。

- [ ] **Step 7: 依存している機能が壊れていないか確認する**

```bash
zsh -ic 'yamllint --version'                          # pipx backend 経由
zsh -ic 'git lfs version'                             # ~/.gitconfig の filter
zsh -ic 'psql --version'                              # brew の libpq (据え置き)
zsh -ic 'echo $HOMEBREW_PREFIX; ls $HOMEBREW_PREFIX/share/zsh-autosuggestions'  # zsh プラグイン (据え置き)
```

Expected: `yamllint` がバージョンを返す (pipx を mise 版に移しても解決できている)。`git lfs` が動く。`psql` が `/opt/homebrew/opt/libpq/bin/psql`。zsh プラグインのディレクトリが存在する。

- [ ] **Step 8: brew 版の uninstall コマンドを提示する (実行しない)**

次を**会話に出すだけ**にする。実行は利用者に委ねる。

```
brew uninstall coreutils direnv gh git-lfs jq ripgrep tmux pipx awscli copilot openjdk@17 azure-cli cloudflare-wrangler
```

あわせて次を伝える。

- `coreutils` を消すと `gls` `gdate` `gsed` などの `g` プレフィックス付きコマンドが失われる。対話シェルで使っているなら残すこと
- `openjdk@17` を消しても mise の temurin-17 が PATH 上の正なので影響はないが、他の brew formula が依存していないか `brew uses --installed openjdk@17` で確認すること

- [ ] **Step 9: Commit**

```bash
cd ~/.dotfiles
git add .config/mise/config.toml .zshrc
git commit -m "$(cat <<'EOF'
Move thirteen brew formulae to mise [tools]

Everything the aqua registry ships as a binary moves; zsh plugins,
libpq, the source-built tap formula and the four with no registry entry
stay in brew. java pins temurin-17 because a bare "17" resolves to the
OpenJDK GA build that stopped at 17.0.2 in 2022. Drop the JAVA_HOME
lines from .zshrc now that mise sets it.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_019d2gCNuSHyoPgLbsVMDz8m
EOF
)"
```

---

## Task 4: [bootstrap.packages] で cask 22 本と formula 10 本を宣言する

mise の brew-cask は artifact 種別のカバー範囲が狭い。**どれが通るかは流すまで分からない**ので、全部宣言 → dry-run → 落ちたものを `[tasks.bootstrap]` へ退避、という順で進める。

**Files:**
- Modify: `mise.toml`

**Interfaces:**
- Consumes: Task 1 の `mise.toml`
- Produces: `[tasks.bootstrap]` (Task 6 で本格的に整えるが、退避が必要ならここで先に作る)

- [ ] **Step 1: Red — 現在の宣言状態を確認する**

```bash
cd ~/.dotfiles
mise bootstrap -n --only packages
```

Expected: `[bootstrap.packages]` が無いので何も出ない。

- [ ] **Step 2: mise.toml に [bootstrap.packages] を足す**

`[dotfiles]` ブロックの**前**に挿入する (読み手にとってパッケージ → symlink の順が bootstrap の実行順と一致する)。

```toml
# ---------------------------------------------------------------------------
# system packages (旧 README の brew install 手順)
#
# 注: mise の brew backend は brew CLI を呼ばず Homebrew API から直接メタデータを
# 取り bottle を展開する。cask は artifact 種別のカバー範囲が狭く、扱えないものは
# 下の [tasks.bootstrap] で brew install に退避してある。
#
# adopt = true は必須。mise が cask を入れ直すと /Applications/*.app が置き換わり、
# macOS の TCC 権限 (アクセシビリティ・画面収録・フルディスクアクセス) がリセット
# される。karabiner-elements・ghostty・1password が直撃する。adopt は既存バンドルを
# 差し替えず所有権だけ記録する。
# ---------------------------------------------------------------------------
[bootstrap.brew]
adopt = true

[bootstrap.brew.taps]
"laishulu/homebrew" = "https://github.com/laishulu/homebrew-homebrew"
"d12frosted/emacs-plus" = "https://github.com/d12frosted/homebrew-emacs-plus"

[bootstrap.packages]
# --- formula: mise に移せなかったもの ---
# mise 本体 (自己参照なので mise では入れられない)
"brew:mise" = "latest"
# zsh プラグイン。バイナリでなく .zshrc が $HOMEBREW_PREFIX/share/ を source する
"brew:zsh-autosuggestions" = "latest"
"brew:zsh-autocomplete" = "latest"
"brew:zsh-completions" = "latest"
# ライブラリ + psql。aqua registry に無い。.zshrc が PATH に入れ、Emacs の
# sql-mode が psql を引く
"brew:libpq" = "latest"
# Emacs の minibuffer で IME を off にする。tap のソースビルド
"brew:laishulu/homebrew/macism" = "latest"
# 以下 4 本は aqua registry にエントリが無い
"brew:timg" = "latest"      # ghostel に kitty graphics で画像を出す
"brew:wget" = "latest"
"brew:nkf" = "latest"
"brew:screen" = "latest"

# --- cask ---
"brew-cask:emacs-plus-app" = "latest"
"brew-cask:ghostty" = "latest"
"brew-cask:karabiner-elements" = "latest"
"brew-cask:google-japanese-ime" = "latest"
"brew-cask:1password" = "latest"
"brew-cask:claude" = "latest"
"brew-cask:visual-studio-code" = "latest"
"brew-cask:google-chrome" = "latest"
"brew-cask:google-chrome@canary" = "latest"
"brew-cask:slack" = "latest"
"brew-cask:microsoft-teams" = "latest"
"brew-cask:chatwork" = "latest"
"brew-cask:orbstack" = "latest"
"brew-cask:postman" = "latest"
"brew-cask:mysqlworkbench" = "latest"
"brew-cask:nordlayer" = "latest"
# フォント
"brew-cask:font-blex-mono-nerd-font" = "latest"
"brew-cask:font-hackgen" = "latest"
"brew-cask:font-hackgen-nerd" = "latest"
"brew-cask:font-source-han-code-jp" = "latest"
"brew-cask:font-udev-gothic" = "latest"
"brew-cask:font-udev-gothic-nf" = "latest"
```

- [ ] **Step 3: dry-run で扱えないものを洗い出す**

```bash
cd ~/.dotfiles
mise bootstrap -n --only packages 2>&1 | tee /tmp/mise-packages-dryrun.txt
```

Expected: 32 件それぞれの判定が出る。**エラーになったもの・未対応の artifact 種別として報告されたものを記録する。** 想定される候補は `postflight_steps` を持つ `emacs-plus-app` と `orbstack`、`pkg` の `karabiner-elements` `google-japanese-ime` `microsoft-teams` `nordlayer`、tap のソースビルドになる `macism`。

- [ ] **Step 4: 落ちたものを [tasks.bootstrap] へ退避する**

Step 3 で落ちたものを `[bootstrap.packages]` から**コメントアウトして理由を書き**、`mise.toml` の末尾に次を足す。`<落ちた cask>` `<落ちた formula>` は Step 3 の実測で置き換える。落ちたものが 1 つも無ければこの Step は飛ばす。

```toml
# ---------------------------------------------------------------------------
# 宣言で表せない補助 (bootstrap の step 15 で自動実行される escape hatch)
# 冪等に保つこと。毎回実行される。
# ---------------------------------------------------------------------------
[tasks.bootstrap]
description = "mise の brew backend で扱えないパッケージを brew から入れる"
run = [
  # mise の brew-cask が扱えない artifact 種別を使う cask。brew 管理下に置くので
  # 以後は brew upgrade --cask でまとめて上げられる。
  "command -v brew >/dev/null 2>&1 && brew install --cask <落ちた cask> || true",
  # tap のソースビルドなど、mise の brew backend で解決できない formula。
  "command -v brew >/dev/null 2>&1 && brew install <落ちた formula> || true",
]
```

- [ ] **Step 5: 再度 dry-run して残りが通ることを確認する**

```bash
cd ~/.dotfiles
mise bootstrap -n --only packages
```

Expected: エラーが 0 件。すべてのエントリが「導入済み (adopt) 」または「導入予定」として報告される。

- [ ] **Step 6: Green — 適用して収束を確認する**

適用前に、TCC 権限を持つアプリのバージョンを控えておく (adopt が効かず入れ替わった場合に気づけるように)。

```bash
for a in "Karabiner-Elements" "Ghostty" "1Password"; do printf "%-20s %s\n" "$a" "$(defaults read "/Applications/$a.app/Contents/Info.plist" CFBundleShortVersionString 2>/dev/null || echo '(not found)')"; done
cd ~/.dotfiles
mise bootstrap --only packages
mise bootstrap status
```

Expected: `[bootstrap.packages]` の全件が `current`。上のバージョンが適用前後で変わっていないこと。

- [ ] **Step 7: TCC 権限が失われていないことを確認する**

```bash
for a in "Karabiner-Elements" "Ghostty" "1Password"; do printf "%-20s %s\n" "$a" "$(defaults read "/Applications/$a.app/Contents/Info.plist" CFBundleShortVersionString 2>/dev/null || echo '(not found)')"; done
```

Expected: Step 6 で控えた値と一致。Karabiner-Elements が動いていること (キーリマップが効くか実際に確認する) と、Ghostty で画面収録権限が要る操作が通ることを目視で確かめる。

- [ ] **Step 8: Commit**

```bash
cd ~/.dotfiles
git add mise.toml
git commit -m "$(cat <<'EOF'
Declare brew casks and the ten formulae that stayed

Twenty-two casks plus the formulae mise could not take over. adopt is on
for the whole brew backend: reinstalling a cask replaces the app bundle
and resets its TCC grants, which would cost Karabiner, Ghostty and
1Password their accessibility and screen-recording permissions. What the
brew-cask backend cannot handle falls back to brew in the bootstrap task.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_019d2gCNuSHyoPgLbsVMDz8m
EOF
)"
```

---

## Task 5: macOS のシステム設定とログインシェル

**Files:**
- Modify: `mise.toml`

**Interfaces:**
- Consumes: Task 4 の `mise.toml`
- Produces: なし

- [ ] **Step 1: Red — 現在値を控える**

```bash
for kv in "NSGlobalDomain KeyRepeat" "NSGlobalDomain InitialKeyRepeat" "NSGlobalDomain ApplePressAndHoldEnabled" "NSGlobalDomain NSAutomaticCapitalizationEnabled" "NSGlobalDomain com.apple.keyboard.fnState" "com.apple.dock autohide" "com.apple.dock tilesize" "com.apple.finder AppleShowAllFiles" "com.apple.finder FXPreferredViewStyle"; do set -- $kv; printf "%-28s %-34s = %s\n" "$1" "$2" "$(defaults read "$1" "$2" 2>/dev/null || echo '<unset>')"; done
dscl . -read ~/ UserShell
```

Expected: `KeyRepeat=2` `InitialKeyRepeat=30` `ApplePressAndHoldEnabled=0` `NSAutomaticCapitalizationEnabled=0` `fnState=1` `autohide=1` `tilesize=64` `AppleShowAllFiles=True` `FXPreferredViewStyle=Nlsv`。`UserShell: /bin/zsh`。**この出力を控える。**

- [ ] **Step 2: mise.toml に足す**

`[dotfiles]` ブロックの後に挿入する。

```toml
# ---------------------------------------------------------------------------
# macOS のシステム設定
#
# 注: mise 2026.9.1 はスカラー値 (bool / int / float / string) しか受け付けない。
# ドキュメントにある配列・テーブル値はこの版では黙って落ちるので、Dock の
# persistent-apps のような構造値はここでは表現できない。
#
# 反映には killall が要る。mise 自身はアプリを落とさないので hook で行う。
# ---------------------------------------------------------------------------
[bootstrap.macos.defaults]
# ApplePressAndHold を切るのは、長押しでアクセント候補を出さずキーリピートさせるため
# (Emacs で必須)。fnState は F1-F12 をファンクションキーとして扱う。
"NSGlobalDomain" = { KeyRepeat = 2, InitialKeyRepeat = 30, ApplePressAndHoldEnabled = false, NSAutomaticCapitalizationEnabled = false, "com.apple.keyboard.fnState" = true }
"com.apple.dock" = { autohide = true, tilesize = 64 }
# AppleShowAllFiles は旧 README が `defaults write ... True` と文字列で書いていた
# ため実機には文字列 "True" が入っている。mise は bool を書くので初回だけ差分として
# 報告される。Finder はどちらも受け付けるので実害はない。
"com.apple.finder" = { AppleShowAllFiles = true, FXPreferredViewStyle = "Nlsv" }

[bootstrap.hooks]
post-defaults = "killall Dock Finder || true"

# ---------------------------------------------------------------------------
# ログインシェル (旧 init.sh には無く、手で chsh していた)
# ---------------------------------------------------------------------------
[bootstrap.user]
login_shell = "/bin/zsh"
```

- [ ] **Step 3: dry-run で差分を確認する**

```bash
cd ~/.dotfiles
mise bootstrap -n --only macos-defaults,user
```

Expected: `AppleShowAllFiles` だけが差分 (文字列 → bool)。他の 8 件と `login_shell` は変更なし。差分がこれ以外に出たら Step 1 で控えた値と宣言が食い違っている。

- [ ] **Step 4: Green — 適用する**

```bash
cd ~/.dotfiles
mise bootstrap --only macos-defaults,user
mise bootstrap status
```

Expected: 全件 `current`。`killall Dock Finder` により Dock と Finder が再起動する。

- [ ] **Step 5: 実際の挙動を確認する**

```bash
defaults read com.apple.finder AppleShowAllFiles
defaults read NSGlobalDomain ApplePressAndHoldEnabled
dscl . -read ~/ UserShell
```

Expected: `1` (bool として書かれた)、`0`、`/bin/zsh`。あわせて Finder で隠しファイルが見えること、キーの長押しでリピートすることを目視で確かめる。

- [ ] **Step 6: Commit**

```bash
cd ~/.dotfiles
git add mise.toml
git commit -m "$(cat <<'EOF'
Declare the macOS defaults and the login shell

Only the values actually set on this machine, read back from defaults.
mise 2026.9.1 takes scalars alone -- the array and table values the docs
describe are dropped silently on this version -- so structured settings
like the Dock's persistent-apps stay out. killall moves to a
post-defaults hook since mise never restarts apps itself.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_019d2gCNuSHyoPgLbsVMDz8m
EOF
)"
```

---

## Task 6: update / verify タスク

**Files:**
- Modify: `mise.toml`

**Interfaces:**
- Consumes: Task 4 の `[tasks.bootstrap]` (存在する場合)
- Produces: `mise run update` と `mise run verify`

- [ ] **Step 1: Red — タスクが無いことを確認する**

```bash
cd ~/.dotfiles
mise tasks
```

Expected: `update` と `verify` が一覧に無い (`bootstrap` は Task 4 で作っていれば有る)。

- [ ] **Step 2: mise.toml の末尾に足す**

```toml
# ---------------------------------------------------------------------------
# 設定の妥当性検証 (非破壊)
#   mise run verify
#
# [dotfiles] のエントリ内の未知キーは警告なしに無視されるため、目視レビューでは
# 誤りを検出できない。dry-run が唯一の検証手段になる。
# ---------------------------------------------------------------------------
[tasks.verify]
description = "設定の妥当性を検証する (非破壊)"
run = [
  # bootstrap 全体の dry-run。パッケージトークンの解決、[dotfiles] の source 存在、
  # defaults の妥当性、login shell、tools の解決までを一括で検証する。
  # 不正な設定があれば非ゼロで終了する。
  #
  # --force-dotfiles を付けるのは「既存ファイルを壊す」検査を黙らせるためではなく、
  # その検査が環境依存で偽陽性になるため。~/.config などは実ディレクトリとして
  # 存在しがちで、素の環境では必ず衝突として報告される。ここで検証したいのは設定の
  # 妥当性であって実行環境の状態ではない。dry-run なので何も書き換わらない。
  "mise bootstrap -n --force-dotfiles",
]

# ---------------------------------------------------------------------------
# 日常の更新
#   mise run update
#
# 更新経路が 3 系統に分かれており、どれか 1 つでは全部は上がらない:
#   1. brew   [bootstrap.packages] の formula / cask と mise 本体。
#             mise bootstrap は導入済みならスキップするだけでアップグレードしない
#             ので、これらを上げられるのは brew だけ。
#   2. mise upgrade
#             .config/mise/config.toml の [tools]。"latest" 指定なので最新へ上がる。
#   3. mise run bootstrap
#             brew backend で扱えず退避したものの導入。冪等。
#
# 注: 対話的に (自分のシェルから) 実行すること。自己更新機構を持つ cask
# (Slack / Google Chrome) は /Applications 配下が root:wheel になっており
# chown に sudo が要る。先に `sudo -v` を済ませ、対象アプリは終了しておく。
# ---------------------------------------------------------------------------
[tasks.update]
description = "brew と mise の管理下をまとめて最新化する"
run = [
  '''
if command -v brew >/dev/null 2>&1; then
  brew update
  # cask は対象アプリが起動していると chown に失敗し、その 1 本のために brew 全体が
  # 非ゼロで終わる。ここで止めると後続の mise upgrade まで巻き添えになるので、案内
  # だけ出して先へ進める。
  brew upgrade || echo "brew upgrade: 一部失敗。起動中のアプリを終了してから brew upgrade を流し直すこと。"
else
  echo "brew not found; skipping system packages"
fi
''',
  "mise upgrade",
]
```

Task 4 で `[tasks.bootstrap]` を作った場合は、`[tasks.update]` の `run` の末尾に `"mise run bootstrap",` を足す。作っていなければ足さない (存在しないタスクを呼ぶと失敗する)。

- [ ] **Step 3: Green — verify が通ることを確認する**

```bash
cd ~/.dotfiles
mise tasks
mise run verify
echo "exit=$?"
```

Expected: `mise tasks` に `verify` と `update` が出る。`mise run verify` が exit 0 で終わる。非ゼロなら宣言のどこかが不正。

- [ ] **Step 4: update が壊れていないことを確認する (実行はしない)**

`mise run update` は brew を実際に上げるので、ここでは構文だけ確認する。

```bash
cd ~/.dotfiles
mise tasks info update
```

Expected: `run` の中身がエラーなく表示される。

- [ ] **Step 5: Commit**

```bash
cd ~/.dotfiles
git add mise.toml
git commit -m "$(cat <<'EOF'
Add the verify and update tasks

verify is the only real check on this config: unknown keys inside a
[dotfiles] entry are ignored without a warning, so a dry-run is what
catches a typo. update covers all three upgrade paths, since
mise bootstrap skips what is already installed rather than upgrading it.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_019d2gCNuSHyoPgLbsVMDz8m
EOF
)"
```

---

## Task 7: init.sh の縮小と README の書き換え

**Files:**
- Modify: `init.sh`
- Modify: `README.md`

**Interfaces:**
- Consumes: Task 2 の `.gitconfig` (`[ghq] root`)、Task 3 の `[tools]` の `ghq`
- Produces: なし

- [ ] **Step 1: Red — 現在の init.sh が symlink を張る責務を持っていることを確認する**

```bash
cd ~/.dotfiles
grep -c "ln -sf" init.sh
grep -c "git config --global" init.sh
```

Expected: `ln -sf` が 9、`git config --global` が 4。これらはすべて `mise.toml` の宣言に移っている。

- [ ] **Step 2: init.sh を seed スクリプトへ書き換える**

```sh
#!/bin/bash
set -euo pipefail

# 新規マシンの seed。symlink や brew パッケージの導入は mise.toml の宣言が持つ
# ので、ここでは `mise bootstrap` を動かすのに最低限必要なものだけを入れる。
#
#   curl -fsSL https://raw.githubusercontent.com/wamei/.dotfiles/master/init.sh | bash
#
# 既にこのリポジトリを clone してあるなら、リポジトリ直下で
# `mise trust && mise bootstrap` を叩くだけでよい。

DOTFILES_ROOT="$HOME/projects/github.com/wamei/.dotfiles"

# Xcode Command Line Tools (git が入る)。導入済みなら何もしない。
xcode-select -p >/dev/null 2>&1 || xcode-select --install

# Homebrew。mise と ghq の入手経路であり、cask の導入にも要る。
if ! command -v brew >/dev/null 2>&1; then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi
eval "$(/opt/homebrew/bin/brew shellenv)"

# mise と ghq。どちらも [tools] / [bootstrap.packages] でも宣言しているが、
# clone の時点ではまだ mise が動いていないのでここでは brew から入れる。
brew install mise ghq

# ghq.root は git config ではなく環境変数で渡す。~/.gitconfig は [dotfiles] が
# symlink で管理するため、ここで git config --global を叩くと実ファイルが作られ、
# 直後の mise bootstrap が symlink を張れずに衝突する。恒久的な設定は
# リポジトリの .gitconfig が持っている。
if [ ! -d "$DOTFILES_ROOT" ]; then
  GHQ_ROOT="$HOME/projects" ghq get git@github.com:wamei/.dotfiles.git
fi

cd "$DOTFILES_ROOT"
mise trust
mise bootstrap
```

- [ ] **Step 3: 静的検査する**

```bash
cd ~/.dotfiles
bash -n init.sh && echo "syntax OK"
command -v shellcheck >/dev/null 2>&1 && shellcheck init.sh || echo "shellcheck not installed; skipped"
```

Expected: `syntax OK`。shellcheck が入っていれば警告 0 件。

- [ ] **Step 4: README.md を書き換える**

```markdown
# dotfiles

macOS の環境構築を [mise](https://mise.jdx.dev/) の `bootstrap` に寄せてある。

## 新規マシン

```sh
curl -fsSL https://raw.githubusercontent.com/wamei/.dotfiles/master/init.sh | bash
```

Xcode Command Line Tools、Homebrew、mise、ghq を入れ、このリポジトリを ghq
レイアウト (`~/projects/github.com/wamei/.dotfiles`) へ clone して
`mise bootstrap` に渡す。以降は宣言が面倒を見る。

## 既に clone してある場合

リポジトリ直下で実行する。mise は cwd の設定階層をマージするので、
リポジトリ外から実行すると `[bootstrap.*]` が見えない。

```sh
mise bootstrap        # 適用
mise bootstrap -n     # dry-run (適用せず差分だけ表示)
mise bootstrap status # 収束状態の確認
mise run verify       # 設定の妥当性を非破壊で検証
mise run update       # brew と mise の管理下をまとめて最新化
```

`mise bootstrap` が宣言的・冪等に適用するもの:

| 宣言 | 内容 |
|---|---|
| `[bootstrap.packages]` | brew の formula と cask |
| `[dotfiles]` | `$HOME` への symlink |
| `[bootstrap.macos.defaults]` | macOS のシステム設定 |
| `[bootstrap.user]` | ログインシェル |
| `[tools]` | ランタイムと単体 CLI (`.config/mise/config.toml`) |
| `[tasks.bootstrap]` | 宣言で表せない補助 |

## 設定ファイルの置き場

| ファイル | 内容 |
|---|---|
| `mise.toml` | `[dotfiles]` `[bootstrap.*]` `[tasks.*]` |
| `.config/mise/config.toml` | `[tools]` `[settings]`。`~/.config/mise/config.toml` へ symlink されるグローバル設定 |

`[tools]` を `mise.toml` へ移していないのは、グローバル設定としてどのディレクトリ
でもランタイムを解決させるためと、`.zshrc` の `show_env_mise` がそのパスを基準に
「global 由来か」を判定しているため。

## リポジトリの置き場

`ghq.root` は `~/projects`。GitHub のリポジトリは
`~/projects/github.com/<owner>/<repo>` に落ちる。

```sh
ghq get <url>
```

## 注記

- mise は 2026.9.1 を前提にしている。公開ドキュメントはこれより新しい版を記述
  しており、`[dotfiles]` の `mode = "track"` や `[bootstrap.macos.defaults]` の
  配列値はこの版では動かない
- `[dotfiles]` のエントリ内の未知キーは警告なしに無視される。編集したら必ず
  `mise run verify` で確認すること
- cask は `[bootstrap.brew] adopt = true` で扱っている。入れ直すと
  `/Applications/*.app` が置き換わり macOS の TCC 権限がリセットされるため
```

- [ ] **Step 5: Green — 記述と実態が一致していることを確認する**

```bash
cd ~/.dotfiles
mise tasks                                    # README に書いたタスクが実在するか
git config --get ghq.root                     # README に書いた root と一致するか
grep -c "ln -sf" init.sh || echo "0 (期待値)"  # symlink の責務が残っていないか
```

Expected: `verify` と `update` が実在。`ghq.root` が `~/projects`。`init.sh` に `ln -sf` が 0 件。

- [ ] **Step 6: Commit**

```bash
cd ~/.dotfiles
git add init.sh README.md
git commit -m "$(cat <<'EOF'
Reduce init.sh to a seed and rewrite the README

init.sh no longer links anything -- [dotfiles] owns that now. What is
left is the part mise bootstrap cannot do for itself: the Xcode CLT,
Homebrew, mise and ghq, and the clone into the ghq layout. ghq.root
comes from the environment rather than git config, so the seed does not
leave a real ~/.gitconfig for [dotfiles] to collide with.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_019d2gCNuSHyoPgLbsVMDz8m
EOF
)"
```

---

## Task 8: リポジトリを ghq レイアウトへ移す

**移行の最終工程。** 既存の symlink はすべて `/Users/wamei/.dotfiles/...` を指しているので、移動と張り直しはセットで行う。

**Files:**
- なし (ファイルシステム上の移動のみ)

**Interfaces:**
- Consumes: Task 1〜7 のすべて。特に `[dotfiles]` の 13 件
- Produces: リポジトリが `~/projects/github.com/wamei/.dotfiles` に移り、`$HOME` の symlink が新パスを指す

- [ ] **Step 1: Red — 現在の symlink を控える**

失敗時に戻せるようにする。

```bash
for t in ~/.inputrc ~/.zshenv ~/.zshrc ~/.tmux.conf ~/.config/mise/config.toml ~/.config/yamllint/config ~/.aws/update-mfa-profile ~/.emacs.d/init.el ~/.emacs.d/early-init.el ~/bin/rpbcopy ~/.config/karabiner/assets/complex_modifications/ja.json ~/.gitconfig ~/.gitignore; do printf "%-62s -> %s\n" "$t" "$(readlink "$t" || echo '(not a symlink)')"; done | tee /tmp/dotfiles-symlinks-before.txt
```

Expected: 13 件すべてが `/Users/wamei/.dotfiles/...` を指している。**この出力を会話に残す。**

- [ ] **Step 2: 作業ツリーが clean であることを確認する**

移動前にコミット漏れがないか確認する。他のセッションが `.emacs.d/` を触っている可能性があるので、自分の変更だけか確かめる。

```bash
cd ~/.dotfiles
git status --short
git log --oneline -8
```

Expected: `git status` が空。Task 1〜7 のコミットが並んでいる。dirty なら移動しない。

- [ ] **Step 3: 移動する**

**移動中は新しい shell を開かない** (`~/.zshrc` が一時的に壊れたリンクになる)。

```bash
mkdir -p ~/projects/github.com/wamei
mv ~/.dotfiles ~/projects/github.com/wamei/.dotfiles
ls -la ~/projects/github.com/wamei/.dotfiles/mise.toml
```

Expected: 新パスに `mise.toml` が存在する。

- [ ] **Step 4: Green — symlink を張り直す**

```bash
cd ~/projects/github.com/wamei/.dotfiles
mise trust
mise bootstrap --only dotfiles --force-dotfiles
```

Expected: 13 件すべてが新パスへ張り替えられる。`--force-dotfiles` が要るのは、既存の symlink が旧パスを指す「衝突」として報告されるため。

- [ ] **Step 5: 全 symlink が新パスを指していることを確認する**

```bash
for t in ~/.inputrc ~/.zshenv ~/.zshrc ~/.tmux.conf ~/.config/mise/config.toml ~/.config/yamllint/config ~/.aws/update-mfa-profile ~/.emacs.d/init.el ~/.emacs.d/early-init.el ~/bin/rpbcopy ~/.config/karabiner/assets/complex_modifications/ja.json ~/.gitconfig ~/.gitignore; do printf "%-62s -> %s\n" "$t" "$(readlink "$t" || echo '(not a symlink)')"; done
for t in ~/.zshrc ~/.gitconfig ~/.emacs.d/init.el; do [ -e "$t" ] && echo "OK   $t (解決する)" || echo "DEAD $t (壊れたリンク)"; done
```

Expected: 13 件すべてが `/Users/wamei/projects/github.com/wamei/.dotfiles/...` を指し、いずれも解決する (壊れたリンクが 0 件)。

- [ ] **Step 6: 新しい shell が起動することを確認する**

```bash
zsh -ic 'echo "zsh OK"; command -v mise; mise ls --current | head -5'
git config --global user.email
git -C ~/projects/github.com/wamei/.dotfiles status --short
```

Expected: `zsh OK` が出る。`mise ls --current` が動く。`user.email` が引ける。`git status` が空。

- [ ] **Step 7: 収束を確認する**

```bash
cd ~/projects/github.com/wamei/.dotfiles
mise bootstrap status
mise run verify
echo "exit=$?"
```

Expected: 全宣言が `current`。`verify` が exit 0。

- [ ] **Step 8: 稼働中の Emacs を再起動する**

稼働中の Emacs は `~/.emacs.d/init.el` 経由で旧パスの inode を掴んでいる。再起動して init.el が新パスから読まれることを確認する。

```bash
emacsclient -e '(file-truename user-init-file)' 2>/dev/null || echo "Emacs not running"
```

Expected: 再起動後は `/Users/wamei/projects/github.com/wamei/.dotfiles/.emacs.d/init.el`。旧パスが返るなら再起動できていない。

- [ ] **Step 9: 移行完了を報告する**

次を会話に出す。

- 新しいリポジトリパス: `~/projects/github.com/wamei/.dotfiles`
- 別タスクとして残っているもの: `~/projects` 配下の既存 64 ディレクトリの ghq 再配置 (spec の「対象外」節)
- 未実行のまま提示だけしてあるもの: Task 3 Step 8 の `brew uninstall`

このタスクはファイルを変更しないのでコミットはない。

---

## Self-Review

**Spec coverage**

| spec の節 | 対応するタスク |
|---|---|
| 構成 / init.sh の役割 | Task 1 (mise.toml 骨格), Task 7 (init.sh) |
| `[dotfiles]` — symlink | Task 1 (既存 9 件), Task 2 (新規 4 件) |
| `[tools]` — brew から移す formula | Task 3 |
| coreutils の実体 | Task 3 Step 5 |
| java のバージョン指定 | Task 3 Step 2, Step 6 |
| 付随して必要な `.zshrc` の変更 | Task 3 Step 6 |
| brew 版の後始末 | Task 3 Step 8 (提示のみ) |
| `[bootstrap.packages]` cask / formula | Task 4 |
| 宣言しないもの | Task 4 (宣言に含めないことで満たす) |
| `[bootstrap.macos.defaults]` | Task 5 |
| `[bootstrap.user]` | Task 5 |
| git 設定 | Task 2 Step 3 |
| `[tasks.*]` | Task 4 (bootstrap), Task 6 (verify / update) |
| リポジトリの移動 | Task 8 |
| 検証方法 | 各タスクの Red / Green ステップ |
| 対象外 | Task 8 Step 9 で申し送り |

漏れなし。

**Placeholder scan**

Task 4 Step 4 の `<落ちた cask>` `<落ちた formula>` は placeholder に見えるが、これは Step 3 の dry-run 結果でしか決まらない値であり、その旨と埋め方を明記してある。事前に確定できない値なので許容する。それ以外に TBD / TODO / 「適切に処理する」の類は無い。

**Type consistency**

- `[dotfiles]` のキーは Task 1 / Task 2 / Task 8 を通して同一の 13 個。Task 8 Step 1 と Step 5 の確認リストも同じ 13 個で一致
- `[tasks.bootstrap]` は Task 4 で作られ Task 6 で `[tasks.update]` から参照される。Task 6 Step 2 に「Task 4 で作った場合だけ足す」条件を明記済み
- `.gitignore_global` は Task 2 で作られ Task 2 の `[dotfiles]` から参照される。名前は一貫
- リポジトリパスは Task 7 の `DOTFILES_ROOT` と Task 8 の移動先で `~/projects/github.com/wamei/.dotfiles` に一致
- `ghq.root = ~/projects` は Task 2 の `.gitconfig`、Task 7 の `GHQ_ROOT`、README で一致
