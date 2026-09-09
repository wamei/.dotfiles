# 環境変数
export LANG=en_US.UTF-8
export TZ=Asia/Tokyo
export PAGER='less -R'

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo '

# OS 別の設定
case ${OSTYPE} in
    # Mac用の設定
    darwin*)
        alias ls='gls --color=auto --group-directories-first -v'
        alias updatedb='sudo /usr/libexec/locate.updatedb'
        ;;
    #Linux用の設定
    linux*)
        alias ls='ls --color=auto --group-directories-first -v'
        ;;
esac

# emacs 風キーバインドにする
bindkey -e

# 色を使用出来るようにする
autoload -Uz colors
colors

# コマンドライン実行時に # 以降をコメントとして扱う
setopt interactive_comments

# ヒストリの補完
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
# 高機能なワイルドカード展開を使用する
setopt extended_glob
# ^R でヒストリ検索をするときに * でワイルドカードを使用出来るようにする
bindkey '^R' history-incremental-pattern-search-backward
# ヒストリの設定
HISTFILE=${HOME}/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups
# ヒストリファイルに保存するとき、すでに重複したコマンドがあったら古い方を削除する
setopt hist_save_nodups
# ヒストリに保存するときに余分なスペースを削除する
setopt hist_ignore_space
setopt hist_reduce_blanks
# 履歴を他のシェルとリアルタイム共有する
setopt share_history
# 実行時に履歴をファイルに追加していく
setopt inc_append_history

# 単語の区切り文字を指定する
autoload -Uz select-word-style
select-word-style default
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified

# ディレクトリスタックに追加
setopt auto_pushd

## PROMPT内で変数展開・コマンド置換・算術演算を実行する。
setopt prompt_subst
# レポジトリ情報の表示
autoload -Uz vcs_info
zstyle ':vcs_info:*' max-exports 5
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "!"
zstyle ':vcs_info:git:*' unstagedstr "+"
zstyle ':vcs_info:*' formats '%s:(' '%b' ')' '%c%u'
zstyle ':vcs_info:*' actionformats '%s:(' '%b' ')' '%c%u' '%a'
precmd () {
    psvar=()
    vcs_info
    if [[ -z ${vcs_info_msg_0_} ]]; then
        psvar[1]=""
        psvar[2]=""
        psvar[3]=""
        psvar[4]=""
        psvar[5]=""
    else
        [[ -n "$vcs_info_msg_0_" ]] && psvar[1]=( "${vcs_info_msg_0_}" )
        [[ -n "$vcs_info_msg_1_" ]] && psvar[2]=( "${vcs_info_msg_1_}" )
        [[ -n "$vcs_info_msg_2_" ]] && psvar[3]=( "${vcs_info_msg_2_}" )
        [[ -n "$vcs_info_msg_3_" ]] && psvar[4]=( "${vcs_info_msg_3_}" )
        if command git status --porcelain 2> /dev/null \
            | awk '{print $1}' \
            | command grep -F '??' > /dev/null 2>&1 ; then

            # unstagedに追加
            psvar[4]+='?'
        fi
        [[ -n "$vcs_info_msg_4_" ]] && psvar[5]=( "${vcs_info_msg_4_}" )
    fi
}
show_env() {
  show_env_mise
}
# mise が今のディレクトリで有効にしているツール (node / bun / python / ruby ...) を出す。
# go:github.com/... のような backend 付き (go install / npm / cargo などで入れた単体ツール)
# は言語のバージョンではなく単に入れたバイナリなので、プロンプトからは除く。
# global 設定 (~/.config/mise/config.toml) 以外から解決されたものは赤で出す。
# direnv の layout python で venv に入っているときは python に (venv 名) を付けて赤で出す。
# `mise ls --current` の 1 行: "<tool>  <version>  [(missing)]  <source>  <requested>"
show_env_mise() {
  # config が symlink だと source は実体側のパスで出るので、realpath 同士で比べる
  local global=${${:-~/.config/mise/config.toml}:A}
  local line tool ver src
  local -a f
  mise ls --current 2>/dev/null | while IFS= read -r line; do
    f=(${(z)line})
    tool=$f[1] ver=$f[2] src=$f[3]
    # backend 付き (<backend>:<pkg>) は除外
    [[ $tool == *:* ]] && continue
    if [[ $src == '(missing)' ]]; then
      ver+='(missing)'
      src=$f[4]
    fi
    if [[ $tool == python && -n $VIRTUAL_ENV && -n $DIRENV_DIR ]]; then
      echo -n " %F{009}$tool:$ver(${VIRTUAL_ENV:t})%f"
      continue
    fi
    if [[ ${${src/#\~/$HOME}:A} == $global ]]; then
      echo -n " $tool:$ver"
    else
      echo -n " %F{009}$tool:$ver%f"
    fi
  done
}

# prompt表示設定
PROMPT="%B%F{white}%(?..%K{red}            status code -%?-            %{%k%}
)%{%k%f%b%}%F{magenta}%~%f"
PROMPT+=" %F{green}%1v%f%F{yellow}%2v%f%F{green}%3v%4v%f %F{red}%5v%f"
PROMPT+='$(show_env)'
PROMPT+="
$ "

PROMPT2='[%n]> '

export VIRTUAL_ENV_DISABLE_PROMPT=1

# 圧縮ファイルの解凍
function extract() {
    case $1 in
        *.tar.gz|*.tgz) tar xzvf $1;;
        *.tar.xz) tar Jxvf $1;;
        *.zip) unzip $1;;
        *.lzh) lha e $1;;
        *.tar.bz2|*.tbz) tar xjvf $1;;
        *.tar.Z) tar zxvf $1;;
        *.gz) gzip -dc $1;;
        *.bz2) bzip2 -dc $1;;
        *.Z) uncompress $1;;
        *.tar) tar xvf $1;;
        *.arj) unarj $1;;
    esac
}

#圧縮ファイルを実行すると解凍するように
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz}=extract

# 環境変数関係
# PATH の組み立て順が重要:
#   1. path_helper と brew shellenv はどちらも PATH を作り直して自分の dir を先頭に置くので最初に通す
#      (direnv / mise の実体は brew 配下にあり、activate より前に brew が要る)
#   2. 各種ツールの bin を足す
#   3. mise (node / bun / ruby / python) は他のどの dir より前に来る必要があるので最後に activate する
# 親 shell (ghostel / tmux) から mise 入りの PATH を継いでいても、1 がその前に system dir や
# brew を割り込ませるので、「既に PATH にあるか」で activate を省いてはいけない。
# 毎回 activate して先頭に付け直し、重複は typeset -U で除く (先に現れた方が残る)。
setopt no_global_rcs
if [ -x /usr/libexec/path_helper ]; then
    eval `/usr/libexec/path_helper -s`
fi
typeset -U path PATH

# brew
eval "$(/opt/homebrew/bin/brew shellenv)"
export PATH="$HOME/.local/bin:$PATH"
# brew end

export PATH=${HOME}/bin:${PATH}
export PATH=${HOME}/fvm/default/bin:${PATH}

# bun (global bin)。PATH から引きたい npm パッケージは mise 管理に寄せた
# (~/.config/mise/config.toml) ので普段は空。手で bun add -g したときの受け皿。
export BUN_INSTALL="$HOME/.bun"
case ":$PATH:" in
  *":$BUN_INSTALL/bin:"*) ;;
  *) export PATH="$BUN_INSTALL/bin:$PATH" ;;
esac
# bun end

# direnv
eval "$(direnv hook zsh)"
# direnv end

# Added by Antigravity
export PATH="/Users/wamei/.antigravity/antigravity/bin:$PATH"

# libpq (psql / pg_dump)。postgresql と衝突するため keg-only で symlink されず、bin を明示的に足す。
# Emacs の sql-mode (M-x sql-postgres) が psql を PATH から引く。
export PATH="/opt/homebrew/opt/libpq/bin:$PATH"

# mise (node / bun / ruby / python)。activate 時と precmd / chpwd で有効なツールの bin を PATH 先頭に差し込むので、
# PATH を組み終えた最後に置く。設定は ~/.config/mise/config.toml (dotfiles の .config/mise)。
eval "$(mise activate zsh)"
# mise end

# load local settings
[[ -f ${HOME}/.zshrc.local ]] && source ${HOME}/.zshrc.local

# HOMEBREW_PREFIX は上の brew shellenv が export する (brew を 2 回起動しないため $(brew --prefix) は使わない)
source $HOMEBREW_PREFIX/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source $HOMEBREW_PREFIX/share/zsh-autocomplete/zsh-autocomplete.plugin.zsh

# bindkey              '^I' menu-select
# bindkey "$terminfo[kcbt]" menu-select
# bindkey -M menuselect              '^I'         menu-complete
# bindkey -M menuselect "$terminfo[kcbt]" reverse-menu-complete

# 端末タイトルに直前に実行したコマンドを流す。
# Emacs (ghostel) の端末一覧がこのタイトルを拾って表示する。
# 既存の precmd() を壊さないよう add-zsh-hook を使う。
# precmd 側では戻さないので、コマンド終了後も最後のコマンド名が残る。
autoload -Uz add-zsh-hook
_wamei_set_terminal_title() { printf '\033]0;%s\007' "$1" }
add-zsh-hook preexec _wamei_set_terminal_title

# Emacs (ghostel) 内でだけ効くセッション復元の連携 (term-restore.el)。
# プロンプトの位置は ghostel が OSC 133 のシェル統合を自動注入して拾うので、
# ここでは何も出さない (vterm 時代は OSC 51;A を自前で出していた)。
if [[ $INSIDE_EMACS == *ghostel* ]]; then
  # 復元された端末では前回の出力の末尾が WAMEI_TERM_RESTORE のファイルに入っている
  # (色は term-restore.el が SGR エスケープにして書いてある)。最初のプロンプトの前に
  # そのまま出し、子プロセスに引き継がないよう unset する。
  # 出す前に画面とスクロールバック (\e[3J) を消す。login のバナー
  # ("Last login: ... on ttys000") は .zshrc より前に出るので、消さないと復元した
  # 出力の上に残り、次の保存でそれごと巻き取られて復元のたびに 1 行ずつ増える。
  if [[ -n $WAMEI_TERM_RESTORE && -r $WAMEI_TERM_RESTORE ]]; then
    printf '\e[H\e[2J\e[3J'
    cat -- "$WAMEI_TERM_RESTORE"
  fi
  unset WAMEI_TERM_RESTORE

  # シェルから Emacs 側を開く。ghostel_cmd は ghostel のシェル統合が定義する関数で、
  # OSC 52;e を通して Emacs の ghostel-eval-cmds に載っている関数だけを呼べる
  # (magit-status-setup-buffer は init.el で追加している)。相対パスは端末バッファの
  # default-directory (OSC 7 でシェルの cwd に追従) 基準で解決される。
  # 端末パネルは下部の side window なので、other-window 系で主領域に開く。
  e()   { local f; for f in "$@"; do ghostel_cmd find-file-other-window "$f"; done }
  dow() { ghostel_cmd dired-other-window "${1:-$PWD}" }
  gst() { ghostel_cmd magit-status-setup-buffer "$PWD" }
fi
