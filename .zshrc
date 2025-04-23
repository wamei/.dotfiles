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
HISTSIZE=1000000
SAVEHIST=1000000
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
  if [[ -n "$(rbenv local 2>/dev/null)" ]]; then
    echo -n " %F{009}rb:$(rbenv version-name)%f"
  else
    echo -n " rb:$(rbenv version-name)"
  fi
  if [[ -n "$VIRTUAL_ENV" && -n "$DIRENV_DIR" ]]; then
    echo -n " %F{009}py:$(pyenv version-name)($(basename $VIRTUAL_ENV))%f"
  elif [[ -n "$(pyenv local 2>/dev/null)" ]]; then
    echo -n " %F{009}py:$(pyenv version-name)%f"
  else
    echo -n " py:$(pyenv version-name)"
  fi
  if [[ -n "$(nodenv local 2>/dev/null)" ]]; then
    echo -n " %F{009}node:$(nodenv version-name)%f"
  else
    echo -n " node:$(nodenv version-name)"
  fi
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

# load local settings
[[ -f ${HOME}/.zshrc.local ]] && source ${HOME}/.zshrc.local

source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source $(brew --prefix)/share/zsh-autocomplete/zsh-autocomplete.plugin.zsh

bindkey              '^I' menu-select
bindkey "$terminfo[kcbt]" menu-select
bindkey -M menuselect              '^I'         menu-complete
bindkey -M menuselect "$terminfo[kcbt]" reverse-menu-complete

# 環境変数関係
setopt no_global_rcs
if [ -x /usr/libexec/path_helper ]; then
    eval `/usr/libexec/path_helper -s`
fi

export PATH=${HOME}/bin::${PATH}
export PATH=${HOME}/fvm/default/bin:${PATH}

# rbenv
[[ -d ${HOME}/.rbenv ]] && \
case ":$PATH:" in
  *".rbenv"*) ;;
  *) eval "$(rbenv init - zsh)" ;;
esac
# rbenv end

# nodenv
[[ -d ${HOME}/.nodenv ]] && \
case ":$PATH:" in
  *".nodenv"*) ;;
  *) eval "$(nodenv init - zsh)" ;;
esac
# nodenv end

# pnpm
export PNPM_HOME="/Users/wamei/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# direnv
eval "$(direnv hook zsh)"
# direnv end

# brew
eval "$(/opt/homebrew/bin/brew shellenv)"
export PATH="$HOME/.local/bin:$PATH"
# brew end

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
# pyenv end
