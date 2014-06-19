# 環境変数
export LANG=en_US.UTF-8
export TZ=Asia/Tokyo
export EDITOR="emacsclient"

# alias
alias ls='ls -a'
alias la='ls -a'
alias ll='ls -al'
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'

alias gst='git status'

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo '

# OS 別の設定
case ${OSTYPE} in
    darwin*)
        # Mac用の設定
        # export CLICOLOR=1
        alias ls='ls -aG'
        alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
        alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
        ;;
    linux*)
        #Linux用の設定
        alias ls='ls -a --color=auto'
        ;;
esac

alias ec='emacsclient'
alias en='emacsclient -nw'
alias screen='screen -U'

# emacs 風キーバインドにする
bindkey -e

# 色を使用出来るようにする
autoload -Uz colors
colors

# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# '#' 以降をコメントとして扱う
setopt interactive_comments

# 補完機能を有効にする
autoload -Uz compinit
compinit
# 補完で小文字でも大文字にマッチさせる
#zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z} r:|[._-]=*'
# ../ の後は今いるディレクトリを補完しない
zstyle ':completion:*' ignore-parents parent pwd ..
# sudo の後ろでコマンド名を補完する
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin
# ps コマンドのプロセス名補完
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'
# 補完候補が複数あるときに自動的に一覧表示する
setopt auto_menu
## 補完方法毎にグループ化する。
### 補完方法の表示方法
###   %B...%b: 「...」を太字にする。
###   %d: 補完方法のラベル
zstyle ':completion:*' format '%B%d%b'
zstyle ':completion:*' group-name ''
## 補完侯補をメニューから選択する。
### select=2: 補完候補を一覧から選択する。
###           ただし、補完候補が2つ以上なければすぐに補完する。
zstyle ':completion:*:default' menu select=2
## 補完候補に色を付ける。
### "": 空文字列はデフォルト値を使うという意味。
zstyle ':completion:*:default' list-colors ""

# ヒストリの補完
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups
# ヒストリファイルに保存するとき、すでに重複したコマンドがあったら古い方を削除する
setopt hist_save_nodups
# スペースから始まるコマンド行はヒストリに残さない
setopt hist_ignore_space
# ヒストリに保存するときに余分なスペースを削除する
setopt hist_reduce_blanks

# 高機能なワイルドカード展開を使用する
setopt extended_glob
# ^R でヒストリ検索をするときに * でワイルドカードを使用出来るようにする
bindkey '^R' history-incremental-pattern-search-backward

# 単語の区切り文字を指定する
autoload -Uz select-word-style
select-word-style default
# ここで指定した文字は単語区切りとみなされる
# / も区切りと扱うので、^W でディレクトリ１つ分を削除できる
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified

# ディレクトリ名だけでcdする
setopt auto_cd
# ディレクトリスタックに追加
setopt auto_pushd
setopt pushd_ignore_dups

## PROMPT内で変数展開・コマンド置換・算術演算を実行する。
setopt prompt_subst
## PROMPT内で「%」文字から始まる置換機能を有効にする。
setopt prompt_percent
# レポジトリ情報の表示
autoload -Uz vcs_info
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "!"
zstyle ':vcs_info:git:*' unstagedstr "+"
zstyle ':vcs_info:*' formats '[%s:%b%c%u'
zstyle ':vcs_info:*' actionformats '[%s:%b%c%u|%a'
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
    untracked=""
    git_status=$(git status -s 2> /dev/null)
    if echo "$git_status" | grep "^??" > /dev/null 2>&1; then
        untracked="?"
    fi
    [[ -n "$untracked" ]] && psvar[2]="$untracked"
}
# prompt表示設定
PROMPT="%{%B%F{white}%(?..%K{red}            status code -%?-            
)%}%{%k%f%b%}[%D{%Y/%m/%d %H:%M}] %{${fg[magenta]}%}%~%{${reset_color}%}%F{green}%1(v|%F{green}%1v%2v]%f|)%{%f%}
%n@%m $ "

PROMPT2='[%n]> '

# nvm読み込み
if [[ -s ~/.nvm/nvm.sh ]] ; then source ~/.nvm/nvm.sh ; fi

# tmux起動
if [ -z "$TMUX" -a -z "$STY" ]; then
    if type tmuxx >/dev/null 2>&1; then
        tmuxx
    elif type tmux >/dev/null 2>&1; then
        if tmux has-session && tmux list-sessions | grep -qE '.*]$'; then
            tmux attach && echo "tmux attached session "
        else
            tmux new-session && echo "tmux created new session"
        fi
    fi
fi

# 空でenterしたときにls git status
function do_enter() {
    if [ -n "$BUFFER" ]; then
        zle accept-line
        return 0
    fi
    echo
    git status -sb 2> /dev/null
    echo
    echo
    precmd
    zle reset-prompt
    return 0
}
zle -N do_enter
bindkey '^m' do_enter

# cd ls
function chpwd() { ls }
