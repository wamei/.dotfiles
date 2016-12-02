# 環境変数
export LANG=en_US.UTF-8
export TZ=Asia/Tokyo
export PAGER='less -R'
export PATH=$HOME/bin:$PATH:$HOME/Library/Android/sdk/platform-tools
if [ -x "`which go`" ]; then
    export GOROOT=`go env GOROOT`
    export GOPATH=$HOME/go
    export PATH=$PATH:$GOPATH/bin:$GOROOT/bin
fi

# git関係alias
alias g='git'
alias gst='git status'
alias glo='git mylog'
alias ggr='git graph'
alias gdi='git diff'
alias gbr='git branch'
alias gco='git checkout'
function git_root() {
    if git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
        cd `git rev-parse --show-toplevel`
    fi
}
alias gro=git_root
alias gitroot=git_root

if [ "$EMACS" ]; then
    export EDITOR="emacsclient"
    alias ec='emacsclient -n'
else
    export EDITOR="emacsclient -t"
    alias ec='emacsclient -t'
fi

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo '

# OS 別の設定
case ${OSTYPE} in
    # Mac用の設定
    darwin*)
        alias ls='ls -G'
        ;;
    #Linux用の設定
    linux*)
        alias ls='ls --color=auto'
        ;;
esac

[[ $TERM = "eterm-color" ]] && TERM=xterm-color

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
zstyle ':completion:*:default' menu select=2 interactive
## 補完候補に色を付ける。
### "": 空文字列はデフォルト値を使うという意味。
zstyle ':completion:*:default' list-colors ""
# Go completion
if [ -f $GOROOT/../share/zsh/site-functions/go ]; then
    source $GOROOT/../share/zsh/site-functions/go
fi

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
PROMPT="%B%F{white}%(?..%K{red}            status code -%?-            %{%k%}
)%{%k%f%b%}%K{white}%F{black} %D{%Y/%m/%d %H:%M} %k%f %F{magenta}%~%f%F{green}%1(v|%1v%2v]|)%f
%n@%m $ "

PROMPT2='[%n]> '

# nvm読み込み
if [[ -s ~/.nvm/nvm.sh ]] ; then source ~/.nvm/nvm.sh ; fi

# cd ls
function chpwd() { ls }

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

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM function

## Invoke the ``dired'' of current working directory in Emacs buffer.
function dired () {
  emacsclient -t -e "(dired \"${1:a}\")"
}

## Chdir to the ``default-directory'' of currently opened in Emacs buffer.
function cde () {
    EMACS_CWD=`emacsclient -e "(return-current-working-directory-to-shell)" | sed 's/^"\(.*\)"$/\1/'`
    echo "chdir to $EMACS_CWD"
    cd "$EMACS_CWD"
}

function tmux-set-buffer () {
    [ ! -z "$TMUX" ] && tmux set-buffer $1
}

function tmux-get-buffer () {
    [ ! -z "$TMUX" ] && tmux show-buffer
}

# コピペ関係
function delete-region() {
    zle kill-region
    CUTBUFFER=$killring[1]
    shift killring
}
zle -N delete-region

function backward-delete-char-or-region() {
    if [ $REGION_ACTIVE -eq 0 ]; then
        zle backward-delete-char
    else
        zle delete-region
    fi
}
zle -N backward-delete-char-or-region

function delete-char-or-list-or-region() {
    if [ $REGION_ACTIVE -eq 0 ]; then
        zle delete-char-or-list
    else
        zle delete-region
    fi
}
zle -N delete-char-or-list-or-region

bindkey "^h" backward-delete-char-or-region
bindkey "^d" delete-char-or-list-or-region

function tmux-copy-region() {
    zle copy-region-as-kill
    REGION_ACTIVE=0
    tmux-set-buffer $CUTBUFFER
    which rpbcopy 1>/dev/null 2>&1 && echo -n $CUTBUFFER | rpbcopy
}
zle -N tmux-copy-region
bindkey "^[w" tmux-copy-region

function tmux-backward-kill-word-or-region() {
    if [ $REGION_ACTIVE -eq 0 ]; then
        zle backward-kill-word
    else
        zle kill-region
    fi
    tmux-set-buffer $CUTBUFFER
    which rpbcopy 1>/dev/null 2>&1 && echo -n $CUTBUFFER | rpbcopy
}
zle -N tmux-backward-kill-word-or-region
bindkey "^w" tmux-backward-kill-word-or-region

function tmux-kill-line () {
    zle kill-line
    tmux-set-buffer $CUTBUFFER
    which rpbcopy 1>/dev/null 2>&1 && echo -n $CUTBUFFER | rpbcopy
}
zle -N tmux-kill-line
bindkey "^k" tmux-kill-line

function tmux-kill-whole-line () {
    zle kill-whole-line
    tmux-set-buffer $CUTBUFFER
    which rpbcopy 1>/dev/null 2>&1 && echo -n $CUTBUFFER | rpbcopy
}
zle -N tmux-kill-whole-line
bindkey "^u" tmux-kill-whole-line

function tmux-yank () {
    [ ! -z "$TMUX" ] && CUTBUFFER=$(tmux-get-buffer)
    zle yank
}
zle -N tmux-yank
bindkey "^y" tmux-yank
