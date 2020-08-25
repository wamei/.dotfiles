# 環境変数
export LANG=en_US.UTF-8
export TZ=Asia/Tokyo
export PAGER='less -R'

# Emacs
if [ "$EMACS" ]; then
    export EDITOR="emacsclient"
    alias ec='emacsclient -n'
else
    export EDITOR="emacsclient -t"
    alias ec='emacsclient -t'
fi

# emacsclientのdiredで開く
function dired () {
  ec -e "(dired \"${1:a}\")"
}

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

# 補完機能を有効にする
autoload -Uz compinit
compinit
# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z} r:|[._-]=*'
# 補完方法毎にグループ化する
zstyle ':completion:*' format '%B%d%b'
zstyle ':completion:*' group-name ''
# 補完侯補をメニューから選択する
zstyle ':completion:*:default' menu select=2
# 補完候補に色を付ける
zstyle ':completion:*:default' list-colors "${LS_COLORS}"
# コマンドにsudoを付けても補完する
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin
# 補完リストが多いときに尋ねない
LISTMAX=1000

# ヒストリの補完
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end
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
setopt hist_reduce_blanks

# 単語の区切り文字を指定する
autoload -Uz select-word-style
select-word-style default
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified

# ディレクトリスタックに追加
setopt auto_pushd

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

# WSL時のemacs設定
if [[ `uname -a` =~ Linux && `uname -a` =~ microsoft ]]; then
    if [ "$INSIDE_EMACS" ]; then
        TERM=eterm-color
    fi

    if uname -v | grep -v -q 'Microsoft'; then
        if ! service ssh status > /dev/null 2>&1; then
            sudo service ssh start
            # sudo でパスワード入力の不要設定をしていない場合は、上記の代わりに次のコマンドを使う
            # wsl.exe -d "$WSL_DISTRO_NAME" -u root service ssh start
        fi

        if ! ss -lt4 | grep -q '127.0.0.1:6020'; then
            setsid ssh.exe -p 10022 -f -N -R 6020:127.0.0.1:6000 $USER@localhost &
        fi
    fi

    umask 022
    if [ -z "$DISPLAY" ]; then
        if uname -v | grep -q 'Microsoft'; then
            export DISPLAY=:0
        else
            if ss -lt4 | grep -q '127.0.0.1:6020'; then
                export DISPLAY=:20
            else
                # export DISPLAY=$(awk '/^nameserver/ {print $2; exit}' /etc/resolv.conf):0.0
                export DISPLAY=$(ip route | awk '/^default/ {print $3; exit}'):0.0
            fi
        fi
    fi

    # export NO_AT_BRIDGE=1
    export LIBGL_ALWAYS_INDIRECT=1
    # export GIGACAGE_ENABLED=no

    # 必要であれば、以下をアンコメント化する
    keychain -q ~/.ssh/id_rsa
    source ~/.keychain/`hostname`-sh

fi

# load local settings
[[ -f ${HOME}/.zshrc.local ]] && source ${HOME}/.zshrc.local
