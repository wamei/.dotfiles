# 環境変数
export TZ=Asia/Tokyo
export LANG=en_US.UTF-8
if [ -x "`which go`" ]; then
    export GOROOT=`go env GOROOT`
    export GOPATH=$HOME/go
    export PATH=$PATH:$GOPATH/bin:$GOROOT/bin
fi

# alias
alias ls='ls -a'
alias la='ls -a'
alias ll='ls -al'

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

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo '

if [ `uname` = "Darwin" ]; then
    alias ls='ls -aG'
    alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
    alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
    export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t"
elif [ `uname` = "Linux" ]; then
    alias ls='ls -a --color=auto'
    export EDITOR="emacsclient -t"
fi

alias en='emacsclient -n'
alias ew='emacsclient -nw'
alias screen='screen -U'

if [[ -s ~/.nvm/nvm.sh ]] ; then source ~/.nvm/nvm.sh ; fi

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM function
