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

if [ `uname` = "Darwin" ]; then
    alias ls='ls -aG'
    alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
    alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
    export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
elif [ `uname` = "Linux" ]; then
    alias ls='ls -a --color=auto'
    export EDITOR="emacsclient"
fi

alias en='emacsclient -n'
alias ew='emacsclient -nw'
alias screen='screen -U'

if [[ -s ~/.nvm/nvm.sh ]] ; then source ~/.nvm/nvm.sh ; fi

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM function
