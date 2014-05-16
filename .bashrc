export TZ=Asia/Tokyo
export LANG=en_US.UTF-8

if [ `uname` = "Darwin" ]; then
    alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
    alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
    alias ls='ls -aG'
elif [ `uname` = "Linux" ]; then
    alias ls='ls -a --color=auto'
fi
alias ec='emacsclient'
alias screen='screen -U'
