export LANGUAGE=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8
export LANG=ja_JP.UTF-8

if [ `uname` = "Darwin" ]; then
    alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
    alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
    alias ls='ls -aG'
elif [ `uname` = "Linux" ]; then
    alias ls='ls -a --color=auto'
fi
alias ec='emacsclient'
alias screen='screen -U'

## create emacs env file
perl -wle \
    'do { print qq/(setenv "$_" "$ENV{$_}")/ if exists $ENV{$_} } for @ARGV' \
    PATH > ~/.emacs.d/elisp/shellenv.el
