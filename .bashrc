export LANG=ja_JP.UTF-8

alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
alias ec='emacsclient'
alias ls='ls -aG'

## create emacs env file
perl -wle \
    'do { print qq/(setenv "$_" "$ENV{$_}")/ if exists $ENV{$_} } for @ARGV' \
    PATH > ~/.emacs.d/shellenv.el
