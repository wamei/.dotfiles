#! /bin/bash

# make symbolic links
ln -sf ~/.dotfiles/.inputrc ~/.inputrc
ln -sf ~/.dotfiles/.zshrc ~/.zshrc
#ln -sf ~/.dotfiles/.gitconfig ~/.gitconfig
ln -sf ~/.dotfiles/.tmux.conf ~/.tmux.conf
ln -sfn ~/.dotfiles/.emacs.d/ ~/.emacs.d

# copy bin
mkdir -p ~/bin
cp -f ~/.dotfiles/bin/rpbcopy ~/bin

# setting git config
git config --global color.ui auto
git config --global core.excludesfile "~/.gitignore"
