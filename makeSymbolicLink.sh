#! /bin/bash
ln -sf ~/.dotfiles/.inputrc ~/.inputrc
ln -sf ~/.dotfiles/.make-filelist.rb ~/.make-filelist.rb
ln -sf ~/.dotfiles/.zshrc ~/.zshrc
#ln -sf ~/.dotfiles/.gitconfig ~/.gitconfig
ln -sf ~/.dotfiles/.tmux.conf ~/.tmux.conf
ln -sfn ~/.dotfiles/.emacs.d/ ~/.emacs.d

git config --global color.ui auto
git config --global core.excludesfile "~/.gitignore"
