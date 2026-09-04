#! /bin/bash

# make symbolic links
ln -sf ~/.dotfiles/.inputrc ~/.inputrc
ln -sf ~/.dotfiles/.zshenv ~/.zshenv
ln -sf ~/.dotfiles/.zshrc ~/.zshrc
#ln -sf ~/.dotfiles/.gitconfig ~/.gitconfig
ln -sf ~/.dotfiles/.tmux.conf ~/.tmux.conf

mkdir -p ~/.config/mise
ln -sf ~/.dotfiles/.config/mise/config.toml ~/.config/mise/config.toml

mkdir -p ~/.aws
ln -sf ~/.dotfiles/.aws/update-mfa-profile ~/.aws/update-mfa-profile

mkdir -p ~/.emacs.d/
ln -sf ~/.dotfiles/.emacs.d/init.el ~/.emacs.d/init.el
ln -sf ~/.dotfiles/.emacs.d/early-init.el ~/.emacs.d/early-init.el

# copy bin
mkdir -p ~/bin
cp -f ~/.dotfiles/bin/rpbcopy ~/bin

# setting git config
git config --global user.name "wamei"
git config --global user.email "wamei.cho@gmail.com"
git config --global color.ui auto
git config --global core.excludesfile "~/.gitignore"
