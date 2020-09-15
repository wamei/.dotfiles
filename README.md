## Init
```
$ ./init.sh
```

## Homebrew (Mac)
```
$ ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
$ brew update

$ brew install coreutils

$ brew cask install hiddenbar
$ brew cask install google-chrome
$ brew cask install google-japanese-ime
$ brew cask install karabiner-elements
$ brew cask install 1password
$ brew cask install slack
$ brew cask install discord

$ brew tap homebrew/cask-fonts
$ brew cask install font-hackgen
$ brew cask install font-hackgen-nerd

$ defaults write com.apple.finder AppleShowAllFiles True
```

## apt (Ubuntu)
```
$ sudo apt-get update
```

## zsh
```
# Mac
$ brew install zsh
$ chsh -s $(which zsh)

# Ubuntu
$ sudo apt-get install zsh
$ chsh -s $(which zsh)
```
zsh compinit: insecure directories, run compaudit for list.って言われたら
```
$ chmod 755 /usr/local/share/zsh/site-functions
$ chmod 755 /usr/local/share/zsh
```

## xsel (pbcopyの代替)
```
# Ubuntu
$ sudo apt-get install xsel
```

## Emacs
```
# Mac
$ brew cask install emacs

# Ubuntu
$ sudo apt-get install emacs
```

## Docker
```
# Mac
$ brew install docker
$ brew cask install docker

# Ubuntu
$ sudo apt-get install docker-ce docker-compose
```

#### Language Server Protocol
https://emacs-lsp.github.io/lsp-mode/page/languages/

## The Silver Searcher
```
# Mac
$ brew install ag

# Ubuntu
$ sudo apt-get install silversearcher-ag
```

## ripgrep
```
# Mac
$ brew install rg

# Ubuntu
$ sudo apt-get install ripgrep
```

## nodenv
```
# Mac
$ brew install nodenv

# Ubuntu
$ git clone git://github.com/nodenv/nodenv.git ~/.nodenv
$ git clone git://github.com/nodenv/node-build.git ~/.nodenv/plugins/node-build
```
