## Init

```:console
./init.sh
```

## Homebrew (Mac)

```:console
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew update
brew tap homebrew/cask-versions

brew install coreutils

brew tap homebrew/cask-fonts
brew install --cask font-hackgen
brew install --cask font-hackgen-nerd

brew install --cask google-japanese-ime
brew install --cask karabiner-elements
brew install --cask 1password

brew install --cask google-chrome
brew install --cask google-chrome-canary

brew install --cask slack
brew install --cask discord
brew install --cask chatwork
brew install --cask microsoft-teams
brew install --cask gather

brew install --cask postman
brew install --cask mysqlworkbench

brew install android-sdk
brew install --cask android-studio

defaults write com.apple.finder AppleShowAllFiles True
```

## apt (Ubuntu)

```:console
sudo apt-get update
```

## zsh

```:console
# Mac
$ brew install zsh
$ chsh -s $(which zsh)

# Ubuntu
$ sudo apt-get install zsh
$ chsh -s $(which zsh)
```

zsh compinit: insecure directories, run compaudit for list.って言われたら

```:console
chmod 755 /usr/local/share/zsh/site-functions
chmod 755 /usr/local/share/zsh
```

## xsel (pbcopyの代替)

```:console
# Ubuntu
$ sudo apt-get install xsel
```

## Emacs

```:console
# Mac
$ brew install --cask emacs

# Ubuntu
$ sudo apt-get install emacs
```

## Docker

```:console
# Mac
$ brew install --cask docker

# Ubuntu
$ sudo apt-get install docker-ce docker-compose
```

#### Language Server Protocol

<https://emacs-lsp.github.io/lsp-mode/page/languages/>

## The Silver Searcher

```:console
# Mac
$ brew install ag

# Ubuntu
$ sudo apt-get install silversearcher-ag
```

## ripgrep

```:console
# Mac
$ brew install rg

# Ubuntu
$ sudo apt-get install ripgrep
```

## nodenv

```:console
# Mac
$ brew install nodenv

# Ubuntu
$ git clone git://github.com/nodenv/nodenv.git ~/.nodenv
$ git clone git://github.com/nodenv/node-build.git ~/.nodenv/plugins/node-build
```
