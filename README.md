## Init
```
$ ./init.sh
```

## Homebrew (Mac)
```
$ ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
$ brew update
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

## tmux
```
# Mac
$ brew install tmux

# Ubuntu
# aptからだと少し古い
$ sudo apt-get install tmux

# sourceからbuild
$ sudo apt install git automake bison build-essential pkg-config libevent-dev libncurses5-dev
$ cd /usr/local/src/
$ git clone https://github.com/tmux/tmux
$ cd ./tmux/
$ ./autogen.sh
$ ./configure --prefix=/usr/local
$ make
$ sudo make install
```

## Emacs
```
# Mac
$ brew install emacs
$ brew cask install emacs

# Ubuntu
$ sudo apt-get install emacs
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

## nodenv
```
# Mac
$ brew install nodenv

# Ubuntu
$ git clone git://github.com/nodenv/nodenv.git ~/.nodenv
$ git clone git://github.com/nodenv/node-build.git ~/.nodenv/plugins/node-build
```
