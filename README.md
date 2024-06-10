## Init

```sh
./init.sh
```

## Homebrew (Mac)

```sh
defaults write com.apple.finder AppleShowAllFiles True

ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew update
brew tap homebrew/cask-versions

brew install coreutils
brew install zsh-completions

brew tap homebrew/cask-fonts
brew install --cask font-hackgen
brew install --cask font-hackgen-nerd

brew install --cask google-japanese-ime
brew install --cask karabiner-elements
brew install --cask 1password
brew install --cask nordlayer

brew install --cask google-chrome
brew install --cask google-chrome-canary

brew install --cask slack
brew install --cask discord
brew install --cask chatwork
brew install --cask microsoft-teams
brew install --cask gather

brew install --cask visual-studio-code
brew install --cask postman
brew install --cask mysqlworkbench
brew install --cask orbstack
brew install --cask warp

brew install awscli
brew install copilot

brew install nodenv
brew install rbenv
brew install pyenv
```

## apt (Ubuntu)

```sh
sudo apt-get update

sudo apt-get install xsel
sudo apt-get install docker-ce docker-compose

git clone git://github.com/nodenv/nodenv.git ~/.nodenv
git clone git://github.com/nodenv/node-build.git ~/.nodenv/plugins/node-build
```
