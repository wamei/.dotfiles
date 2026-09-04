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
brew install laishulu/homebrew/macism  # Emacs の minibuffer で IME を off にする
brew install --cask karabiner-elements
brew install --cask 1password
brew install --cask nordlayer

brew install --cask google-chrome
brew install --cask google-chrome-canary

brew install --cask slack
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
