export PATH=$HOME/bin:$PATH

# rbenv
[[ -d ${HOME}/.rbenv ]] && \
  export PATH=${HOME}/.rbenv/bin:${PATH} && \
  eval "$(rbenv init -)"

# nodenv
[[ -d ${HOME}/.nodenv ]] && \
  export PATH=${HOME}/.nodenv/bin:${PATH} && \
  eval "$(nodenv init -)"
