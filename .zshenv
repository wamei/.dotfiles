setopt no_global_rcs
if [ -x /usr/libexec/path_helper ]; then
    eval `/usr/libexec/path_helper -s`
fi
export PATH=${HOME}/bin:${PATH}

# rbenv
[[ -d ${HOME}/.rbenv ]] && \
  export PATH=${HOME}/.rbenv/shims:${HOME}/.rbenv/bin:${PATH} && \
  eval "$(rbenv init -)"

# nodenv
[[ -d ${HOME}/.nodenv ]] && \
  export PATH=${HOME}/.nodenv/bin:${HOME}/.nodenv/shims:${PATH} && \
  eval "$(nodenv init -)"
