#!/bin/bash
#/etc/cron.d/wamei
#*/1 * * * * ubuntu $HOME/.dotfiles/bin/generate_tmux_caption.sh > /dev/null
IFS=$'\n'
MESSAGE=(`cat /tmp/tmux_caption_list.txt`)
echo ${MESSAGE[$(($RANDOM % ${#MESSAGE[@]}))]} > /tmp/tmux_caption.txt
