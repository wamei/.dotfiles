#! /bin/sh
cpu_usage="/tmp/cpu.usage"
if [ "$(uname)" == 'Darwin' ]; then
    which sar > /dev/null 2>&1 && sar -u 1 | tail -n 1 | awk '{print "CPU:" $5 "%"}'
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    [ -f $cpu_usage ] && cat $cpu_usage | awk '{print "CPU:" $1}'
    ~/.dotfiles/bin/cpu_usage_sub.sh > /dev/null 2>&1 &
fi
