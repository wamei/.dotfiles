#! /bin/sh
cpu_usage="/tmp/cpu.usage"
if [ "$(uname)" == 'Darwin' ]; then
    which top > /dev/null 2>&1 && top -R -F -u -l 1 | head -n 4 | tail -n 1 | awk '{print "CPU:" (100.0-substr($7, 1, index($7, "%")-1)) "%"}'
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    [ -f $cpu_usage ] && cat $cpu_usage | awk '{print "CPU:" $1}'
    ~/.dotfiles/bin/cpu_usage_sub.sh > /dev/null 2>&1 &
fi
