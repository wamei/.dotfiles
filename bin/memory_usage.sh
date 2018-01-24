#! /bin/bash
if [ "$(uname)" == 'Darwin' ]; then
    which vm_stat > /dev/null 2>&1 && vm_stat | head -n 8 | tail -n 7 | awk '{m+=$3} NR==1{f+=$3} NR==4{f+=$3} END{printf "Mem:%2.1f%%\n", (m-f)/m*100}'
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    which free > /dev/null 2>&1 && free | awk 'NR==2 {printf "Mem:%2.1f%%\n", $3/$2*100}'
fi
