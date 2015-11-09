#! /bin/sh
which df > /dev/null 2>&1 && df -P | awk 'NR==2 {printf "HDD:%2.1f%%\n", $4/$2*100}'
