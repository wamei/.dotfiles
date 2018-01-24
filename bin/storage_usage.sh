#! /bin/bash
which df > /dev/null 2>&1 && df -P | awk '{sum += $2; used += $3} END {printf "HDD:%2.1f%%\n", used/sum*100}'
