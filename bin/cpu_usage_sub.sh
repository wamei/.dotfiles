#! /bin/bash
cpu_usage_buffer="/tmp/cpu.usage.buffer"
cpu_usage="/tmp/cpu.usage"
which top > /dev/null 2>&1 && ps aux | grep -v "[t]op -b -n 2" | top -b -n 2 | grep "Cpu(s):.*id" | tail -n 1 | awk '{print (100.0-substr($5, 1, index($5, "i")-2)) "%"}' > $cpu_usage_buffer && cp $cpu_usage_buffer $cpu_usage
