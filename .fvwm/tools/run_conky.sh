#!/bin/bash
#
# Author: Yang, Ying-chao@gmail.com, 07-11-2011
#

ps aux | grep conky | grep -v grep >/dev/null 2 >&1
if [ $? -eq 0 ]; then
    exit 1
fi
notify-send "Starting conky!"
cpu_num=`cat /proc/cpuinfo | grep processor | wc -l`
if [ $cpu_num -gt 1 ]; then
    /usr/bin/conky -d
else
    notify-send "Can not start conky: Only one CPU detected!"
fi

