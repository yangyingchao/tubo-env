#!/bin/bash
#
# Author: Yang, Ying-chao@gmail.com, 07-11-2011
#
notify-send $PWD
ps aux | grep conky | grep -v grep | grep -v run_conky
if [ $? -eq 0 ]; then
    notify-send "Conky hash been started."
    exit 1
fi
notify-send "Starting conky!"
cpu_num=`cat /proc/cpuinfo | grep processor | wc -l`
if [ $cpu_num -gt 1 ]; then
    cat $HOME/.fvwm/tools/conky_head.tpl > $HOME/.conkyrc
    x_pos=$(($1-250))
    echo "gap_x $x_pos" >> $HOME/.conkyrc
    cat  $HOME/.fvwm/tools/conky_text.tpl >> $HOME/.conkyrc
    /usr/bin/conky -d
else
    echo "Can not start conky: Only one CPU detected!"
    notify-send "Can not start conky: Only one CPU detected!"
fi

