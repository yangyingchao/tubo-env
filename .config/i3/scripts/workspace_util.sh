#!/bin/bash
#
# Author: Yang,Ying-chao <yangyingchao@gmail.com>, 2018-03-22
#

source ~/.config/i3/scripts/common.sh

if [ -n "${WAYLAND_DISPLAY}" ]; then
    edebug "Sway"
    MSG_CMD=swaymsg
    workspaces=`swaymsg -r -t get_workspaces |   tr "\n" " " | sed -E "s/ +//g"`
else
    edebug "I3"
    MSG_CMD=i3-msg
    workspaces=`i3-msg -t get_workspaces`
fi

cur=`echo $workspaces | tr } "\n" | grep '"focused":true' | tr , "\n" |grep num | awk -F ":" '{print $2}'`

function snd_msg ()
{
    edebug "$@"

    eval "${MSG_CMD} -q $@"

    if [ -n "$debug" ]; then
        dunstify -t 600 "$@"
    fi

    # # Issue mouse click..
    # screen_res=`xrandr | grep -e " connected.*\+" |sed "s/ /\n/g" | grep -e ".*x.*+.*+.*" | awk -F "+" '{print $1}'`
    # xpos=`echo $screen_res | awk -F "x" '{print $1/2}'`
    # ypos=`echo $screen_res | awk -F "x" '{print $2/2}'`

    # xdotool mousemove $xpos $ypos
    # xdotool click 1
}

declare -a wks # all available workspaces.
slot=0

for wk in `echo $workspaces  | tr , "\n" |grep num | awk -F ":" '{print $2}'`; do
    edebug "Workspace: $wk"

    wks[$slot]=$wk

    if [ $wk -eq $cur ]; then
        cur_slot=$slot
    fi

    slot=$((slot+1))
done

total_slots=$((slot-1))

edebug "First: $first, Last: $last, Cur: $cur, ALL: ${wks[@]}"


case $1 in
    go_right)
         next=$((cur_slot+1))
         edebug  "NEXT: ${next}, ALL${wks[*]}"

        if [ $next -eq ${#wks[@]} ]; then
            next=0
            edebug "Wrap to 0: ${wks[$next]}"
        fi
        snd_msg "workspace ${wks[$next]}"
        ;;
    go_left)
        next=$((cur_slot-1))
        snd_msg "workspace ${wks[$next]}"
        ;;
    go_first)
        snd_msg "workspace ${wks[0]}"
        ;;
    go_last)
       snd_msg "workspace ${wks[-1]}"
        ;;
    new)
        next=0
        for i in `seq 1 9`; do
            for j in `seq 0 ${total_slots}`; do
                exist=0
                if [ $i -eq ${wks[$j]}  ]; then
                    exist=1
                    break
                fi
            done

            if [ $exist -eq 0 ]; then
                next=$i
                break
            fi
        done
        snd_msg "workspace $next"
        ;;
    *)
        echo "Usage: $0 go_right|go_left|go_first|go_last|new"
    ;;
esac
