#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-19
#
case $1 in
    run)
        rofi -show run -run-command "${term} -e \"{cmd}\""
        ;;
    window)
        rofi  -show window -config ~/.config/rofi/themes/Switch.rasi
        ;;
    search)
        rofi  -show Search -modi "Search:${HOME}/.config/i3/scripts/search.sh"
        ;;
    ssh)
        TERMINAL="alacritty" rofi -show ssh
        ;;
    power)
        ~/.config/rofi/scripts/Powermenu.sh -theme-str
        ;;
    *)
        ICON_SIZE=3.2
        if [ -z "${WAYLAND_DISPLAY}" ]; then
            w=`i3-msg -t get_workspaces | tr '}' "\n" | grep '"focused":true' | tr , "\n" |grep width | awk -F ":" '{print $2}'`
            if [ $w -gt 1366 ]; then
                ICON_SIZE=3.6
            fi
        fi

        rofi -theme-str "element-icon { size: ${ICON_SIZE}ch;}" -show drun
        ;;
esac
