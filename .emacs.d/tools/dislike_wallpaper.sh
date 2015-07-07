#!/bin/bash
#
[ -f /tmp/CURRENT_WALLPAPERS ] || exit 0

FILES=`cat /tmp/CURRENT_WALLPAPERS`
N=`echo $FILES | awk '{print NF}'`
if [ $N -eq 1 ]; then
    rm $FILES
else
    FS=
    for fn in `echo $FILES`; do
        F=
        if [ -z "$FS" ]; then
            FS="${fn##*/}"
        else
            FS="${FS}|${fn##*/}"
        fi
    done

    f=$(rofi -sep "|" -dmenu -i  -p 'Choose file' -hide-scrollbar -line-padding 4 \
             -padding 20 -lines 4 <<< \
             "$FS")
    if [ -n "$f" ]; then
        rm ${HOME}/.config/wallpapers/$f
    fi
fi
