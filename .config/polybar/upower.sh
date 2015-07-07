#!/bin/bash
#
###
### Get battery status via upower.
###
### Usage:
###   upower [option]
###
### Options:
###	  -c		Print icon charactor of current status.
###	  -i		Print icon name of current status.
###	  -n		Notify current status via notify-send/dunstify.
###	  -s		Print battery status
###	  -p		Print percentage
###	  -h		Show this message.

function help() {
    sed -rn 's/^### ?//;T;p' "$0"
}

info_cmd='upower -i /org/freedesktop/UPower/devices/DisplayDevice'

function get_state ()
{
    eval ${info_cmd} | grep "state" | awk -F ":" '{print $2}' | sed  's/ //g'
}

function get_percentage ()
{
    eval ${info_cmd} | grep percentage | awk '{print $2}' | sed 's/%//g'
}

function get_icon_char ()
{
    local s=$(get_state)
    local p=$(get_percentage)
    case $s in
        discharging)
            if [ $p -lt 10 ]; then
                echo ""
            elif [ $p -lt 25 ]; then
                echo ""
            elif [ $p -lt 60 ]; then
                echo ""
            elif [ $p -lt 80 ]; then
                echo ""
            else
                echo ""
            fi
            ;;
        charging)
            if [ $p -lt 20 ]; then
                echo ""
            elif [ $p -lt 40 ]; then
                echo "$"
            elif [ $p -lt 60 ]; then
                echo ""
            elif [ $p -lt 80 ]; then
                echo ""
            elif [ $p -lt 90 ]; then
                echo ""
            else
                echo ""
            fi
            ;;
        *)
            echo "??? $s"
            ;;
    esac
}


while getopts hnficps var; do
    case $var in
        h)
            help
            exit 0
            ;;
        n)
            info=`eval ${info_cmd}`
            notify-send "PowerInfo" "$info"
            exit 0
            ;;
        c)
            get_icon_char
            exit 0
            ;;
        p)
            get_percentage
            exit 0
            ;;
        s)
            get_state
            exit 0
            ;;
        *)
            echo "Wrong usage."
            help
            exit 1
            ;;
    esac
done
shift $(($OPTIND - 1))

get_icon_char
