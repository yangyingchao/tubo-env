#!/bin/bash
#

function setup_idle ()
{
    ps -u $USER -o command | grep '[s]wayidle' && return 0

    swayidle -w \
             timeout 300 '~/.config/sway/scripts/swaylock.sh' \
             timeout 600 'swaymsg "output * dpms off"' \
             resume 'swaymsg "output * dpms on"' \
             before-sleep '~/.config/sway/scripts/swaylock.sh'
}


function exec_lock ()
{
    ps -u $USER -o command | grep -v idle | grep '[s]waylock'  && return 0

    ~/.config/sway/scripts/swaylock.sh
}

if [ $# -ne 0 ]; then
    case "$1" in
        init)
            setup_idle
            ;;
        lock)
            exec_lock;;
        *)
        ;;
    esac
fi
