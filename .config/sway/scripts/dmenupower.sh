#!/usr/bin/env bash

## Shows a drop down menu with power options
export FONT_SIZE=18
action=`echo -e " Logout\n⏾ Sleep\n Shutdown\n Reboot\n Lock\n" | \
   ${HOME}/.config/sway/scripts/bemenu-run.sh -l 5 -p "Power:"`

case "$action" in
    *Lock)
        ~/.config/sway/scripts/setup-idle.sh lock
        ;;
    *Logout)
        swaymsg exit
        ;;
    *Sleep)
        systemctl suspend
        ;;
    *Shutdown)
        systemctl poweroff
        ;;
    *Reboot)
        systemctl reboot
        ;;
esac
