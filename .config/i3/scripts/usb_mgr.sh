#!/bin/sh

source ${HOME}/.config/i3/scripts/common.sh

umount_device()
{
    n_devices=$(($(df -h |  grep "run/media"| wc -l) - 1))
    selected_row=$(df -h | grep "run/media"| rofi -dmenu -p "Select the device to umount" -lines $n_devices)
    device=$(echo "$selected_row" | awk '{print $1}')

    if [ -n "$device" ]; then
        udisksctl unmount -b ${device}
        dunstify "usb-rofi" "Device umounted, now you can remove it safely" -i "${icondir}/usb_icon.png"
    fi
}

mount_device()
{
    devices=$(lsblk -Jplno NAME,TYPE,RM,SIZE,MOUNTPOINT,VENDOR)
    output=""
    counter=0

    echo "$devices" | jq -r '.blockdevices[] | select(.type == "part") | select(.rm == true) | select(.mountpoint == null) | .name'
    echo "$devices"
    for unmounted in $(echo "$devices" | jq -r '.blockdevices[] | select(.type == "part") | select(.rm == true) | select(.mountpoint == null) | .name'); do
        echo "${unmounted}"
        unmounted=$(echo "$unmounted" | tr -d "[:digit:]")
        unmounted=$(echo "$devices" | jq -r '.blockdevices[] | select(.name == "'"$unmounted"'") | .vendor')
        unmounted=$(echo "$unmounted" | tr -d ' ')

        echo "${unmounted}"

        if [ $counter -eq 0 ]; then
            space=""
        else
            space="   "
        fi
        counter=$((counter + 1))

        output="$output$space#1 $unmounted"
    done

    rofi_cmd="rofi -dmenu -p \"Device '$2 $1' detected. Select the action\" -lines $n_devices"

    echo "O: $output"

    # udiskctl mountxxx
}

export DISPLAY=:0

while getopts 'mu' op ; do
    case $op in
        m)
            mount_device
            ;;
        u) umount_device
           ;;
        *) ;;
    esac
done

exit 0
