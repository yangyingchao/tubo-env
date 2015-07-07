#!/bin/bash
#

dev=`swaymsg -r -t get_outputs | jq '. | reverse | to_entries | .[] | select(.value.focused == true) | .value.name'`

file=`cat /tmp/CURRENT_WALLPAPERS | jq ". | to_entries | .[] | select(.value.dev == ${dev} ) |.value.file" |\
  sed 's/"//g'`

if [ -f ${file} ]; then
    rm -rf ${file}
fi

# download and set up.
~/.local/bin/download_wallpaper.sh
