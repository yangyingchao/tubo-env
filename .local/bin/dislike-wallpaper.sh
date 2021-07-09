#!/bin/bash
#

dev=`swaymsg -r -t get_outputs | jq '. | reverse | to_entries | .[] | select(.value.focused == true) | .value.name'`

file=`cat /tmp/CURRENT_WALLPAPERS | jq ". | to_entries | .[] | select(.value.dev == ${dev} ) |.value.file" |\
  sed 's/"//g'`

echo "CURRENT:"
cat /tmp/CURRENT_WALLPAPERS

echo ""
echo "DEV: ${dev}"

echo ""
echo "Going to delete file: ${file}, continue?"
read ans

case "${ans}" in
  y|Y)
    if [ -f ${file} ]; then
      rm -rf ${file}
      echo "File ${file} deleted."
    else
      echo "File ${file} does not exist."
    fi
  ;;
  *)
    echo "File ${file} is not deleted"
  ;;
esac

echo ""
echo "Download new wallpaper from wallhaven?"


read ans

case "${ans}" in
  y|Y)
      ~/.local/bin/download_wallpaper.sh
  ;;
  *)
    echo "Setting wallpaper randomly..."
    ~/.config/sway/scripts/setup-display.sh Wallpaper
    echo "done"
  ;;
esac
