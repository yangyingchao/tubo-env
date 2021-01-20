#!/usr/bin/env bash

source ~/.local/bin/common.sh

if [ -z "${FONT_SIZE}" ]; then
    FONT_SIZE=12
fi

BEMENU_ARGS=(
    -n -i -p ''
    -m $(swaymsg -r -t get_outputs | jq '. | reverse | to_entries | .[] | select(.value.focused == true) | .key'))

if [ -f /tmp/dark-theme ]; then
    BEMENU_ARGS+=(
        --tb "#1a1a1a"
        # --tf "#268bd2"
        --fb "#2E3440"
        --nb "#191919"
        --hb "#1a1a1a"
        --hf "#268bd2"
    )
else
    BEMENU_ARGS+=(
        --tb "#E5EFF9"
        --nf "#252525"
        --fb "#c0c0c0"
        --ff "#282b35"
        --nb "#f5f5f5"
        --hb "#3584E4"
        --hf "#FFFFFF"
    )

fi


BEMENU_ARGS+=(
    --fn "WenQuanYi Micro Hei ${FONT_SIZE}"
    "$@")

if [ "$1" = 'dmenu' ]; then
    bemenu-run "${BEMENU_ARGS[@]}"
else
  bemenu "${BEMENU_ARGS[@]}"
fi
