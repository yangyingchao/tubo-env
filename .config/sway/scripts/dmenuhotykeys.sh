#!/usr/bin/env bash

## Shows all Sway hotkeys

swaymsg "$(sed -E 's/^bindsym\s*(\S*)\s*(\S.*$)/\1\\\2/;t;d' "$HOME/.config/sway/sway.d/05_hotkeys.conf" | column -s'\' -t | ${HOME}/.config/sway/scripts/bemenu-run.sh -l 30 | cut -d' ' -f2- | xargs -0)"
