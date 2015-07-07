#!/usr/bin/env sh

## Add this to your wm startup file.

# Terminate already running bar instances
pkill -9 polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar > /dev/null; do sleep 0.5; done

for m in $(polybar --list-monitors | cut -d":" -f1); do
    polybar --reload $m  -c ~/.config/polybar/config &
done

wait
