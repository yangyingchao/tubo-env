#!/bin/bash
#
###
### Brief introduction here.
###
### Usage:
###   start-sway.sh <input> <output>
###
### Options:
###   -h        show help.
###   -d        debug mode, logs will be redirected to ~/tmp/sway.log


export WLR_DRM_NO_MODIFIERS=1

# Fixes issues on jetbrains ides
export _JAVA_AWT_WM_NONREPARENTING=1

# Force firefox into wayland and enable hw video decoding (ff 75+)
export MOZ_ENABLE_WAYLAND=1
export MOZ_WAYLAND_USE_VAAPI=1

# QT apps theme
export QT_QPA_PLATFORMTHEME=qt5ct

export CLUTTER_BACKEND=wayland
export QT_QPA_PLATFORM=wayland
export SDL_VIDEODRIVER=wayland
export GDK_BACKEND=wayland
export XDG_CURRENT_DESKTOP=Unity


export XIM=fcitx
export XIM_PROGRAM=fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
export SDL_IM_MODULE=fcitx

function help() {
    sed -rn 's/^### ?//;T;p' "$0"
}

CMD=("sway")

while getopts dh var; do
    case $var in
        h)
            help
            exit 0
            ;;
        d)
            CMD+=("-d")
            ;;
        *)
            ;;
    esac
done
shift $(($OPTIND - 1))

if [ -f ~/tmp/sway.log ]; then
    rm -rf ~/tmp/sway.old.log
    mv ~/tmp/sway.log ~/tmp/sway.old.log
fi

CMD+=(">~/tmp/sway.log"
      " 2>&1")

eval "${CMD[@]}"
