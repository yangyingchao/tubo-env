#!/bin/bash
#

# export LANG=zh_CN
export XIM="fcitx5"
export GTK_IM_MODULE="fcitx5"
export XMODIFIERS="@im=fcitx5"
export QT_IM_MODULE="fcitx5"

export WINEPREFIX="/home/yyc/.wine32"
export WINEARCH="win32"

if [[ $1 =~ wine.* ]]; then
    eval "$@"
else
    wine $@
fi
