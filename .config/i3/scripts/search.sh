#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-26
#

#PUT THIS FILE IN ~/.local/share/rofi/finder.sh
#USE: rofi  -show find -modi find:~/tmp/aaa.sh
if [ ! -z "$@" ]
then
    QUERY=$@
    coproc (google-chrome-stable "? ${QUERY#!}" >/dev/null 2>&1 )
    exec 1>&-
    exit;
fi
