#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-19
#
DEBUG=

scriptdir=${0%/*}
icondir=${scriptdir}/icons
term=urxvt

function die ()
{
    set +xe
    echo ""
    echo "================================ DIE ==============================="
    echo "$@"
    echo "Call stack:"
    local n=$((${#BASH_LINENO[@]}-1))
    local i=0
    while [ $i -lt $n ]; do
        local line=${BASH_LINENO[i]}
        local func=${FUNCNAME[i+1]}

        i=$((i+1))

        echo "    [$i] -- line $line -- $func"
    done
    echo "================================ END ==============================="
    exit 1
}

function edebug ()
{
    if [ -n "$DEBUG" ]; then
        echo "$@"
    fi
}

which xrandr >/dev/null 2>&1
if [ $? -eq 0 ] && [ -n "${DISPLAY}" ] ; then
    DISPLAY_RES_RE="([0-9]+x[0-9]+)\\+([0-9]+)\\+([0-9]+)"

    INTERNAL_INFO=`xrandr | grep -e " connected primary*\+"`
    INTERNAL_DEV=`echo $INTERNAL_INFO | awk '{print $1}'`
    INTERNAL_RES=

    if [[ $INTERNAL_INFO =~ $DISPLAY_RES_RE ]]; then
        INTERNAL_RES=${BASH_REMATCH[1]}
    fi

    EXTERNAL_INFO=`xrandr | grep -sw 'connected' | grep -v "primary"`
    EXTERNAL_DEV=`echo $EXTERNAL_INFO | awk '{print $1}'`
    EXTERNAL_RES=
    if [[ "$EXTERNAL_INFO" =~ $DISPLAY_RES_RE ]]; then
        EXTERNAL_RES=${BASH_REMATCH[1]}
    fi
fi

function get_sway_socket ()
{
    for item in `ls -1 /var/run/user/${UID}/sway-ipc.${UID}.*.sock`; do
        echo $item
        return 0
    done

    return 1
}

function is_sway_running ()
{
    get_sway_socket > /dev/null 2>&1
    return $?
}

is_sway_running && term=alacritty
