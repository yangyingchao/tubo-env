#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-19
#
DEBUG=
BEMENU="${HOME}/.config/sway/scripts/bemenu-run.sh"

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

function PDEBUG ()
{
    if [ -n "$DEBUG" ]; then
        local n=$((${#BASH_LINENO[@]}-1))
        local i=0
        local line=${BASH_LINENO[i]}
        local func=${FUNCNAME[i+1]}
        ts=`date "+%H:%M:%S:%N" | awk -F ":" '{print $1":"$2":"$3"."%4/1000}'`
        echo "DEBUG: $ts: $0:$line ($func) -- $@"
    fi
}

function executable-find ()
{
    [ $# -eq 1 ] || die "Usage: executable-find program"

    which $1 >/dev/null 2>&1
    r=$?

    PDEBUG "Find $1 returns $r"

    return $r
}
