#!/bin/bash
#
###
### Launch application in specified workspace, and switch to that workspace
###
### Usage:
###   lauch-app.sh application [workspace]
###
### Options:
###   application: application to be launched.
###   workspace: workspace in which application will be launched.
###   -h        Show this message.
###   -D        Debug mode, output debug logs.

function help() {
    sed -rn 's/^### ?//;T;p' "$0"
}

DEBUG=

function PDEBUG()
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

while getopts hD var; do
    case $var in
        h)
            help
            exit 0
            ;;
        D)
            DEBUG=1
            ;;
        *)
            ;;
    esac
done
shift $(($OPTIND - 1))

if [ $# -lt 1 ]; then
    printf "Wrong usage, application not specified, showing usage.\n\n"
    help
    exit
fi

APPLICATION=$1

if [ $# -eq 2 ]; then
    swaymsg "workspace $2; exec $1"
fi
