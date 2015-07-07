#!/bin/bash
#
###
### Parse and select hosts to ssh.
###
### Usage:
###   wofi_ssh
###
### Options:
###   -h        Show this message.

while getopts h var; do
    case $var in
        h)
            help
            exit 0
            ;;
        *)
            ;;
    esac
done
shift $(($OPTIND - 1))

hosts=`cat ${HOME}/.ssh/config | grep "Host " | awk '{print $2}' | sort -h`
hosts2=`cat ${HOME}/.ssh/known_hosts |  awk '{print $1}' |grep -v ":"| awk -F ',' '{print $1}'| sort -h`

target=`echo $hosts $hosts2 | tr  ' ' '\n' |sort -h  |  ${HOME}/.config/sway/scripts/bemenu-run.sh -l 60 -p "SSH:"`

if [ -n "$target" ]; then
    foot ssh ${target}
fi
