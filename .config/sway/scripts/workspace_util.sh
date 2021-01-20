#!/bin/bash
#
# Author: Yang,Ying-chao <yangyingchao@gmail.com>, 2018-03-22
#

###
### Brief introduction here.
###
### Usage:
###   workspace_util.sh  ACTION [ARGS]
###
### Options:
###   action:
###          go:   go to specified workspace, possible args:
###                right | left | first | last
###          new:  create new workspace
###          find: find workspace containing ARG and switch to that workspace.
###   -h	 Show this message.

source ${HOME}/.local/bin/common.sh

function snd_msg ()
{
    PDEBUG "$@"
    eval "swaymsg -q $@"
    if [ -n "$debug" ]; then
        dunstify -t 600 "$@"
    fi
}

declare -a wks # all available workspaces.
cur_slot=0

function workspace_parse ()
{
    workspaces=`swaymsg -r -t get_workspaces |   tr "\n" " " | sed -E "s/ +//g"`
    cur=`echo $workspaces | tr } "\n" | grep '"focused":true' | tr , "\n" |grep num | awk -F ":" '{print $2}'`
    slot=0

    for wk in `echo $workspaces  | tr , "\n" |grep num | awk -F ":" '{print $2}'`; do
        PDEBUG "Workspace: $wk"

        wks[$slot]=$wk

        if [ $wk -eq $cur ]; then
            cur_slot=$slot
        fi

        slot=$((slot+1))
    done
    total_slots=$((slot-1))
    PDEBUG "First: $first, Last: $last, Cur: $cur, ALL: ${wks[@]}"
}

function worksace_go ()
{
    workspace_parse

    case "$1" in
        right)
            next=$((cur_slot+1))
            PDEBUG  "NEXT: ${next}, ALL${wks[*]}"

            if [ $next -eq ${#wks[@]} ]; then
                next=0
                PDEBUG "Wrap to 0: ${wks[$next]}"
            fi
            snd_msg "workspace ${wks[$next]}"
            ;;
        left)
            next=$((cur_slot-1))
            snd_msg "workspace ${wks[$next]}"
            ;;
        first)
            snd_msg "workspace ${wks[0]}"
            ;;
        last)
            snd_msg "workspace ${wks[-1]}"
            ;;
        *)
            die "unknown arg: $2"
            ;;
    esac
}

function workspace_new ()
{
    workspace_parse

    next=0
    for i in `seq 1 9`; do
        for j in `seq 0 ${total_slots}`; do
            exist=0
            if [ $i -eq ${wks[$j]}  ]; then
                exist=1
                break
            fi
        done

        if [ $exist -eq 0 ]; then
            next=$i
            break
        fi
    done
    snd_msg "workspace $next"
}

function workspace_find_app ()
{
    if [ $# -ne 1 ]; then
        die "Usage: workspace_find_app app_name"
    fi
    tree=`swaymsg -r -t get_tree`
    outputs=$(echo $tree | jq '.nodes')
    length=$(echo $outputs | jq 'length')

    PDEBUG "L: $length"

    target_workspace=0

    i=0
    while [ $i -lt $length ]; do
        PDEBUG "i: $i"
        output=`echo $outputs | jq ".[$i]"`
        id=`echo $output | jq '.name'`
        if [ $id = '"__i3"' ]; then
            i=$((i+1))
            continue
        fi
        PDEBUG "id $id"

        workspaces=`echo $output | jq '.nodes'`
        num=`echo $workspaces | jq 'length'`
        j=0
        while [ $j -lt $num ]; do
            wk=`echo $workspaces | jq ".[$j]"`
            apps=`echo $wk | jq '.nodes'`
            n_apps=`echo $apps | jq 'length'`
            i_apps=0
            while [ ${i_apps} -lt ${n_apps} ]; do
                app_id=`echo ${apps} | jq ".[${i_apps}]" | jq '.app_id'`
                if [[ "${app_id}" =~ $1 ]]; then
                    PDEBUG "switching: $((j+1))"
                    snd_msg workspace $((j+1))
                    PDEBUG "done: $((j+1))"
                    exit 0
                fi

                i_apps=$((i_apps+1))
            done

            j=$((j+1))
        done

        i=$((i+1))
    done
}


function help() {
    sed -rn 's/^### ?//;T;p' "$0"
}


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


case $1 in
    go)
        worksace_go $2
        ;;
    new)
        workspace_new
        ;;
    find)
        workspace_find_app $2
        ;;
    *)
        help
        eixt 1
    ;;
esac
