#!/bin/bash
###
### Wrapper of macism to make it behave like fcitx-remote
###
### Usage:
###   macism_wraper.sh [option]
###
### Options:
###   -o        Switch to English.
###   -c        Switch to Chinese.
###   -h        Show this message.
###
###  Without option, check for current status.
###

function help() {
    gsed -rn 's/^### ?//;T;p' "$0"
}



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

which macism >/dev/null || die "Failed to find macism"

IM_ABC="com.apple.keylayout.ABC"
IM_SOGOU="com.sogou.inputmethod.sogou.pinyin"

function check_status ()
{
    cur=`macism`

    if [ "$cur" = "$IM_SOGOU" ]; then
        echo "2"
    else
        echo "1"
    fi

    exit 0
}

function set_chinese ()
{
    macism ${IM_SOGOU} &&  exit 0
}

function set_english ()
{
    macism ${IM_ABC} &&   exit 0
}


######### main function begins ########
while getopts hoc var; do
    case $var in
        h)
            help
            exit 0
            ;;
        o)
            set_english
            ;;
        c)
            set_chinese
            ;;
        *)
            ;;
    esac
done
shift $(($OPTIND - 1))

if [ $# -eq 0 ]; then
    check_status
else
    die "Wrong number of argument: $@"
fi
