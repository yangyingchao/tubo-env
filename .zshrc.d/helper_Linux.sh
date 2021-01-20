#!/bin/bash
#
# Author: Yang,Ying-chao <yangyingchao@g-data.com>, 2018-01-19
#

which mimeopen >/dev/null 2>&1 open_cmd="mimeopen -n" ||  open_cmd="xdg-open"

function open ()
{
    ${open_cmd} $@ &
}


function do_dmesg ()
{
    local ver=`/bin/dmesg --version  | awk -F "." '{print $2}'`
    if [ ${ver} -ge 35 ]; then
        /bin/dmesg -T --color=always $@
    else
        /bin/dmesg -T --color $@
    fi
}

alias dmesg=do_dmesg
alias ll="llo --group-directories-first"
alias eupdate="sudo su -c 'emerge --sync && layman -S && emerge  --changed-use -uNDav world system'"
alias less="less -R"

export WINEPREFIX=~/.wine32
export WINEARCH=win32


LINUXBREW_BIN=/home/linuxbrew/.linuxbrew/bin
LINUXBREW_BAK=/tmp/env_${USER}.sh
function brew_activate ()
{
    local brew_file=${LINUXBREW_BIN}/brew
    [ -f ${brew_file} ] || die "Directory ${brew_file} does not exist."

    env | awk -F "=" '{print "export " $1 "=\"" $2"\""}' > ${LINUXBREW_BAK}
    eval `${brew_file} shellenv`
    export HOMEBREW_FORCE_BREWED_CURL=1
    echo "linuxbrew activated..."
}

function brew_deactivate ()
{
    brew --help >/dev/null
    if [ $? -ne 0 ]; then
        echo "brew is not activated.."
        return 0
    fi

    if [ -f ${LINUXBREW_BAK} ]; then
        source ${LINUXBREW_BAK}
        echo "linuxbrew activated..."
    else
        echo "linuxbrew not activated by me..."
    fi
}

function brew_link ()
{
    local src=
    local tgt=
    local promote=

    if [ $# -eq 1 ]; then
        src=${LINUXBREW_BIN}/$1
        tgt=${HOME}/.local/bin/$1
    elif [ $# -eq 2 ]; then
        src=${LINUXBREW_BIN}/$1
        tgt=$2

        [[ $tgt =~ ^/usr/.* ]] && promote=1
    else
        die "usage: linuxbrew_link src [tgt]"
    fi

    if [ ! -f ${src} ]; then
        # might be hidden inside python cellar...
        src=`dirname $(realpath /home/linuxbrew/.linuxbrew/bin/python3)`/$1
    fi

    if [ ! -f ${src} ]; then
        echo "Failed to find executable $1 ($src --> $tgt)..."
        return 1
    fi


    echo "Creating link: ${src} --> ${tgt}..."
    if [ -z ${promote} ]; then
        ln -sf $src $tgt
    else
        sudo ln -sf $src $tgt
    fi
    echo "done..."
}

function brew_unlink ()
(
    if [ $# -ne 1 ]; then
        echo "Usage: linuxbrew_unlink file"
        return 1
    fi


    # usage: brew_unlink_in_dir file
    brew_unlink_in_dir ()
    {
        [ ! -e $1 ] && return 0

        local fname=$1
        local bname=`basename $1`
        local real_src=`realpath ${LINUXBREW_BIN}/${bname}`
        local real_tgt=`realpath ${fname}`

        if [ "${real_src}" = "$real_tgt" ]; then
            echo "Unlink file: ${fname}..."
            sudo rm $fname
            echo "done..."
            return 0
        else
            echo "Will not delete target: ${tgt}, real path (${real_tgt}) does not point to linuxbrew."
            return 1
        fi
    }

    brew_unlink_in_dir ${HOME}/.local/bin/$1
    brew_unlink_in_dir /usr/local/bin/$1
)
