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
        /bin/dmesg -T --color=always | less -r
    else
        /bin/dmesg -T --color | less -r
    fi
}

alias dmesg=do_dmesg
alias ll="ls -lAh --group-directories-first"
