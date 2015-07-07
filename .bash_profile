#!/bin/bash
[ -e /etc/profile.d/autojump.bash ] && . /etc/profile.d/autojump.bash

function try_load ()
{
    for fn in $*; do
        [ -e $fn ] && . $fn
    done
}
source ~/.bashrc

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

