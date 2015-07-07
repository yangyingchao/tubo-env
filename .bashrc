#!/bin/bash
export PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \W \$\[\033[00m\] '
source ~/.zshrc.d/01_env.zsh
source ~/.zshrc.d/02_functions.zsh
if [ -d ~/.bashrc.d/ ]; then
    for fn in `find ~/.bashrc.d/ -name "*.sh"`; do
        source $fn
    done
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# finally, export things...
if [ ${#LOCAL_PATH[@]} -ne 0 ]; then
    for p in $(echo ${LOCAL_PATH[@]}); do
        PATH="$p:$PATH"
    done

    export PATH
fi
