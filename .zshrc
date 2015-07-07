# colors
[ -e /etc/zsh/zprofile ] && source /etc/zsh/zprofile
[ -e /opt/etc/zsh/zprofile ] && source /opt/etc/zsh/zprofile

which dircolors >> /dev/null && eval `dircolors $HOME/.zshrc.d/colors`

bindkey -e

autoload -U zutil

# Resource files
for file in $HOME/.zshrc.d/*.zsh; do
	source $file
done


# finally, export things...
if [ ${#LOCAL_PATH[@]} -ne 0 ]; then
    for p in $(echo ${LOCAL_PATH[@]}); do
        PATH="$p:$PATH"
    done

    export PATH
fi

if [ ${#LOCAL_LIB_PATH[@]} -ne 0 ]; then
    for p in $(echo ${LOCAL_LIB_PATH[@]}); do
        LD_LIBRARY_PATH="$p:$LD_LIBRARY_PATH"
    done
    export LD_LIBRARY_PATH
fi

if [ ${#LOCAL_PKG_PATH[@]} -ne 0 ]; then
    for p in $(echo ${LOCAL_PKG_PATH[@]}); do
        PKG_CONFIG_PATH="$p:$PKG_CONFIG_PATH"
    done

    export PKG_CONFIG_PATH
fi

# Emacs tramp fix
if [[ "$TERM" == "dumb" ]]; then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='$ '
else
    if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
        ~/.config/sway/scripts/start-sway.sh
        exit
    fi
fi
