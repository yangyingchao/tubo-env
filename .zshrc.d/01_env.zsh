export HISTSIZE=1024
export SAVEHIST=10000
export HISTFILE=~/.zhistory
export WORDCHARS='*?_-.[]~&;!#$%^(){}<>'
export EIX_LIMIT=0

declare -a LOCAL_PATH
declare -a LOCAL_PKG_PATH
declare -a LOCAL_LIB_PATH
function collect_env_from ()
{
    # skip if directory not exists...
    [ -d $1 ] || return 0

    if [ -d "$1/bin" ]; then
        LOCAL_PATH+=("$1/bin")
    fi

    if [ -d "$1/sbin" ]; then
        LOCAL_PATH+=("$1/sbin")
    fi

    if [ -d "$1/lib" ]; then
        LOCAL_LIB_PATH+=("$1/lib")
    fi

    if [ -d "$1/lib64" ]; then
        LOCAL_LIB_PATH+=("$1/lib64")
    fi

    if [ -d "$1/lib/pkgconfig" ]; then
        LOCAL_PKG_PATH+=("$1/lib/pkgconfig")
    fi
}

export PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH"

collect_env_from "${HOME}/.local"
collect_env_from "${HOME}/.npm"
collect_env_from "${HOME}/.cargo"

if [ -d "${HOME}/.local/texlive/2020/bin/x86_64-linux/" ]; then
    LOCAL_PATH+=("${HOME}/.local/texlive/2020/bin/x86_64-linux")
fi

if [ -d "/usr/local/texlive/2020/bin/x86_64-darwin" ]; then
    LOCAL_PATH+=("/usr/local/texlive/2020/bin/x86_64-darwin")
fi

# Utilities.
alias ll="ls -lah"
# alias rm="rm -i"
# alias cp="cp -i"
alias l="ls -ail"
alias cl="clear"

which dircolors >> /dev/null && alias ls="ls --color" || alias ls="ls -G"
alias gst="git status"
alias gpush="git push"
alias gpull="git pull"
alias gco="git checkout"
alias gcm="git commit -a -m \"auto\""
alias gcmp="git commit -a -m \"auto\" && git push"
alias gcs="git commit -a -m \"updated submodules\""
alias gcus="git commit -a -m \"Updated submodules \""

alias kxcode="killall -9 Xcode"
alias reboot="shutdown -r now"

# do a du -hs on each dir on current path
alias lsdir="for dir in *;do;if [ -d \$dir ];then;du -hsL \$dir;fi;done"

alias rcp="rsync -a -P --exclude='.ccls-cache' --exclude='.ccls_cached/'  --exclude='cmake_build_*'"
which dcfldd > /dev/null 2>&1 && alias dd="dcfldd"

alias tma="tmux attach || tmux"
alias ttop="top -u $UID"

# make alias if source file exists (source > target)
function alias_if_exists ()
{
    if [ $# -lt 2 ]; then
        echo "Usage: alias_if_exists target source [options]"
        return 1
    fi

    target=$1
    shift

    which $1 >/dev/null 2>&1 && eval alias $target=\"$*\"
}

alias_if_exists top htop
alias_if_exists ping prettyping --nolegend
alias_if_exists vi vim


if [ -d ${HOME}/.local/lib64/python2.7/site-packages ]; then
    export PYTHONPATH="{HOME}/.local/lib64/python2.7/site-packages:$PYTHONPATH"
fi

# Home brew...
export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles
export HOMEBREW_NO_AUTO_UPDATE=1
alias brew_clean_all="[ -e Brewfile ] && rm Brewfile; brew bundle dump && brew bundle --force cleanup"

# rustup

export RUSTUP_DIST_SERVER=https://mirrors.tuna.tsinghua.edu.cn/rustup
export ASAN_OPTIONS="detect_leaks=0:detect_odr_violation=0"
