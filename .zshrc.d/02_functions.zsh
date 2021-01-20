## Following settings should be shared by both bash and zsh.
#environement variables

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

    if [[ "$-" =~ i ]]; then
        return 1
    else
        exit 1
    fi
}

alias ee=run-emacs
alias eet="run-emacs -t"
alias mkdir='mkdir -pv'
alias ssh='TERM=xterm-256color ssh'

### following functions requires fzf to be installed..

# wrapper of fzf.
export FZF_DEFAULT_OPTS='--height 60% --border'
which bat > /dev/null 2>&1
if [ $? -eq 0 ]; then
    export BAT_PAGER=""
    FZF_PREVIEW_OPTS='bat --color "always" {} || head -n 500 {}'
    alias cat='bat --color "always" -pp '
else
    FZF_PREVIEW_OPTS='cat {} || head -n 500 {}'
fi

function fzp ()
{
    which fzf >/dev/null || die "fzf not installed..."
    fzf --preview  ${FZF_PREVIEW_OPTS}
}

# fzf + editor
function open_after_fzf ()
{
    if [ $# -gt 1 ]; then
        echo "usage: edit_fzf editor"
        return 1
    fi

    which $1 >/dev/null 2>&1 ||
    if [ $? -ne 0 ]; then
        echo "$1 not found."
        return 1
    fi

    local app=$1

    file=`fzp`

    if [ -n "$file" ]; then
        eval "$app $file"
    fi
}

function eef ()
{
    open_after_fzf run-emacs
}

function vif ()
{
    open_after_fzf vi
}

alias fze=eef
alias fzv=vif

# fzf + ssh

function fzf-ssh () {
    local selected_host=$(
        command /bin/cat <(/bin/cat ~/.ssh/config /etc/ssh/ssh_config 2> /dev/null | \
                               command grep -i '^host ' | \
                               command grep -v '[*?]' | \
                               awk '{for (i = 2; i <= NF; i++) print $1 " " $i}') \
                <(command grep -oE '^[[a-z0-9.,:-]+' ~/.ssh/known_hosts | tr ',' '\n' | \
                      tr -d '[' | \awk '{ print $1 " " $1 }') \
                <(command grep -v '^\s*\(#\|$\)' /etc/hosts | \
                      command grep -Fv '0.0.0.0') | \
            awk '{if (length($2) > 0) {print $2}}' | sort -u | fzf --query "$LBUFFER" --prompt="SSH Remote > ")

    if [ -n "$selected_host" ]; then
        BUFFER="ssh ${selected_host}"
        zle accept-line
    fi
    zle reset-prompt
}

# git related functions.
function gtop ()
{
    dir=`git rev-parse --show-toplevel`
    cd $dir
}

function kill_all ()
{
    if [ $# -lt 1 ]; then
        echo "Usage: kill_all app [apps]"
        return 1
    fi

    fn=`mktemp -u -t kill_all_XXX.sh`
    for app in $*; do
        cat <<EOF > $fn
#!/bin/bash
ps aux | grep '$app' | grep -v grep | awk -F " " '{print \$2}' | xargs kill -9 {} \;
EOF
        bash $fn
    done
    rm -rf $fn
}


alias up="~/.local/bin/unpackfile"
alias rget="wget -c -r -np -k -L -p"
alias wget="wget -c"

function eget ()
{
    __get_item() {
        wget -c "https://mirrors.tuna.tsinghua.edu.cn/gentoo/distfiles/$1" || \
        wget -c "http://mirrors.163.com/gentoo/distfiles/$1"
    }

    local failed_list=
    for item in $*; do
        ok=
        __get_item $item
        if [ $? -eq 0 ]; then
            ok=1
        else
            for suffix in "tar.gz" "tar.bz2" "tar.xz" "tgz" "zip"; do
                $fn=
                __get_item "$item.$suffix"
                if [ $? -eq 0 ]; then
                    ok=1
                    break
                fi
            done
        fi

        if [ -z $ok ]; then
            failed_list="$failed_list $item"
        fi
    done

    sync

    # output failed files...
    if [ ! -z "${failed_list}" ]; then
        echo "Failed to download some files:$failed_list"
    fi

}

# Load autojump profile.
if [ -n "${BASH}" ]; then
    shell="bash"
elif [ -n "${ZSH_NAME}" ]; then
    shell="zsh"
elif [ -n "${__fish_datadir}" ]; then
    shell="fish"
elif [ -n "${version}" ]; then
    shell="tcsh"
else
    shell=$(echo ${SHELL} | awk -F/ '{ print $NF }')
fi

function load_shell_extentions ()
{
    local dir=$1
    local pattern=$2

    if [ -d ${dir} ]; then
        for sh in `find ${dir} -maxdepth 1 -name "${pattern}"`; do
	        [ -r "$sh" ] && source $sh
        done
    fi
}

# prevent circular loop for sh shells
if [ "${shell}" = "sh" ]; then
    echo "skip" > /dev/null
else
    # special handling for autojump
    load_shell_extentions ~/.autojump/share/autojump/ "*.${shell}"
    load_shell_extentions /usr/share/autojump/ "*.${shell}"
fi


load_shell_extentions /usr/local/etc/profile.d/ "*.sh"
load_shell_extentions /usr/local/etc/profile.d/ "*.${shell}"
load_shell_extentions /etc/profile.d/ "*.sh"
load_shell_extentions /etc/profile.d/ "*.${shell}"
load_shell_extentions ${HOME} "*.${shell}"

alias ppid="ps -o ppid= -p"

function ppidof ()
{
    proc=$1
    pids=`pidof $proc`
    if [ -z "$pids" ]; then
        echo "Can't find process, make sure $1 is running..."
        return
    fi

    NF=`echo "$pids" | awk -F " " '{print NF}'`
    if [ $NF -gt 1 ]; then
        echo "Multiple process found for $proc, choose one:"
        echo "$pids"
        read pid
    else
        pid=$pids
    fi

    if [ -z $pid ]; then
        echo "Empty pid..."
        return
    fi

    ps -o ppid= -p $pid
}

# Similar to update_env_from, but export vars immediately.
function update_env_from ()
{
    local target=`realpath $1`

    # skip if directory not exists...
    [ -d $target ] || return 0

    [ -d "$target/usr" ] && update_env_from $target/usr

    local exported=()

    local vars=()
    [ -d "$target/bin" ] && vars+=("$target/bin")
    [ -d "$target/sbin" ] && vars+=("$target/sbin")

    if [ ${#vars[@]} -ne 0 ]; then
        for p in $(echo ${vars[@]}); do
            PATH="$p:$PATH"
        done

        export PATH
        exported+=("PATH")
    fi

    vars=()
    [ -d "$target/lib" ] && vars+=("$target/lib")
    [ -d "$target/lib64" ] && vars+=("$target/lib64")

    if [ ${#vars[@]} -ne 0 ]; then
        for p in $(echo ${vars[@]}); do
            LD_LIBRARY_PATH="$p:$LD_LIBRARY_PATH"
        done
        export LD_LIBRARY_PATH
        exported+=("LD_LIBRARY_PATH")
    fi

    if [ -d "$target/lib/pkgconfig" ]; then
        export PKG_CONFIG_PATH="$target/lib/pkgconfig:${PKG_CONFIG_PATH}"
        exported+=("PKG_CONFIG_PATH")
    fi


    if [ ${#exported[@]} -ne 0 ]; then
        echo "Updated envs: ${exported[*]}..."
    else
        echo "Nothing was found in $target ..."
    fi
}

alias uef="update_env_from"


setup_tunnel ()
{
    if [ $# -lt 2 ]; then
        echo "Usage: setup_tunnel PORT USER@HOST"
        return 1
    fi

    echo "Setting up tunnel..."
    ssh -N -f -D $@
    echo "done..."
}
