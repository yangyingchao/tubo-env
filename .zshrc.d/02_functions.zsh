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
FZF_PREVIEW_OPTS='bat --color "always" {} || head -n 500 {}'
function fzp ()
{
    which fzf >/dev/null || die "fzf not installed..."
    fzf --preview  ${FZF_PREVIEW_OPTS}
}

function my_fzf ()
{
    if [ $# -eq 0 ]; then
        fzf --preview  ${FZF_PREVIEW_OPTS}
    elif [ $# -eq 1 ]; then
        find  . -name $1 | fzf --preview  ${FZF_PREVIEW_OPTS}
    else
        find  . $@ | fzf --preview  ${FZF_PREVIEW_OPTS}
    fi
}



# fzf + editor
function open_after_fzf ()
{
    [ $# -ge 1 ] || die "usage: edit_fzf editor [args]"
    which $1 >/dev/null 2>&1 || die "$1 not found."

    local app=$1
    shift 1
    file=`my_fzf $@`

    if [ -n "$file" ]; then
        $app $file
    fi
}

# fzf + mpv
function fzm ()
{
    open_after_fzf mpv $@
}

function eef ()
{
    open_after_fzf emacs_edit $@
}

function vif ()
{
    open_after_fzf vi $@
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

# function to call valgrind and show output...
function tval ()
{
    if [ $# -lt 1 ]; then
        cat <<EOF
Usage: tval EXECUTABLE [args]
EOF
        return 0
    fi

    app=`basename $1`
    tmpfile=$(mktemp --suffix=".log" valgrind_"$app"_XXXXXXXX)
    echo "Will write to file: $tmpfile"

    valgrind  --leak-check=full --undef-value-errors=no \
              --log-fd=1 --log-file=$tmpfile "$@" &

    tail -f $tmpfile
}

function tperf-record()
{
    if [ -e perf.data ]; then
        sudo mv perf.data "perf_`date +'%m_%d_%H:%M:%S'`.data"
    fi

    sudo perf record \
         -e cycles,instructions,branch-misses,cache-misses \
         $*
}


#mkdir and cd
function mcd ()
{
    mkdir $1 && cd $1
}


function svnedit ()
{
    if [ $# -lt 2 ]; then
        echo "Usage: svnedit revision URL"
        return
    fi

    svn propedit -r $1 --revprop svn:log $2
}


function o_dump_addr ()
{
    local exe=
    local before=0
    local after=

    while getopts e:a:b var; do
        case $var in
            e)
                exe="$OPTARG"
                if [ ! -f $exe ]; then
                    printf "file $exe does not exist\n"
                    return 2
                fi
                ;;
            a)
                after="$OPTARG"
                ;;
            b)
                before="$OPTARG"
                ;;
            ?)
            printf "Usage: o_dump_addr -e binary addr[addr...]\n"
            return 2
            ;;
        esac
    done
    shift $(($OPTIND - 1))

    if [ $# -eq 0 ]; then
        printf "\nMissing addresses, showing help:\n"
        printf "Usage: o_dump_addr -e binary addr[addr...]\n"
        return 2
    fi

    for addr in $*; do
        if [ $(expr $addr : "0x[0-9a-fA-F]\+$") -eq 0 ]; then
            if [ $(expr $addr : "[0-9a-fA-F]\+$") -eq 0 ]; then
                printf "Warning: %s is not valid address, skipped...\n" $addr
                continue
            else
                addr="0x$addr"
            fi
        fi

        start_address=$addr
        new_addr=$(($addr - $before))
        if [ $new_addr -gt 0 ]; then
            start_address=$new_addr
        fi

        if [ -z $after ]; then
            objdump -d -S --start-address=$start_address $exe | awk '{print $0} $3~/retq?/{exit}'
        else
            if [ $after -eq 0 ]; then
                after=1
            fi
            end_address=$(($addr + $after))
            objdump -d -S --start-address=$start_address --stop-address=$end_address $exe | \
                grep -v 'elf64-'
        fi
    done
}

function o_dump_exe ()
{
    if [ $# -ne 1 ]; then
        die "usage: o_dump_exe exe"
    fi

    objdump -j .text -d $1 | \
        sed -r "s/^[[:blank:]]+[[:xdigit:]]+:[[:blank:]][ a-z0-9]+[[:blank:]]/\t/g" | \
        sed -r "s/[[:xdigit:]]+[[:blank:]]+</</g" | \
        sed -r "s/0x[[:xdigit:]]+(\(%.*?)[[:blank:]]+# (<.*?>)/\2\1/g" |\
        sed -r "s/0x[[:xdigit:]]+/VAL/g" |\
        > $1.s
    echo "Dumpped to: $1.s"
}

function g_dump_symbol ()
{
    if [ $# -lt 2 ]; then
        printf "Usage: g_dump_symbol binary symbol[symbol...]\n"
        return
    fi

    gdb --version > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        printf "g_dump_symbol requires gdb which is not available...\n"
        return
    fi

    exe=$1
    if [ ! -f $exe ]; then
        printf "file $exe does not exist\n"
        return
    fi

    shift

    for symbol in $*; do
        gdb -batch -ex "file $exe" -ex "disassemble $symbol" | sed -r "s/0x0+/0x/g"
    done
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

# ssh related.
function get_ssh_target ()
{
    local target=$1
    local pattern='^[0-9.]+$'
    if [[ $target =~ $pattern ]]; then
        if [ -z $(expr $1 : "\(.*\?\..*\?\..*\?\.\)") ]; then # not xx.xx.xx.xx
            if [ ! -z $(expr $1 : "\(.*\?\..*\?.*\?\.\)") ]; then # xx.xx.xx, wtf? not support.
                echo "WTF"
            else
                if [ ! -z $(expr $1 : "\(.*\?\..*\?\)") ]; then # xx.xx, add 192.168
                    target="192.168.$1"
                else #
                    target="192.168.103.$1"
                fi
            fi
        fi
    fi
    echo $target
}


function calc_port ()
{
    local port=`echo $1 |  awk 'BEGIN { FS = "." }
{
     total=0
     for (i = 1; i <= NF; i++) {
         total+= $i * ( 2 ^ (14 - i))
     }
    print total
}'`
    port=$((port+$$+$RANDOM))
    port=$((port%65536))
    if [ $port -lt 2000 ]; then
        port=$((port+2000))
    fi
    echo $port
}

function ash ()
{
    if [ $# -ne 2 ]; then
        cat <<EOF
Usage: ash USER HOST
EOF
        return 1
    fi
    local port_next=`calc_port $2`
    local step=$(($$%5+2))
    while [ 1 ] ; do
        port=$port_next
        echo "Connecting with command: autossh -M $port $1@$2"
        autossh -M $port $1@$2
        if [ $? -ne 0 ]; then
            port_next=$((port+step))
            echo "Failed to connect to $1@$2 with port: $port, try next port $port_next"
            sleep 0.2
        else
            break
        fi
    done
}

function osh ()
{
    echo "Install autossh and set SSH to ash for better experiences..."

    if [ $# -ne 2 ]; then
        cat <<EOF
Usage: osh USER HOST
EOF
        return 1
    fi

    ssh -l $1 $2
}

SSH=osh

which autossh > /dev/null 2>&1
if [ $? -eq 0 ]; then
    SSH=ash
fi

function ysh ()
{
    $SSH  yyc $(get_ssh_target $1)
}

function rsh ()
{
    $SSH  root $(get_ssh_target $1)
}

function msh ()
{
    $SSH  ${USER} $(get_ssh_target $1)
}

# USAGE: $SSH user host
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


function cp_usb ()
{
    PACK_DIR=
    case `uname -s` in
        Darwin)
            PACK_DIR=`mount | grep "/Volumes/" | awk -F " " '{print $3}'`
            ;;
        *)
            PACK_DIR=`mount | grep "/run/media" | awk -F " " '{print $3}'`
            ;;
    esac

    if [ -z ${PACK_DIR} ]; then
        echo "No usb disk detected"
        return 1
    fi

    cp $* ${PACK_DIR}

    printf "\nFlushing data...\n"
    sync
    echo "\nDone\n\n"
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
        tgt=/usr/local/bin/$1
        promote=1
    elif [ $# -eq 2 ]; then
        src=${LINUXBREW_BIN}/$1
        tgt=$2
        die "not implement"
    else
        die "usage: linuxbrew_link src [tgt]"
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
{
    if [ $# -ne 1 ]; then
        die "Usage: linuxbrew_unlink file"
    fi

    local tgt=/usr/local/bin/$1

    if [ -f ${tgt}  ]; then
        local real_src=`realpath ${LINUXBREW_BIN}/$1`
        local real_tgt=`realpath ${tgt}`

        if [ "${real_src}" = "$real_tgt" ]; then
            echo "Unlink file: ${tgt}..."
            sudo rm $tgt
            echo "done..."
        else
            die "Will not delete target: ${tgt}, real path (${real_tgt}) does not point to linuxbrew."
        fi
    else
        die "File ${tgt} does not exist."
    fi
}
