function gcms ()
{
    NOW=`date +"%H:%M:%S %m/%d/%Y"`
    git commit -a -m "Synced with Server at $NOW"
}

# find files and dump ..
# need to set OBJDUMP & CPPFILT.
function dump_obj ()
{
    if [ -z $OBJDUMP ]; then
        export OBJDUMP=objdump
    fi
    if [ -z $CPPFILT ]; then
        export CPPFILT=c++filt
    fi

    for fn in `find . -name "$1" -print`; do
        fn1="$fn.s"
        printf "Dumping %s to file:%s\n" $fn $fn1
        $OBJDUMP -D -S -l "$fn" | $CPPFILT > "$fn1"
        printf "Done\n\n"
    done

    printf "All work done \n\n"
}

function _icrash_internal ()
{
    _SEP="------------------------------------------------------------------------------"
    printf "\n%s\nShowing log: %s\n%s\n" $_SEP $2 $_SEP
    cmd=`cat <<EOF
(progn
(require 'iPhone_Crash_Log)
(iphone-crash "$1" "$2"))
EOF`

    res=`emacsclient -e "$cmd"`
    echo $res
}

# dump crash logs
function icrash ()
{
    if [ $# -lt 2 ]; then
        echo "Usage: icrash App CrashFile"
        return 1
    fi
    app=$1
    shift 1

    for lg in $*; do
        _icrash_internal $app $lg
    done
}

alias tlogcat="adb logcat -c >/dev/null 2>&1 && adb logcat | tee tlog.TNT"

function show_fd ()
{
    if [ $# -ne 1 ]; then
        cat <<EOF
Usage: show_fd executable name.
EOF
        return 1
    fi

    PID=`pidof -s $1`
    if [ $? -ne 0 ]; then
        cat <<EOF
Can't find PID for $1.
EOF
        return 1
    fi

    TOP=$PWD

    SOCKETS=()
    EPOLLS=()
    FILES=()
    PIPES=()
    UNKNOWN=()
    COUNT=0

    cd /proc/$PID/fd
    for fn in *; do
        type=`file $fn | awk -F "link to " '{print $2}'`
        if [ `expr $type : '^/'` -ne 0  ]; then
            FILES+=($fn)
        elif [ `expr $type : 'socket'` -ne 0 ]; then
            SOCKETS+=($fn)
        elif [ `expr $type : 'pipe'` -ne 0 ]; then
            PIPES+=($fn)
        elif [ `expr $type : 'anon_inode'` -ne 0 ]; then
            EPOLLS+=($fn)
        else
            UNKNOWN+=($fn)
        fi
        COUNT=$(($COUNT+1))
    done

    cd $TOP

    cat <<EOF

Total fds: $COUNT.

  FILES: ${#FILES[@]}: ==> $FILES
  SOCKETS: ${#SOCKETS[@]}: ==> $SOCKETS
  EPOLLS: ${#EPOLLS[@]}: ==> $EPOLLS
  PIPES: ${#PIPES[@]}: ==> $PIPES
  UNKNOWN: ${#UNKNOWN[@]}: ==> $UNKNOWN
EOF
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


# template functions, should be overloaded...
function eject ()
{
    echo "Not implemented."
}

PLATFORM_RC=$(dirname $0)/helper_$(uname -s).sh
if [ -f $PLATFORM_RC ]; then

    source $PLATFORM_RC
else
    echo "File $PLATFORM_RC is missing..."
fi

## vterm settings, execute only when INSIDE_EMACS
if [ -n "${INSIDE_EMACS}" ]; then
    if [ -f ${HOME}/.emacs.d/quelpa/build/vterm/etc/emacs-vterm-zsh.sh ]; then
        source ${HOME}/.emacs.d/quelpa/build/vterm/etc/emacs-vterm-zsh.sh
    fi

    vterm_set_directory() {
        vterm_cmd update-pwd "$PWD/"
    }

    autoload -U add-zsh-hook
    add-zsh-hook -Uz chpwd (){ vterm_set_directory }
fi
