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
