#! /bin/bash

function git_goto_lastest_tag ()
{
    TAG=`git describe --tags`
    if [ $? -eq 0 ]; then
        echo "Goto tag: $TAG"
        git co $TAG
    else
        git reset HEAD --hard
    fi
}

# This function will do several things:
#   1. Get latest code from repository
#   2. Try to set HEAD to the latest tag
#   3. Try to compile..
function git_pull_and_compile ()
{
    echo "Pulling $PWD ..."
    tmpfile=$(mktemp)
    git co master
    echo "Entering master ... "
    git pull | tee $tmpfile
    if [ $? -ne 0 ]; then
        echo "Failed to pull!"
        return $? # This will break "git submodule foreach" loop
    fi

    if [ -e .gitmodules ]; then
        git submodule update --init
    fi

    # git_goto_lastest_tag

    cat $tmpfile | grep "Updating " > /dev/null 2>&1
    if [ $? -eq 0 ]; then
        if [ -e "Makefile" ]; then
            git_try_make
        fi
    fi
    return 0
}

function git_submodule_update ()
{
    tmpfile=$(mktemp)
    git submodule update --init | tee $tmpfile
    TOP=$PWD
    for dir in `cat $tmpfile | grep "\.emacs.d/site-lisp"`; do
        dirn=`echo $dir | awk -F "'" '{print $2}'`
        if [ -d $HOME/$dirn ]; then
            printf "\ncd to $HOME/$dirn\n"
            cd $HOME/$dirn
            if [ -e Makefile ]; then
                make
            else
                printf "Not support for now.\n"
            fi
        fi
    done
}


function git_try_make ()
{
    printf "\nTrying to make: %s\n" `pwd`
    if [ -e Makefile ]; then
        make clean
        make
        make autoloads
        make lisp
        make bytecompile
    else
        printf "\n Byte compile files...\n"
        cat <<EOF >> Makefile 
EMACS = emacs

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch

SRCS = $(wildcard *.el)

OBJS = $(SRCS:.el=.elc)

%.elc: %.el
	${EMACS} $(BATCHFLAGS) -f batch-byte-compile $^

all: $(OBJS)

clean:
	-rm -f $(OBJS)

EOF
        make all
        rm -rf Makefile
    fi

    return 0
}

if [ $# -ne 1 ]; then
    echo <<EOF
Usage: utils su|tm
    su: update submodule and call make for each submodule.
    tm: try to call make command for each submodule
EOF
else
    case $1 in
        su)
            git_pull_and_compile
            ;;
        tm)
            git_try_make
            ;;
        *)
            ;;
    esac
fi

