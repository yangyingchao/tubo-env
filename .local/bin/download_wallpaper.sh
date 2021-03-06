#!/bin/bash
#
###
### Download wallpaper from wallhaven.
###
### Usage:
###   download_wallpaper.sh [OPTIONS]  [N]
###
### Options:
###  -n: dry run
###  -q: quiet mode, don't review before saving.
###
###   N: number of files to be downloaded, at least 1.

old_dir=${PWD}

trap "cd ${old_dir}; exit 0" 0 1 2 13 15

source ~/.local/bin/common.sh

NUM=1
DRY_RUN=
QUIET=

help () {
    sed -rn 's/^### ?//;T;p' "$0"
}

download_from_url ()
{
    if [ $# -ne 1 ]; then
        die "Usage: download_from_url url"
    fi

    URL=$1
    FILE_NAME=~/.local/share/wallpapers/`basename $URL`

    cmd="curl -o ${FILE_NAME} ${PIC_URL}"

    echo "CMD: $cmd"

    if [ -n "$DRY_RUN" ]; then
        return 0
    fi

    eval $cmd


    ~/.local/bin/mpi ${FILE_NAME} &
    wait

    while [ 1 ]; do
        echo "Keep it?"
        read ans
        case "${ans}" in
            y|Y)
                echo "File ${FILE_NAME} saved."

                display_name=`swaymsg -r -t get_outputs | jq '. | reverse | to_entries | .[] | select(.value.focused == true) | .value.name'`

                swaymsg output ${display_name} background ${FILE_NAME} fill
                echo "File ${FILE_NAME} saved."
                return 0
                ;;
            n|N)
                rm ${FILE_NAME}
                echo "File ${FILE_NAME} deleted."
                break
                ;;
            *)
                echo "Should answer y or n."
                continue
                ;;
        esac
    done

    return 1
}

download_from_wallhaven ()
{
    i=0

    while [ $i -lt $NUM ]; do
        SEED=`basename $(mktemp -u -t 'XXXXXX')`
        # URL="https://wallhaven.cc/api/v1/search?sorting=random&atleast=2560x1440&seed=${SEED}"
        URL="https://wallhaven.cc/api/v1/search?sorting=random&atleast=2560x1440&seed=${SEED}&q=-anime"
        echo "URL: ${URL}"

        JSON=`curl ${URL}`

        j=0

        # there are 24 entries in each page.
        while [ $j -lt 24 ] && [ $i -lt $NUM ]; do

            PIC=`echo ${JSON}| jq ".data[${j}]"`
            PIC_URL=`echo ${PIC}| jq ".path" | sed 's/"//g'`
            j=$((j+1))


            dim_x=`echo $PIC | jq '.dimension_x'`
            dim_y=`echo $PIC | jq '.dimension_y'`
            if [ ${dim_x} -lt ${dim_y} ]; then
                echo "Skip wallpaper, resotion: ${dim_x}x${dim_y}"
                continue
            fi

            download_from_url ${PIC_URL}

            if [ $? -eq 0 ]; then
                i=$((i+1))
            fi

        done
    done
}





while getopts hqn var; do
    case $var in
        h)
            help
            exit 0
            ;;
        n)
            DRY_RUN=1
            ;;
        q)
            QUIET=1
            ;;
        *)
            echo "Unkown option: $var, showing usage..."
            help
            exit 1
            ;;
    esac
done
shift $(($OPTIND - 1))


if [ $# -gt 1 ]; then
    help
    eixt 0
elif [ $# -eq 1 ]; then
    NUM=$1
fi

cd  ~/.local/share/wallpapers
