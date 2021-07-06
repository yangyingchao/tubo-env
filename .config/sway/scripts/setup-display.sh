#!/bin/bash
#
###
### Brief introduction here.
###
### Usage:
###   setup-display.sh [mode]
###
### Options:
###   <mode>    Mode, could be init, Extend, Mirror, Internal, External
###   -h        Show this message.
###   -D        Debug mode, output debug logs.

function help() {
    sed -rn 's/^### ?//;T;p' "$0"
}
source ${HOME}/.local/bin/common.sh


while getopts Dh var; do
    case $var in
        h)
            help
            exit 0
            ;;
        D)
            DEBUG=1
            ;;
        *)
            ;;
    esac
done
shift $(($OPTIND - 1))

[ $UID -eq 0 ] && die "Don't call it as root..."

action=$1

outputs=`swaymsg -r -t get_outputs`
primary_dev="eDP-1"
secondary_dev=`echo ${outputs} | jq '. [] | select (.name != "eDP-1") | .name'`
active_dev=`echo ${outputs} | jq '. | reverse | to_entries | .[] | select(.value.focused == true)|.value.name'| sed 's/"//g'`

image_dir=${HOME}/.local/share/wallpapers
wallpapers=/tmp/CURRENT_WALLPAPERS

function exec_swaymsg ()
{
    PDEBUG "COMMAND: $@"
    swaymsg  $@
}

function pick_wallpapers ()
{
    rm -rf ${wallpapers}

    local num_outputs=`echo ${outputs} | jq 'length'`

    images=`ls ${image_dir}/* | sort -R | tail -n ${num_outputs}`
    array=($images)

    PDEBUG "Pick wallpapers for ${num_outputs} outputs..."

    A=("[")
    i_output=0
    while [ ${i_output} -lt ${num_outputs} ]; do
        dev=`echo ${outputs} | jq ".[${i_output}] | .name"`
        file="${array[${i_output}]}"
        PDEBUG "    [$i_output]: $dev --> $file"

        if [ $i_output -ne 0 ]; then
            A+=(",")
        fi

        A+=(`echo "{}" | jq ".dev=${dev} | .file=\"$file\""`)
        i_output=$((i_output+1))
    done

    A+=("]")
    echo "${A[@]}" | jq '.' > ${wallpapers}
}


# returns 0 if dark-theme should be used, or 1 if bright theme should be used.
update-theme ()
{
    local value=0
    local active_paper=$1

    # PDEBUG "FILE: ${active_paper}"

    # which convert >/dev/null 2>&1
    # if [ $? -eq 0 ]; then
    #     # calculate theme for waybar


    #     local width=`identify $1 | awk  '{print $3}'`

    #     value=$(convert "${active_paper}"  -crop ${width}x100+0+0 +repage -colorspace hsb -resize 1x1 txt:- | awk -F '[%$]' 'NR==2{gsub(",",""); printf "%.0f\n", $(NF-1)}');
    # fi

    # PDEBUG "FILE: ${active_paper}, WIDTH: ${width}, VALUE: ${value}"

    if [ $value -le 50 ]; then
        touch /tmp/dark-theme
        waybar_color=dark
    else
        rm -rf /tmp/dark-theme
        waybar_color=bright
    fi

    ln -sf ~/.config/waybar/color-${waybar_color}.css ~/.config/waybar/color.css
    killall -SIGUSR2 waybar || waybar &
}

# pass the property as the first argument
mpv_communicate() {
  printf '{ "command": ["get_property", "%s"] }\n' "$1" | socat - "/tmp/mpvsocket" | jq -r ".data"
}

background_from_mpv ()
{
    local file=$(mpv_communicate "path")

    if [ ! -f ${file} ]; then
        file=$(mpv_communicate "working-directory")/${file}
    fi

    [ $file ] || return 1

    exec_swaymsg output "${active_dev}" background  "${file}" fill
    echo "FILE: ${file}"
    update-theme ${file}

    return 0
}

function setup_wallpapaer ()
{
    PDEBUG "enter"
    background_from_mpv && return 0

    if [ ! -f ${wallpapers} ]; then
        pick_wallpapers
    fi

    while [ 1 ]; do
        papers=`cat ${wallpapers} | jq '.'`
        n_papers=`echo ${papers} | jq 'length'`


        i_papers=0
        reset= # reset, pickup wallpapers again.

        while [ ${i_papers} -lt ${n_papers} ]; do
            dev=`echo ${papers} | jq ".[${i_papers}] | .dev" | sed 's/"//g'`
            file=`echo ${papers} | jq ".[${i_papers}] | .file"  | sed 's/"//g'`

            if [ -f ${file} ]; then
                exec_swaymsg output "$dev" background  "${file}" fill
            else
                PDEBUG "File ${file} is missing, pick wallpapers again..."
                reset=1
                break
            fi

            i_papers=$((i_papers+1))
        done

        if [ -n "${reset}" ]; then
            pick_wallpapers
        else
            break
        fi
    done

    active_paper=`cat /tmp/CURRENT_WALLPAPERS| jq ". |.[]|select (.dev==\"${active_dev}\")|.file"|sed 's/"//g'`

    update-theme ${active_paper}
}


N=`exec_swaymsg -t get_outputs | grep name | wc -l`

if [ -z "$action" ]; then
    export FONT_SIZE=18
    MENU="$(echo -e " Extend\n Mirror\n Internal\n External\n Wallpaper\n" | ${HOME}/.config/sway/scripts/bemenu-run.sh -l 5 -p "DISPLAY:")"
elif [ "$action" = "init" ]; then

    # special handling for PHL 2K display
    swaymsg -t get_outputs | grep -q "PHL"
    if [ $? -eq 0 ]; then
        exec_swaymsg output ${secondary_dev} scale 1.2
    fi

    # called during i3 startup, set MENU based on LID status.
    LID_STATUS=/proc/acpi/button/lid/LID/state
    if [ -f ${LID_STATUS} ]; then
        case `/bin/cat ${LID_STATUS} | awk '{print $2}'` in
            closed)
                MENU=External
                ;;
            *)
                # check if internal monitor is disabled.
                exec_swaymsg -p -t get_outputs  | grep Output | grep eDP |grep -q inactive
                if [ $? -eq 0 ]; then
                    MENU=External
                else
                    MENU=Extend
                fi
                ;;
        esac
    else
        MENU=Extend
    fi
else
    MENU="$action"
fi

PDEBUG "$1 --> MENU: $MENU"

case "$MENU" in
    *Extend)
        exec_swaymsg output ${primary_dev} enable
        exec_swaymsg output ${secondary_dev} enable

        exec_swaymsg output ${primary_dev} pos 0 0
        exec_swaymsg output ${secondary_dev} pos 1366 0
        ;;
    *Mirror)
        exec_swaymsg output ${primary_dev} enable
        exec_swaymsg output ${secondary_dev} enable

        exec_swaymsg output  ${primary_dev} pos 0 0
        exec_swaymsg output  ${secondary_dev} pos 0 0
        ;;
    *Internal)
        exec_swaymsg output ${secondary_dev} disable
        exec_swaymsg output ${primary_dev} enable
        ;;
    *External)
        exec_swaymsg output ${secondary_dev} enable
        exec_swaymsg output ${primary_dev} disable
        ;;
    *Wallpaper)
        rm -rf /tmp/CURRENT_WALLPAPERS
        ;;
    *)
        exit 0
        ;;
esac

setup_wallpapaer
