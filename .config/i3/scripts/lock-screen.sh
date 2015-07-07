#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-29
#


source ${HOME}/.config/i3/scripts/common.sh


# hue=(-level "0%,100%,0.6")
# effect=(-filter Gaussian -resize 20% -define "filter:sigma=1.5" -resize 500.5%)

image=/tmp/i3lock_bg.png
hue=(-level "0%,100%,0.6")
effect=(-filter Gaussian -resize 20% -define "filter:sigma=1.5" -resize 500.5%)
i3lock_cmd=(i3lock -i "$image")
shot_custom=false

# l10n support
text="Type password to unlock"

function prepare-image ()
{
    rm -rf /tmp/${image}

    which scrot >/dev/null 2>&1 &&  shot=(scrot) || shot=(import -window root)

    shot+=($image)

    local content=`cat /tmp/CURRENT_WALLPAPERS 2>/dev/null`
    if [ `echo $content | awk '{print NF}'` -eq 1 ]; then
        new_shot=(convert)

        local mode=`cat /tmp/CURRENT_DISPLAY_MODE 2>/dev/null`
        case `echo "$mode" | tr '[:upper:]' '[:lower:]'` in
            internal)
                new_shot+=(-resize ${INTERNAL_RES}!)
            ;;
            external)
                new_shot+=(-resize ${EXTERNAL_RES}!)
                ;;
            *)
            ;;
        esac

        new_shot+=($content $image)
    fi

    if [ -n "$new_shot" ]; then
        command -- "${new_shot[@]}"
        if [ $? -ne 0 ]; then
            command -- "${shot[@]}"
        fi
    else
        command -- "${shot[@]}"
    fi

    value="60" #brightness value to compare to

    color=$(convert "$image" -gravity center -crop 100x100+0+0 +repage -colorspace hsb \
                    -resize 1x1 txt:- | awk -F '[%$]' 'NR==2{gsub(",",""); printf "%.0f\n", $(NF-1)}');

    if [[ $color -gt $value ]]; then #white background image and black text
        bw="black"
        icon="${HOME}/.config/i3/scripts/icons/lockdark.png"
        PARAMS_I3=("--insidecolor=0000001c"  "--ringcolor=0000003e"
                   "--linecolor=00000000" "--keyhlcolor=ffffff80" "--ringvercolor=ffffff00"
                   "--separatorcolor=22222260" "--insidevercolor=ffffff1c"
                   "--ringwrongcolor=ffffff55" "--insidewrongcolor=ffffff1c"
                   "--verifcolor=ffffff00" "--wrongcolor=ff000000" "--timecolor=ffffff00"
                   "--datecolor=ffffff00" "--layoutcolor=ffffff00")
    else #black
        bw="white"
        icon="${HOME}/.config/i3/scripts/icons/lock.png"
        PARAMS_I3=("--insidecolor=ffffff1c" "--ringcolor=ffffff3e"
                   "--linecolor=ffffff00" "--keyhlcolor=00000080" "--ringvercolor=00000000"
                   "--separatorcolor=22222260" "--insidevercolor=0000001c"
                   "--ringwrongcolor=00000055" "--insidewrongcolor=0000001c"
                   "--verifcolor=00000000" "--wrongcolor=ff000000" "--timecolor=00000000"
                   "--datecolor=00000000" "--layoutcolor=00000000")
    fi

    DISPLAY_RE="([0-9]+)x([0-9]+)\\+([0-9]+)\\+([0-9]+)"
    IMAGE_RE="([0-9]+)x([0-9]+)"

    PARAMS_CONV=("$image" "${hue[@]}" "${effect[@]}" -pointsize 26 -fill "$bw")

    PIXELATE=false
    # Read user input
    POSITIONAL=()


    set -- "${POSITIONAL[@]}" # restore positional parameters

    #Get dimensions of the lock image:
    icon_IMAGE_INFO=`identify $icon`
    [[ $icon_IMAGE_INFO =~ $IMAGE_RE ]]
    IMAGE_WIDTH=${BASH_REMATCH[1]}
    IMAGE_HEIGHT=${BASH_REMATCH[2]}

    #Execute xrandr to get information about the monitors:
    while read LINE
    do
        #If we are reading the line that contains the position information:
        if [[ $LINE =~ $DISPLAY_RE ]]; then
            #Extract information and append some parameters to the ones that will be given to ImageMagick:
            WIDTH=${BASH_REMATCH[1]}
            HEIGHT=${BASH_REMATCH[2]}
            X=${BASH_REMATCH[3]}
            Y=${BASH_REMATCH[4]}
            POS_X=$(($X+$WIDTH/2-$IMAGE_WIDTH/2))
            POS_Y=$(($Y+$HEIGHT/2-$IMAGE_HEIGHT/2))

            TEXT_X=$(($X+$WIDTH/2-140))
            TEXT_Y=$(($Y+$HEIGHT/2+160))

            PARAMS_CONV+=($icon -geometry +$POS_X+$POS_Y -composite
                          -annotate +$TEXT_X+$TEXT_Y \"$text\")
        fi
    done <<<"`xrandr | grep connected`"

    PARAMS_CONV+=($image)

    time eval convert ${PARAMS_CONV[@]}

    rm -rf /tmp/lock_screen.sh
    cat <<EOF >/tmp/lock_screen.sh
#! /bin/bash
    ${i3lock_cmd[@]} ${PARAMS_I3[@]} >/dev/null 2>&1 || ${i3lock_cmd[@]}
EOF
}

function exec_lock ()
{
    [ -f $image ] && [ -f /tmp/lock_screen.sh ] || prepare-image
    # try to use i3lock with prepared parameters
    bash /tmp/lock_screen.sh
}


if [ $# -ne 0 ] && [ "$1" = "prepare" ]; then
    prepare-image
else
    ps -u $USER -o command | grep '[i]3lock' | grep -v "xss-lock" || exec_lock &
fi
