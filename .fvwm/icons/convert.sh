#!/usr/bin/env bash
batch_convert()
{
    cd 48x48
    for fn in $(ls *); do
        echo $fn;
        convert -resize 24x24 $fn ../24x24/$fn;
    done
}

if [ $# == 0 ]; then
    echo "Staring batch convert!"
    batch_convert
else
    for fn in $@; do
        echo "Resizing into 48x48: "$fn
        ff=`basename $fn`
        convert -resize 48x48 $fn 48x48/$ff
    done
    echo "Calling batch convert!"
    batch_convert
fi
