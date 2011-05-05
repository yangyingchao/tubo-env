#!/usr/bin/env bash
#
# Author: Yang, Ying-chao@gmail.com, 05- 5-2011
#

function usage () {
    echo "Usage: $0 input_file output_file"
    echo "Where:"
    echo "    input_file: full path of file to be converted (extension name included)"
    echo "    output_file: basename of out_putfile, without extension."
}

if [ $# -ne 2 ]; then
    echo "Wrong parameters!"
    usage
    exit 1
fi

input_file=$1
if [ -e $input_file ]; then
    output_file_48=$2"_48.png"
    output_file_64=$2"_64.png"

    echo "Converting $input_file into $output_file_48 and $output_file_64"

    convert -resize 64x64 $input_file $output_file_64 && \
        convert -resize 48x48 $input_file $output_file_48
    if [ $? -ne 0 ]; then
        echo "Failed to convert file: $input_file!"
        exit 1
    else
        echo "Succeeded in converting file."
        exit 0
    fi
else
    echo "File $input_file does not exist!"
    exit 1
fi

