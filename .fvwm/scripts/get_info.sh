#!/bin/bash
#
# Author: Yang, Ying-chao@gmail.com, 09-10-2011
#

export LANG=C
str1=$(date "+%b %e, %a,%R")
which acpi > /dev/null 2>&1

if [ $? -ne 0 ]; then
    str2="Unknown"
else
    acpi -a | grep "on-line" > /dev/null 2>&1
    if [ $? -eq 0 ]; then
        str2="AC"
    else
        str2=$(acpi -b | awk -F ", " '{print $2}')
        str2="DC, $str2"
    fi
fi

echo "$str1, $str2"

