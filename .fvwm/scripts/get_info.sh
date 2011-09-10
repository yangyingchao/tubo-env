#!/bin/bash
#
# Author: Yang, Ying-chao@gmail.com, 09-10-2011
#

which acpi > /dev/null 2>&1

if [ $? -ne 0 ]; then
    echo "Unknown"
else
    acpi -a | grep "on-line" > /dev/null 2>&1
    if [ $? -eq 0 ]; then
        echo "AC"
    else
        echo "DC"
        acpi -b
    fi
fi

