#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-26
#

source ${HOME}/.local/bin/common.sh

scriptdir=${0%/*}
icondir=${scriptdir}/icons

URL_BASE="https://www.dogedoge.com/results?q="
 # "https://www.google.com/search?q=${query}"

export FONT_SIZE=18
Q=$(echo -e ""| ${scriptdir}/bemenu-run.sh -p "GOOGLE: " -l 4)

if [ -n "${Q}" ]; then
    query=$(echo $Q | sed 's/ /+/g')
    xdg-open "${URL}${query}"
    ${scriptdir}/workspace_util.sh find 'Google-chrome'
fi
