#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-26
#

source ${HOME}/.local/bin/common.sh

scriptdir=${0%/*}

# URL_BASE="https://www.dogedoge.com/results?q="
URL_BASE="https://www.google.com/search?q="

export FONT_SIZE=18
Q=$(echo -e ""| ${scriptdir}/bemenu-run.sh -p "Search: " -l 4)

if [ -n "${Q}" ]; then
    query=$(echo $Q | sed 's/ /+/g')
    google-chrome "${URL_BASE}${query}"
    ${scriptdir}/workspace_util.sh find "\"Google-chrome\""
fi
