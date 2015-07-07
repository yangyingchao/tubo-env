#!/bin/bash
#
# Author: Yang, Ying-chao@gmail.com, 2016-02-26
# should be called like: nohup /home/yyc/.git_scripts/utils/update_tags.sh >/dev/null 2>&1 &
#

marker=/tmp/$(basename `git rev-parse --show-toplevel`).pid

# call gtags if argument is provided, or global if not.
function update_tag ()
{
    local pid
    if [ $# -ne 0 ]; then
        gtags -v &
        pid=$!
        echo $pid > $marker
        wait $pid
    else
        global -uv &
        pid=$!
        echo $pid > $marker
        wait $pid
        if [ $? -ne 0 ]; then
            gtags -v &
            pid=$!
            wait $pid
        fi
    fi
}

if [ -f $marker ]; then
    pid=`cat $marker`
    echo "Killing process $pid"
    kill -9 $pid
fi

echo $$ > $marker
update_tag $*
rm -rf $marker
