#!/bin/bash
#
# Author: Yang,Ying-chao <yangyingchao@gmail.com>, 2017-06-16
#
function eject ()
{
    osascript -e 'tell application "Finder" to eject (every disk whose ejectable is true)'
}
export CC="clang"
export CXX="clang++"


# if [ -d "/usr/local/opt/" ]; then
#     for var in /usr/local/opt/*; do
#         collect_env_from $var
#     done
# fi

# if [ -d /usr/local/lib/ruby/gems ]; then
#     for var in /usr/local/lib/ruby/gems/*; do
#         collect_env_from $var
#     done
# fi


alias ldd="otool -L"
alias dmesg="dmesg | less -r"
