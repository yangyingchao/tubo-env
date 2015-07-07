#!/bin/bash
#
# Author: Yang,Ying-chao <yangyingchao@g-data.com>, 2017-01-09
#

DIR=${PWD}

do_unpack() {
    cd ${HOME}
    rm -rf ${HOME}/.emacs.d/elpa
    tar xvf ${DIR}/elpa.tar && mv elpa ${HOME}/.emacs.d/

    tar xvf ${DIR}/emacs_git.tar

    # Fix permissions of .ssh directory.
    chmod 700 .ssh
    chmod 600 .ssh/*
}

if [ -z $HOME ]; then
    echo "Can't find HOME Directory...."
else
    do_unpack
fi
