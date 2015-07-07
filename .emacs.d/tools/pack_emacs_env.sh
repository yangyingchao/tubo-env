#!/bin/bash
#
# Author: Yang,Ying-chao <yangyingchao@g-data.com>, 2017-01-09
#
OLD_DIR=${PWD}
PACK_DIR=/tmp

PACK_NAME=emacs_packed
DEST=/tmp/${PACK_NAME}
SCRIPT_DIR=${0%/*}
EMACS_D=${SCRIPT_DIR%/*}

echo "S: ${SCRIPT_DIR}"
echo "D: ${EMACS_D}"

rm -rf ${DEST}
mkdir -p ${DEST}

cd ${EMACS_D}
cp tools/unpack_emacs_env.sh ${DEST}

tar cvf ${DEST}/elpa.tar elpa

cd ~ && git archive --format=tar HEAD > ${DEST}/emacs_git.tar

cd ${DEST}/..

tar cvf ${PACK_DIR}/${PACK_NAME}.tar ${PACK_NAME}
rm -rf $DEST

cd ${OLD_DIR}

echo ""
echo "Final Package: ${PACK_DIR}/${PACK_NAME}.tar"
echo ""

sync

if [ -f ${SCRIPT_DIR}/post-pack_emacs_env.sh ]; then
    ${SCRIPT_DIR}/post-pack_emacs_env.sh ${PACK_DIR}/${PACK_NAME}.tar
fi
