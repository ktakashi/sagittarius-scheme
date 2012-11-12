#!/bin/sh

if [ $# -ne 1 ]; then
    echo Usage: $0 src-dir
    exit -1
fi

SRC_DIR=$1

# check current directory
PWD=`pwd`
if [ ${PWD} = ${SRC_DIR} ]; then
    echo $0 must be run with out-of-tree build
    exit -2
fi

if [ -e ./build/sash ]; then
    version=`./build/sash -v | perl -e 'while (<>) {if ($_ =~ /version (\d+?\.\d+?\.\d+?)/) {print $1;}}'`
else
    echo ERROR: sash does not exist.
    exit -3
fi

DIST_NAME=sagittarius-${version}

# remove tar file if exists
if [ -e ${DIST_NAME} ]; then
    rm -rf ${DIST_NAME}
fi

# copy to tmp directory
mkdir ${DIST_NAME}
# use rsync to exclude .hg stuff
# hopefully this command exists any where...
rsync -r --exclude='.hg*' ${SRC_DIR}/* ${DIST_NAME}

tar -czvf ${DIST_NAME}.tar.gz ${DIST_NAME}

# clean up
rm -rf ${DIST_NAME}

echo done!