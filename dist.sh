#!/bin/sh

version=`./build/sash -v | perl -e 'while (<>) {if ($_ =~ /version (\d+?\.\d+?\.\d+?)/) {print $1;}}'`
export DIST_NAME=sagittarius-$version

# just in case
if [ -e ${DIST_NAME} ]; 
then
    rm -rf ${DIST_NAME}
fi

if [ -e src/sagittarius/config.h ]; then
    # this should not be in tar file.
    rm -f src/sagittarius/config.h
fi

# copy to tmp directory
mkdir ${DIST_NAME}
cp -r boot/ ${DIST_NAME}
cp -r src/ ${DIST_NAME}
cp -r lib/ ${DIST_NAME}
cp -r sitelib/ ${DIST_NAME}
cp -r cmake/ ${DIST_NAME}
cp -r script/ ${DIST_NAME}
cp -r test/ ${DIST_NAME}
cp -r unicode/ ${DIST_NAME}
cp -r doc/ ${DIST_NAME}
cp -r ext/ ${DIST_NAME}
cp -r run-test.scm ${DIST_NAME}
cp -r autogen.sh ${DIST_NAME}
cp -r geninsn.sh ${DIST_NAME}
cp -r cmake_uninstall.cmake.in ${DIST_NAME}
cp -r CMakeLists.txt ${DIST_NAME}
cp -r README ${DIST_NAME}
cp -r Copyright ${DIST_NAME}

tar -czvf ${DIST_NAME}.tar.gz ${DIST_NAME}

# clean up
rm -rf ${DIST_NAME}
