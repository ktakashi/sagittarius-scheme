#!/bin/sh

SASH=${SASH:-"sash"}

# merged from autogen.sh
precomp()
{
    echo Generating compiled library files
    cd src
    $SASH genlib $1
    echo generating instruction
    $SASH ./geninsn $1
    # done :)
    cd ../
}

stub()
{
    echo Generating library from stub
    cd src
    $SASH ./genstub $1
    cd ../
}

srfi()
{
    echo Generating R7RS style SRFI libraries
    # in case
    $SASH ./script/r7rs-srfi-gen.scm -p ./ext -p ./sitelib/srfi $1
}

dist() {
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
	version=`./build/sash -v | perl -e 'while (<>) {if ($_ =~ /version ([\d\.]+)?/) {print $1;}}'`
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
}

usage()
{
    echo "usage: $0 dist|precomp|stub|stub|clean"
    echo "    dist:       create distribution file"
    echo "    gen:        generate all files"
    echo "    precomp:    generate precompiled files"
    echo "    stub:       generate stub files"
    echo "    srfi:       generate R7RS style SRFI libraries"
    echo "    clean:      clean generated files"
}


if [ $# -ge 1 ] ; then
    # must be dist
    name=$1
    shift
    case $name in
	dist)    dist $1;;
	stub)    stub $1;;
	gen)     stub $1; precomp $1; srfi;;
	srfi)    srfi $1;;
	precomp) precomp $1;;
	clean)   stub "-c"; precomp "-c"; srfi "-c" ;;
	*)       usage ;;
    esac
else
    usage
fi
