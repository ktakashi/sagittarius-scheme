#!/bin/sh

SASH=${SASH:-"sagittarius"}

# merged from autogen.sh
geninsn()
{
    case `pwd` in
	*src) ;;
	*) cd src ;;
    esac
    $SASH ./geninsn $1
    cd ../
}

precomp()
{
    echo Generating compiled library files
    cd src
    $SASH genlib $1
    echo generating instruction
    geninsn $1
    # done :)
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
    $SASH ./script/r7rs-srfi-gen.scm -p ./ext -p ./sitelib/srfi $1
}

tzdata()
{
    echo Generating TZ database
    $SASH ./script/compile-tzdatabase.scm \
	-o ext/time/sagittarius/tzdata.scm \
	-w ext/time/sagittarius/win-mappings.scm \
	-r $1
}

unicode()
{
    echo Generating Unicode codepoints
    $SASH ./script/compile-unicode.scm $1
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
    
    if [ -e ./build/sagittarius ]; then
	version=`./build/sagittarius -v | perl -e 'while (<>) {if ($_ =~ /version ([\d\.]+)?/) {print $1;}}'`
    else
	echo ERROR: sagittarius does not exist.
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
    
    echo ${version} > latest-version.txt
    echo done!
}

usage()
{
    echo "usage: $0 dist|precomp|stub|stub|srfi|tz|clean"
    echo "    dist:       creates distribution file"
    echo "    gen:        generates all files"
    echo "    precomp:    generates precompiled files"
    echo "    insn:       generates VM instructions"
    echo "    stub:       generates stub files"
    echo "    srfi:       generates R7RS style SRFI libraries"
    echo "    tz:         generates TZ database"
    echo "    unicode:    generates Unicode code points"
    echo "    clean:      cleans generated files"
}


if [ $# -ge 1 ] ; then
    # must be dist
    name=$1
    shift
    case $name in
	dist)    dist $1;;
	stub)    stub $1;;
	gen)     stub $1; precomp $1; srfi; tzdata; unicode;;
	srfi)    srfi $1;;
	tz)      tzdata $1;;
	unicode) unicode $1;;
	precomp) precomp $1;;
	clean)   stub "-c"; precomp "-c"; srfi "-c" ; tzdata "-c";;
	insn)    geninsn $1;;
	*)       usage ;;
    esac
else
    usage
fi
