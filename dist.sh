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

    cd tools/scripts
    echo generating builtin keyword
    $SASH builtin-keywords.scm
    echo generating builtin symbol
    $SASH builtin-symbols.scm
    cd ../..
    # done :)
}

stub()
{
    echo Generating library from stub
    cd src
    $SASH -L../sitelib ./genstub $1
    cd ../
}

srfi()
{
    echo Generating R7RS style SRFI libraries
    $SASH ./tools/scripts/r7rs-srfi-gen.scm -p ./ext -p ./sitelib/srfi $1
}

tzdata()
{
    echo Generating TZ database
    $SASH ./tools/scripts/compile-tzdatabase.scm \
	-o ext/time/sagittarius/tzdata.scm \
	-w ext/time/sagittarius/win-mappings.scm \
	-l ext/time/sagittarius/leap-table.scm \
	-r $1
}

unicode()
{
    echo Generating Unicode codepoints
    $SASH ./tools/scripts/compile-unicode.scm $1
    if [ "$1" = "" ]; then
	mkdir -p sitelib/sagittarius/char-sets

	echo 'Generating (sagittarius char-sets grapheme)'
	$SASH ./tools/scripts/extract-unicode-props.scm \
	      -l'(sagittarius char-sets grapheme)' \
	      -o sitelib/sagittarius/char-sets/grapheme.scm \
	      --derived unicode/data/GraphemeBreakProperty.txt \
	      Prepend Control Extend ZWJ Regional_Indicator \
	      spacing-mark=SpacingMark \
	      extend-or-spacing-mark=Extend,SpacingMark  \
	      hangul-l=:L \
	      hangul-v=:V \
	      hangul-t=:T \
	      hangul-lv=:LV \
	      hangul-lvt=:LVT

	echo 'Generating (sagittarius char-sets word)'
	$SASH ./tools/scripts/extract-unicode-props.scm \
	      -l'(sagittarius char-sets word)' \
	      -o sitelib/sagittarius/char-sets/word.scm \
	      --derived unicode/data/WordBreakProperty.txt \
	      Newline Extend ZWJ Regional_Indicator Format \
	      Numeric Katakana Hebrew_Letter \
	      a-letter=ALetter mid-num-let=MidNumLet \
	      mid-letter=MidLetter \
	      mid-num=MidNum \
	      extend-num-let=ExtendNumLet \
	      w-seg-space=WSegSpace

	echo 'Generating (sagittarius char-sets emojis)'
	$SASH ./tools/scripts/extract-unicode-props.scm \
	      -l'(sagittarius char-sets emojis)' \
	      -o sitelib/sagittarius/char-sets/emojis.scm \
	      --derived unicode/data/emoji-data.txt \
	      Emoji Emoji_Presentation Emoji_Modifier \
	      Emoji_Modifier_Base Emoji_Component \
	      Extended_Pictographic

	echo 'Generating (sagittarius char-sets incb)'
	$SASH ./tools/scripts/extract-unicode-props.scm \
	      -l '(sagittarius char-sets incb)' \
	      -o  sitelib/sagittarius/char-sets/incb.scm \
	      --derived unicode/data/DerivedCoreProperties.txt \
	      'InCB.Linker' \
	      'InCB.Consonant' \
	      'InCB.Extend'

	echo 'Generating grapheme-data.scm'
	$SASH ./tools/scripts/unicode-break-test-generator.scm \
	      -o test/tests/text/unicode/grapheme-data.scm \
	      unicode/data/GraphemeBreakTest.txt
	
    fi
}

html()
{
    echo Generating HTML entities
    $SASH ./tools/scripts/html-entities.scm -o sitelib/text/xml/entities-list.scm $1
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
	version=$(./build/sagittarius -e '(display (sagittarius-version))(exit)')
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
# use rsync to exclude hidden or CI files
# I hope this command exists any where...
    rsync -r --exclude='.hg*' --exclude='*.git*' --exclude='.ci*' \
	  ${SRC_DIR}/* ${DIST_NAME}
    
    tar --format ustar -czvf ${DIST_NAME}.tar.gz ${DIST_NAME}
    
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
    echo "    html:       generates HTML entities"
    echo "    clean:      cleans generated files"
}


if [ $# -ge 1 ] ; then
    # must be dist
    name=$1
    shift
    case $name in
	dist)    dist $1;;
	stub)    stub $1;;
	gen)     stub $1; precomp $1; srfi; tzdata; unicode; html;; 
	srfi)    srfi $1;;
	tz)      tzdata $1;;
	unicode) unicode $1;;
	html)    html $1;;
	precomp) precomp $1;;
	clean)   
	    stub "-c"; precomp "-c"; srfi "-c" ; 
	    tzdata "-c"; unicode "-c"; html "-c";;
	insn)    geninsn $1;;
	*)       usage ;;
    esac
else
    usage
fi
