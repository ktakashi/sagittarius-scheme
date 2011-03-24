#!/bin/sh


do_compile()
{
    echo Generating c file from compiler.scm
    cd boot
    ../script/gen-compiler.scm compiler.scm sagittarius > compiler-sg.scm
    gosh -I. vm.scm compiler-sg.scm c "(sagittarius compiler)" ../src/compiler.c
    cd ../
}

do_library()
{
    echo Generating compiled library files
    cd boot
    echo '(display "dummy")' > dummy.scm
    gosh -I. vm.scm dummy.scm lc ../src
    rm dummy.scm
    cd ../
}

do_stub()
{
    echo Generating library from stub
    cd src
    Ypsilon --sitelib=../sitelib ./genstub
    cd ../
}


param="$@"

if [ $param != "" ] 
then
    for buf in $param
    do
	case $buf in
	    compiler)
		do_compile
		;;
	    library)
		do_library
		;;
	    stub)
		do_stub
		;;
	    *)
		echo "usage: $0 compiler|library|stub"
		echo "    compiler: generate compiler.c"
		echo "    library:  generate compiled libraries"
		echo "    stub:     generate stub files"
		echo "  These can be combined and if you did not specify this script"
		echo "  runs every thing"
		;;
	esac
    done
else
    echo "generate all."
    do_compile
    do_library
    do_stub
fi
