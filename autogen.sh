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

do_builtin()
{
    echo Generating c file from compiler.scm
    cd boot
    gosh -I. vm.scm ./lib/match_core.scm c "(match core)" ../src/lib_match_core.c
    cd ../
}


do_stub()
{
    echo Generating library from stub
    cd src
    #Ypsilon --sitelib=../sitelib ./genstub
    sash ./genstub
    cd ../
}

show_usage()
{
    echo "usage: $0 compiler|library|stub|all"
    echo "    compiler: generate compiler.c"
    echo "    library:  generate compiled libraries"
    echo "    builtin:  generate compiled builtin libraries"
    echo "    stub:     generate stub files"
    echo "    all:      generate do everything above"
    echo "  These can be combined and if you did not specify this script"
    echo "  runs every thing"
}

param="$@"

if [ "$param" != "" ] 
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
	    builtin)
		do_builtin
		;;
	    stub)
		do_stub
		;;
	    all)
		echo "generate all."
		do_compile
		do_library
		do_stub
		;;
	    *)
		show_usage
		;;
	esac
    done
else
    show_usage
fi

