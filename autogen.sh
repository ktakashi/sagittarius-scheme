#!/bin/sh

# need to use gosh?
# export SCM="gosh -I." ./autogen.sh
if [ x"$SCM" == x"" ]; then
    export SCM=sash
fi

# force flag for genstub
force=""

do_compile()
{
    echo Generating c file from compiler.scm
    cd boot
    $SCM ../script/gen-compiler.scm compiler.scm sagittarius sagittarius.scheme.vm > compiler-sg.scm
    $SCM vm.scm compiler-sg.scm c "(sagittarius compiler)" ../src/compiler.c
    cd ../
}

do_library()
{
    echo Generating compiled library files
    cd boot
    echo '(display "dummy")' > dummy.scm
    $SCM vm.scm dummy.scm lc ../src
    rm dummy.scm
    cd ../
#    cd src
    # for now we add the library
    # enable it after 0.4.11
#    $SCM -L../sitelib genlib -f ../boot/lib/errors.scm ../boot/lib/arith.scm \
#	../boot/lib/enums.scm 
}

do_precomp()
{
    echo Generating compiled library files
    cd src
    # for now we add the library
    # enable it after 0.4.11
    $SCM -L../sitelib genlib -n -f ../boot/lib/scmlib.scm \
  	-i "null" -i "(sagittarius)" -l "(core base)"
    $SCM -L../sitelib genlib -n -f ../boot/lib/macro.scm \
 	-l "(core syntax-case)" \
 	-i "null" -i "(core base)" -i "(core errors)" -i "(sagittarius)" \
 	-i "(for (smatch) expand)" \
 	-i "(sagittarius vm)" \
 	-a ../boot/lib/smatch.scm
    $SCM -L../sitelib genlib -n -f ../boot/compiler-aux.scm \
	-l "(sagittarius compiler util)" \
 	-i "null" -i "(sagittarius)" \
	-i "(for (core syntax-case) expand)" -i "(for (smatch) expand)" \
	-i "(core errors)" \
	-a ../boot/lib/smatch.scm
    $SCM -L../sitelib genlib -n -f ../boot/compiler.scm \
	-l "(sagittarius compiler)" \
 	-i "null" -i "(core base)" -i "(sagittarius)" \
	-i "(core syntax-case)" -i "(for (except (rnrs) syntax-rules) expand)" \
	-i "(core errors)" -i "(for (smatch) expand)" \
	-i "(for (core misc) expand)" \
	-i "(for (compat r7rs) expand)" \
	-i "(sagittarius vm)" -i "(sagittarius vm instruction)" \
	-i "(sagittarius vm debug)" \
	-i "(sagittarius compiler util)" \
	-i "(sagittarius compiler procedure)" \
	-I ../boot \
	-a ../boot/lib/smatch.scm \
	-e compile -e compile-p1 -e compile-p2 -e compile-p3 \
	-e compile-p4 -e compile-p5    
    $SCM -L../sitelib genlib -f ../boot/lib/errors.scm
    $SCM -L../sitelib genlib -f ../boot/lib/arith.scm
    $SCM -L../sitelib genlib -f ../boot/lib/enums.scm
}

do_builtin()
{
    echo Generating c file from compiler.scm
    cd boot
# we no longer need (match core)
#    gosh -I. vm.scm ./lib/match_core.scm c "(match core)" ../src/lib_match_core.c
    $SCM vm.scm ./lib/repl.scm c "(sagittarius interactive)" ../src/lib_repl.c
    cd ../
}


do_stub()
{
    echo Generating library from stub
    cd src
    #Ypsilon --sitelib=../sitelib ./genstub
    sash -L../lib -L../sitelib ./genstub $force
    #sash ./genstub $force
    cd ../
}

do_insn()
{
    echo generating instruction
#    ./script/gen-instruction.scm scheme ./boot/instructions.scm ./boot/insn.scm
#    ./script/gen-instruction.scm c++ ./boot/instructions.scm ./src/sagittarius/instruction.h
    cd src
    sash -L../sitelib ./geninsn
#    sash ./geninsn
    cd ../
}

show_usage()
{
    echo "usage: $0 compiler|library|stub|all"
    echo "    compiler:   generate compiler.c"
    echo "    library:    generate compiled libraries"
#    echo "    builtin:    generate compiled builtin libraries"
    echo "    stub:       generate stub files"
    echo "    stub_force: generate stub files (re-create all)"
    echo "    insn:       generate insn"
    echo "    all:        generate do everything above"
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
#	    builtin)
#		do_builtin
#		;;
	    stub)
		force=""
		do_stub
		;;
	    stub_force)
		force="-f"
		do_stub
		;;
	    insn)
		do_insn
		;;
	    precomp)
		do_precomp
		;;
	    all)
		echo "generate all."
		do_insn
		do_compile
		do_library
		do_builtin
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

