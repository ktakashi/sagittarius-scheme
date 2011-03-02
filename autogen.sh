#!/bin/sh

echo Generating c file from compiler.scm
cd boot
../script/gen-compiler.scm compiler.scm sagittarius > compiler-sg.scm
gosh -I. vm.scm compiler-sg.scm c "(sagittarius compiler)" ../src/compiler.c
echo Generating compiled library files
echo '(display "dummy")' > dummy.scm
gosh -I. vm.scm dummy.scm lc ../src
rm dummy.scm
cd ../

. autogenstub.sh
