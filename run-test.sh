#!/bin/sh
# simple test runnner
# 
# if I can find the way to configure CMake, then this won't be needed anymore.

if [ -e ./build/sash ]; then
    ./build/sash -L./lib -L./test/r6rs-test-suite ./test/r6rs-test-suite/tests/r6rs/run.sps
    ./build/sash -L./lib -L./sitelib -L./test ./test/tests.scm
else
    echo "Please make sure you have built Sagittarius Scheme"
fi

