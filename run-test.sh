#!/bin/sh
# simple test runnner
# 
# if I can find the way to configure CMake, then this won't be needed anymore.

if [ -e ./build/sash ]; then
    echo "testing R6RS test suite"
    ./build/sash $1 -L./lib -L./test/r6rs-test-suite ./test/r6rs-test-suite/tests/r6rs/run.sps
    echo "testing sitelib"
    ./build/sash -L./lib -L./sitelib -L./test -L./ext/regex -D./build ./test/tests.scm
    echo "testing extensions"
    cd ext
    ../build/sash -L../lib -L../sitelib ./all-tests.scm
    cd ../
else
    echo "Please make sure you have built Sagittarius Scheme"
fi

