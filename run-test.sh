#!/bin/bash

for i in $(seq 1 10); do
    ./build/sagittarius -Llib -Lsitelib -L'ext/*' -Dbuild \
	test/runner.scm test/tests/net/socket.scm
    if [ $? -ne 0 ]; then break; fi
done
