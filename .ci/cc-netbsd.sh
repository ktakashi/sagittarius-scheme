#!/bin/sh

gcc=$(pkg_info | grep gcc | sed -n 's/^\(gcc[0-9][0-9]*\)-.*/\1/p' | sort -n | tail -1)
export CC=/usr/pkg/$gcc/bin/gcc
export CXX=/usr/pkg/$gcc/bin/g++
