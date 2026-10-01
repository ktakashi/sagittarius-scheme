#!/bin/sh

# We use gccc14 instead of gcc15, as 11.0_2026Q3 remove it for some reason

pkg_add -IU curl libffi boehm-gc cmake bash gmake gcc14 libatomic
