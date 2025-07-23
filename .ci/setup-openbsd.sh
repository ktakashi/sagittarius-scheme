#!/bin/sh

pkg_add -IUv curl libffi boehm-gc cmake bash gmake
ldconfig -r
