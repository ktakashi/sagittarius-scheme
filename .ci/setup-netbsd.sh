#!/bin/sh

echo "===== NetBSD ====="
uname -a
uname -m
echo "===== pkg_add ====="
pkg_add -V
echo "===== PKG_PATH ====="
echo "$PKG_PATH"
echo "===== pkg.conf ====="
echo "$PKG_PATH"
cat /etc/pkg_install.conf 2>/dev/null || true
echo "===================="

pkg_add -IU curl libffi boehm-gc cmake bash gmake gcc15 libatomic
