#!/bin/bash

NEED_SUDO=$1

REPOSITORY_URL=https://bitbucket.org/ktakashi/sagittarius-scheme/downloads

curl -kLo version ${REPOSITORY_URL}/latest-version.txt
curl -kLo sagittarius.tar.gz ${REPOSITORY_URL}/sagittarius-`cat version`.tar.gz

tar --no-same-owner -xvf sagittarius.tar.gz
cd sagittarius-`cat version`

cmake .
make -j8 sash
make

case ${NEED_SUDO} in
    yes) sudo make install;;
    *) make install;;
esac
