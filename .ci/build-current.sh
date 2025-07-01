#!/bin/bash

NEED_SUDO=$1

REPOSITORY_URL=https://github.com/ktakashi/sagittarius-scheme
RELEASES=${REPOSITORY_URL}/releases
# FIXME: ugly...
VERSION=$(curl -ksI "${RELEASES}/latest" \
	| grep -i '^Location:' \
	| tr '\r\n' ' ' \
	| sed "s|Location: ${RELEASES}/tag/v||i" \
        | sed 's/ //g')

DOWNLOAD="${RELEASES}/download/v${VERSION}/sagittarius-${VERSION}.tar.gz"
curl -kLo sagittarius.tar.gz $DOWNLOAD

tar --no-same-owner -xvf sagittarius.tar.gz
cd "sagittarius-${VERSION}"

# Add  -DCMAKE_C_FLAGS="-fPIC" -DCMAKE_CXX_FLAGS="-fPIC"
# for FreeBSD with aarch64 (need more generic condition...)
processor=$(uname -p)
if [ "$processor" == "aarch64" ]; then
    flags="-fPIC"
fi

cmake . -DCMAKE_C_FLAGS="$flags" -DCMAKE_CXX_FLAGS="$flags"
make -j8 sash
make

case ${NEED_SUDO} in
    yes) sudo make install;;
    *) make install;;
esac
