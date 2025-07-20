#!env bash

CUR_DIR="$(cd "$(dirname "$0")" && pwd)"

NEED_SUDO=$1

REPOSITORY_URL=https://github.com/ktakashi/sagittarius-scheme
RELEASES=${REPOSITORY_URL}/releases
# FIXME: ugly...
VERSION=$(curl -ksI "${RELEASES}/latest" \
	| grep -i '^Location:' \
	| tr '\r\n' ' ' \
	| sed "s|.ocation: ${RELEASES}/tag/v||" \
        | sed 's/ //g')

DOWNLOAD="${RELEASES}/download/v${VERSION}/sagittarius-${VERSION}.tar.gz"
curl -kLo sagittarius.tar.gz $DOWNLOAD

tar --no-same-owner -xzvf sagittarius.tar.gz
if [ "$?" != "0" ]; then
    # It seems OpenBSD version of tar doesn't support --no-same-owner by default
    tar -xzvf sagittarius.tar.gz
fi

PATCH_FILE="$CUR_DIR/patches/patch-${VERSION}.patch"
SAGITTARIUS_DIR="sagittarius-${VERSION}"
if [ -f "$PATCH_FILE" ]; then
    patch -tud $SAGITTARIUS_DIR < "$PATCH_FILE"
fi

cd $SAGITTARIUS_DIR

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
