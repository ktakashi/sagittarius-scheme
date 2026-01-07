#!/bin/sh

VERSION=$1

REPOSITORY_URL=https://github.com/ktakashi/sagittarius-scheme
RELEASES=${REPOSITORY_URL}/releases


case ${VERSION} in
    latest)
	VERSION=`curl -ksI "${RELEASES}/latest" \
	| grep -i '^Location:' \
	| tr '\r\n' ' ' \
	| sed "s|Location: ${RELEASES}/tag/v||i" \
	| sed 's/ //g'`
	echo latest version: $VERSION
	;;
    *) ;;
esac

DOWNLOAD="${RELEASES}/download/v${VERSION}/sagittarius-${VERSION}.tar.gz"
curl -kLo sagittarius.tar.gz $DOWNLOAD

tar xf sagittarius.tar.gz

if [ -f patches/patch-$VERSION.patch ]; then
    patch -p0 < patches/patch-$VERSION.patch
fi

cd sagittarius-$VERSION

cmake .
make -j8
make install
