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

tar xvf sagittarius.tar.gz
cd sagittarius-$VERSION

for p in ../*.patch; do 
    patch -p1 < ${p}
done

cmake .
make -j8
make install
