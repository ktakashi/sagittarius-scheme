#!/bin/sh

VERSION=$1

case ${VERSION} in
    latest)
	curl -kLo version https://bitbucket.org/ktakashi/sagittarius-scheme/downloads/latest-version.txt
	VERSION=`cat version`
	;;
    *) ;;
esac

curl -kLo sagittarius.tar.gz https://bitbucket.org/ktakashi/sagittarius-scheme/downloads/sagittarius-${VERSION}.tar.gz
tar xvf sagittarius.tar.gz
cd sagittarius-$VERSION

for p in ../*.patch; do 
    patch -p1 < ${p}
done

cmake .
make -j8
make install
