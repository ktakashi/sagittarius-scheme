#!/bin/sh

echo Generating library from stub
cd src
Ypsilon --sitelib=../sitelib ./genstub
cd ../
