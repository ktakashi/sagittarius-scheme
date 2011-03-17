#!/bin/sh

echo generating instruction
cd src
Ypsilon --sitelib=../sitelib ./geninsn
cd ../
