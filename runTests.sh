#!/bin/sh

HCBASE=/usr/bin/
HC=$HCBASE/ghc
GHCFLAGS='--make -Wall -fno-warn-unused-matches -cpp'
ISPOSIX=-DHAVE_UNIX_PACKAGE

rm -f moduleTest
mkdir -p dist/debug
echo Building...
$HC $GHCFLAGS $ISPOSIX -DDEBUG -odir dist/debug -hidir dist/debug -idist/debug/:.:tests/HUnit-1.0/src tests/ModuleTest.hs -o moduleTest 2> stderr
RES=$?
if [ $RES != 0 ]
then
    cat stderr >&2
    exit $RES
fi
echo Running...
./moduleTest
echo Done

