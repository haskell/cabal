#!/bin/sh

HCBASE=/usr/bin/
HC=$HCBASE/ghc
GHCFLAGS='--make -Wall -fno-warn-unused-matches -cpp'
ISPOSIX=-DHAVE_UNIX_PACKAGE

rm moduleTest
mkdir -p dist/debug
$HC $GHCFLAGS $ISPOSIX -DDEBUG -odir dist/debug -hidir dist/debug -idist/debug/:.:tests/HUnit-1.0/src tests/ModuleTest.hs -o moduleTest 
./moduleTest
