#!/bin/sh
. ./common.sh

require_ghc_ge 801

cd includes2
mv cabal.project.internal cabal.project
cabal new-build exe
dist-newstyle/build/*/*/Includes2-*/c/exe/build/exe/exe | fgrep "minemysql minepostgresql"
