#!/bin/sh
. ./common.sh

require_ghc_ge 801

cd includes2
mv cabal.project.external cabal.project
cabal new-build exe
dist-newstyle/build/*/*/exe-*/c/exe/build/exe/exe | fgrep "minemysql minepostgresql"
