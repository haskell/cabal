. ./common.sh

require_ghc_ge 708

cd T4049
cabal sandbox init > /dev/null
cabal install --enable-shared > /dev/null
sh -c "$CCOMP UseLib.c -o UseLib -l myforeignlib -L \"$PWD/.cabal-sandbox/lib\" > /dev/null"
cabal exec "$PWD/UseLib"
