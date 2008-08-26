#!/bin/sh

# A script to bootstrap cabal-install.

# It works by downloading and installing the Cabal, zlib and
# HTTP packages. It then installs cabal-install itself.
# It expects to be run inside the cabal-install directory.

CABAL_VER="1.4.0.2"
HTTP_VER="3001.0.4"
ZLIB_VER="0.4.0.4"

HACKAGE_URL="http://hackage.haskell.org/packages/archive"
CABAL_URL=${HACKAGE_URL}/Cabal/${CABAL_VER}/Cabal-${CABAL_VER}.tar.gz
HTTP_URL=${HACKAGE_URL}/HTTP/${HTTP_VER}/HTTP-${HTTP_VER}.tar.gz
ZLIB_URL=${HACKAGE_URL}/zlib/${ZLIB_VER}/zlib-${ZLIB_VER}.tar.gz

wget ${CABAL_URL} ${HTTP_URL} ${ZLIB_URL}

tar -zxf Cabal-${CABAL_VER}.tar.gz
pushd Cabal-${CABAL_VER}
ghc --make Setup
./Setup configure --user && ./Setup build && ./Setup install
popd

tar -zxf HTTP-${HTTP_VER}.tar.gz
pushd HTTP-${HTTP_VER}
runghc Setup configure --user && runghc Setup build && runghc Setup install
popd

tar -zxf zlib-${ZLIB_VER}.tar.gz
pushd zlib-${ZLIB_VER}
runghc Setup configure --user && runghc Setup build && runghc Setup install
popd

runghc Setup configure --user && runghc Setup build && runghc Setup install

echo
echo "If all went well then 'cabal' is in $HOME/.cabal/bin/"
echo "You may want to add this dir to your PATH"
