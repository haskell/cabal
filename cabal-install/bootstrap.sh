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

case `which wget curl` in
  *curl)
    curl -O ${CABAL_URL} -O ${HTTP_URL} -O ${ZLIB_URL}
    ;;
  *wget)
    wget ${CABAL_URL} ${HTTP_URL} ${ZLIB_URL}
    ;;
  *)
    echo "Failed to find a downloader, 'wget' or 'curl' is required" >&2
    exit 2
    ;;
esac

tar -zxf Cabal-${CABAL_VER}.tar.gz
cd Cabal-${CABAL_VER}
ghc --make Setup
./Setup configure --user && ./Setup build && ./Setup install
cd ..

tar -zxf HTTP-${HTTP_VER}.tar.gz
cd HTTP-${HTTP_VER}
runghc Setup configure --user && runghc Setup build && runghc Setup install
cd ..

tar -zxf zlib-${ZLIB_VER}.tar.gz
cd zlib-${ZLIB_VER}
runghc Setup configure --user && runghc Setup build && runghc Setup install
cd ..

runghc Setup configure --user && runghc Setup build && runghc Setup install

echo
echo "If all went well then 'cabal' is in $HOME/.cabal/bin/"
echo "You may want to add this dir to your PATH"
