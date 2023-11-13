#!/bin/sh

set -eu
set -o pipefail

RELEASE=$1
VERSION=${RELEASE#cabal-install-v}
URL=https://downloads.haskell.org

cd "gh-release-artifacts/cabal-${VERSION}"

sftp -b - $URL <<EOF
cd cabal

mkdir cabal-install-$VERSION
cd cabal-install-$VERSION

put -r .

cd ..
rm cabal-install-latest
symlink cabal-install-$VERSION cabal-install-latest
EOF

curl -X PURGE "https://downloads.haskell.org/~cabal/cabal-install-$VERSION/"
curl -X PURGE "https://downloads.haskell.org/cabal/cabal-install-$VERSION/"
