#!/usr/bin/env bash

# A script to download the release binary files for a given cabal release
# from upstream "downlods.haskell.org".
# It accepts the first and only argument as the release number.
#
# useage:-
#   $ download-cabal-install-release-binaries.sh "3.10.1.0"
#
# This was initally made to be used with ./create-release-metadata-for-ghcup.sh

set -eu
set -o pipefail

RELEASE=$1

echo "RELEASE: $RELEASE"

for com in wget sha256sum ; do
        command -V ${com} >/dev/null 2>&1
done

[ ! -d "binary-downloads/cabal-install-${RELEASE}-binaries" ]

mkdir -p "binary-downloads/cabal-install-${RELEASE}-binaries"

cd "binary-downloads/cabal-install-${RELEASE}-binaries"

## Download release files
echo "Downloading form: \"https://downloads.haskell.org/~cabal/cabal-install-${RELEASE}/\""
wget --no-parent -r --reject "index.html*" --no-directories "https://downloads.haskell.org/~cabal/cabal-install-${RELEASE}/"

## Verify that sha256 sums of downloaded files match the ones mentioned in the upstream SHA256SUMS file
echo "verifying checksums for downloaded files..."

if sha256sum --check ./SHA256SUMS; then
    echo "All checksums match!"
    echo "Successfully downloaded binaries for release: ${RELEASE}"
else
    echo "checksums of downloaded files do no match the ones listed in upstream SHA256SUMS file."
    echo "please try deleting \"binary-downloads/cabal-install-${RELEASE}-binaries\" folder and downloading again."
fi
