#!/bin/bash

set -eu
set -o pipefail

RELEASE=$1
VERSION=${RELEASE#cabal-install-v}
SIGNER=$2

echo "RELEASE: $RELEASE"
echo "SIGNER: $SIGNER"

for com in gh gpg curl sha256sum ; do
	command -V ${com} >/dev/null 2>&1
done

[ ! -e "gh-release-artifacts/cabal-${VERSION}" ]

mkdir -p "gh-release-artifacts/cabal-${VERSION}"

cd "gh-release-artifacts/cabal-${VERSION}"

# github
gh release download "$RELEASE"

# cirrus
curl --fail -L -o "cabal-install-${VERSION}-x86_64-portbld-freebsd.tar.xz" \
	"https://api.cirrus-ci.com/v1/artifact/github/haskell/cabal/build/binaries/out/cabal-install-${VERSION}-x86_64-portbld-freebsd.tar.xz?branch=${RELEASE}"


sha256sum ./* > SHA256SUMS
gpg --detach-sign -u "${SIGNER}" SHA256SUMS

gh release upload "$RELEASE" "cabal-install-${VERSION}-x86_64-portbld-freebsd.tar.xz" SHA256SUMS SHA256SUMS.sig

