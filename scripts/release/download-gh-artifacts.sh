#!/bin/sh

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

git archive --format=tar.gz -o "gh-release-artifacts/cabal-${VERSION}/cabal-${VERSION}-src.tar.gz" --prefix="cabal-${VERSION}/" HEAD

cd "gh-release-artifacts/cabal-${VERSION}"

# github
gh release download "$RELEASE"

sha256sum ./* > SHA256SUMS
gpg --detach-sign -u "${SIGNER}" SHA256SUMS

echo
echo "Now run the following:"
echo "  ( cd gh-release-artifacts/cabal-${VERSION} && gh release upload $RELEASE cabal-${VERSION}-src.tar.gz SHA256SUMS SHA256SUMS.sig )"
echo
echo "And afterwards to upload to downloads.haskell.org:"
echo "  ./scripts/release/upload-artifacts.sh cabal-install-v${VERSION}"
echo
echo "And don't forget to finalize the release at https://github.com/stable-haskell/cabal/releases/tag/cabal-install-v${VERSION}"
echo
echo "Also create a PR at https://github.com/haskell/ghcup-metadata/pulls for the vanilla-channel from the output of the following script:"
echo "  ./scripts/release/create-yaml-snippet.sh cabal-install-v${VERSION}"

