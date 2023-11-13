#!/bin/bash

set -eux

env
pwd
ls -lah

cd out
case "${TARBALL_EXT}" in
    zip)
        unzip ./cabal-install-*-"${ARTIFACT}.${TARBALL_EXT}"
        ;;
    tar.xz)
        tar xf ./cabal-install-*-"${ARTIFACT}.${TARBALL_EXT}"
        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac
cd ..

ghcup --no-verbose install ghc --set --install-targets "${GHC_TARGETS}" "${GHC_VERSION}"

cabal update

# TODO: we want to avoid building here... we should just
# be using the previously built 'cabal-tests' binary
cabal run ${ADD_CABAL_ARGS} cabal-testsuite:cabal-tests -- \
  --with-cabal "$(pwd)/out/cabal" \
  --intree-cabal-lib "$(pwd)" \
  --test-tmp "$(pwd)/testdb" \
  --skip-setup-tests \
  -j "$(nproc)"

