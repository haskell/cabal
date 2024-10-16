#!/bin/bash

set -eux

# shellcheck disable=SC1091
. .github/scripts/env.sh
# shellcheck disable=SC1091
. .github/scripts/common.sh

uname -a
uname -p
uname
pwd
env

# ensure ghcup
install_ghcup

# build
ghcup install ghc "${GHC_VERSION}"
ghcup set ghc "${GHC_VERSION}"
sed -i.bak -e '/DELETE MARKER FOR CI/,/END DELETE/d' cabal.project # see comment in cabal.project
ecabal update
ecabal user-config diff
ecabal user-config init -f
"ghc-${GHC_VERSION}" --info
"ghc" --info

# https://github.com/haskell/cabal/issues/7313#issuecomment-811851884
if [ "$(getconf LONG_BIT)" == "32" ] || [ "${DISTRO}" == "CentOS" ] ; then
    echo 'constraints: lukko -ofd-locking' >> cabal.project.release.local
fi

# shellcheck disable=SC2206
args=(
    -w "ghc-$GHC_VERSION"
    --disable-profiling
    --enable-executable-stripping
    --project-file=cabal.project.release
    ${ADD_CABAL_ARGS}
)

run cabal v2-build "${args[@]}" cabal-install

mkdir -p "$CI_PROJECT_DIR/out"
# shellcheck disable=SC2154
cp "$(cabal list-bin "${args[@]}" cabal-install:exe:cabal)" "$CI_PROJECT_DIR/out/cabal$ext"
cp dist-newstyle/cache/plan.json "$CI_PROJECT_DIR/out/plan.json"
cd "$CI_PROJECT_DIR/out/"

# create tarball/zip
TARBALL_PREFIX="cabal-install-$("$CI_PROJECT_DIR/out/cabal" --numeric-version)"
case "${TARBALL_EXT}" in
    zip)
        zip "${TARBALL_PREFIX}-${ARTIFACT}.${TARBALL_EXT}" "cabal${ext}" plan.json
        ;;
    tar.xz)
        tar caf "${TARBALL_PREFIX}-${ARTIFACT}.${TARBALL_EXT}" "cabal${ext}" plan.json
        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac

rm cabal plan.json

