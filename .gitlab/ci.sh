#!/usr/bin/env bash

set -Eeuo pipefail

source "$CI_PROJECT_DIR/.gitlab/common.sh"

export GHCUP_INSTALL_BASE_PREFIX="$CI_PROJECT_DIR/toolchain"
export CABAL_DIR="$CI_PROJECT_DIR/cabal"

case "$(uname)" in
    MSYS_*|MINGW*)
        export CABAL_DIR="$(cygpath -w "$CABAL_DIR")"
        GHCUP_BINDIR="${GHCUP_INSTALL_BASE_PREFIX}/ghcup/bin"
        EXE_EXT=".exe"
        ;;
    *)
        GHCUP_BINDIR="${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/bin"
        EXE_EXT=""
        ;;
esac

mkdir -p "$CABAL_DIR"
mkdir -p "$GHCUP_BINDIR"
export PATH="$GHCUP_BINDIR:$PATH"

export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_GHC_VERSION=$GHC_VERSION
export BOOTSTRAP_HASKELL_CABAL_VERSION=$CABAL_INSTALL_VERSION
export BOOTSTRAP_HASKELL_ADJUST_CABAL_CONFIG=yes

# for some reason the subshell doesn't pick up the arm64 environment on darwin
# and starts installing x86_64 GHC
case "$(uname -s)" in
    "Darwin"|"darwin")
        case "$(/usr/bin/arch)" in
            aarch64|arm64|armv8l)
                curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | arch -arm64 /bin/bash
                ;;
            *)
                curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
                ;;
        esac
        ;;
    *)
        curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
        ;;
esac

# https://github.com/haskell/cabal/issues/7313#issuecomment-811851884
# and
# https://github.com/haskellari/lukko/issues/17
#
# $PLATFORM comes from CI.
if [ "$(getconf LONG_BIT)" = "32" -o "${PLATFORM:=xxx}" = "x86_64-linux-centos7" ] ; then
    echo 'constraints: lukko -ofd-locking' >> cabal.project.release.local
fi

args=(
    -w "ghc-$GHC_VERSION"
    --disable-profiling
    --enable-executable-stripping
    --project-file=cabal.project.release
    ${ADD_CABAL_ARGS}
)

run cabal v2-build ${args[@]} cabal-install

mkdir "$CI_PROJECT_DIR/out"
cp "$(cabal list-bin ${args[@]} cabal-install:exe:cabal)" "$CI_PROJECT_DIR/out/cabal$EXE_EXT"
cp dist-newstyle/cache/plan.json "$CI_PROJECT_DIR/out/plan.json"
cd "$CI_PROJECT_DIR/out/"

# create tarball/zip
TARBALL_PREFIX="cabal-install-$("$CI_PROJECT_DIR/out/cabal" --numeric-version)"
case "${TARBALL_EXT}" in
    zip)
        zip "${TARBALL_PREFIX}-${TARBALL_ARCHIVE_SUFFIX}.${TARBALL_EXT}" "cabal${EXE_EXT}" plan.json
        ;;
    tar.xz)
        tar caf "${TARBALL_PREFIX}-${TARBALL_ARCHIVE_SUFFIX}.${TARBALL_EXT}" "cabal${EXE_EXT}" plan.json
        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac

rm cabal plan.json
