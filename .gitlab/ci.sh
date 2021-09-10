#!/usr/bin/env bash

set -Eeuo pipefail

source "$CI_PROJECT_DIR/.gitlab/common.sh"


export GHCUP_INSTALL_BASE_PREFIX="$CI_PROJECT_DIR/toolchain"
export CABAL_DIR="$CI_PROJECT_DIR/cabal"

case "$(uname)" in
    MSYS_*|MINGW*)
        export CABAL_DIR="$(cygpath -w "$CABAL_DIR")"
		GHCUP_BINDIR="${GHCUP_INSTALL_BASE_PREFIX}/ghcup/bin"
        ;;
	*)
		GHCUP_BINDIR="${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/bin"
		;;
esac

mkdir -p "$CABAL_DIR"
mkdir -p "$GHCUP_BINDIR"
export PATH="$GHCUP_BINDIR:$PATH"

export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_GHC_VERSION=$GHC_VERSION
export BOOTSTRAP_HASKELL_CABAL_VERSION=$CABAL_INSTALL_VERSION
export BOOTSTRAP_HASKELL_VERBOSE=1
export BOOTSTRAP_HASKELL_ADJUST_CABAL_CONFIG=yes

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

run cabal v2-install cabal-install \
    -w "ghc-$GHC_VERSION" \
    --installdir="$CI_PROJECT_DIR/out" \
    --install-method=copy \
    --overwrite-policy=always \
    --enable-executable-static \
    --disable-profiling \
    --enable-split-sections \
    --enable-executable-stripping

cp dist-newstyle/cache/plan.json "$CI_PROJECT_DIR/out/plan.json"
