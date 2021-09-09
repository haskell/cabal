#!/usr/bin/env bash

set -Eeuo pipefail

TOP="$(pwd)"
source "$TOP/.gitlab/common.sh"

# We don't have sudo access in GitLab CI, so we can't just dump binaries in
# the usual PATH locations.
toolchain="$(pwd)/toolchain"
mkdir -p "$toolchain/bin"
export PATH="$toolchain/bin:$PATH"

export CABAL_DIR="$TOP/cabal"
mkdir -p "$CABAL_DIR"

# Platform-specific bits
MAKE="make"
case "$(uname)" in
    MSYS_*|MINGW*)
        CABAL_DIR="$(cygpath -w "$CABAL_DIR")"
        ;;
    FreeBSD)
        MAKE="gmake"
        ;;
esac

fetch_cabal_install_unix() {
    local cabal_url="$1"
    run curl -L -o cabal.tar.xz "$cabal_url"
    run tar --directory "$toolchain/bin/" -xf cabal.tar.xz cabal
    chmod +x "$toolchain/bin/cabal"
    CABAL="$toolchain/bin/cabal"
}

setup_cabal_install() {
    if [ -z "${CABAL:-}" ]; then
        info "Fetching GHC..."
        case "$(uname)" in
            FreeBSD)
                fetch_cabal_install_unix "https://hasufell.de/d/d3e215db133e4fcaa61e/files/?p=/cabal-install-$CABAL_INSTALL_VERSION-x86_64-portbld-freebsd.tar.xz&dl=1" ;;
            Darwin)
                fetch_cabal_install_unix "https://downloads.haskell.org/~cabal/cabal-install-$CABAL_INSTALL_VERSION/cabal-install-$CABAL_INSTALL_VERSION-x86_64-apple-darwin17.7.0.tar.xz" ;;
            MSYS_*|MINGW*)
                cabal_url="https://downloads.haskell.org/~cabal/cabal-install-$CABAL_INSTALL_VERSION/cabal-install-$CABAL_INSTALL_VERSION-x86_64-unknown-mingw32.zip"
                run curl -L -o cabal.zip "$cabal_url"
                run unzip cabal.zip
                cp cabal.exe "$toolchain/bin/cabal.exe"
                chmod +x "$toolchain/bin/cabal.exe"
                CABAL="$toolchain/bin/cabal.exe"
                ;;
            *) fail "no cabal-install bindist for $(uname)"
        esac

    fi
    export CABAL
    run "$CABAL" --version
}


fetch_ghc_unix() {
    local ghc_url="$1"
    run curl -sSfL -o ghc.tar.xz "$ghc_url"
    run tar -xf ghc.tar.xz

    pushd "ghc-${GHC_VERSION}"
    run ./configure --prefix="$toolchain"
    run "$MAKE" install
    popd
    GHC="$toolchain/bin/ghc"
}

setup_ghc() {
    if [ -z "${GHC:-}" ]; then
        info "Fetching GHC..."
        case "$(uname)" in
            FreeBSD)
                fetch_ghc_unix "https://downloads.haskell.org/~ghc/$GHC_VERSION/ghc-$GHC_VERSION-x86_64-unknown-freebsd.tar.xz" ;;
            Darwin)
                fetch_ghc_unix "https://downloads.haskell.org/~ghc/$GHC_VERSION/ghc-$GHC_VERSION-x86_64-apple-darwin.tar.xz" ;;
            MSYS_*|MINGW*)
                ghc_url="https://downloads.haskell.org/~ghc/$GHC_VERSION/ghc-$GHC_VERSION-x86_64-unknown-mingw32.tar.xz"
                run curl -sSfL -o ghc.tar.xz "$ghc_url"
                run tar -xf ghc.tar.xz
                cp -R ghc-$GHC_VERSION/* "$toolchain"
                GHC="$toolchain/bin/ghc.exe"
                run "$GHC" --version
                GHC="$(cygpath -w $GHC)"
                return
                ;;
            *) fail "no GHC bindist for $(uname)"
        esac
    fi

    export GHC
    run "$GHC" --version
}

setup_cabal_install
setup_ghc

run "$CABAL" update
run "$CABAL" v2-install cabal-install \
    -w "$GHC" \
    --installdir="$TOP/out" \
    --install-method=copy \
    --overwrite-policy=always \
    --enable-executable-static \
    --disable-profiling \
    --enable-split-sections \
    --enable-executable-stripping
