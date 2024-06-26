export GHCUP_INSTALL_BASE_PREFIX="$CI_PROJECT_DIR/toolchain"

case "$(uname)" in
    MSYS_*|MINGW*)
        GHCUP_BINDIR="${GHCUP_INSTALL_BASE_PREFIX}/ghcup/bin"
        ;;
    *)
        GHCUP_BINDIR="${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/bin"
        ;;
esac

mkdir -p "$GHCUP_BINDIR"
export PATH="$GHCUP_BINDIR:$PATH"

export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_GHC_VERSION=$GHC_VERSION
export BOOTSTRAP_HASKELL_CABAL_VERSION=$CABAL_INSTALL_VERSION
export BOOTSTRAP_HASKELL_ADJUST_CABAL_CONFIG=yes
# We don't use stack, and it isn't available on i386-deb9
export BOOTSTRAP_HASKELL_INSTALL_NO_STACK=yes

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
