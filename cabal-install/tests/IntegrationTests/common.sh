# A globally available set of useful utilities.  Test
# scripts can include this by saying ". ./common.sh"

# Helper to run Cabal
cabal() {
    "$CABAL" $CABAL_ARGS "$@"
}

die() {
    echo "die: $@"
    exit 1
}

require_ghc_le() {
    GHCVER="$(echo main = print __GLASGOW_HASKELL__ | runghc -XCPP)"
    if [ "$GHCVER" -gt "$1" ]; then
        echo "Skipping test that needs GHC <= $1 (actual version $GHCVER)"
        exit 0
    fi
}

require_ghc_ge() {
    GHCVER="$(echo main = print __GLASGOW_HASKELL__ | runghc -XCPP)"
    if [ "$GHCVER" -lt "$1" ]; then
        echo "Skipping test that needs GHC >= $1 (actual version $GHCVER)"
        exit 0
    fi
}
