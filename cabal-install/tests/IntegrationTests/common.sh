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
