cabal() {
    "$CABAL" $CABAL_ARGS "$@"
}

die() {
    echo "die: $@"
    exit 1
}
