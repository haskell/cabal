# Helper to run Cabal
cabal() {
    "$CABAL" $CABAL_ARGS_NO_CONFIG_FILE "$@"
}

die() {
    echo "die: $@"
    exit 1
}
