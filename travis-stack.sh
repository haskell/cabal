#!/bin/sh

if [ -z ${STACK_CONFIG+x} ]; then
    echo "STACK_CONFIG environment variable not set."
    echo "This build case is not configured correctly."
    exit 1
fi

. ./travis-common.sh

# ---------------------------------------------------------------------
# Build Cabal via Stack(age).
# ---------------------------------------------------------------------

stack build \
    --no-terminal \
    --stack-yaml "$STACK_CONFIG" \
    --test \
    --bench --no-run-benchmarks
