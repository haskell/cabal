#!/usr/bin/env bash

set -euxo pipefail

ORIGDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TARDIR=$(mktemp -d)
WORKDIR=$(mktemp -d)

function cleanup {
    rm -rf "$TARDIR"
    rm -rf "$WORKDIR"
}

trap cleanup EXIT

stack sdist --tar-dir "$TARDIR"

cd $WORKDIR

tar zxfv $TARDIR/*.tar.gz
cd *

cp "$ORIGDIR/../../stack.yaml" .
stack test
