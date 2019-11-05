#!/bin/sh

. ./travis-common.sh

# ---------------------------------------------------------------------
# Check that auto-generated files/fields are up to date.
# ---------------------------------------------------------------------

# Regenerate the AUTHORS file.
git fetch --unshallow
./Cabal/misc/gen-authors.sh > AUTHORS

timed cabal update

# Regenerate files
timed make lexer
timed make gen-extra-source-files
timed make spdx

# Fail if the diff is not empty.
timed ./Cabal/misc/travis-diff-files.sh
