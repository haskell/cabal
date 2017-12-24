#!/bin/sh

. ./travis-common.sh

export PATH=/opt/cabal/head/bin:$PATH

# ---------------------------------------------------------------------
# Check that auto-generated files/fields are up to date.
# ---------------------------------------------------------------------

# Regenerate the CONTRIBUTORS file.
# Currently doesn't work because Travis uses --depth=50 when cloning.
#./Cabal/misc/gen-authors.sh > AUTHORS

# Regenerate files
timed make lexer
timed make gen-extra-source-files

# Fail if the diff is not empty.
timed ./Cabal/misc/travis-diff-files.sh
