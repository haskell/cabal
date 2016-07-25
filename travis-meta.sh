#!/bin/sh

. ./travis-common.sh

# ---------------------------------------------------------------------
# Check that auto-generated files/fields are up to date.
# ---------------------------------------------------------------------

# Regenerate the CONTRIBUTORS file.
# Currently doesn't work because Travis uses --depth=50 when cloning.
#./Cabal/misc/gen-authors.sh > AUTHORS

# Regenerate the 'extra-source-files' field in Cabal.cabal.
(cd Cabal && timed ./misc/gen-extra-source-files.sh Cabal.cabal) || exit $?

# Regenerate the 'extra-source-files' field in cabal-install.cabal.
(cd cabal-install && ../Cabal/misc/gen-extra-source-files.sh cabal-install.cabal) || exit $?

# Fail if the diff is not empty.
timed ./Cabal/misc/travis-diff-files.sh
