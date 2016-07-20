#!/bin/sh

. ./travis-common.sh

# ---------------------------------------------------------------------
# Bootstrap cabal, to verify bootstrap.sh script works.
# ---------------------------------------------------------------------

bootstrap_jobs="-j"

(cd cabal-install && timed env EXTRA_CONFIGURE_OPTS="" ./bootstrap.sh $bootstrap_jobs --no-doc)
timed $HOME/.cabal/bin/cabal --version
PATH=$HOME/.cabal/bin:$PATH

# ---------------------------------------------------------------------
# Verify that installation from tarball works.
# ---------------------------------------------------------------------

# The following scriptlet checks that the resulting source distribution can be
# built & installed.
install_from_tarball() {
   SRC_TGZ=$(cabal info . | awk '{print $2 ".tar.gz";exit}') ;
   export SRC_TGZ
   if [ -f "dist/$SRC_TGZ" ]; then
      cabal install --force-reinstalls $jobs "dist/$SRC_TGZ" -v2;
   else
      echo "expected 'dist/$SRC_TGZ' not found";
      exit 1;
   fi
}

timed cabal update

echo Cabal
(cd Cabal && timed cabal sdist)
(cd Cabal && timed install_from_tarball)

echo cabal-install
(cd cabal-install && timed cabal sdist)
(cd cabal-install && timed install_from_tarball)
