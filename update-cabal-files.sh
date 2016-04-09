#!/bin/sh
(cd Cabal; misc/gen-extra-source-files.sh Cabal.cabal)
(cd cabal-install; ../Cabal/misc/gen-extra-source-files.sh cabal-install.cabal)
