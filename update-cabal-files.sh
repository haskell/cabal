#!/bin/sh
(cd Cabal; misc/gen-extra-source-files.hs Cabal.cabal)
(cd cabal-install; ../Cabal/misc/gen-extra-source-files.hs cabal-install.cabal)
