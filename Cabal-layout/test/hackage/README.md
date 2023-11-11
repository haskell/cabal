# Hackage parsing test

Usage: main DIR

Checks that every single manifest in the index can be parsed and rendered back
into an identical representation. Not all manifests are processed verbatim:
irrelevant whitespace is altered to make identity checks possible and
patches from `Hackage.Cabal.Patches` are applied to relevant files.

DIR is the root of the unarchived Hackage index. The archive at the time of writing this
can be downloaded [here](https://hackage.haskell.org/packages/archive/00-index.tar.gz).
