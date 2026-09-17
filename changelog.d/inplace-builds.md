---
synopsis: Add `inplace-builds` to install a project's own packages into the store
packages: [cabal-install]
prs: 0
---

By default `cabal` builds the packages that make up a project in place, and
only installs their dependencies into the store. Setting `inplace-builds: False`
in `cabal.project` (or passing `--inplace-builds=False`) installs the project's
own packages into the store as well, so the whole build lands in a single
relocatable directory with one library tree and one package database.

This is for build systems that drive `cabal` over a source tree and then have
to copy the result somewhere else -- GHC's `Makefile` assembles its binary
distribution out of exactly such a directory. It costs what in-place builds
buy: every edit reinstalls the package into the store instead of rebuilding it
in situ.

A package built from a local directory has no source hash, so its store entry
cannot be named after one. Such packages are installed under their package id
(`mylib-0.1.0.0`) rather than a content hash, and no `cabal-hash.txt` is
written for them.
