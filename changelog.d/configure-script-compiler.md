---
synopsis: Tell `configure` scripts which compiler Cabal is using
packages: [Cabal]
issues: [7452, 2947]
prs: 12340
significance: significant
---

A `build-type: Configure` package's `configure` script now receives the path
of the compiler Cabal is configuring the package with: as the standard
`HC=/path/to/ghc` argument, in `--with-compiler` and `--with-hc-pkg`, and, for
GHC, in the `GHC` and `GHC_PKG` environment variables. Previously, when the
compiler was given as a program path (as cabal-install always does), the
script only got the flavour name `ghc` and had to find the compiler on `PATH`,
which may be a different one.

This deserves a release notes entry: a `configure` script that relied on
`--with-compiler=ghc` naming whatever `ghc` is on `PATH` now gets the compiler
Cabal is actually using. To get the old value back, pass
`--configure-option=--with-compiler=ghc` (or `configure-options:
--with-compiler=ghc` in `cabal.project`), and likewise for `HC=`; Cabal passes
these arguments before the user's configure options, and autoconf keeps the
last one.

For the `Cabal` library API: `runConfigureScript` takes the `Compiler` and its
`CompilerPrograms`, as recorded in `PackageBuildDescr`.
`configureInstallDirArgs` gives the `--prefix`/`--bindir`/... arguments on
their own; `configureArgs` is unchanged but no longer used by
`runConfigureScript`.
