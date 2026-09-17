---
synopsis: Tell `configure` scripts which compiler Cabal is using
packages: [Cabal]
issues: [7452, 2947]
prs: 12340
---

A `build-type: Configure` package's `configure` script now receives the path
of the compiler Cabal is configuring the package with: as the standard
`HC=/path/to/ghc` argument, in `--with-compiler` and `--with-hc-pkg`, and, for
GHC, in the `GHC` and `GHC_PKG` environment variables. Previously, when the
compiler was given as a program path (as cabal-install always does), the
script only got the flavour name `ghc` and had to find the compiler on `PATH`,
which may be a different one.
