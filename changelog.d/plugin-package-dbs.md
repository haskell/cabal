---
synopsis: Pass package databases to GHC during linker capability checks
packages: [Cabal]
prs: [12486]
---

Cabal now passes the configured package database stack when invoking GHC for
linker capability checks. This fixes configuration failures such as
`cannot satisfy -plugin-package` when an installed compiler plugin is enabled
through `ghc-options` without being declared in `build-depends` (e.g. this is
natural setup for
[`ghc-tags-plugin`](https://hackage.haskell.org/package/ghc-tags-plugin). An
explicit `-package-db` in `ghc-options` is no longer needed when the plugin is
installed in a database already included in Cabal's configured stack.

Relative database paths are resolved against the configured working directory,
and databases that have not been created yet are skipped.
