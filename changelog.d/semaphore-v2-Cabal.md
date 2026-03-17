---
synopsis: Expose `jsemVersion` to detect `-jsem` protocol mismatches
packages: [Cabal]
prs: 11628
issues: 9993
---

The `Compiler` record now exposes a `jsemVersion :: Compiler -> Maybe Int`
accessor that reads the `"Semaphore version"` field of `ghc --info`.
Returns `Just v` when GHC reports a value, `Nothing` for compilers
that don't report the field (older GHCs, or non-GHC compilers).

cabal-install consumes this to detect a protocol mismatch with the
selected GHC before invoking `ghc -jsem`, so it can fall back to
its in-process coordinator and warn the user rather than handing GHC
a name it can't speak.
