---
synopsis: Detect semaphore version mismatch between cabal-install and GHC
packages: [Cabal, cabal-install]
prs: 0000
issues: 0000
significance: significant
---

When using `--semaphore`, cabal-install now checks whether the selected GHC's
semaphore protocol version is compatible before passing `-jsem`.  If the GHC
reports no `Semaphore version` field (GHC 9.8–9.14, which use v1) and
cabal-install uses v2, a warning is emitted and cabal-install falls back to
normal parallelism control instead of passing an incompatible semaphore name.

On Windows, v1 and v2 are always compatible (same Win32 API), so semaphore
coordination is preserved across all version combinations.

- `Cabal`: add `jsemVersion :: Compiler -> Maybe Int` to read the
  `Semaphore version` field from `ghc --info`.
- `cabal-install`: add `isJsemCompatible` check in `newJobControlFromParStrat`;
  emit a warning and fall back to `-jN` when versions are incompatible.
