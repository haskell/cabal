Cabal and Cabal-syntax 3.18.2.0 changelog and release notes
---

### Significant changes

- Mark `CabalSpecV3_18` as the latest supported specification [#12271](https://github.com/haskell/cabal/issues/12271) [#12273](https://github.com/haskell/cabal/pull/12273)

  `cabalSpecLatest` remained `CabalSpecV3_16` after `CabalSpecV3_18` was
  introduced. As a result, `cabal-install` treated a `build-type: Simple` package
  that declared `cabal-version: 3.18` as a future-format package. It then tried to
  compile an external `Setup.hs` without a Cabal setup dependency, which caused
  the build to fail.

  `cabalSpecLatest` now points at `CabalSpecV3_18`. Affected packages use the
  in-process setup path again.

- Fix `renameFileWithRetry` leaving temporary files after copying [#12244](https://github.com/haskell/cabal/issues/12244) [#12246](https://github.com/haskell/cabal/pull/12246)

  `cabal-install build` left a number of temporary (`.tmp` extension)
  files in the user’s temporary folder (e.g. `/tmp`).  This has been fixed.

### Other changes

- Fix concurrent store creations [#11329](https://github.com/haskell/cabal/issues/11329) [#12114](https://github.com/haskell/cabal/pull/12114)

  Previously, when several cabal processes share one `--store-dir` and that store
  is cold, they all race `createPackageDBIfMissing`. Precisely hitting the warning
  above it, noting that it is not thread-safe.

  Fix this by using a fd-based lock, prior to attempting to create the index.
