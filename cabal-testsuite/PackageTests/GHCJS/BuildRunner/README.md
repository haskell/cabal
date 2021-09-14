This test verifies that the `-build-runner` option is passed to GHCJS when the cabal commands `test`/`run`/`bench` are run with the `--ghcjs` option.

This test was created in order to avoid regressions in running GHCJS executables, e.g.:

* https://github.com/haskell/cabal/issues/6175
* https://github.com/haskell/cabal/issues/6361
