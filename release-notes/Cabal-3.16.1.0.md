Cabal and Cabal-syntax 3.16.1.0 changelog and release notes
---

- Always pass `--interactive` as the first ghc argument [#11099](https://github.com/haskell/cabal/issues/11099) [#11101](https://github.com/haskell/cabal/pull/11101)

  We recently changed Cabal to use response files for all GHC arguments by default.
  Unfortunately, this broke a couple of downstream consumers of cabal, notably Haskell Language Server and doctest, which both assume that `--interactive` is the first argument of the `ghc` invocation by `cabal repl`.

  This regression was fixed by implementing the `--with-repl` (#9115) argument, and tools, such as hie-bios and doctest, have been updated to take advantage of this. However, this renders already published HLS binaries, so any HLS version <=2.12.0.0, incompatible with `cabal-3.16`, as these old binaries can't be easily updated.

  In other words, no released HLS binary (at the point of this commit), is compatible with the cabal-3.16.0.0. Users have to build HLS from source to have a working toolchain.

  To give us a slightly better migration window, we undo some of the improvements to cabal, and make sure that `--interactive` is always passed as the first argument to the underlying `ghc` invocation. This hack is supposed to be temporary, and removed for cabal 3.18.

  (Ed. note: see https://github.com/haskell/cabal/issues/11305 re deprecation.)
