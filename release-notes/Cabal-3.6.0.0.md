Cabal 3.6.0.0 Changelog
---


- Backported to 3.4 [#6964](https://github.com/haskell/cabal/pull/6964) [#6968](https://github.com/haskell/cabal/pull/6968)
- CI setup [#6959](https://github.com/haskell/cabal/pull/6959)

  - Remove travis scripts

- Code organization [#6960](https://github.com/haskell/cabal/pull/6960) [#6963](https://github.com/haskell/cabal/pull/6963) [#6970](https://github.com/haskell/cabal/pull/6970) [#6974](https://github.com/haskell/cabal/pull/6974) [#6975](https://github.com/haskell/cabal/pull/6975)

  - Move Cabal sources into Cabal/src
  - Move cabal-install sources to cabal-install/src/
  - Move doc/ to the top-level of the repository
  - Add stylish-haskell config.

- Documentation improvements [#6813](https://github.com/haskell/cabal/issues/6813) [#6971](https://github.com/haskell/cabal/pull/6971) [#7047](https://github.com/haskell/cabal/pull/7047)

  Write highlights

- Add post-checkout-command to source-package-repository [#6664](https://github.com/haskell/cabal/issues/6664) [#7047](https://github.com/haskell/cabal/pull/7047)
- Add `hsc2hs-options`, for specifying additional options to pass to `hsc2hs` [#6295](https://github.com/haskell/cabal/pull/6295)
- Include cmm-sources when linking shared objects [#7182](https://github.com/haskell/cabal/issues/7182) [#7252](https://github.com/haskell/cabal/pull/7252)

  - Previously `cmm-sources` were not included in the final link when building a library as a shared object. Fix this.

- Add language extensions for GHC 9.2 [#7312](https://github.com/haskell/cabal/issues/7312)
- --dry-run and --only-download effect v2-configure, v2-freeze, v2-run, and v2-exec [#7379](https://github.com/haskell/cabal/issues/7379)
- Alert user and suggest command on spelling mistakes
