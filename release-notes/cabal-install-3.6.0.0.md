### Backports

- Backported to 3.4 [#6964](https://github.com/haskell/cabal/pull/6964) [#6968](https://github.com/haskell/cabal/pull/6968)
- CI setup [#6959](https://github.com/haskell/cabal/pull/6959)

  - Remove travis scripts

### Significant Changes

- Add post-checkout-command to source-package-repository [#6664](https://github.com/haskell/cabal/issues/6664) [#7047](https://github.com/haskell/cabal/pull/7047)
- Assume list-bin target selectors are for executables [#7326](https://github.com/haskell/cabal/issues/7326) [#7335](https://github.com/haskell/cabal/pull/7335)
- Add a --only-download flag [#7323](https://github.com/haskell/cabal/issues/7323) [#7347](https://github.com/haskell/cabal/pull/7347)
- Add `hsc2hs-options`, for specifying additional options to pass to `hsc2hs` [#6295](https://github.com/haskell/cabal/pull/6295)
- Make extra-packages work properly [#6972](https://github.com/haskell/cabal/pull/6972)
- Bugfix - stop creating spurious dirs on `init` [#6772](https://github.com/haskell/cabal/issues/6772) [#7262](https://github.com/haskell/cabal/pull/7262)
- Avoid resource exhaustion in `cabal init` [#5115](https://github.com/haskell/cabal/issues/5115) [#7283](https://github.com/haskell/cabal/pull/7283)

  - Read file contents strictly to avoid resource exhaustion in `cabal init`.
  - Ignore UTF-8 decoding errors.

- Add language extensions for GHC 9.2 [#7312](https://github.com/haskell/cabal/issues/7312)
- --dry-run and --only-download effect v2-configure, v2-freeze, v2-run, and v2-exec [#7379](https://github.com/haskell/cabal/issues/7379)
- Fix instantiating an indefinite Backpack from Hackage with an inplace package [#6835](https://github.com/haskell/cabal/issues/6835) [#7413](https://github.com/haskell/cabal/pull/7413)

  Previously, cabal-install would always attempt to put instantiations of indefinite packages from Hackage in
  the global package store, even if they were instantiated with inplace packages.  This would not work
  and GHC would complain about packages being missing from the package database.  We have fixed the
  instantiation algorithm to correctly inplace packages in these situations, removing one of the last
  blockers to widespread use of Backpack packages on Hackage.

- removes the warnings for extraneous versions [#7286](https://github.com/haskell/cabal/issues/7286) [#7470](https://github.com/haskell/cabal/pull/7470)
- Alert user and suggest command on spelling mistakes

### Other improvements

- Code organization [#6960](https://github.com/haskell/cabal/pull/6960) [#6963](https://github.com/haskell/cabal/pull/6963) [#6970](https://github.com/haskell/cabal/pull/6970) [#6974](https://github.com/haskell/cabal/pull/6974) [#6975](https://github.com/haskell/cabal/pull/6975)

  - Move Cabal sources into Cabal/src
  - Move cabal-install sources to cabal-install/src/
  - Move doc/ to the top-level of the repository
  - Add stylish-haskell config.

- Documentation improvements [#6813](https://github.com/haskell/cabal/issues/6813) [#6971](https://github.com/haskell/cabal/pull/6971) [#7047](https://github.com/haskell/cabal/pull/7047)
