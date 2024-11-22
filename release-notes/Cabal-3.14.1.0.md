## Cabal and Cabal-syntax 3.14.1.0 changelog

### Significant changes

- Fix build ways for modules in executables [#10418](https://github.com/haskell/cabal/issues/10418) [#10419](https://github.com/haskell/cabal/pull/10419)

  - Modules belonging to executables were being built in too many ways. For instance, if you
  had configured to build profiled library files then your executable modules would also
  be built profiled. Which was a regression in behaviour since `Cabal-3.12`.

- Fix `./setup install` command [#10416](https://github.com/haskell/cabal/issues/10416) [#10417](https://github.com/haskell/cabal/pull/10417)

  - `./setup install` was failing with a `fromFlag NoFlag` error. It is now fixed.

### Other changes

- Add new options from ghc 9.12 [#10468](https://github.com/haskell/cabal/pull/10468)

  - ghc 9.12 adds several new command line options, divided between
    `LANGUAGE`s (already added), warnings, new preprocessor control options,
    and compilation control options. Two options needed to be added to the
    list of options requiring `Int` parameters.

    The new options, excluding warning and language options, are:

    * `-fexpose-overloaded-unfoldings`
    * `-fmax-forced-spec-args=N`
    * `-fno-expose-overloaded-unfoldings`
    * `-fno-object-determinism`
    * `-fobject-determinism`
    * `-fwrite-if-compression=N`
    * `-optCmmP…`
    * `-optJSP…`
    * `-pgmCmmP`
    * `-pgmJSP`

    As they all affect compilation and store hashes, the only necessary
    change was to list the two numeric options so they will be parsed
    correctly. To the best of our understanding, `-pgm*` and `-opt*`
    options are already handled as a group.
