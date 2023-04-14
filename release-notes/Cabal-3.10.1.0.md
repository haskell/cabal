Cabal and Cabal-syntax 3.10.1.0 changelog and release notes
---

Release 3.10.1.0 of cabal now sets the `--enable-documentation` option by default
when running `cabal haddock` (https://github.com/haskell/cabal/issues/7462).
If the new default does not fit your workflow, pass an explicit `--disable-documentation`
option or consider (helping to implement) the other workarounds proposed
in https://github.com/haskell/cabal/issues/8725.

This version of cabal introduces support for JS cross-compilation (https://github.com/haskell/cabal/pull/8636).
We've seen related Windows segfaults on CI, so please kindly report if you can reproduce either
cabal or GHC segfaults when cross-compiling for JS or compiling normally but with any files
listed in the `js-sources` field of a .cabal file.

This release of cabal also fixes a lot of bugs in cabal 3.8.1.0 and not all of the fixes
are listed here.


### Significant changes

- Add support for the XDG Base Directory Specification [#680](https://github.com/haskell/cabal/issues/680) [#7386](https://github.com/haskell/cabal/pull/7386)

  Cabal/cabal-install now uses the XDG Base Directory Specification to
  store configuration, caches, and the store.  Specifically,
  `$XDG_CONFIG_HOME/cabal` stores the configuration file,
  `$XDG_CACHE_HOME/cabal` stores downloaded packages and similar, and
  `$XDG_STATE_HOME/cabal` mainly contains the store of compiled
  packages.  Further, `cabal install` will put executables in
  `~/.local/bin` by default.

  The `dist`/`dist-newstyle` directories are not affected.

  On Windows, these XDG paths are mapped to other (hopefully)
  appropriate locations.  See the Cabal User Guide for information.

  If the `CABAL_DIR` environment variable is set, the indicated
  directory will be used to store all Cabal-related files, as in
  previous versions.

  **Backwards compatibility**: If `~/.cabal` already exists, this will be
  interpreted as `CABAL_DIR=~/.cabal`.  This means that upgrading on an
  existing system already using cabal-install should not cause any
  change in behaviour.  An existing system can be migrated by deleting
  `~/.cabal` (possibly copying `~/.cabal/config` to
  `~/.config/cabal/config` first).

- Add `PackageInfo_` module [#8534](https://github.com/haskell/cabal/pull/8534)

  - Add `PackageInfo_` module to embed portable package-related informations (issue #3909)

- *Cabal-syntax* Add language extensions `DeepSubsumption` and `TypeData` [#8493](https://github.com/haskell/cabal/pull/8493)

  - adds support for the `DeepSubsumption` language extension ([GHC proposal #511](https://github.com/ghc-proposals/ghc-proposals/pull/511))
  - adds support for the `TypeData` language extension ([GHC proposal #106](https://github.com/ghc-proposals/ghc-proposals/pull/106))

- Add support for GHC 9.4+ `-fprof-late` flag. [#8528](https://github.com/haskell/cabal/pull/8528)

- This adds the new `late-toplevel` (and its alias `late`) profiling-detail option which enables `-fprof-late`-based automatic cost centre annotations for GHCs that support it ([earliest is GHC 9.4.1](https://downloads.haskell.org/ghc/9.4.1/docs/users_guide/profiling.html#ghc-flag--fprof-late)).


### Other changes

- Remove "'-main-is' is not portable" check [#8646](https://github.com/haskell/cabal/issues/8646) [#8651](https://github.com/haskell/cabal/pull/8651)

  `cabal check` no longer complains about `-main-is` flag in `ghc-options`

- Fix generation of Path_ modules with relocatable [#8219](https://github.com/haskell/cabal/issues/8219) [#8220](https://github.com/haskell/cabal/pull/8220)

  The generation of the functions `minusFileName` and `splitFileName`
  are now in the same conditional block as their call,
  preventing generation of inconsistent Paths_ files
  where those functions are used but not defined.

- Remove warning on option -threaded when building libraries [#774](https://github.com/haskell/cabal/issues/774) [#8431](https://github.com/haskell/cabal/issues/8431) [#8432](https://github.com/haskell/cabal/pull/8432)

  - Accompanied by option `-flink-rts`, option `-threaded` defines the flavour of
    the ghc RTS library the built library will be linked against. Note that bare
    ghc does not warn when option `-threaded` is used for building a library
    either.
  - Note that the changes require modification of the regression check for issue
    #774 which can be regarded as a proper test for this PR.

- Add warning about expensive globs [#5311](https://github.com/haskell/cabal/issues/5311) [#8441](https://github.com/haskell/cabal/pull/8441)

  - Now cabal check will emit a warning when package uses
  recursive globs starting at root of the project

- Order `extra*` stanzas [#8458](https://github.com/haskell/cabal/issues/8458) [#8499](https://github.com/haskell/cabal/pull/8499)

  Ensure that `extra-src-dirs`, extra sources, and extra other modules all are added using `ordNub` rather than incidentally alphabetized.

- Prepend rather than append `extra-prog-path` [#6304](https://github.com/haskell/cabal/issues/6304) [#8506](https://github.com/haskell/cabal/pull/8506)

  Prepends the `extra-prog-path` to the system path rather than appending, to allow binaries in the extra path to override defaults.

- don't send non-extant `extra-lib-dirs` to GHC [#6492](https://github.com/haskell/cabal/issues/6492) [#8510](https://github.com/haskell/cabal/pull/8510)

  If an extra-libs-dir does not exist, it does not get sent to ghc, which can error on windows.

- Improve mutually recursive unit identifier error message [#8582](https://github.com/haskell/cabal/pull/8582)

  Improves the error message in case of mutually recursive unit identifiers
  by specifying the name of the identifier, the name of the signature, and a suggestion
  to check the 'build-depends:' section.

- Specify default exe extension on wasm32 to be .wasm [#8633](https://github.com/haskell/cabal/pull/8633)

  Specify default exe extension on wasm32 to be .wasm, following the convention in other WebAssembly toolchains.

- Support `js-sources` with GHC, not only with GHCJS [#8636](https://github.com/haskell/cabal/pull/8636)

  - Take into account js-sources when building library components with GHC
  - Missing support for js-sources in executable components is tracked in #8639

- Tiny refactor of how Cabal handles configure scripts [#8648](https://github.com/haskell/cabal/pull/8648)

  None of this is visible downstream

  - Remove needless parameter on one private function.

  - Move another internal function (and ones that only it uses from the same module) to new private module.

- Warn if expected files are omitted from extra-doc-files [#3964](https://github.com/haskell/cabal/issues/3964) [#8657](https://github.com/haskell/cabal/pull/8657)

  - Emit a warning if there exist a “changelog” file at the root of the
    package which is not included in any field.
  - Emit a warning if a “changelog” file at the root of the package is included
    in a field different from “extra-doc-files” (Cabal spec >= 1.18) or
    “extra-source-files” (spec < 1.18).

- Disallow GHC <8.0 for  [#7531](https://github.com/haskell/cabal/issues/7531) [#8715](https://github.com/haskell/cabal/issues/8715) [#8794](https://github.com/haskell/cabal/pull/8794)

  Disallow GHC <8.0 by restricting the version of base that can be used to at least 4.9

- Avoid spurious warnings from -with-rtsopts [#4255](https://github.com/haskell/cabal/issues/4255) [#8183](https://github.com/haskell/cabal/pull/8183)

- Do not print "up to date" for commands unless running "cabal build" [#4994](https://github.com/haskell/cabal/issues/4994) [#8569](https://github.com/haskell/cabal/pull/8569)

- The `configure` script of `build-type: configure` packages now has access to the flag assignment of the package being built via the `CABAL_FLAGS` and `CABAL_FLAG_<flag>` environment variables [#8564](https://github.com/haskell/cabal/issues/8564) [#8565](https://github.com/haskell/cabal/pull/8565)

- Export pretty printer `ppPackageCheck` from [#8311](https://github.com/haskell/cabal/pull/8311)

- Add check for upper bound on any dependency in `cabal check` [#8291](https://github.com/haskell/cabal/issues/8291) [#8339](https://github.com/haskell/cabal/pull/8339)

- *Cabal-syntax* Relax language selection parsing in `cabal init` [#8278](https://github.com/haskell/cabal/issues/8278) [#8281](https://github.com/haskell/cabal/pull/8281)
