Cabal 3.8.1.0 Changelog
---

### Significant changes

- Split out package `Cabal-syntax` for `.cabal` file syntax and parsing [#7559](https://github.com/haskell/cabal/issues/7559) [#7620](https://github.com/haskell/cabal/pull/7620)

- Unmarked "visibility: public" and "build-depends: pkg:lib" syntaxes as experimental, allowing Hackage upload of packages exposing or using multiple public libraries [#6801](https://github.com/haskell/cabal/issues/6801) [#7286](https://github.com/haskell/cabal/issues/7286) [#8089](https://github.com/haskell/cabal/pull/8089)

- Add code-generators field to test-suite stanza [#4500](https://github.com/haskell/cabal/issues/4500) [#7688](https://github.com/haskell/cabal/pull/7688)

  Test-suite stanzas now may contain a `code-generators:` field that can be used to run executables as preprocessors which take existing locations of library code and cabal-generated ghc build flags, and output new modules for use in the test stanza. This can be used to automatically generate drivers for "discover" style tests, including doctests.

- Windows: redo the fix to breakage caused by new autoconf; the wrong fix made cabal sometimes fail with old autoconf [#7494](https://github.com/haskell/cabal/issues/7494) [#7649](https://github.com/haskell/cabal/issues/7649)

  - Reverts #7510 that failed on Windows when used with pre-generated scripts included in packages such as network, time, process.
  - Adds a subtler fix/workaround for the deficiencies of new autoconf versions on Windows.

- Windows: rewrite paths to configure [#7494](https://github.com/haskell/cabal/issues/7494) [#7649](https://github.com/haskell/cabal/issues/7649)
- Enabled foreign library building on apple silicon [#7837](https://github.com/haskell/cabal/issues/7837) [#8227](https://github.com/haskell/cabal/issues/8227) [#8232](https://github.com/haskell/cabal/pull/8232)

  - Enabled foreign library building on apple silicon
  - Updated error message for foreign library builds on unsupported platforms

- Allow preprocessors to register a reordering [#55](https://github.com/haskell/cabal/issues/55) [#1906](https://github.com/haskell/cabal/issues/1906)

  - Changes the PreProcessor type to include a field ppOrdering so that modules are presented to the preprocessor in the right order (w.r.t. dependencies)

- Use ghc -flink-rts option when available [#7763](https://github.com/haskell/cabal/issues/7763) [#7764](https://github.com/haskell/cabal/pull/7764) [#8111](https://github.com/haskell/cabal/pull/8111)

  Previously Cabal did quite some headstands to link against libHSrts.
  Note only this is complex but it couples very tightly to GHC's implementation.
  Thankfully, as of GHC 9.0 GHC provides a -flink-rts flag for precisely this purpose.
  Use it when available.

  It fixed a bug which make Cabal unusable to build foreign libraries for windows and ghc 9.0 or 9.2.
  See <https://gitlab.haskell.org/ghc/ghc/-/issues/20520>

- Remove the Generic instance of LicenseId [#8074](https://github.com/haskell/cabal/issues/8074) [#8117](https://github.com/haskell/cabal/pull/8117)

  - The `Generic` instance of LicenseId is removed, due to the extraordinary amount of memory required to compile
    the Generic instances of large sum types in GHC.


### Other changes

- Handle option argument parse errors without 'error' [#7573](https://github.com/haskell/cabal/issues/7573) [#7579](https://github.com/haskell/cabal/pull/7579)

  - Errors parsing arguments such as `-v=3` no longer result in
    stack traces.
  - `Distribution.ReadE.readEOrFail` was removed.

- Fix bugs in filename validation [#7426](https://github.com/haskell/cabal/issues/7426) [#7429](https://github.com/haskell/cabal/pull/7429) [#7479](https://github.com/haskell/cabal/pull/7479)

  - Relative paths starting with single-character directories are now properly allowed
  - `cabal check` rejects paths which are invalid on Windows or in tarballs

- Fix test --enable-coverage for multi-package projects [#5213](https://github.com/haskell/cabal/issues/5213) [#5433](https://github.com/haskell/cabal/issues/5433) [#7200](https://github.com/haskell/cabal/issues/7200) [#7250](https://github.com/haskell/cabal/pull/7250) [#7467](https://github.com/haskell/cabal/pull/7467)

  - Fix `cabal test --enable-coverage` for multi-package projects by invoking `hpc markup --include=` and listing only modules of libraries of the single tested package.

- Make cabal respect setgid permission bit when creating directories [#7560](https://github.com/haskell/cabal/issues/7560) [#7572](https://github.com/haskell/cabal/pull/7572)

  - Previously cabal would override an admin's choice to use setgid on the
    .cabal directory by using hardcoded file and directory modes upon
    creation. Instead we now take what the system decides and only add to
    that set of permissions.

- Handle conditionals in duplicate module checks [#4629](https://github.com/haskell/cabal/issues/4629) [#7525](https://github.com/haskell/cabal/issues/7525) [#7616](https://github.com/haskell/cabal/pull/7616)

  Improves `cabal check` logic for duplicate modules to take into account conditional branches. If a module appears on both sides of an `if/else` clause in a cabal file, it is now correctly not reported as a duplicate.

- Allow glob-star matches with literal filenames (no extensions) [#5883](https://github.com/haskell/cabal/issues/5883) [#8005](https://github.com/haskell/cabal/pull/8005)

  - Cabal file glob syntax extended to allow matches of the form dir/**/FileNoExtension

- Flatten duplicate warnings about experimental features [#8023](https://github.com/haskell/cabal/pull/8023)

  - Make builds that use experimental Cabal language features less noisy. At -v1
    (normal) we show just first instance of use of experimental cabal language
  features, along with count of further occurences in the same file.

- Fix Cabal not finding public sublibraries of installed packages [#7270](https://github.com/haskell/cabal/issues/7270) [#8089](https://github.com/haskell/cabal/pull/8089)

  Fix bug in lookup of installed sublibraries with nonexact dependencies (ie. when
  not using --exact-configuration). That is the mode of operation of distribution
  packages, so this should allow distros to package Cabal packages with multiple
  libraries.

- Make type field optional for tests and benchmarks [#7459](https://github.com/haskell/cabal/issues/7459) [#8115](https://github.com/haskell/cabal/pull/8115)

  Allow the omission of the `type` field in `test-suite` and `benchmark` stanzas
  when the type can be inferred by the presence of `main-is` or `test-module`.

- Improve error message for empty --allow-newer=  [#7740](https://github.com/haskell/cabal/issues/7740) [#8140](https://github.com/haskell/cabal/pull/8140)

  Instead of internal error, the message now explains that empty argument for

  argument) means.

- Fix haddock command via Setup.hs for internal libraries [#1919](https://github.com/haskell/cabal/issues/1919) [#7827](https://github.com/haskell/cabal/pull/7827)
- `ghc-options` and `--with-gcc` are now passed to GHC when compiling C and C++ sources [#4439](https://github.com/haskell/cabal/issues/4439) [#5440](https://github.com/haskell/cabal/pull/5440) [#7874](https://github.com/haskell/cabal/pull/7874)
- 'cabal check' to fail when no upper bounds for base or Cabal are present in setup dependencies [#4683](https://github.com/haskell/cabal/issues/4683) [#5370](https://github.com/haskell/cabal/pull/5370) [#7409](https://github.com/haskell/cabal/pull/7409)
- --repl-options doesn’t split on whitespace [#6190](https://github.com/haskell/cabal/issues/6190) [#7799](https://github.com/haskell/cabal/pull/7799)
- '--repl-no-load' option skips startup modules load in REPL [#7541](https://github.com/haskell/cabal/issues/7541) [#7578](https://github.com/haskell/cabal/pull/7578)
- Add -c alias for --constraint command line flag [#7765](https://github.com/haskell/cabal/issues/7765) [#7766](https://github.com/haskell/cabal/pull/7766)
- Add fields extra-libraries-static and extra-lib-dirs-static [#6688](https://github.com/haskell/cabal/issues/6688) [#7399](https://github.com/haskell/cabal/issues/7399) [#7536](https://github.com/haskell/cabal/pull/7536)
- Paths passed to GHC are now relative to the current working directory
- raise lower bound of process and remove compatibility shims [#7922](https://github.com/haskell/cabal/pull/7922)
- changes the cabal check warning about long synopsis, so it warns only synopsis bigger than the set size [#7932](https://github.com/haskell/cabal/issues/7932) [#7933](https://github.com/haskell/cabal/pull/7933)
- Remove bootstrapping plan files from version control, and simplify bootstrap update Makefile targets. [#7949](https://github.com/haskell/cabal/pull/7949)
- Remove deprecated file finding functions [#7955](https://github.com/haskell/cabal/pull/7955)
- Support GHC 9.4's `clang`-based Windows toolchain [#8062](https://github.com/haskell/cabal/pull/8062)
- Add support for the 64-bit S390X architecture [#8065](https://github.com/haskell/cabal/pull/8065)
- Add wasm32-wasi to recognized arch/os [#8096](https://github.com/haskell/cabal/pull/8096)

- *Cabal-syntax* Derive Eq for DependencyMap [#7849](https://github.com/haskell/cabal/issues/7849) [#8061](https://github.com/haskell/cabal/pull/8061)
- *Cabal-syntax* Allow trailing whitespace after flags in cabal.project [#7279](https://github.com/haskell/cabal/issues/7279) [#8006](https://github.com/haskell/cabal/pull/8006)
- *Cabal-syntax* Update the SPDX License List to version 3.16 [#8126](https://github.com/haskell/cabal/issues/8126) [#8127](https://github.com/haskell/cabal/pull/8127)

  The LicenseId and LicenseExceptionId types are updated to reflect the SPDX License List version 3.16 (2022-02-06).

- *Cabal-syntax* Future compat for liftA2 being exported from Prelude [#8823](https://github.com/haskell/cabal/pull/8823)

  - liftA2 will be exported from Prelude in the future(see https://github.com/haskell/core-libraries-committee/issues/50).
      Cabal-syntax was future-proofed to have no warnings when that happens.

- *Cabal-syntax* cabal init -n: avoid extra blank lines [#8236](https://github.com/haskell/cabal/issues/8236) [#8292](https://github.com/haskell/cabal/pull/8292)

- Added fields :pkg-field:`extra-libraries-static` and
  :pkg-field:`extra-lib-dirs-static` to allow Haskell libraries to remember
  linker flags needed for fully static linking of system libraries into executables. [#7536](https://github.com/haskell/cabal/pull/7536)

- The existing field :pkg-field:`pkgconfig-depends` can used to append the relevant
  output of ``pkg-config --libs --static`` to these new fields automatically.
  When :pkg-field:`extra-libraries-static` is not given, it defaults to
  :pkg-field:`extra-libraries`. When :pkg-field:`extra-lib-dirs-static` is not
  given, it defaults to :pkg-field:`extra-lib-dirs`. [#7536](https://github.com/haskell/cabal/pull/7536)

- Remove the GHC version upper bound when deciding whether to filter GHC arguments

  - Previously, for unknown new versions of GHC, it was not filtering GHC arguments at all, while now it filters them in the same way as for the last known GHC version. This seems a better default and it's one less place to update at release time. Perhaps erroring out or emitting a warning would be safer, but we already emit a general warning elsewhere.
