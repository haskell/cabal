Cabal and Cabal-syntax 3.14.2.0 changelog and release notes
---

### Significant changes

- GHC arguments are no longer swallowed by enabling documentation [#10782](https://github.com/haskell/cabal/issues/10782) [#10783](https://github.com/haskell/cabal/pull/10783)

  Arguments to GHC are now properly concatenated with `-haddock` if documentation is enabled.

- Honour the `-working-dir` flag when executing testsuite executables. [#10704](https://github.com/haskell/cabal/issues/10704) [#10725](https://github.com/haskell/cabal/pull/10725)

  Honour the `-working-dir` flag when executing testsuite executables. This fixes a
  regression in Cabal-3.14.0.0.

- Fix duplicate environment variables in test and benchmark runs [#10718](https://github.com/haskell/cabal/issues/10718) [#10827](https://github.com/haskell/cabal/pull/10827)

  Cabal no longer creates duplicate environment variables when running test
  suites, benchmarks, or internal executables. Previously, when setting up the
  environment for these processes, Cabal would append the overridden environment
  to the existing environment, creating duplicates of the same variable.

- Set `<pkgname>_datadir` to an absolute path when running tests [#10717](https://github.com/haskell/cabal/issues/10717) [#10830](https://github.com/haskell/cabal/pull/10830)

  Fix a regression where `<pkgname>_datadir` was set to a relative path. This
  caused issues when running testsuites which changed the working directory and
  accessed datafiles.

### Other changes

- Remove descriptions for sandbox in replCommand [#10482](https://github.com/haskell/cabal/issues/10482) [#10493](https://github.com/haskell/cabal/pull/10493)

  Text provided by `Setup.hs repl --help` contained outdated information concerning sandbox, which have now been removed.

- Adds more version range checks to `cabal check`. [#9806](https://github.com/haskell/cabal/issues/9806) [#10554](https://github.com/haskell/cabal/pull/10554)

  For dependencies, warns about and checks that version range bounds for:

  - lower bounds are inclusive, don't use (>)
  - upper bounds are exclusive, don't use (<=)
  - upper bounds don't have trailing zeros, don't end with (*.0).

- OpenBSD `--strip-unneeded` sometimes strips too much [#10616](https://github.com/haskell/cabal/pull/10616)

  OpenBSD's `--strip-unneeded` thinks some symbols are unneeded that are in fact
  needed when C bits are involved, so suppress its use.

  Taken from the OpenBSD ports repo (https://cvsweb.openbsd.org/cgi-bin/cvsweb/~checkout~/ports/lang/ghc/patches/patch-libraries_Cabal_Cabal_Distribution_Simple_Program_Strip_hs);
  brought to our attention by maerwald.


- Suppress ghc-pkg warnings emitted when an empty packages is being registered [#9997](https://github.com/haskell/cabal/pull/9997)

  When a package contains a library that has no modules, `./Setup register`
  tries to register the library with fields `haddock-interfaces:` and
  `haddock-html:` pointing at non-existent files or directories, and `ghc-pkg
  register` warns about them.

  To suppress the warnings prevent these fields from being generated when
  there are no modules.

