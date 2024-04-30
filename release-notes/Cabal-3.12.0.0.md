Cabal and Cabal-syntax 3.12.0.0 changelog and release notes
---


### Significant changes

- Add support for asm, cmm, and js sources in executable components [#8639](https://github.com/haskell/cabal/issues/8639) [#9061](https://github.com/haskell/cabal/pull/9061)

    Executable components now support the inclusion of asm, cmm, and js source
    files in a cabal file using the same syntax as is used for these sources
    in library components, similar to how c and c++ sources are supported in
    both types of components. This syntax was already parsed in cabal files,
    but was silently ignored in the build step, so no changes to syntax are
    made.

- Add `--haddock-output-dir` flag to `cabal haddock`. [#8720](https://github.com/haskell/cabal/issues/8720) [#8788](https://github.com/haskell/cabal/pull/8788)

    This flag gives the user full control over the directory where the documentation is placed. It allows both relative and absolute paths.

- Add `--semaphore` option to `./Setup build` interface [#8557](https://github.com/haskell/cabal/pull/8557)

    When `./Setup build --semaphore <SEM>` is called, `ghc` will be called
    with the `-jsem` option. It is the responsibility of the caller of
    `./Setup build` to manage the semaphore according to the GHC Jobserver
    Protocol.

    This low level interface is intended to be called by a high-level tool
    such as `cabal-install` which can create and manage the semaphore
    appropriately.

    The protocol is specified by [GHC Proposal #540](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0540-jsem.rst).

- Add `--promised-dependency` flag to `./Setup configure` interface [#8726](https://github.com/haskell/cabal/pull/8726)

    There is a new flag `--promised-dependency`to allow users to configure a
    package *without* having previously built the dependency.  Instead, we
    promise to the configure phase that we will have built it by the time we
    build the package. This allows us to configure all the packages we intend
    to load into the repl without building any dependenices which we will load
    in the same session, because the promise is satisifed due to loading the
    package and its dependency into one multi-session which ensures the
    dependency is built before it is needed.

    A user of ./Setup configure specifies a promised dependency by using the
    "--promised-dependency" flag with a normal dependency specification. For
    example:

    ```
    '--promised-dependency=cabal-install-solver=cabal-install-solver-3.9.0.0-inplace'
    ```

- Add `--ignore` to `cabal check` [#8587](https://github.com/haskell/cabal/issues/8587) [#9442](https://github.com/haskell/cabal/pull/9442)

    - `Distribution.PackageDescription.Check.Warning` now exports
      `filterPackageChecksById`, this can be used by third-party
      tools to filter warnings.

- Add support for `GHC2024` [#9736](https://github.com/haskell/cabal/issues/9736)

  Support for the `GHC2024` language edition, introduced by GHC 9.10, has been
  added. It can now be used in the `default-language` and `other-languages`
  fields, and will be offered as an option by `cabal init`.

- Remove `initialBuildSteps` from `Distribution.Simple.Build` [#9474](https://github.com/haskell/cabal/pull/9474)

  Calling `initialBuildSteps` to prepare source files for a package is error
  prone, as `initialBuildSteps` only handles things like the paths module
  and nothing else.

  To mimick `initialBuildSteps` behaviour when there is no custom Setup, you
  can call `repl_setupHooks`.

  If you are dealing with a custom setup, you have to invoke
  `./Setup repl --repl-multi-file`.

- Cabal and Cabal-syntax 3.12 support GHC version 8.4.4 and up.

  Support for all previous GHC versions is deprecated.

### Other changes

- `cabal init` should not suggest Cabal < 2.0 [#8680](https://github.com/haskell/cabal/issues/8680)

    'cabal init' no longer suggests users to set cabal-version to less than
    2.0.

- Remove Distribution.Utils.TempTestDir module from Cabal library [#9453](https://github.com/haskell/cabal/issues/9453) [#9454](https://github.com/haskell/cabal/pull/9454)

    This library was only used by internal tests, and now lives in the
    `Cabal-tests` library which is shared across test components.

- PkgConfig individual calls [#9134](https://github.com/haskell/cabal/pull/9134)

    `cabal` invokes `pkg-config` individually for each lib if querying for all
    doesn't return the expected result.

- Split up `Distribution.Simple.Setup` [#8130](https://github.com/haskell/cabal/pull/8130)

  The external interface of 'Distribution.Simple.Setup' has been kept the
  same, but internally it has been broken up into smaller modules.  This
  improves build times in two ways:

    1. GHC is superlinear in the size of files, meaning that splitting up a
       large file can reduce overall compile times.
    2. Breaking up the module allows dependent modules to refine their imports
       to just the parts they require, allowing them to start buildling quicker
       when GHC is run in parrallel make mode ('--ghc-options -j').

- Reimplementing `cabal check` [#7423](https://github.com/haskell/cabal/issues/7423) [#8427](https://github.com/haskell/cabal/pull/8427)

   - `checkPackage` signature has been simplified,
     you do not need to pass a specific configuration of the package, since
     we do not flatten GenericPackageDescription any more.
   - `checkPackageFileNames` has been removed,
     use `checkPackageFiles` instead.
   - `checkPackageFilesGPD` has been introduced,
     a function similar to `checkPackageFiles` that works on
     `GenericPackageDescription`. You do not need to use
     `flattenPackageDescription` anymore.

- Installation of extra-compilation-artifacts directory [#8662](https://github.com/haskell/cabal/pull/8662)

    GHC plugins now can store custom data in the 'extra-compilation-artifacts' directory which gets installed with the package.

- Add option to ./Setup repl to write repl arguments to file [#8726](https://github.com/haskell/cabal/pull/8726)

    The `./Setup repl` command is modified to allow a user to defer starting
    the repl and instead instruct the command to write the necessary build
    flags to a directiory. The option is called `--repl-multi-file <DIR>`.

    This is useful when starting multi-component sessions, as we want to query
    Setup.hs for the arguments which are needed to build each component but
    not for ./Setup to start the repl itself.

- Add Haiku as a known platform [#9006](https://github.com/haskell/cabal/pull/9006)

    Cabal: Distribution now recognises Haiku as a valid platform, and also
    implements Haiku's unique directory layout.

- Installation of .hie files [#8685](https://github.com/haskell/cabal/issues/8685) [#9019](https://github.com/haskell/cabal/pull/9019)

    Hie files generated by GHC are now stored in the
    `extra-compilation-artifacts` directory which gets installed with the
    package.

- Include the GHC "Project Unit Id" in the cabal store path [#8114](https://github.com/haskell/cabal/issues/8114) [#9326](https://github.com/haskell/cabal/pull/9326)

    This allows the use of several **API incompatible builds of the same
    version of GHC** without corrupting the cabal store.

    This relies on the "Project Unit Id" which is available since GHC 9.8.1,
    older versions of GHC do not benefit from this change.

- Fix the platform string for GNU/Hurd [#9434](https://github.com/haskell/cabal/pull/9434)

    Depending whom you ask, GNU/Hurd will be labelled "gnu" or "hurd". The
    autotools use "gnu", so ghc follows this for installed files, even if the
    ghc source code uses OSHurd. We thus need to add the translation between
    the two.

- Use linker capability detection to improve linker use [#9443](https://github.com/haskell/cabal/pull/9443)

    Previously the GHC version number and platform were used as a proxy for
    whether the linker can generate relocatable objects.

    Now, the ability of the linker to create relocatable objects is detected.

- Merge globbing implementations [#5349](https://github.com/haskell/cabal/issues/5349) [#9673](https://github.com/haskell/cabal/pull/9673)

    The common aspects of the globbing functionality between `Cabal` and
    `cabal-install` have been factored out. The only change in the user-facing
    API is that we now record when a glob does not match exactly, but matches
    a directory with that same name, with the new constructor
    `GlobMatchesDirectory` of `GlobResult`.

    To illustrate, this change means that when `foo/dir` is a directory, the
    glob `*/dir/` matches exactly `foo/dir` (as before), but now `*/dir`
    produces `GlobMatchesDirectory` instead of failing.  This allows callers
    to decide whether to allow or discard such inexact matches.

- Document `remote-repo-cache` as implemented. [#8737](https://github.com/haskell/cabal/issues/8737) [#8738](https://github.com/haskell/cabal/pull/8738)

- Deduplicate LD_LIBRARY_PATH when running tests [#8728](https://github.com/haskell/cabal/pull/8728)

- Add support for a number of architectures:

    - RISC-V [#9062](https://github.com/haskell/cabal/pull/9062)
    - 64-bit LoongArch [#9215](https://github.com/haskell/cabal/pull/9215)
    - 64-bit SPARC as a separate architecture [#9445](https://github.com/haskell/cabal/pull/9445)

- Don't report `index.html` file as created, if not created by Haddock [#5120](https://github.com/haskell/cabal/issues/5120) [#9332](https://github.com/haskell/cabal/pull/9332)

- Enable using $ORIGIN in RPATH on GNU/Hurd [#9441](https://github.com/haskell/cabal/pull/9441)


- Make check comply with Hackage requirements [#8897](https://github.com/haskell/cabal/pull/8897)

  - `cabal check` will only return exitcode 1 when the package is not fit
    for Hackage. E.g. it will not error anymore when your `synopsis:` is
    larger than `description:`, just emit a warning.
  - Cabal: Distribution.Client.Check now exports `isHackageDistError`, for
    third-party tools to know if a specific error will preclude a package
    from being uploaded to Hacakge.

- Add language extension `ExtendedLiterals` [#8992](https://github.com/haskell/cabal/pull/8992)

    Adds support for the `ExtendedLiterals` language extension (GHC proposal #451)

- Warn about inconsistent indentation [#8975](https://github.com/haskell/cabal/pull/8975)

    - Make Cabal warn about inconsistent indentation in .cabal files. For
      example warn about somewhat common decreasing indentation like in

    ```cabal
    library
    default-language: Haskell2010
    build-depends: base
    ghc-options: -Wall
    ```

    The change is `readFields` function.

    This is an effect of using `indentOfAtLeast` method/approach: any
    indentation greater than current offset is accepted.

    That behavior is desirable to parsing multiline field contents, but it is
    a bit surprising for fields in sections, which we expect to be aligned.

    Such insonsistency seems to be always a mistake, and it's easy to fix once
    a machine points it out.

- Add `LexBraces` lexer warning [#8577](https://github.com/haskell/cabal/issues/8577)

    `LexBraces` warning is issued when brace delimiting syntax is used.  This
    way, using `readFields'`, a low-lever consumer may decide whether braces
    were used.

    Looking for a brace character in the input is imprecise, as braces can
    occur inside field content.

    This warning is not propagated to parser warnings, so e.g.
    `readGenericPackageDescription` doesn't warn about it.  This is because all
    parser warnings prevent uploads to Hackage, and using braces (or not) is
    a matter of opinion.

- Distinguish `powerpc64le`, by adding `PPC64LE` constructor to type `Arch` [#9534](https://github.com/haskell/cabal/issues/9534) [#9535](https://github.com/haskell/cabal/pull/9535)

    Adds constructor `PPC64LE` to type `Arch` to distinguish architecture
    powerpc64le from powerpc64. Existing constructor `PPC64` now exclusively
    represents powerpc64.

- PkgConfig individual calls [#9134](https://github.com/haskell/cabal/pull/9134)

    `cabal` invokes `pkg-config` individually for each lib if querying for all doesn't return the expected result

- Add language extension `ListTuplePuns` [#8854](https://github.com/haskell/cabal/pull/8854)

    Adds support for the `ListTuplePuns` language extension (GHC proposal #475)

- Add `mkVersionIntervals` for creating a `VersionIntervals` from a list [#9034](https://github.com/haskell/cabal/pull/9034)

    If external tools want to change the version intervals of dependencies, they
    need to be able to create a `VersionRange` from a `[VersionInterval]` list. Adding
    the function `mkVersionIntervals` makes this possible again.

- Add language extension `TypeAbstractions` [#9496](https://github.com/haskell/cabal/issues/9496) [#9502](https://github.com/haskell/cabal/pull/9502)

- Update SPDX License List to version `3.23 2024-02-08` [#9818](https://github.com/haskell/cabal/pull/9818)

  - LicenseId and LicenseExceptionId now conform to SPDX License List
    version 3.23 2024-02-08.
