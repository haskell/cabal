Cabal and Cabal-syntax 3.18.0.0 changelog and release notes
---

### Significant changes

- Do not delete duplicate C header files [#11176](https://github.com/haskell/cabal/issues/11176) [#11733](https://github.com/haskell/cabal/pull/11733)

  PR 4874 introduced a change which removes C headers that duplicate
  `autogen-includes`.  This is unacceptable behavior, and in addition can be
  exploited on Windows to remove any file on the system. Issue a warning about
  undefined C compiler behavior instead.

  Original PR: https://github.com/haskell/cabal/pull/4874/changes#diff-e3cd8c042e1d9d4f3d54c4ec03a508cc2e61598aa7f88033d1cf847e5b712647R1658

  Security advisory for arbitrary file removal: https://haskell.github.io/security-advisories/advisory/HSEC-2026-0006.html

- Pass *-options and -pgmc gcc to GHC when compiling ordinary Haskell sources [#4435](https://github.com/haskell/cabal/issues/4435) [#9801](https://github.com/haskell/cabal/issues/9801) [#10969](https://github.com/haskell/cabal/pull/10969)

  `cc-options`, `cxx-options`, `jspp-options`, `asm-options`, `cmm-options`,
  `ld-options`, `cpp-options` should be always passed when invoking GHC,
  similarly as `ghc-options` should be always used when invoking `ghc` -
  regardless of what is the intention of a particular GHC-call.  GHC might use
  or not use the options, Cabal cannot know and should not guess.

  `cc-options` and `cxx-options` still need to be separated (C/C++) for
  versions below 8.10.

- Cabal library support for logging handles [#9987](https://github.com/haskell/cabal/issues/9987) [#11077](https://github.com/haskell/cabal/pull/11077)

  The Cabal library now supports setting the handles used for logging, as
  opposed to always using stdout & stderr.

  To achieve this, the `Verbosity` data type has been modified:

    1. The old `Verbosity` data type is now `VerbosityFlags`. This consists of
       verbosity & logging information that can be passed via the command-line
       interface.
    2. The new `Verbosity` data type consists of `VerbosityFlags` together with
       `VerbosityHandles`, which store the handles used for logging.
       As `Handle`s cannot be serialised, neither can we serialise this new
       `Verbosity`.

  The end result is that functions such as `createDirectoryIfMissingVerbose` or
  `runProgramInvocation`, which take a `Verbosity` argument, now support logging
  to arbitrary handles. Their type signature remains textually unchanged, as it is
  the `Verbosity` type itself that has changed.

  Several additional changes have been made in relation to the `VerbosityFlags`
  data type (which, recall, is what `Verbosity` used to be):

    1. The `Ord` instance of `VerbosityFlags` has been removed. To compare
       verbosity levels, use the `Ord` instance on `VerbosityLevel` via
       `verbosityLevel :: Verbosity -> VerbosityLevel`.
    2. The `Eq` instance of `VerbosityFlags` now takes into account all the fields,
       and not only the verbosity level.
    3. The `Enum` and `Bounded` instances of `VerbosityFlags` have been removed.
       If you were using these, you might want to consider using the `Enum` and
       `Bounded` instances of `VerbosityLevel` instead, in conjunction with
       the new function `mkVerbosityFlags :: VerbosityLevel -> VerbosityFlags`.

  In addition, the `modifyVerbosity` function has been removed. It allowed
  arbitrarily changing the verbosity level, which is undesirable in general (e.g.
  in practice one wants the "silent" verbosity level to remain "silent"). To
  migrate, one should instead use the existing `moreVerbose`, `lessVerbose`
  combinators, or the new `makeVerbose` function which turns "normal" verbosity
  into "verbose" verbosity.


  Users of the command-line interface do not substantially benefit from this
  change, as the logging handles continue to be set for the spawned process, e.g.

  ```hs
    Process.createProcess $
      (Process.proc ...)
        { Process.std_out = customHandle1, Process.std_err = customHandle2 }
  ```

  To migrate custom `Setup` scripts and `SetupHooks` hooks, in the typical
  situation in which one retrieves the verbosity from flags (such as `ConfigFlags`
  or `BuildFlags`), one can define the following compatibility helper:

  ```hs
  mkVerbosityCompat
    ::
  #if MIN_VERSION_Cabal(3,17,0)
      Flag VerbosityFlags
  #else
      Flag Verbosity
  #endif
    -> Verbosity
  mkVerbosityCompat v =
  #if MIN_VERSION_Cabal(3,17,0)
    mkVerbosity defaultVerbosityHandles $
  #endif
    fromFlag v
  ```

  This means that code such as:

  ```hs
  doSomething :: BuildFlags -> IO ()
  doSomething flags = do
    let verbosity = fromFlag $ buildVerbosity flags
    createDirectoryIfMissingVerbose verbosity True dir
    runProgramInvocation verbosity prog
    ...
  ```

  will become:

  ```hs
  doSomething :: BuildFlags -> IO ()
  doSomething flags = do
    let verbosity = mkVerbosityCompat $ buildVerbosity flags
    createDirectoryIfMissingVerbose verbosity True dir
    runProgramInvocation verbosity prog
    ...
  ```

- Directly call in-library functions to build packages [#11703](https://github.com/haskell/cabal/pull/11703)

  The way `cabal-install` builds packages has been significantly overhauled. In
  most circumstances, `cabal-install` will directly call `Cabal` library functions
  to build packages:

    - We no longer need `cabal-install` to act as a Setup (the `--act-as-setup`
      flag). We used to need this to set the working directory and to redirect
      logging output, but that can now be done via `Cabal` library functions.
    - Packages with `build-type: Hooks` are now also built via `Cabal` library
      functions instead of the `Setup.hs` interface. `cabal-install` achieves this
      by building an external hooks executable with which it communicates to
      run `SetupHooks`.

  The main upside of this change is that we waste less time re-running the entire
  `Cabal` `configure` step; instead `cabal-install` directly starts off with
  the information it already knows (compiler, versions of dependencies given by
  the solver, flag assignment, etc). This necessitated refactoring the `Cabal`
  `configure` code in order to skip running the unnecessary initial steps that
  are made redundant by the information from `cabal-install`'s `ElaboratedReadyPackage`.

  There should be no outward-facing change in behaviour beside speeding up the
  `configure` step.

- Add `--enable-library-bytecode` flag [#11188](https://github.com/haskell/cabal/issues/11188) [#4560](https://github.com/haskell/cabal/pull/4560)

  Introduce the `--enable-library-bytecode` flag to build bytecode libraries
  next to the usual object and shared artifacts. Bytecode libraries are useful
  when asking GHCi to use bytecode for library dependencies (e.g. `-fprefer-byte-code`) and are
  currently only available with GHC 9.15 or newer; the flag is ignored on older
  compilers.

  Implements the Cabal Proposal: [Cabal Support for Bytecode Objects and Bytecode Libraries](https://github.com/haskell/cabal-proposals/blob/master/proposals/bytecode-files.md).

- Decomission `build-type: Make` [#11610](https://github.com/haskell/cabal/issues/11610) [#11894](https://github.com/haskell/cabal/pull/11894)

  `build-type: Make` has not been functional since at least version 3.4, and
  we communicate this more clearly now. Cabal files with `build-type: Make`
  remain parseable unless they specify `cabal-version: 3.18` or newer.

  `Cabal-syntax` package retains ability to parse `build-type: Make` as long
  as the package file specifies Cabal format prior to 3.18. But for packages
  specifying `cabal-version: 3.18` or newer `build-type: Make` is no longer a
  valid syntax.

  `Cabal`-the-library no longer contains `Distribution.Make` module.

  `cabal-install` used to fail in the middle of compilation process with a
  cryptic error about `Unrecognised flags`. Now it will say it clear that
  `build-type: Make is no longer supported`.

- Replace cabal project parsing with `parsec` parser [#6101](https://github.com/haskell/cabal/issues/6101) [#7748](https://github.com/haskell/cabal/issues/7748) [#8889](https://github.com/haskell/cabal/pull/8889)

  Replaced the legacy cabal.project parser with a new implementation based on the same
  parsing infrastructure as cabal files.

  The new parser replicates the grammar of the legacy parser, ensuring that it
  generates identical `ProjectConfig` values.  The implementation leverages
  existing Parsec infrastructure, including FieldGrammar and other utilities
  from the .cabal file parser.  The error messages are now more accurate and
  include the line and column of the error.

  There is a new flag
  `--project-file-parser=<legacy|default|parsec|fallback|compare>` which can
  be used to select the parser to use.

  * `legacy` - the old parser
  * `default` - the default, by default the `fallback` strategy is used unless you have compiled `cabal-install` with `-f+legacy-comparison`.
  * `parsec` - the new parser using parsec
  * `fallback` - the new parser using parsec, but falling back to the old parser if it fails
  * `compare` - the new parser using parsec, but comparing the results with the old parser

  In the next release we plan to remove the legacy parser.

  `Distribution.Parsec.Error` now exports `PErrorWithSource` and
  `showPErrorWithSource`.  `PErrorWithSource` replaces `PError` as the
  return type of `runParseResult`.

  `readProjectConfig` now requires an additional `ProjectFileParser`
  parameter.

- Fixed build failure with absolute paths in `BuildInfo` `cSources` [#12005](https://github.com/haskell/cabal/issues/12005) [#11998](https://github.com/haskell/cabal/pull/11998)

    Cabal no longer fails the build when hooks append absolute symbolic paths
    to `BuildInfo`'s `cSources` field.  Previously, appending an absolute path
    would cause a build failure at the program link stage, because the
    implementation expected only relative paths.  This conflicted with
    `BuildInfo`'s `cSources` type, which permits absolute paths.  The
    generated cSources must be placed in the component's `autogen` directory
    obtained through `autogenComponentModulesDir`.


### Other changes

- Better document Cabal's `--semaphore=SEMAPHORE` option in light of `semaphore-compat-2.0.0` [#12022](https://github.com/haskell/cabal/pull/12022)

    - Cabal's `--semaphore=SEMAPHORE` option is better documented in the user's guide and in `--help`.

- Change the input type for the `distProjectFile` function [#11995](https://github.com/haskell/cabal/pull/11995)

    Change the `DistDirLayout` record's `distProjectFile` field type. It was a
    function from `String` to `FilePath` but is now a function from `ProjectFileKey`
    to `FilePath`. The input sum type to this function has cases for the main
    project file, the `.local` file and the `.freeze` file.

    Other functions that were taking `String` for the extension or description of
    these project files now take a `ProjectFileKey` instead;

        - `readProjectFileSkeleton`
        - `readProjectFileSkeletonLegacy`
        - `readProjectFileSkeletonParsec`
        - `readProjectFileSkeletonFallback`
        - `readProjectFileSkeletonCompare`

    Remove the `distProjectFileMain` function.

- Drop support for anything below GHC 8.0.0, base 4.9.0.0, Cabal 1.24.0.0 [#11630](https://github.com/haskell/cabal/pull/11630)

  Drop support for anything below GHC 8.0.0, base 4.9.0.0, Cabal 1.24.0.0
  These versions were no longer tested and likely non-functional.

- Remove legacy support for GHC < 7.6 [#11710](https://github.com/haskell/cabal/pull/11710)

  Removed compatibility logic for GHC versions prior to 7.6. This simplifies
  package database stack handling and unifies the flag generation logic in
  `Distribution.Simple.GHC` and `Distribution.Simple.Program.GHC`.

- Adding an explicit pgmc flag for interaction with GHC [#11713](https://github.com/haskell/cabal/pull/11713)

  Added an explicit `pgmc` flag to Cabal's GHC invocation logic. This allows
  for reliable and consistent passing of `cc-options`, `ld-options`, and
  `cpp-options` to GHC by removing conditional filtering of these arguments.

- Add explicit `pgmcxx` flag for GHC invocation [#11763](https://github.com/haskell/cabal/pull/11763)

  Add an explicit `pgmcxx` flag to Cabal's GHC invocation logic. This aligns
  with the existing `pgmc` behavior and accounts for the GHC 9.4+ toolchain
  requirements, resolving inconsistencies in how C++ compiler options are
  handled.

  This change allows users to remove `extra-libraries: stdc++` from their
  configuration files, as `g++` automatically handles C++ standard library
  linking, unlike the legacy approach of invoking C++ via `gcc`.

- Build dynamic libraries when needed for intra-package dependencies [#7684](https://github.com/haskell/cabal/issues/7684) [#11791](https://github.com/haskell/cabal/pull/11791)

  When building a package with a sublibrary, Cabal now properly takes into
  account intra-package dependencies when deciding whether each sublibrary
  should be built in the dynamic way (e.g. because another library that
  depends on it uses TemplateHaskell or QuasiQuotes).

- Only write setup-config once [#11619](https://github.com/haskell/cabal/issues/11619) [#11949](https://github.com/haskell/cabal/pull/11949)

  We now ensure that the `setup-config` file, written by
  `writePersistBuildConfig`, is only ever written once in every code path.

- Improve Backpack broken-package error to show unfilled signatures [#11669](https://github.com/haskell/cabal/pull/11669)

  When an indefinite Backpack package is installed separately (e.g. via
  nix `callCabal2nix`), configuring a consumer that depends on it would
  show an opaque hashed UnitId like
  `framework-0.1.0.0+95RTb42ZWxa9J13cUStM0q`. The error now shows the
  package name, which signatures are unfilled, and advises rebuilding
  in the same cabal project so cabal can fill them.

- Caching of `InstalledPackageIndex` [#11767](https://github.com/haskell/cabal/pull/11767)

  `cabal-install` now keeps a running `InstalledPackageIndex` that it updates
  as packages in a project get built. This allows skipping expensive `ghc-pkg`
  calls when configuring each package (within `computePackageInfo` in the
  Cabal `configure` function).

  To enable this, a new function in the `Cabal` library,
  `computePackageInfoFromIndex` , has been extracted from the
  `computePackageInfo` function (all inside `Distribution.Simple.Configure`).
  This allows passing an explicit `InstalledPackageIndex` instead of querying
  `ghc-pkg` to obtain it.

- Pre-configure compiler program database [#11768](https://github.com/haskell/cabal/pull/11768)

  The compiler program database, containing `ghc`, `ghc-pkg`, `haddock` and
  various toolchain programs (such as `ar`, `ld`) is now configured ahead of
  time within `cabal-install`, so that we don't have to re-configure all of
  those programs once for every package.

  See the pre-existing Note [Caching the result of configuring the compiler]
  in Distribution.Client.ProjectPlanning and Note [Constructing the ProgramDb]
  in Distribution.Client.SetupWrapper for additional details.

  This required a tiny change to `Cabal` to define and expose
  `clearUnconfiguredPrograms :: ProgramDb -> ProgramDb` for use in
  `cabal-install`.

- Recompilation checking for `SetupHooks` pre-build rules [#11730](https://github.com/haskell/cabal/issues/11730) [#11731](https://github.com/haskell/cabal/pull/11731)

  Pre-build rules are now only re-run when stale, according to the conditions
  described in the [SetupHooks API](https://hackage.haskell.org/package/Cabal-hooks/docs/Distribution-Simple-SetupHooks.html). That is, a rule is re-run if any of the following conditions are
  satisfied:

    - The rule is new, or
    - A dependency of the rule is stale.
      That is, either we have re-run another rule that this rule depends on,
      or one of the file inputs to the rule is newer than the oldest output of the
      rule (or the rule output doesn't exist at all), or
    - The rule itself has changed, e.g. the parameters stored in `RuleData`
      have changed.

  In particular, Cabal will now write per-component caches of pre-build rules
  in order to compute which rules have changed between runs, with file name
  "setup-hooks-rules.cache".

- Deduplicate `hsc2hs` command-line arguments [#11005](https://github.com/haskell/cabal/pull/11005)

  Fix a problem where `hsc2hs` becomes non-operational on packages with
  lots of dependencies and projects that specify lots of
  `extra-include-dirs` or `extra-lib-dirs`.

- Allow relocatable builds on FreeBSD [#11042](https://github.com/haskell/cabal/pull/11042)

  Properly enables relocatable builds on FreeBSD, just like on
  OSX and Linux.

- Add `NFData` instances [#11097](https://github.com/haskell/cabal/pull/11097)

  Add `NFData` instances necessary for HLS plugins to function.

- Remove `Distribution.Compat.MonadFail` [#11396](https://github.com/haskell/cabal/pull/11396)

  Remove obsolete compatibility layer of `Distribution.Compat.MonadFail`, use
  `Control.Monad.Fail` instead.

- Fix the OS string encoding for GNU/Hurd [#11401](https://github.com/haskell/cabal/pull/11401)

  Following [#9434](https://github.com/haskell/cabal/pull/9434/), and as seen
  in the various `gnu_HOST_OS` uses in the GHC source code, it is expected that
  GNU/Hurd is advertised as "gnu", so the OS String encoding for OSHurd was
  corrected to "gnu".

- Clean up `Distribution.Compat.Semigroup` [#11403](https://github.com/haskell/cabal/pull/11403)

  Remove obsolete compatibility exports from `Distribution.Compat.Semigroup`,
  leaving there only `gmempty` and `gmappend`. Use `Data.Semigroup` and
  `Data.Monoid` instead. Newtypes `First'`, `Last'` and `Option'` are gone
  now, use normal `First` and `Last`.

- Remove `Distribution.Compat.Directory` and `Distribution.Client.Compat.Directory` [#11407](https://github.com/haskell/cabal/pull/11407)

  Remove obsolete compatibility layers for `directory < 1.3.7`, including
  `Distribution.Compat.Directory` and `Distribution.Client.Compat.Directory`.
  Use `System.Directory` instead.

- Remove `Distribution.Compat.FilePath` [#11413](https://github.com/haskell/cabal/pull/11413)

  Remove obsolete compatibility layer of `Distribution.Compat.FilePath`, use
  `System.FilePath` instead.

- Remove `Distribution.Compat.SnocList` [#11403](https://github.com/haskell/cabal/pull/11403)

  Remove unused `Distribution.Compat.SnocList`.

- Remove unused `Distribution.Compat.Stack.annotateCallStackIO` [#11423](https://github.com/haskell/cabal/pull/11423)

  Remove `Distribution.Compat.Stack.annotateCallStackIO`, it's unused.

- Fix haddock interface and HTML paths for sub-libraries [#11475](https://github.com/haskell/cabal/issues/11475) [#11476](https://github.com/haskell/cabal/pull/11476)

  Previously, `cabal haddock --haddock-all` could not find the `.haddock`
  interface file for internal sub-libraries, causing missing documentation and
  "could not find link destinations" warnings. This was because the registered
  `haddockInterfaces` and `haddockHTMLs` paths in
  `generalInstalledPackageInfo` did not include the sub-library name
  subdirectory.

- Remove `Distribution.Compat.Time.calibrateMtimeChangeDelay` [#11496](https://github.com/haskell/cabal/pull/11496)

  Remove `calibrateMtimeChangeDelay` from `Distribution.Compat.Time`, it's
  irrelevant for modern file systems, all of which have submicrosecond
  precision of file modification times. Inside Cabal this function was used
  only in the test suite.

- Better document, in-app, Cabal's `--semaphore=SEMAPHORE` option [#11503](https://github.com/haskell/cabal/pull/11503)

  Cabal's `--semaphore=SEMAPHORE` option is better documented on `--help`.

- Change unsupported ghc version warning to info [#9734](https://github.com/haskell/cabal/issues/9734) [#11514](https://github.com/haskell/cabal/pull/11514)

  The warning "Unknown/unsupported 'ghc' version detected (Cabal 3.12.1.0
  supports 'ghc' version < 9.12): /usr/bin/ghc-9.12.3 is version 9.12.3" is
  now only shown at Info level of verbosity.

- Clean up `Distribution.Compat.Time` [#11549](https://github.com/haskell/cabal/pull/11549)

  * Remove unused `posixSecondsToModTime`.
  * Hide the constructor of `ModTime`, it's supposed to be opaque.
  * Implement `getModTime` as a wrapper over `System.Directory.getModificationTime` instead of our own implementation.

- Pre-build rules can generate extra sources and bundled libraries [#10791](https://github.com/haskell/cabal/issues/10791) [#11607](https://github.com/haskell/cabal/issues/11607) [#11573](https://github.com/haskell/cabal/pull/11573)

  It is now possible to write pre-build rules that generate source files other
  than Haskell files, as well as extra bundled library files.

  Because there is no counterpart to `autogen-modules` for non-Haskell source
  files, you will need to proceed in two steps:

    1. In a per-component pre-configure hook, add the files you want to generate
       to the relevant fields, e.g. the `cSources` field of `BuildInfo` for a
       C source file.

       These files must be relative to `autogenCompModulesDir`.

    2. Pre-build rules generating these files will now be demanded. This avoids
       getting an error message "The following pre-build rules are not demanded
       and will not be run".

  Note that include files (such as `.h` files) are a bit different: any files
  listed under `includes`/`autogen-includes` are required at **configure**
  time.  This gives `SetupHooks` authors two choices: either list the include
  files in `autogen-includes` but generate them in a pre-configure hook, or
  don't list them there and generate them in a pre-build rule, relying on the
  files getting picked up from included directories (this may be brittle).

- Replace `getDirectoryContents` with `listDirectory` [#11599](https://github.com/haskell/cabal/pull/11599)

  In course of an internal refactoring, replacing uses of
  `getDirectoryContents` with `listDirectory`, we also renamed a field of
  `Distribution.PackageDescription.Check.CheckPackageContentOps` of the same
  name.

- Introduce `removeFileForcibly`, remove `removeExistingFile` [#11616](https://github.com/haskell/cabal/pull/11616)

  Introduce a robust file removing helper
  `Distribution.Simple.Utils.removeFileForcibly`, similar in spirit to
  `System.Directory.removePathForcibly`.

  Remove `Distribution.Client.Utils.removeExistingFile`, use aforementioned
  `removeFileForcibly` instead.

- Replace `removeDirectoryRecursive` with `removePathForcibly` [#11604](https://github.com/haskell/cabal/pull/11604) [#11938](https://github.com/haskell/cabal/pull/11938)

  We replace `System.Directory.removeDirectoryRecursive` calls with a more
  robust `System.Directory.removePathForcibly`. Additionally, some functions
  (most notably the one responsible for `cabal clean`) now run
  `removeDirectoryRecursive` twice on all platforms if the first time was not
  successful (previously only on Windows) and include a small delay in
  between.

- Fix typos in the word "explicit" [#11621](https://github.com/haskell/cabal/pull/11621)

  Introduce `CheckExplanation` constructor `CVExplicitDepsCustomSetup` and
  `CheckExplanationID` constructor `CICVExplicitDepsCustomSetup`.

  These constructors were previously misspelled in the "explicit" part.

- Recognise `ExplicitLevelImports` and `ImplicitStagePersistence` extensions [#11936](https://github.com/haskell/cabal/pull/11936)

  `Cabal` and `cabal-install` now recognise extensions from GHC 9.14
  (`ImplicitStagePersistence` and `ImplicitStagePersistence`).

- Improve name assigning for temporary folders [#11872](https://github.com/haskell/cabal/pull/11872)

  Now `createTempDirectory` from `Distribution.Compat.Internal.TempFile` uses a global counter as a part of temporary folder name template, so that probing is less likely to fail. The change should be invisible for users.

- Bump `process` [#11923](https://github.com/haskell/cabal/issues/11923) [#11928](https://github.com/haskell/cabal/pull/11928)

  `Cabal` and `cabal-install` now depend on a newer version of `process`
  (`1.6.29.0`).  This avoids segfaults on macOS, where certain symbols
  are available, but *only* for targets that support them.

- Recognise `Modifiers` and `QualifiedStrings` extensions [#11936](https://github.com/haskell/cabal/pull/11936)

  `Cabal` and `cabal-install` now recognise extensions from GHC 10.0
  (`Modifiers` and `QualifiedStrings`).

- Drop support of building with GHC < 9.4 [#11966](https://github.com/haskell/cabal/pull/11966)

  The support window of GHCs, which can be used to build `Cabal`, has been
  narrowed from 5 to 3 years.  We now support only GHC 9.4+.

  `cabal-install` remains capable to drive older GHCs, as long as supporting
  it takes minimal effort.

- Require `deepseq-1.4.0.0` or newer [#11967](https://github.com/haskell/cabal/pull/11967)

  Drop compatibility layer for `deepseq < 1.4`, now `Cabal-syntax` and `Cabal`
  require `deepseq-1.4.0.0` (which is 11 years old) or newer.

- Add SPDX license data version 3.28 [#11978](https://github.com/haskell/cabal/pull/11978)

  - Update SPDX license list to version 3.28.0 2026-02-20

- Expose `jsemVersion` to detect `-jsem` protocol mismatches [#9993](https://github.com/haskell/cabal/issues/9993) [#11628](https://github.com/haskell/cabal/pull/11628)

  The `Compiler` record now exposes a `jsemVersion :: Compiler -> Maybe Int`
  accessor that reads the `"Semaphore version"` field of `ghc --info`.
  Returns `Just v` when GHC reports a value, `Nothing` for compilers
  that don't report the field (older GHCs, or non-GHC compilers).

  cabal-install consumes this to detect a protocol mismatch with the
  selected GHC before invoking `ghc -jsem`, so it can fall back to
  its in-process coordinator and warn the user rather than handing GHC
  a name it can't speak.

- Fix links to installed documentation when running `cabal haddock` [#11218](https://github.com/haskell/cabal/pull/11218)

- Fix parse warnings being emitted in reverse order [#11269](https://github.com/haskell/cabal/issues/11269) [#11479](https://github.com/haskell/cabal/pull/11479)

  Parse warnings were prepended to the accumulator list, causing them to
  be emitted in reverse file order. They are now sorted so they come out
  in file order, consistent with GHC's behavior.

- Fix mistake in `BuildInfo/Lens.hs` mixing up `c-sources` and `cxx-sources` [#10609](https://github.com/haskell/cabal/pull/10609)

  Fix a typo that would cause cxx-source and c-sources get mixed up.

- Use `text` package to decode UTF-8 instead of doing it ourselves [#11462](https://github.com/haskell/cabal/pull/11462)

  Cabal used to decode and encode UTF-8 from `[Word8]` to `String` and back
  itself. The code to do it was reasonably complex, requiring expertise
  outside of Cabal main domain, and likely slow. Now we outsource UTF-8
  matters to the `text` package.

  As a result `Distribution.Utils.String` no longer exports and
  `Distribution.Utils.ShortText` no longer re-exports `decodeStringUtf8` and
  `encodeStringUtf8`. Use `Data.Text.Encoding` instead.

- Warning for "dotlines" with cabal 3.0+. [#11518](https://github.com/haskell/cabal/issues/11518) [#11571](https://github.com/haskell/cabal/pull/11571)

  Cabal 3.0 changed the parser for "free text" fields (e.g. description) such
  that:

    1. Empty lines and indentation are preserved.

    2. Empty lines with a dot '.' (and possible whitespace) are
        interpreted literally; they are no longer interpreted as an
        empty line.

  Consequently, dotlines no longer serve a purpose and only add extra noise,
  hence these are now a warning with `cabal check`.

- Remove duplicated constraints from `CondTree` [#11626](https://github.com/haskell/cabal/pull/11626)

  `CondTree` is often instantiated with `a` being a component that has
  `BuildInfo` and `c` as `[Dependency]`. The `[Dependency]` is derived from
  the `BuildInfo` during construction.

  The accessors are exposed, this duplication of `[Dependency]` can cause the
  data to be inconsistent.

  Cabal exact print aims to allow modifications to the GPD. Not having a
  single source of truth can also confuse programmers using GPD as an API to
  exact print.

- Require `Applicative (g s)` as a superclass of `FieldGrammar` [#11821](https://github.com/haskell/cabal/pull/11821)

  This allows us to scrap a bit of boilerplate.

- Make `ShortText` a type synonym of `Text` [#11846](https://github.com/haskell/cabal/pull/11846)

  The data type `Distribution.Utils.ShortText` is now just a type synonym for `Data.Text.Text`.
  The constructor was not exported, so the change should not be noticeable by users
  unless they used to define instances for `ShortText`, in which case
  they might need to enable `{-# LANGUAGE TypeSynonymInstances #-}` or perhaps
  drop their instances entirely, if they duplicate instances for `Text`.

- Don't warn on `mhs-options` [#11341](https://github.com/haskell/cabal/issues/11341) [#11344](https://github.com/haskell/cabal/pull/11344)

- Re-export `Text.ParserCombinators.ReadP` [#11851](https://github.com/haskell/cabal/issues/11851) [#11852](https://github.com/haskell/cabal/pull/11852)

  The list of types and functions exported by `Distribution.Deprecated.ReadP`
  remains the same in name but by for the most part these are now re-exports from
  `Text.ParserCombinators.ReadP`. Everything except the type synonym `Parser`, and
  functions `skipSpaces1` and `readP_to_E` is a re-export from this module of
  `base` package.  Consequently, the type of `ReadP` has changed to take only one
  type parameter, one fewer than before.

  Before this change, we had:

  ```haskell
  data Parser r s a
  type ReadP r a = Parser r Char a
  ```

  Afterwards, we have `data ReadP a` from `base` and `Parser is a type synonym:

  ```haskell
  type Parser = T.ReadP
  ```
