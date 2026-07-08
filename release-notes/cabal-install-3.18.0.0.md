cabal-install and cabal-install-solver 3.18.1.0 changelog and release notes
---

### Significant changes

- Implement recursive globs in file monitoring [#10064](https://github.com/haskell/cabal/issues/10064) [#11658](https://github.com/haskell/cabal/pull/11658)

  Directory recursive globs (e.g. `foo/**/*.hs`) are now supported by
  `cabal-install`'s file monitoring implementation. A double asterisk (`**`)
  in a glob indicates recursive directory traversal (much like in most
  Unix-style shells).

  Just like elsewhere in Cabal (and unlike most shells), `**` is limited to
  the last directory component of a path, so `/foo/bar/**/*.txt` is valid, but
  `/foo/**/bar/*.txt` is not.

- Directly call in-library functions to build packages [#11703](https://github.com/haskell/cabal/pull/11703)

  The way `cabal-install` builds packages has been significantly overhauled. In
  most circumstances, `cabal-install` will directly call `Cabal` library functions
  to build packages:

    - We no longer need `cabal-install` to act as a Setup (the `--act-as-setup`
      flag). We used to need this to set the working directory and to redirect
      logging output, but that can now be done via `Cabal` library functions.
    - Packages with `build-type: Hooks` are now also built via `Cabal` library
      functions instead of the `Setup.hs` interface. `cabal-install` achieves
      this by building an external hooks executable with which it communicates
      to run `SetupHooks``.

  The main upside of this change is that we waste less time re-running the entire
  `Cabal` `configure` step; instead `cabal-install` directly starts off with
  the information it already knows (compiler, versions of dependencies given be
  the solver, flag assignment, etc). This necessitated refactoring the `Cabale
  `configure` code in order to skip running the unnecessary initial steps thae
  are made redundant by the information from `cabal-install`'s `ElaboratedReadyPackage`e

  There should be no outward-facing change in behaviour beside speeding up the
  `configure` step.

- Add `--enable-library-bytecode` flag [#11188](https://github.com/haskell/cabal/issues/11188) [#4560](https://github.com/haskell/cabal/pull/4560)

  Introduce the `--enable-library-bytecode` flag to build bytecode libraries
  next to the usual object and shared artifacts. Bytecode libraries are useful
  when asking GHCi to use bytecode for library dependencies (e.g.
  `-fprefer-byte-code`) and are currently only available with GHC 9.15 or
  newer; the flag is ignored on older compilers.

  Implements the Cabal Proposal: [Cabal Support for Bytecode Objects and Bytecode Libraries](https://github.com/haskell/cabal-proposals/blob/master/proposals/bytecode-files.md).

- Allow reinstalling packages like `base` and `template-haskell` for GHC>=9.14 [#10087](https://github.com/haskell/cabal/issues/10087) [#10982](https://github.com/haskell/cabal/pull/10982)

  Historically cabal-install disallowed reinstalling packages like `base` and
  `template-haskell`.  As of GHC 9.14, the reasons for this have been lifted.
  We update cabal-install to become aware of this and allow reinstalling more
  packages.  Certain packages like `ghc` and `ghc-internal` still cannot be
  reinstalled.

- Detect non-cyclical duplicate project imports [#9562](https://github.com/haskell/cabal/issues/9562) [#9578](https://github.com/haskell/cabal/pull/9578) [#10933](https://github.com/haskell/cabal/pull/10933)

  Detect and report on duplicate imports that are non-cyclical. Give more detail
  when reporting duplicate imports.

  ```
  $ cabal build --project-file=cabal.project
  ...
  Warning: 2 imports of config/config-3.config;
    config/config-3.config
      imported by: cabal.project
    config/config-3.config
      imported by: config-2.config
      imported by: config/config-1.config
      imported by: cabal.project
  ```

  Move these types and functions from `cabal-install-solver` to `cabal-install`:

  - `docProjectConfigFiles`
  - `cyclicalImportMsg`
  - `untrimmedUriImportMsg`

  `ProjectConfigSkeleton` type synonym has now changed from from
  `CondTree ConfVar ([ProjectConfigPath], ProjectConfig)` to
  `CondTree ConfVar ([(Maybe URI, ProjectConfigPath)], ProjectConfig)`.

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

### Other changes

- Fix monitoring of project file imports [#12077](https://github.com/haskell/cabal/pull/12077)

    Watch for changes in all project files, the root `cabal.project` and all local
    files it imports. We don't monitor remote URI imports.

- Add info level logging of project file monitoring [#12069](https://github.com/haskell/cabal/pull/12069)

    If the project configuration being monitored is not absolute, then the
    logging includes the monitored path and the absolute path. The monitored
    file is classified as existing, imported or nonexistent. Nonexistent
    occurs for something like `cabal.project.local` that is monitored but may
    not exist. The non-imported files would be `cabal.project`,
    `cabal.project.freeze` and `cabal.project.local`.

    Also exports `mkNormalizerEnv` for normalized comparisons in tests when
    the output is not marked. Logging at any verbosity level other than normal
    is not marked.

- `cpp-options` apply only to .hs files [#11997](https://github.com/haskell/cabal/pull/11997)

    `cpp-options` apply only to .hs files; GHC ignores `-optP` for non-Haskell
    files (and since 9.10 this behavior is explicit/enforced).

- Replace `pip` with `uv` for building users guide [#11382](https://github.com/haskell/cabal/pull/11382)

  Aside from being faster, by using `uv` to build the user guide:

  * We don't have to explicitly activate a python virtual environment, either in the shell or in our makefile.
  * A clean git clone can `make users-guide` without first `make users-guide-requirements`.
  * We require fewer variables, targets and recipes in the the makefiles.
  * The `make users-guide` recipe output is much shorter as it doesn't include lengthy pip output.

- Default project target for the `repl` command [#11451](https://github.com/haskell/cabal/issues/11451) [#11452](https://github.com/haskell/cabal/pull/11452)

  The REPL for a project always requires an explicit target except in two cases:

  1. The package name is the default implicit target in a single package project.
  2. `all` is the default implicit target with `--enable-multi-repl`.

- Drop support for anything below GHC 8.0.0, base 4.9.0.0, Cabal 1.24.0.0 [#11630](https://github.com/haskell/cabal/pull/11630)

  Drop support for anything below GHC 8.0.0, base 4.9.0.0, Cabal 1.24.0.0
  These versions were no longer tested and likely non-functional.

- Don't use in-library path with mismatched Cabal library version when reconfiguring [#11942](https://github.com/haskell/cabal/issues/11942) [#11950](https://github.com/haskell/cabal/pull/11950)

  `cabal-install` now correctly falls back to the external Setup method when
  reconfiguring a package which was previously configured with a different Cabal
  library version than `cabal-install` was built against (e.g. a package with
  `build-type: Custom` build whose `Setup` was built against an older `Cabal`
  library version).

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

  See the pre-existing Note [Caching the result of configuring the compiler] in
  Distribution.Client.ProjectPlanning and Note [Constructing the ProgramDb]
  in Distribution.Client.SetupWrapper for additional details.

  This required a tiny change to `Cabal` to define and expose
  `clearUnconfiguredPrograms :: ProgramDb -> ProgramDb` for use in `cabal-install`.

- Fix `cabal run` handling of `--` with empty targets [#10487](https://github.com/haskell/cabal/issues/10487) [#10487](https://github.com/haskell/cabal/pull/10487)

  Previously, when using `--` to separate cabal options from executable
  arguments, without an explicit target, the first argument after `--` was
  incorrectly treated as a target selector.

  For example, the following now works as expected while it failed before
  thinking that `+RTS` is a target:

  ```
  $ cabal run -- +RTS -s
  $ cabal run -w /path/to/ghc -- +RTS -s
  ```

- Added revision information to `plan.json` [#6186](https://github.com/haskell/cabal/issues/6186) [#10980](https://github.com/haskell/cabal/pull/10980)

  The contents of `x-revision` `.cabal` fields are now available in
  `plan.json`.  They are located under `pkg-src.repo.pkg-revision`, have type
  `Number`, and are only available for `repo-tar` repositories of type
  `remote-repo` or `secure-repo`.

- Allow relocatable builds on FreeBSD [#11042](https://github.com/haskell/cabal/pull/11042)

  Properly enables relocatable builds on FreeBSD, just like on
  OSX and Linux.

- `cabal update`: create default config without comment about `builddir` [#11050](https://github.com/haskell/cabal/issues/11050) [#11067](https://github.com/haskell/cabal/pull/11067)

  When calling `cabal update` with an empty state (empty `CABAL_DIR` or new
  machine), cabal writes out a default config file with many fields
  commented out to show their defaults. This suggests that the user can
  uncomment the field and override the default. This is wrong for fields
  that aren't allowed in the config file like `builddir`. This patch
  avoids generating a comment with `builddir` in the default config.

- Update Hackage root key set [#11095](https://github.com/haskell/cabal/pull/11095)

  One new [Hackage root keyholder](https://github.com/haskell-infra/hackage-root-keys/tree/master/root-keys) was added to the bootstrap set and one was removed.

  - Added Hackage root key for Tikhon Jelvis.
  - Removed Hackage root key for Mathieu Boespflug.

- Remove Nix integration [#11191](https://github.com/haskell/cabal/pull/11191)

  In the past we deprecated nix integration, now we are removing it completely.
  More explanation can be found in the discourse:
  https://discourse.haskell.org/t/community-survey-removing-cabals-nix-integration/7201

- Add `--version-full` option and move additional version information to it [#11339](https://github.com/haskell/cabal/pull/11339)

  A new option `--version-full` has been added to `cabal` and `Setup.hs`. The
  previously added git revision information has been moved to it, and compiler
  and host information added to it.

  The `--version` option now reports the same information it originally did,
  in case scripts were relying on its output.

  Git revision information still is not provided for release builds, largely
  to avoid bootstrapping issues when building GHC. (Both cabal-install (not
  distributed, but used by hadrian) and the Cabal library are built as part of
  GHC builds.)

  This _should not_ affect most people, unless someone has been relying on the
  output of `--version` instead of using `--numeric-version`; but they would
  have been forced to support both output forms, and that only if supporting
  unreleased cabal builds.

- Recognise `SetupHooks.hs` [#11349](https://github.com/haskell/cabal/issues/11349) [#11351](https://github.com/haskell/cabal/pull/11351)

  `cabal check` now recogsnises `SetupHooks.hs` as a valid setup script.

- Ensure consistency of `Cabal` version for `SetupHooks` [#11331](https://github.com/haskell/cabal/issues/11331) [#11429](https://github.com/haskell/cabal/pull/11429)

  When building a package with `build-type: Hooks`, we build an executable which
  is invoked during the build process to perform build steps. This executable
  needs to be linked against the `Cabal` library, therefore we now add a
  dependency on `Cabal` when solving dependencies for the setup stanza.

  This fixes a bug which resulted in the solver choosing two inconsistent
  versions of the `Cabal` library, avoiding strange error messages like:

  ```
  SetupHooks.hs: error: [GHC-83865]
      • Couldn't match expected type ‘Distribution.Verbosity.Verbosity’
                    with actual type ‘Verbosity’
        NB: ‘Distribution.Verbosity.Verbosity’
              is defined in ‘Distribution.Verbosity’ in package ‘Cabal-3.16.0.0’
            ‘Verbosity’
              is defined in ‘Distribution.Verbosity’ in package ‘Cabal-3.16.0.0’
  ```

  where the error is that there are two different unit IDs for `Cabal` being
  passed when compiling the `SetupHooks` module.

- Enable uploading to a specific repository [#11478](https://github.com/haskell/cabal/pull/11478)

  Enable uploading packages and reports to a specific repository with the
  `-R/--repository` option of `cabal upload`, `cabal get`, and `cabal report`.

- Log if there are multiple local packages with the same ID in a project [#11487](https://github.com/haskell/cabal/pull/11487)

  Log duplicate local packages with the same package identifier, so that the
  user knows that only one will be picked.

- Warning for "dotlines" with cabal 3.0+. [#11518](https://github.com/haskell/cabal/issues/11518) [#11571](https://github.com/haskell/cabal/pull/11571)

  Cabal 3.0 changed the parser for "free text" fields (e.g. description) such
  that:

    1. Empty lines and indentation are preserved.

    2. Empty lines with a dot '.' (and possible whitespace) are
        interpreted literally; they are no longer interpreted as an
        empty line.

  Consequently, dotlines no longer serve a purpose and only add extra noise,
  hence these are now a warning with `cabal check`.

- Require cabal.project packages and/or optional-packages field. [#7401](https://github.com/haskell/cabal/issues/7401) [#11574](https://github.com/haskell/cabal/pull/11574)

  Currently, if a cabal.project file is present and the `packages` and
  `optional-packages` fields do not exist, a warning will be issued, and
  cabal will fail with a different, potentially misleading error message:

    "There is no <pkgname>.cabal package file or cabal.project file..."

  This changes it so that the prior warning is now an error, hence will cause
  cabal to fail before the misleading error is given. That at least one of
  these fields is required is already specified in the docs, hence this is not
  a specification change.

- Introduce `removeFileForcibly`, remove [#11616](https://github.com/haskell/cabal/pull/11616)

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

- Recognise `ExplicitLevelImports` and `ImplicitStagePersistence` extensions [#11936](https://github.com/haskell/cabal/pull/11936)

  `Cabal` and `cabal-install` now recognise extensions from GHC 9.14
  (`ImplicitStagePersistence` and `ImplicitStagePersistence`).

- `active-repositories`: don't override fully-deprecated packages [#11760](https://github.com/haskell/cabal/pull/11760)

  When `active-repositories` includes a repo with `:override`, and that
  repo's `preferred-versions` marks all its versions of a package as
  deprecated, the index-combining step previously still applied full
  override semantics, hiding all versions of that package from earlier
  repos.

  Fix by consulting `preferred-versions` when combining indexes: if no
  version of the package in the override repo is preferred, fall back to
  merge semantics so earlier-repo versions remain visible.

  Fixes [#8502](https://github.com/haskell/cabal/issues/8502)

- Improve name assigning for temporary folders [#11872](https://github.com/haskell/cabal/pull/11872)

  Now `createTempDirectory` from `Distribution.Compat.Internal.TempFile` uses
  a global counter as a part of temporary folder name template, so that
  probing is less likely to fail. The change should be invisible for users.

- Stop using `lukko` [#11915](https://github.com/haskell/cabal/pull/11915)

  In the previous release we switch `lukko` flag off by default. This release
  removes the flag and thus any usage of `lukko` in `cabal-install`, now
  `base:GHC.IO.Handle.Lock` is used unconditionally.

- Bump `process` [#11923](https://github.com/haskell/cabal/issues/11923) [#11928](https://github.com/haskell/cabal/pull/11928)

  `Cabal` and `cabal-install` now depend on a newer version of `process`
  (`1.6.29.0`).  This avoids segfaults on macOS, where certain symbols
  are available, but *only* for targets that support them.

- Recognise `Modifiers` and `QualifiedStrings` extensions [#11936](https://github.com/haskell/cabal/pull/11936)

  `Cabal` and `cabal-install` now recognise extensions from GHC 10.0
  (`Modifiers` and `QualifiedStrings`).

- Drop support of building with GHC < 9.4 [#11966](https://github.com/haskell/cabal/pull/11966)

  The support window of GHCs, which can be used to build Cabal, has been
  narrowed from 5 to 3 years.  We now support only GHC 9.4+.

  `cabal-install` remains capable to drive older GHCs, as long as supporting
  it takes minimal effort.

- Move project configuration import path functions to `cabal-install` [#9562](https://github.com/haskell/cabal/issues/9562) [#10512](https://github.com/haskell/cabal/issues/10512) [#11773](https://github.com/haskell/cabal/pull/11773)

  Move these functions from `cabal-install-solver` to `cabal-install`:

  - `docProjectConfigFiles`
  - `cyclicalImportMsg`
  - `untrimmedUriImportMsg`

- Update to `semaphore-compat` `2.0.1` (`-jsem` protocol v2) [#9993](https://github.com/haskell/cabal/issues/9993) [#11628](https://github.com/haskell/cabal/pull/11628)

  On Linux and other POSIX platforms, cabal-install's `--semaphore`
  jobserver now speaks v2 of the semaphore-compat protocol, which uses
  Unix domain sockets in place of POSIX named semaphores. The v1
  implementation used POSIX named semaphores via `sem_open(3)`, whose
  ABI varies between C standard libraries; a `cabal-install` and `ghc`
  built against different libc could not share a semaphore, breaking
  `-jsem` whenever the toolchain wasn't homogeneous (see
  [cabal #9993](https://github.com/haskell/cabal/issues/9993) and
  [GHC #25087](https://gitlab.haskell.org/ghc/ghc/-/issues/25087)).
  The v2 wire format is independent of libc. Windows is unaffected
  and continues to use the v1 protocol (Win32 named semaphores).

  cabal-install now inspects the selected GHC's `"Semaphore version"`
  entry in `ghc --info` (via the new `jsemVersion` field on the `Cabal`
  library's `Compiler` type) to detect a protocol mismatch ahead of
  time. If GHC's reported version is incompatible with the version
  cabal-install supports, cabal-install emits a warning of the form

      Semaphore version mismatch (cabal-install uses vN, but the
      selected GHC reports vM); not using -jsem, GHC will be invoked
      without semaphore-based parallelism.

  and falls back to its in-process `NumJobs` coordinator instead of
  passing `-jsem` to GHC. The build still succeeds, but loses the
  cross-process module-level parallelism. Upgrading GHC to one that
  supports protocol v2 restores full parallelism.

  See also:

  - the [GHC proposal amendment](https://github.com/ghc-proposals/ghc-proposals/pull/673)
  - the [GHC patch](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/15729)
  - the [semaphore-compat library MR](https://gitlab.haskell.org/ghc/semaphore-compat/-/merge_requests/8)

- Honour `--with-ghc-pkg` when supplied by the user [#11373](https://github.com/haskell/cabal/issues/11373) [#11450](https://github.com/haskell/cabal/pull/11450)

  `cabal-install` now properly honours when a user supplies `--with-ghc-pkg`.

- Add a `README` to the files generated by `cabal init` [#11231](https://github.com/haskell/cabal/issues/11231) [#11260](https://github.com/haskell/cabal/pull/11260)

- Refactor `cabal-install` solver config log output [#10854](https://github.com/haskell/cabal/pull/10854)

### Unresolved

- Cabal package not visible to build auto-generated setup.hs [#11416](https://github.com/haskell/cabal/issues/11416)

    Upgrading from `cabal-install` 3.16.0.0 might give error on autogenerated
    `Setup.hs`.

    This is fixable by running `cabal build --enable-tests`.
