Pre-release cabal-install 3.12.0.0/3.11.0.0 changelog and release notes.

This file will be edited and the changes incorprated into the official
3.12.1.0 cabal-install and cabal-install-solver release notes.

---

### Significant changes

- Add support for asm, cmm, and js sources in executable components [#8639](https://github.com/haskell/cabal/issues/8639) [#9061](https://github.com/haskell/cabal/pull/9061)

    Executable components now support the inclusion of asm, cmm, and js source
    files in a cabal file using the same syntax as is used for these sources
    in library components, similar to how c and c++ sources are supported in
    both types of components. This syntax was already parsed in cabal files,
    but was silently ignored in the build step, so no changes to syntax are
    made.

- Add `--project-dir` flag [#7695](https://github.com/haskell/cabal/issues/7695) [#7940](https://github.com/haskell/cabal/issues/7940) [#8454](https://github.com/haskell/cabal/pull/8454)

    - Added `--project-dir` flag for specifying the project's root directory
    - Deprecated using `--project-file` with an absolute filepath without also using `--project-dir`.

- Support per-component builds when coverage is enabled [#4798](https://github.com/haskell/cabal/issues/4798) [#5213](https://github.com/haskell/cabal/issues/5213) [#6397](https://github.com/haskell/cabal/issues/6397) [#6440](https://github.com/haskell/cabal/issues/6440) [#9464](https://github.com/haskell/cabal/pull/9464)

    Cabal now supports per-component builds when coverage is enabled.This enables
    coverage for packages with internal libraries (#6440), and enables coverage for
    packages that use backpack (#6397), even though we do not get coverage for
    instantiations of an indefinite module (it is not clear what it means for HPC
    to support backpack, regardless of Cabal).

    To achieve this, hpc information (`.mix` files) from a library is now written
    into the package database of a library under `extraCompilationArtifacts`.

    Cabal configure (via the Setup interface) now accepts --coverage-for=<unit-id>,
    a flag which specifies which libraries should be included in the coverage
    report for some testsuite.

- Add `cabal path` command [#8879](https://github.com/haskell/cabal/pull/8879)

    The `cabal path` command prints the file system paths used by Cabal.
    It is intended for use by tooling that needs to read or modify Cabal
    data, such that it does not need to replicate the complicated logic
    for respecting `CABAL_DIR`, `CABAL_CONFIG`, etc.

- Redesign `cabal path` command to account for projects [#9673](https://github.com/haskell/cabal/pull/9673)

  Previously, `cabal path` was only able to query from the global configuration file, e.g., `~/.cabal/config` or the XDG equivalent.
  We take the foundations and enhance `cabal path` to take project configuration, such as `cabal.project`, into account.

  Additionally, we add support for multiple output formats, such as key-value pairs and json.

  The key-value pair output prints a line for each queried key and its respective value:

      key1: value2
      key2: value2

  If only a single key is queried, we print only the value, for example:

      value1

  The json output format is versioned by the cabal-install version, which is part of the json object.
  Thus, all result objects contain at least the key "cabal-install-version".

  We expand the `cabal path` to also produce information of the compiler that is going to be used in a `cabal build` or `cabal repl` invocation.
  To do that, we re-configure the compiler program, and outputs the location, version and compiler flavour.
  This is helpful for downstream tools, such as HLS, to figure out the GHC version required to compile a project with, without dependency solving.

- Add support for authentication tokens for uploading to Hackage [#6738](https://github.com/haskell/cabal/issues/6738) [#9058](https://github.com/haskell/cabal/pull/9058)

    A new flag `--token` (`-t`) has been created. Token authentication takes
    precedence over username and password meaning that, if a token is set,
    the username and password flags are ignored.

- Make --(test-)show-details=direct the default [#8942](https://github.com/haskell/cabal/pull/8942)

    This option leaves it up to the testing framework to decide what and how to print out,
    potentially leading to a prettier output. For example, most of the testing frameworks
    use colors, which wouldn't be seen with any other option.

    This comes with a tradeoff, though: Cabal will not create a log file with this option.
    If you prefer a log file, consider setting `--test-show-details=streaming` (or something
    else) manually.

- Die if package list is missing [#8944](https://github.com/haskell/cabal/pull/8944)

    If a package list is missing, `cabal` will now die and suggest the user to run
    `cabal update` instead of continuing into not being able to find packages coming
    from the remote package server.

- Reject index-state younger than cached index file [#8944](https://github.com/haskell/cabal/pull/8944)

    Requesting to use an index-state younger than the cached version will now fail,
    telling the user to use an index-state older or equal to the cached file, or to
    run `cabal update`.

    The warning for a non-existing index-state has been also demoted to appear only
    on verbose logging.

- Warn early that overwrite policy is needed [#5993](https://github.com/haskell/cabal/issues/5993) [#9268](https://github.com/haskell/cabal/pull/9268)

    Waiting for a long build and then seeing the install fail because a flag was
    missing is frustrating.With this change we skip the wait and warn early,
    before the build, that an overwrite policy flag would be needed for the
    install to succeed.

- Make sure Haskell files in explicit source directories take precedence over autogenerated Haskell files [#8689](https://github.com/haskell/cabal/issues/8689) [#8690](https://github.com/haskell/cabal/pull/8690)

    - Changed order or directories in GHC invocation so that source
    directories explicitly specified in cabal file will be considered
    before Cabal’s internal build directory.

- config file: allow more flags in the init section [#8835](https://github.com/haskell/cabal/issues/8835) [#8839](https://github.com/haskell/cabal/pull/8839)

    The init section of config file now allows the following fields:

- Fix handling of ETag header for remote packages [#9113](https://github.com/haskell/cabal/issues/9113) [#9116](https://github.com/haskell/cabal/pull/9116)

    Remote packages will now be cached regardless of the capitalization of the
    "ETag" header. Previously remote packages would not be cached if the header
    name did not match exactly. Now they will be cached even if the header's
    capitalization is different.

- Clarify the semantics of the `--package-db` flag [#9678](https://github.com/haskell/cabal/issues/9678)

    The `--package-db` flag now only applies to the default
    immutable initial package stack rather than also applying to the store
    package database.

    This fixes an assertion failure which was triggered when using `--package-db` and also
    clarifies how it should interact with `--store-dir` and `--dist-dir` flags.

- Add `--semaphore` flag to enable interaction with GHC Job Server protocol [#8557](https://github.com/haskell/cabal/pull/8557)

    When cabal-install is passed the `--semaphore` flag it will now act as a job server
    according to the GHC Jobserver Protocol.

    In particular this means that cabal-install will create a semaphore which it then
    passes to `./Setup build` (and hence `ghc`) which can be used by `ghc` in order to
    control how much paralellism it uses, coordinating with other simultaneously running
    processes.

    This feature requires ghc-9.8 in order to use, as this is the first version of GHC
    which implements the protocol.

- Add `--ignore` to `cabal check` [#8587](https://github.com/haskell/cabal/issues/8587) [#9442](https://github.com/haskell/cabal/pull/9442)

    - `cabal check` now ignores specific warnings with `--ignore`. E.g.
      `--ignore=missing-upper-bounds` will not display “Missing upper
      bounds” warnings.
    - `cabal check` output now prints the warning identifier too
      (like `[no-category]`).

- Adds functionality for the --offline flag with the "build" command. [#8676](https://github.com/haskell/cabal/pull/8676)

    The --offline flag previously created in #2578 but was only implemented
    for the install command even thought the flag didn't throw an error
    whenever the build command was run. This PR adds functionality for the
    --offline flag with the build command.  Additionally there is a new
    PackageTest for the flag using the build command.

- Add warning for running cabal run, cabal test and cabal bench with +RTS flag [#8709](https://github.com/haskell/cabal/pull/8709)

    This adds a warning when RTS options are passed to cabal instead of the
    binary for the commands 'run', 'bench' and 'test', as most users want to
    pass these options to their binary.

- Add support for loading multiple components into one repl session [#8238](https://github.com/haskell/cabal/pull/8238) [#8491](https://github.com/haskell/cabal/pull/8491) [#8726](https://github.com/haskell/cabal/pull/8726)

    The `repl` command is extended in order to allow starting a repl session
    with multiple local components. When a user specifies a target to the
    "repl" command which resolves to multiple local components then `cabal`
    will start a repl session which loads them all into a single GHC session
    if the multi-repl is enabled.

    The multi-repl can be enabled by passing `--enable-multi-repl`, or writing
    `multi-repl: True` in your cabal.project file.

    The feature is fully explained in [this blog
    post](https://well-typed.com/blog/2023/03/cabal-multi-unit/).

- Warn when project configuration options are going to be ignored. [#8949](https://github.com/haskell/cabal/pull/8949)

    Some project configuration options can only be specified from the command
    line.  If the user specified those options from a project file,
    cabal-install would silently ignore them. Now cabal-install will emit a
    warning.

- Fix precedence for PATH for `build-tools-depends` [#8972](https://github.com/haskell/cabal/pull/8972)

    Fixes a bug introduced in #8506 that caused executables in the path to
    take precedence over those specified in `build-tools-depends`.

- Guard `PackageInfo_*` modules behind `cabal-version` ≥ 3.12 [#9331](https://github.com/haskell/cabal/issues/9331) [#9481](https://github.com/haskell/cabal/pull/9481)

    `cabal check` now warns whenever `PackageInfo_*` autogen modules are
    used with `cabal-version` ≥ 3.12.
    Additionally, `cabal configure` will fail if you try to use `PackageInfo_*`
    with `cabal-version` < 3.12.

- cabal init now generates cabal versions older than 1.12 with the correct >= syntax [#8206](https://github.com/haskell/cabal/issues/8206) [#8860](https://github.com/haskell/cabal/pull/8860)

- `cabal init`: suggest BSD-3 as default license [#8757](https://github.com/haskell/cabal/issues/8757) [#8764](https://github.com/haskell/cabal/pull/8764)

- Do not ask overwrite permissions on blank project [#9150](https://github.com/haskell/cabal/issues/9150) [#9155](https://github.com/haskell/cabal/pull/9155)

- Shorten solver rejection messages by removing repetition [#4251](https://github.com/haskell/cabal/issues/4251) [#9559](https://github.com/haskell/cabal/issues/9559) [#9560](https://github.com/haskell/cabal/pull/9560)

    As before, we show a single rejection as hyphenated package-version.

    For multiple rejections, we show a list of versions preceded by package
    semicolon, a much shorter rendering of the same information.

    ```diff
    - [__0] rejecting: pandoc-3.1.8, pandoc-3.1.7, pandoc-3.1.6.2, pandoc-3.1.6.1,
    - pandoc-3.1.6, pandoc-3.1.5, pandoc-3.1.4, pandoc-3.1.3, pandoc-3.1.2,
    - pandoc-3.1.1, pandoc-3.1, pandoc-3.0.1, pandoc-3.0, pandoc-2.19.2,
    - pandoc-2.19.1, pandoc-2.19, pandoc-2.18, pandoc-2.17.1.1, pandoc-2.17.1,
    + [__0] rejecting: pandoc; 3.1.8, 3.1.7, 3.1.6.2, 3.1.6.1, 3.1.6, 3.1.5, 3.1.4,
    + 3.1.3, 3.1.2, 3.1.1, 3.1, 3.0.1, 3.0, 2.19.2, 2.19.1, 2.19, 2.18, 2.17.1.1,
    ```

- Show provenance of project constraints [#9562](https://github.com/haskell/cabal/issues/9562) [#9578](https://github.com/haskell/cabal/pull/9578)

  Show imports when the solver rejects a package version due to a project
  constraint.  Even though imports are relative to their parent when imported,
  their paths are shown relative to the directory of the project in the solver
  output.

  ```
  $ cabal build all --dry-run
  ...
  [__1] next goal: hashable
  [__1] rejecting: hashable-1.4.3.0
        (constraint from cabal.project requires ==1.4.2.0)
  [__1] rejecting: hashable-1.4.2.0
        (constraint from project-stackage/nightly-2023-12-07.config requires ==1.4.3.0)
          imported by: cabal.project
  ```

  Fixes some test failures detecting cycles in imports, when;

  - the wrong import was reported as starting the cycle
  - a cycle was reported that wasn't actually a cycle

- Adjust BSD-2-Clause and BSD-3-Clause licence text [#9812](https://github.com/haskell/cabal/issues/9812) [#9813](https://github.com/haskell/cabal/pull/9813)

  This change matters to BSD-2-Clause and BSD-3-Clause licences. For these two
  licences, `cabal init` created a licence file that slightly differed from
  wording published at SPDX.  This has been rectified.

- Include the GHC "Project Unit Id" in the cabal store path [#8114](https://github.com/haskell/cabal/issues/8114) [#9326](https://github.com/haskell/cabal/pull/9326)

    This allows the use of several **API incompatible builds of the same
    version of GHC** without corrupting the cabal store.

    This relies on the "Project Unit Id" which is available since GHC 9.8.1,
    older versions of GHC do not benefit from this change.

- Add support for `GHC2024` [#9736](https://github.com/haskell/cabal/issues/9736)

  Support for the `GHC2024` language edition, introduced by GHC 9.10, has been
  added. It can now be used in the `default-language` and `other-languages`
  fields, and will be offered as an option by `cabal init`.

- Add language extension `ExtendedLiterals` [#8992](https://github.com/haskell/cabal/pull/8992)

    Adds support for the `ExtendedLiterals` language extension (GHC proposal #451)

- Add language extension `ListTuplePuns` [#8854](https://github.com/haskell/cabal/pull/8854)

    Adds support for the `ListTuplePuns` language extension (GHC proposal #475)

- Add language extension `TypeAbstractions` [#9496](https://github.com/haskell/cabal/issues/9496) [#9502](https://github.com/haskell/cabal/pull/9502)

### Other changes

- Script cache dir is the base16 hash of the canonical path of the script. [#9459](https://github.com/haskell/cabal/pull/9459)

    This fixes sporadic path failures on both Windows and Linux (e.g. [#9334](https://github.com/haskell/cabal/issues/9334)).

- Add `--haddock-output-dir` flag to `cabal haddock`. [#8720](https://github.com/haskell/cabal/issues/8720) [#8788](https://github.com/haskell/cabal/pull/8788)

  This flag gives the user full control over the directory where the documentation is placed. It allows both relative and absolute paths.

- Remove useles "Log" log level [#9151](https://github.com/haskell/cabal/issues/9151) [#9346](https://github.com/haskell/cabal/pull/9346)

    - Remove "Log" as a log level in favour of "Info".
    - Remove "Show" in Severity and replace by "displaySeverity" function

- `cabal init` should not suggest Cabal < 2.0 [#8680](https://github.com/haskell/cabal/issues/8680)

    'cabal init' no longer suggests users to set cabal-version to less than
    2.0.

- PkgConfig individual calls [#9134](https://github.com/haskell/cabal/pull/9134)

    `cabal` invokes `pkg-config` individually for each lib if querying for all
    doesn't return the expected result.

- Reimplementing `cabal check` [#7423](https://github.com/haskell/cabal/issues/7423) [#8427](https://github.com/haskell/cabal/pull/8427)

  - For `cabal-install` users: `cabal check` do not warn on -O2 or similar
    options if under an off-by-default cabal flag.

- `cabal check`: clearly mark Errors [#8908](https://github.com/haskell/cabal/pull/8908)

    `cabal check` will now mark errors (which make the program return 1 and
    Hackage refuse the package) by prepending them with an "Error: " string in
    the output.

- Don't report `index.html` file as created, if not created by Haddock [#5120](https://github.com/haskell/cabal/issues/5120) [#9332](https://github.com/haskell/cabal/pull/9332)

- Make check comply with Hackage requirements [#8897](https://github.com/haskell/cabal/pull/8897)

  - `cabal check` will only return exitcode 1 when the package is not fit
    for Hackage. E.g. it will not error anymore when your `synopsis:` is
    larger than `description:`, just emit a warning.
  - Cabal: Distribution.Client.Check now exports `isHackageDistError`, for
    third-party tools to know if a specific error will preclude a package
    from being uploaded to Hacakge.

- Also render short options with arguments [#8785](https://github.com/haskell/cabal/issues/8785) [#9043](https://github.com/haskell/cabal/pull/9043)

    Show how arguments are used with both short and long forms of options:

    ```diff
    <-v, --verbose[=n]Control verbosity (n is 0--3, default
    >-v[n], --verbose[=n] Control verbosity (n is 0--3, default
    <-w, --with-compiler=PATH give the path to a particular compiler
    >-w PATH or -wPATH, --with-compiler=PATH
    ```

- Remove `--cabal-file` flags from v2 commands [#6880](https://github.com/haskell/cabal/issues/6880) [#7225](https://github.com/haskell/cabal/issues/7225) [#8395](https://github.com/haskell/cabal/issues/8395) [#9123](https://github.com/haskell/cabal/pull/9123)

    The `--cabal-file` flag was never meant for public use but only for testing.
    To avoid confusing the users any further we removed the flag from v2
    commands.

- Avoid a double space in "Executing install plan ..." [#9376](https://github.com/haskell/cabal/pull/9376)

    The "Executing·install·plan··serially" and other similar "Executing
    install plan··..." outputs no longer contain double spaces.
