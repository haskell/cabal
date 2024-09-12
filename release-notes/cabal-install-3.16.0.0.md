### THIS iS A WIP CHANGELOG FOR 3.16

**It will have to be updated with whatever gets added between 3.14 and 3.16**


### Significant changes

- `haddock-project` support for subcomponents [#9821](https://github.com/haskell/cabal/pull/9821)

  - `haddock-project` handles sublibraries, test suites and benchmarks.
  - `haddock` receives `--package-name` flag which allows to set names of
    components which are included in the main `index.html` file.
  - added `--use-unicode` flag to `haddock` and `haddock-project` commands.
  - The directory structure of `./dist-newstyle` has changed.  `haddock`
    subcommand will install `package:sublib` component in a directory
    `package/sublib` under `l/sublib/doc/html/`.  This is important for
    `haddock-project` command and in the future might will be useful for hackage
    support of sublibraries.  See
    https://github.com/haskell/cabal/pull/9821#discussion_r1548557115.

- Redefine `build-type: Configure` in terms of `Hooks` [#9969](https://github.com/haskell/cabal/pull/9969)

  The `build-type: Configure` is now implemented in terms of `build-type: Hooks`
  rather than in terms of `build-type: Custom`. This moves the `Configure`
  build-type away from the `Custom` issues. Eventually, `build-type: Hooks` will
  no longer imply packages are built in legacy-fallback mode. When that
  happens, `Configure` will also stop implying `legacy-fallback`.

  The observable aspect of this change is `runConfigureScript` now having a
  different type, and `autoconfSetupHooks` being exposed from `Distribution.Simple`.
  The former is motivated by internal implementation details, while the latter
  provides the `SetupHooks` value for the `Configure` build type, which can be
  consumed by other `Hooks` clients (e.g. eventually HLS).

### Other changes

- Add support for building profiled dynamic way [#4816](https://github.com/haskell/cabal/issues/4816) [#9900](https://github.com/haskell/cabal/pull/9900)


  New options for `cabal.project` and `./Setup` interface:

  * `profiling-shared`: Enable building profiling dynamic way
  * Passing `--enable-profiling` and `--enable-executable-dynamic` builds
    profiled dynamic executables.

  Support for using `profiling-shared` is guarded behind a constraint
  which ensures you are using `Cabal >= 3.13`.

  In the `.cabal` file:

  * `ghc-prof-shared-options`, for passing options when building in
    profiling dynamic way

- Fix interaction of `--*-shared` and `--*-executable-dynamic` options. [#10050](https://github.com/haskell/cabal/issues/10050) [#9900](https://github.com/haskell/cabal/pull/9900)

  If you explicitly request `--disable-shared` it should disable the building of
  a shared library and override any automatic ways this option is turned on.

  Passing `--enable-executable-dynamic` turns on `--enable-shared` if the option is
  not specified explicitly.

  Before this patch, writing `--disable-shared` on its own would not disable the building of shared libraries. Writing `--disable-shared` and `--disable-executable-dynamic` would disable shared library
  creation (despite `--disable-executable-dynamic` being the default).

  Now:

  * If you specify `--enable-shared` then shared objects are built.
  * If you specify `--disabled-shared` then shared objects are not built.
  * If you don't explicitly specify whether you want to build shared libraries then
    * `--enable-executable-dynamic` will automatically turn on building shared libraries
    * `--enable-executable-dynamic --enable-profiling` will automatically turn on building
      shared profiling libraries (if supported by your compiler).

- Working directory support for `Cabal` [#9702](https://github.com/haskell/cabal/issues/9702) [#9718](https://github.com/haskell/cabal/pull/9718)

  The `Cabal` library is now able to handle a passed-in working directory, instead
  of always relying on the current working directory of the parent process.

  In order to achieve this, the `SymbolicPath` abstraction was fleshed out, and
  all fields of `PackageDescription` that, if relative, should be interpreted
  with respect to e.g. the package root, use `SymbolicPath` instead of `FilePath`.

  This means that many library functions in `Cabal` take an extra argument of type
  `Maybe (SymbolicPath CWD (Dir "Package"))`, which is an optional (relative or
  absolute) path to the package root (if relative, relative to the current working
  directory). In addition, many functions that used to manipulate `FilePath`s now
  manipulate `SymbolicPath`s, require explicit conversion using e.g. `getSymbolicPath`.

  To illustrate with file searching, the `Cabal` library defines:

  ```haskell
  findFileCwd
    :: forall dir1 dir2 file
     . Verbosity
    -> Maybe (SymbolicPath CWD (Dir dir1))

    -> [SymbolicPath dir1 (Dir dir2)]

    -> RelativePath dir2 File

    -> IO (SymbolicPath dir1 File)
  ```

  See Note [Symbolic paths] in `Distribution.Utils.Path` for further information
  on the design of this API.

- `curl` transport now supports Basic authentication [#10089](https://github.com/haskell/cabal/pull/10089)

  - The `curl` HTTP transport previously only supported the HTTP Digest
    authentication scheme.  Basic authentication is now supported
    when using HTTPS; Curl will use the scheme offered by the server.
    The `wget` transport already supports HTTPS.

- Enhance error detection for cabal root project files, including broken symlinks [#9937](https://github.com/haskell/cabal/issues/9937) [#10103](https://github.com/haskell/cabal/pull/10103)

  - Added proper detection and reporting for issues with cabal root project files. Previously, these files were silently ignored if they were broken symlinks. Now, `cabal` will exit
  with an error in such case.

- Let cabal init remember chosen language within current session [#10096](https://github.com/haskell/cabal/issues/10096) [#10115](https://github.com/haskell/cabal/pull/10115)

  When `cabal init` asks for a language, the last choice made will be used as the new default for the current prompt.

- Filter out `-dinitial-unique` and `-dunique-increment` from package hash [#10122](https://github.com/haskell/cabal/pull/10122)

  `-dinitial-unique` and `-dunique-increment` are now filtered out when computing the
  store hash of a package.

  These options shouldn't affect the output of the package and hence
  shouldn't affect the store hash of a package.

- Warn about `git://` protocol [#10261](https://github.com/haskell/cabal/pull/10261)

  `cabal check` will warn about the insecure (and no longer supported by GitHub or Gitlab, among others) `git://` protocol in `source-repository`.

  See [Git Book](https://git-scm.com/book/en/v2/Git-on-the-Server-The-Protocols#_the_cons_4)
  for an explanation.

- Enable recompilation avoidance during Haddock generation [#9175](https://github.com/haskell/cabal/issues/9175) [#9177](https://github.com/haskell/cabal/pull/9177)

  * Haddock no longer writes compilation files by default, so we do not need to
    pass tmp dirs for `-hidir`, `-stubdir`, and `-odir` via `--optghc`. Indeed, we
    do not *want* to do so, since it results in recompilation for every invocation
    of Haddock via Cabal. We now stop this from happening for Haddock versions
    2.28 and greater, since that is when Hi Haddock was introduced.

  * We no longer define the `__HADDOCK_VERSION__` macro when invoking GHC through
    Haddock, since doing so essentially guarantees recompilation during
    documentation generation. We audited all uses of `__HADDOCK_VERSION__` in
    hackage, ensuring there was a reasonable path forward to migrate away from
    using `__HADDOCK_VERSION__` for each, while generating the same documentation
    as it did before.
    If you are a user of `__HADDOCK_VERSION__`, please take a look at the
    discussion in https://github.com/haskell/cabal/pull/9177 and reach out to us
    if your use case is not covered.

  * Rename the `--haddock-lib` flag to `--haddock-resources-dir` (and
    `haddock-lib:` cabal.project field to `haddock-resources-dir:`), and add this
    flag to the users guide since it was missing an entry.

  * `documentation: true` or `--enable-documentation` now implies `-haddock` for
    GHC.

- Bug fix - Don't pass `--coverage-for` for non-dependency libs of testsuite [#10046](https://github.com/haskell/cabal/issues/10046) [#10250](https://github.com/haskell/cabal/pull/10250)

- Added `--all` and `--haddock-all` switches to `haddock-project` subcommand [#10051](https://github.com/haskell/cabal/issues/10051) [#10163](https://github.com/haskell/cabal/pull/10163)

- Clarify error message when `pkg-config` is not found [#10122](https://github.com/haskell/cabal/pull/10122)

  - The error message when `pkg-config` is not found or querying it fails will no
  longer incorrectly claim that the package is missing in the database.
