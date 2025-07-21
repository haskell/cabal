cabal-install 3.16.0.0 changelog and release notes.
---


### Significant changes

- Allow `build-type: Configure` to work with components (including public sub-libraries) [#4548](https://github.com/haskell/cabal/issues/4548) [#10966](https://github.com/haskell/cabal/pull/10966)

  This change allows the `build-type: Configure` field to be used in
  Cabal packages in conjunction with components. (E.g. public sublibraries).
  This is a significant change as it enables the use of the `Configure` build
  type, which cabal bailed on previously.

  This does change the behaviour of cabal-install for packages that contain
  `build-type: Configure`. We now always build them as components with
  cabal-install.  Previously we would turn packages with main libraries, and
  executables/benchmarks/tests-suites into a single package to be built in a
  compatibility mode.  This is no longer the case.

- Discover targets in a project [#4070](https://github.com/haskell/cabal/issues/4070) [#8953](https://github.com/haskell/cabal/issues/8953) [#9744](https://github.com/haskell/cabal/pull/9744)

  Adds a `cabal target` command that is useful for discovering targets in a
  project for use with other commands taking ``[TARGETS]``.

  Any target form except for a script target can be used with ``cabal target``.

  This command, like many others, takes ``[TARGETS]``. Taken together, these will
  select for a set of targets in the project. When none are supplied, the command
  acts as if ``all`` was supplied. Targets in the returned subset are shown sorted
  and fully-qualified with package, component type and component name such as
  `Cabal-tests:test:hackage-tests`.

- Add `--with-repl` flag to specify alternative REPL program [#9115](https://github.com/haskell/cabal/issues/9115) [#10996](https://github.com/haskell/cabal/pull/10996)

  Added a new `--with-repl` command-line option that allows specifying an alternative
  program to use when starting a REPL session, instead of the default GHC.

  This is particularly useful for tools like `doctest` and `hie-bios` that need to
  intercept the REPL session to perform their own operations. Previously, these tools
  had to use `--with-ghc` which required them to proxy all GHC invocations, including
  dependency compilation, making the implementation more complex.

  The `--with-repl` option only affects the final REPL invocation, simplifying the
  implementation of such wrapper tools.

  Example usage:
  ```bash
  cabal repl --with-repl=doctest
  cabal repl --with-repl=/path/to/custom/ghc
  ```

  This change also removes the special handling for response files with `--interactive`
  mode, as tools are now expected to handle response files appropriately.

### Other changes

- Fix `cabal clean` permissions on Windows [#10182](https://github.com/haskell/cabal/issues/10182) [#10190](https://github.com/haskell/cabal/pull/10190)

  - `cabal clean` now removes the read-only mark recursively in the `dist-newstyle` folder on Windows before attempting to delete it.

- Shallow and concurrent cloning of git repositories [#10254](https://github.com/haskell/cabal/pull/10254)

  - Clone git repositories specified in `source-repository-package` stanzas
    shallowly, since to build the package from the repository we only need to
    read the commit specified. The rest of the repo is not needed.
    Note that this does not change the behaviour of `cabal get -s`, which will
    still clone the repository in full.
  - Clone VCS repositories concurrently, with a maximum of two concurrent tasks
    at the same time (just like when downloading packages asynchronously)

- Don't run submodule commands unless necessary [#10590](https://github.com/haskell/cabal/pull/10590)

  When `cabal` clones a Git repo for a `source-repository-package` listed in a
  `cabal.project`, it will run various commands to check out the correct
  revision, initialize submodules if they're present, and so on.

  Now, `cabal` will avoid running `git submodule` commands unless the cloned
  repository contains a `.gitmodules` file. This will declutter `cabal`'s debug
  output by running fewer commands.

- A trailing colon after a stanza name in `cabal.project` is now an error [#10525](https://github.com/haskell/cabal/pull/10525)

  It is now a hard error to use a trailing colon after a stanza name in
  `cabal.project` or `*.cabal` files:

  ```
  packages: .

  source-repository-package:
      type: git
      location: https://github.com/haskell/cabal
      tag: f34aba976a60940295f41b6649674e9568893894
  ```

  ```
  $ cabal build
  Error parsing project file cabal.project:3:
  'source-repository-package' is a stanza, not a field. Remove the trailing ':' to parse a stanza.
  ```

  Previously, the warning message was easily ignored and somewhat misleading, as
  the difference between a stanza and a field is not immediately obvious to
  Haskellers used to config languages like JSON and YAML (which don't distinguish
  between fields which have string or list values and stanzas which have nested
  fields):

  ```
  Warning: cabal.project: Unrecognized field
  'source-repository-package' on line 3
  ```

- Warn on `cabal format` [#10549](https://github.com/haskell/cabal/pull/10549)

  - Despite its name, `cabal format` is not a proper formatter for cabal files. By chance users have sometimes found the command even though it is not mentioned in the help text, and they used it to format cabal files. This has some downsides like comments are stripped away or common stanzas are inlined, the command is more like a dump of the resolved package description. There are future plans ([#7544](https://github.com/haskell/cabal/issues/7544)) to make it an actual formatter so, rather than going through a deprecation cycle, we decided to keep this command for future use and in the meantime just warn the user about the fact that it is not a proper formatter.

- Quieter Git output [#10587](https://github.com/haskell/cabal/pull/10587)

  When `cabal` clones a Git repo for a `source-repository-package` listed in a
  `cabal.project`, it will run various commands to check out the correct
  revision, initialize submodules if they're present, and so on.

  Now, `cabal` will pass `--quiet` to Git in more cases to help prevent
  cluttering command-line output.

- Report trailing spaces in project import URIs [#10622](https://github.com/haskell/cabal/issues/10622) [#10629](https://github.com/haskell/cabal/pull/10629)

  > A string is a valid URL potentially surrounded by spaces if, after stripping
  > leading and trailing whitespace from it, it is a valid URL."
  > SOURCE: [W3C/HTML5/URLs](https://www.w3.org/TR/2010/WD-html5-20100624/urls.html)

  Fixes a problem of mistaking a URI for a file path when it has trailing spaces
  and warn about such trailing spaces.

- Fix Haddock CSS handling in multi-package projects [#10636](https://github.com/haskell/cabal/issues/10636) [#10637](https://github.com/haskell/cabal/pull/10637)

  When `--css=<css-file>` flag is provided to `cabal haddock-project`:

  - the Haddock index is now properly styled by the provided CSS file
  - each package in the project now has their docs properly styled by the provided CSS file

- Show source of project parse error or warning [#10635](https://github.com/haskell/cabal/issues/10635) [#10644](https://github.com/haskell/cabal/pull/10644)

  Improves warning and error messages shown when parsing project files and their
  imports.

- Improve "Cannot read .cabal file inside ..." errors [#10647](https://github.com/haskell/cabal/pull/10647)

  The error message printed when Cabal fails to read a `.cabal` file inside an
  index (like the Hackage index) has been greatly improved. Additionally,
  warnings in `.cabal` files in indexes are printed instead of being silently
  discarded.

- Add support for Windows AArch64 [#10705](https://github.com/haskell/cabal/pull/10705)

  Adds to preprocessor branches the option to support `aarch64_HOST_ARCH` platform on Windows AArch64 target.
  `ccall` convention is used at AArch64, same as for `x86_64_HOST_ARCH`. Introduce `zIsAArch64` to make paths generation support Windows AArch64 target.

- Merge `flags` stanzas in cabal.project files instead of taking the last [#10767](https://github.com/haskell/cabal/issues/10767) [#10805](https://github.com/haskell/cabal/pull/10805)

  Now

  ```
  flags: -foo
  flags: -bar
  ```

  is equivalent to `flags: -foo -bar` while before it was equivalent to `flags: -bar`
  (only the latest stanza was taken into account, the other ones were silently
  ignored).

- Add checks for Windows reserved filenames in module paths [#10295](https://github.com/haskell/cabal/issues/10295) [#10816](https://github.com/haskell/cabal/pull/10816)

  On Windows, certain filenames are reserved by the operating system such as
  "aux", "con", "prn", "nul", etc. When these names appear in module paths they
  can cause build failures on Windows systems.

  `cabal check` now properly warns about module paths that contain Windows reserved
  filenames, not just filepaths which contain these reserved tokens. These warnings
  are controlled by the existing `invalid-win-path` category.

  For example, module paths like:
  - `Exe.Aux.Test`
  - `Test.Aux.Module`
  - `Bench.Aux.Helpers`

  will now trigger appropriate warnings during `cabal check`.

- Fix `gen-bounds` command to work in multi-package projects [#7504](https://github.com/haskell/cabal/issues/7504) [#10840](https://github.com/haskell/cabal/pull/10840)

  The command has been reimplemented to use the cabal.project infrastructure (similar
  to other v2 commands), allowing it to be aware of all packages defined in the cabal.project
  file, regardless of which directory it's executed from.

  ```
  $ cat cabal.project
  packages: package-a/
            package-b/

  $ cd package-b/
  $ cabal gen-bounds
  Configuration is affected by the following files:
  - cabal.project
  Resolving dependencies...

  The following packages need bounds and here is a suggested starting point...
  For component package-b:lib:package-b:
  package-a >= 0.1.0 && < 0.2,
  ```

- Fix multi-repl when using reexported-modules with renaming for GHC >= 9.12 [#10181](https://github.com/haskell/cabal/issues/10181) [#10880](https://github.com/haskell/cabal/pull/10880)

  Since GHC 9.12, the `-reexported-module` flag has supported module renaming. Therefore
  we now use that functionality when starting the multi-repl if it is needed. A new
  error message is added to catch the case where you attempt to load a project which
  uses this complicated export form but are using < 9.12.

- Remove dead build-tool `greencard` [#10908](https://github.com/haskell/cabal/pull/10908)

  Remove knowledge of the build-tool `greencard` (non-existing since GHC 7.10)
  and the connect of the `.gc` extension to this build-tool.

- Remove knowledge about `hmake` and `haskell-suite` [#10912](https://github.com/haskell/cabal/pull/10912)

  The `hmake` tool has long been abandoned, and the `haskell-suite` compiler did not emerge.
  Knowledge about these tools has been removed from `Cabal`.

- Print out the "Created semaphore" message only in verbose mode [#10885](https://github.com/haskell/cabal/issues/10885) [#10936](https://github.com/haskell/cabal/pull/10936)

- Print out project files only when run below the project root (or verbose) [#10885](https://github.com/haskell/cabal/issues/10885) [#10940](https://github.com/haskell/cabal/pull/10940)

  After [#10507](https://github.com/haskell/cabal/pull/10507) released in
  cabal-install-3.14, cabal prints out what project files are in use on every run.

  This looked too noisy for some users
  ([#10885]((https://github.com/haskell/cabal/issues/10885))). In this patch,
  we implement a more nuanced strategy: print out this info only when cabal is
  run below the root project directory.
  As before, you can get this information unconditionally if run in the verbose mode.

  Bonus: we now also print the project root directory along with the file names.
  Before, we only printed, say, `cabal.project`, but it wasn't clear where this file is
  coming from (can be anywhere up the directory tree).
  The change tries to avoid confusion when cabal picks up stray project files in
  ancestor directories, see discussion in
  [#7930](https://github.com/haskell/cabal/issues/7930).

- Make `lukko` flag automatic and off by default [#10724](https://github.com/haskell/cabal/issues/10724) [#11003](https://github.com/haskell/cabal/pull/11003)

  Make `lukko` flag automatic and off by default, using file locking facilities from `base:GHC.IO.Handle.Lock` and not from the `lukko` package. The change is not expected to affect anyone detrimentally, but one can set the flag on in their configuration to restore the previous behaviour.

- `cabal init` generates explicit export lists for Main [#9889](https://github.com/haskell/cabal/issues/9889) [#9890](https://github.com/haskell/cabal/pull/9890)

  - Lack of explicit export list can degrade performance. The `Main` module in particular should always have an explicit export list that contains just the main function. Then, the compiler can do more aggressive optimizations on all the other non-exported functions.

- Fix PATH changes not triggering REPL reconfiguration [#2015](https://github.com/haskell/cabal/issues/2015) [#10817](https://github.com/haskell/cabal/pull/10817)

- Report trailing spaces in project import URIs [#10622](https://github.com/haskell/cabal/issues/10622) [#10629](https://github.com/haskell/cabal/pull/10629)

  > A string is a valid URL potentially surrounded by spaces if, after stripping
  > leading and trailing whitespace from it, it is a valid URL."

  Source: [W3C/HTML5/URLs](https://www.w3.org/TR/2010/WD-html5-20100624/urls.html)

  Fixes a problem of mistaking a URI for a file path when it has trailing spaces
  and warn about such trailing spaces.

- Configuration messages without duplicates [#10645](https://github.com/haskell/cabal/issues/10645) [#10646](https://github.com/haskell/cabal/pull/10646)

  The "using configuration from" message no longer has duplicates on Windows when
  a `cabal.project` uses forward slashes for its imports but the message reports
  the same import again with backslashes.

### Unresolved

- curl error with `cabal upload -d` [#10920](https://github.com/haskell/cabal/issues/10920)

    Uploading documentation to Hackage can result in a
    `chunk hex-length char not a hex digit` error.

- New git invocation changes the behaviour of source-repository-package [#10605](https://github.com/haskell/cabal/issues/10605)

    Invoking `cabal build` with `source-repository-package` leads to
    errors when the `tag` is specified with abbreviated commit hashes.

- `extra-libraries` is overwritten by configure script [#10961](https://github.com/haskell/cabal/issues/10961)

    If you have two `extra-libraries`, one in the `.cabal` file and the
    second in the configure script, the latter will be appended on the
    right.

    If the correct library is picked in the configure script, it is advisable
    to remove `extra-libraries` from the `.cabal` file.
