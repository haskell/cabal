## cabal-install and cabal-install-solver 3.14.2.0 changelog

### Significant changes

- Honour `extra-prog-path` from the the global configuration  [#9800](https://github.com/haskell/cabal/issues/9800) [#10826](https://github.com/haskell/cabal/pull/10826)

  The initial configuration of programs was done without the `extra-prog-path` from the global cabal configuration. This meant that in some cases, some executables were not found. In particular this manifested as Windows users who didn't add the MSYS2 paths to the global PATH couldn't make use of `pkg-config`.

- Fix regression where build-tool-depends are not used [#10633](https://github.com/haskell/cabal/issues/10633) [#10692](https://github.com/haskell/cabal/issues/10692) [#10731](https://github.com/haskell/cabal/pull/10731)

  Fixes a regression from 3.14.1.0 where a globally found executable would be found
    rather than a dependency specified with `build-tool-depends`.

- Fix `cabal repl --enable-multi-repl` when only specifying some targets from a package. [#10775](https://github.com/haskell/cabal/issues/10775) [#10841](https://github.com/haskell/cabal/pull/10841)

  Fix a bug `cabal repl --enable-multi-repl` where the repl would fail to start if you
  only specified some targets to be loaded.
  In particular, if you are using internal libraries you may be affected by this bug.

### Other changes

- Fix `file+noindex` URI usage on Windows [#10703](https://github.com/haskell/cabal/issues/10703) [#10728](https://github.com/haskell/cabal/pull/10728) [#10746](https://github.com/haskell/cabal/pull/10746)

  `file+noindex` repositories in Windows systems must use the format
  `file+noindex:C:/path/to/repo`.  This is the only syntax for DOS paths fully
  supported by the `network-uri` package, which Cabal uses to interpret URIs in
  repository stanzas.

- Create `noindex.cache` file if missing [#9891](https://github.com/haskell/cabal/issues/9891) [#10730](https://github.com/haskell/cabal/pull/10730)

  Local+noindex repositories will have their `noindex.cache` file created the first time they are accessed.

- Fix regression where 'build' folder was created in CWD when running a standlone script. [#10772](https://github.com/haskell/cabal/issues/10772) [#10800](https://github.com/haskell/cabal/pull/10800)

  Fix a regression where the `build` folder was created in the current directory
  when running a standalone script.

- Fix duplicate environment variables in test and benchmark runs [#10718](https://github.com/haskell/cabal/issues/10718) [#10827](https://github.com/haskell/cabal/pull/10827)

  Cabal no longer creates duplicate environment variables when running test
  suites, benchmarks, or internal executables. Previously, when setting up the
  environment for these processes, Cabal would append the overridden environment
  to the existing environment, creating duplicates of the same variable.

- Deduplicate "using configuration from" message [#10546](https://github.com/haskell/cabal/pull/10546)

  Deduplicates and sorts the list of configuration files and URIs printed with the
  "using configuration from" message. This message is shown when there's a build
  failure. We can trigger that message by using a non-existant package in the
  project, "no-pkg-dir".

  If an import is repeated in a `.project` or `.config` file it only imported once
  but if the same import is made from an imported file then it was being repeated
  in the message. Additional problems were not showing the project first and
  mixing configuration files and URIs together.

  * Before the fix:

    ```
    $ ~/.ghcup/bin/cabal-3.12.1.0 build all --dry-run
    When using configuration from:
    - a-very-extra.config
    - an-extra.config
    - cabal.project
    - https://www.stackage.org/lts-21.25/cabal.config
    - https://www.stackage.org/lts-21.25/cabal.config
    - https://www.stackage.org/lts-21.25/cabal.config
    - z-empty.config
    The following errors occurred:
    - The package location 'no-pkg-dir' does not exist.
    ```

  * After the fix:

    ```
    $ cabal build all --dry-run
    When using configuration from:
    - cabal.project
    - a-very-extra.config
    - an-extra.config
    - z-empty.config
    - https://www.stackage.org/lts-21.25/cabal.config
    The following errors occurred:
    - The package location 'no-pkg-dir' does not exist.
      ```
- Add a custom `Ord` instance for `ProjectConfigPath` ([#10546](https://github.com/haskell/cabal/pull/10546))

  The instance sorts URIs after local
  file paths and longer file paths after shorter ones as measured by the number of
  path segments. If still equal, then sorting is lexical.  The project itself, a
  single element root path, compared to any of the configuration paths it imports,
  should always sort first. Comparing one project root path against another is
  done lexically.

### Unresolved Issues

- Using the short git hashes in source-repository-package will no longer work [#10605](https://github.com/haskell/cabal/issues/10605)

  It has always been assumed, but not checked, that you will specify the
  tag of a repository package using an unambiguous hash. If you encounter this
  problem then replace your short hash with a full long hash for a backward and forward
  compatible fix.

