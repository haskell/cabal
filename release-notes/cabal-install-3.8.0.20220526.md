cabal-install 3.8.0.20220526 Changelog
---

### Significant changes

- `cabal init` rewrite [#1074](https://github.com/haskell/cabal/issues/1074) [#6758](https://github.com/haskell/cabal/issues/6758) [#6864](https://github.com/haskell/cabal/issues/6864) [#7251](https://github.com/haskell/cabal/issues/7251) [#7255](https://github.com/haskell/cabal/issues/7255) [#7256](https://github.com/haskell/cabal/issues/7256) [#7273](https://github.com/haskell/cabal/issues/7273)

  - Restructures the `cabal init` command to fix historical
    issues. All flags are preserved.
    - Codebases for interactive and non-interactive flags
      are disentangled.
    - Data structures now exploit relevant stanza structure
      and formatters only care about stanza data.
    - Heuristics and prompts have a pure and impure implementation.

  - Sets default behavior to be `--interactive` as opposed to
    `--non-interactive`.

  - Rewrites tests to achieve 98% coverage
    - Golden files now test every stanza individually.
    - Every flag is covered by a unit test.
    - Interactive, simple, and non-interactive workflows are
      covered.

- Make `Paths_` modules work with non-standard preludes again [#7494](https://github.com/haskell/cabal/issues/7494) [#7649](https://github.com/haskell/cabal/issues/7649)

  - Reverts [#7510](https://github.com/haskell/cabal/pull/#7510) that failed on Windows when used with pre-generated scripts included in packages such as `network`, `time`, `process`.
  - Adds a subtler fix/workaround for the deficiencies of new `autoconf` versions on Windows.

- Windows: rewrite paths to configure [#7494](https://github.com/haskell/cabal/issues/7494) [#7649](https://github.com/haskell/cabal/issues/7649)

- Expose `cabal-install` as library [#1597](https://github.com/haskell/cabal/issues/1597) [#3781](https://github.com/haskell/cabal/issues/3781) [#4798](https://github.com/haskell/cabal/issues/4798) [#6090](https://github.com/haskell/cabal/issues/6090) [#7224](https://github.com/haskell/cabal/issues/7224) [#7358](https://github.com/haskell/cabal/pull/7358)

- Conditionals and imports in `cabal.project` files [#7556](https://github.com/haskell/cabal/issues/7556) [#7783](https://github.com/haskell/cabal/pull/7783)

  `cabal.project` files now allow conditional logic on compiler version, arch, etc. as well as imports of other local or remote project of freeze files (both old and new style).

- Split out package `Cabal-syntax` for `.cabal` file syntax and parsing [#7559](https://github.com/haskell/cabal/issues/7559) [#7620](https://github.com/haskell/cabal/pull/7620)

### Other changes

- Remove `world` file functionality [#6767](https://github.com/haskell/cabal/issues/6767) [#7746](https://github.com/haskell/cabal/pull/7746)

  In `v1-install`, `world` was used to trace what packages have been
  installed, and re-install everything that is listed in it on-demand. However, in
  `v2-install`, this is no longer needed, and outdated.
  Additionally, `world` code-path is probably not up-to-date, remove it instead of
  having partial features.

  - Don't generate `world` file in `~/.cabal` when `v1-install` is executed.

  - Don't ever read `world` from `~/.cabal`.

  - Remove meta-target `world` from `v1-install`.

- A default configuration is no longer written when a config file is given explicitly [#7705](https://github.com/haskell/cabal/issues/7705) [#7715](https://github.com/haskell/cabal/pull/7715)

  - When no config file exists, `cabal` commands will create a default one.

  - This behavior is discontinued now when the config file is given explicitly
    via the `--config-file` option or the `CABAL_CONFIG` environment variable.

  - If no config file is given explicitly, and the default config file
    (relative to `CABAL_DIR`) does not exist, it is created as it used to be.

- Use `nroff -man | less` instead of `man` as backend for `cabal man` [#7714](https://github.com/haskell/cabal/issues/7714) [#7726](https://github.com/haskell/cabal/pull/7726)

  The previous implementation of `cabal man` used `man` as backend to
  view the generated manpage file.  However, macOS' and BSD's `man` does
  not accept a file as input, so `cabal man` was broken on macOS and
  BSD.

  The issue has been fixed by piping the generated manpage through
  `nroff -man` and then into `$PAGER` which defaults to `less`.  Thus,
  `cabal man` now works both on Linux and macOS/BSD.

  NB: `cabal man` requires `nroff` to be installed which is the case for
  standard setups (but may not be case on slim setups like VMs used in
  CI).  Note that `nroff` is not necessarily a prerequisite of a package
  containing `man`.

- Use `Win32 >= 2.8` to get `Win32.sYNCHRONIZE` [#7835](https://github.com/haskell/cabal/issues/7835) [#7982](https://github.com/haskell/cabal/pull/7982)

  #7982 also completes a changelog entry for `Cabal-3.6.1.0`, noting it is required for `cabal-install >= 3.6`.

- Handle option argument parse errors without `error` [#7573](https://github.com/haskell/cabal/issues/7573) [#7579](https://github.com/haskell/cabal/pull/7579)

  - Errors parsing arguments such as `-v=3` no longer result in
    stack traces.
  - `Distribution.ReadE.readEOrFail` was removed.

- Add `preferred-versions` support for `LocalIndexRepo` [#7294](https://github.com/haskell/cabal/issues/7294) [#7295](https://github.com/haskell/cabal/pull/7295)

  - Previously, the only repo-index-type that reads the preferred-versions file was `RepoRemote`.
    `LocalIndexRepo` now also supports parsing `preferred-versions` file, main purpose is to write tests.
    As a nice side-effect, users can provide their own overlay over package sets to restrict or prefer certain package versions.

- Add `preferred-versions` support for `cabal outdated` [#5603](https://github.com/haskell/cabal/issues/5603) [#7249](https://github.com/haskell/cabal/issues/7249) [#7296](https://github.com/haskell/cabal/pull/7296)

  - `cabal outdated` honours the `preferred-versions` file which might deprecate or prefer certain
    versions of packages. In particular, if the only newer version of a package has been deprecated,
    `cabal outdated` should not report that there are newer versions available.

- Changes to `cabal v2-configure` [#5591](https://github.com/haskell/cabal/issues/5591) [#7180](https://github.com/haskell/cabal/issues/7180) [#7405](https://github.com/haskell/cabal/issues/7405) [#7402](https://github.com/haskell/cabal/pull/7402)

  - removes the `--dry-run` part of the `v2-configure` command
  - add `--enable-append` and `--disable-append` flags to `v2-configure`, which toggle the option for the new configuration to be appended to the old config file
  - add `--enable-backup` and `--disable-backup` flags to `v2-configure`, which toggle the backup feature for the configuration file

- Standalone tests for `cabal init` [#7410](https://github.com/haskell/cabal/issues/7410) [#7424](https://github.com/haskell/cabal/pull/7424)

  - change the behaviour of the `--tests` flag for `cabal init`,
    see https://github.com/haskell/cabal/issues/7410#issuecomment-849913926
  - add a new value `TestSuite` for the `PackageType` type

- `cabal outdated --v2-freeze-file` prints error message if no freeze file can be found [#7406](https://github.com/haskell/cabal/issues/7406) [#7440](https://github.com/haskell/cabal/pull/7440)

  Instead of ignoring a missing `.freeze` file, abort execution and print an error message.

- Make `cabal` respect `setgid` permission bit when creating directories [#7560](https://github.com/haskell/cabal/issues/7560) [#7572](https://github.com/haskell/cabal/pull/7572)

  - Previously `cabal` would override an admin's choice to use setgid on the
    `.cabal` directory by using hardcoded file and directory modes upon
    creation. Instead we now take what the system decides and only add to
    that set of permissions.

- Glob expand `extra-source-files` when tracking for rebuild [#4746](https://github.com/haskell/cabal/issues/4746) [#7608](https://github.com/haskell/cabal/pull/7608)

  - Since 3.4, `extra-source-files` have been tracked by cabal-install in the rebuild-checking logic. However, this tracking did not, until this PR, take into account glob-expansion.

- Improve error message when no test/benchmark is found [#5079](https://github.com/haskell/cabal/issues/5079) [#5255](https://github.com/haskell/cabal/issues/5255) [#7834](https://github.com/haskell/cabal/pull/7834)

  When `cabal test` and the error message recommends setting `tests: True`, the next `cabal test` run is likely to fail with a solver error. The user might incorrectly conclude that setting `tests: True` made the problem worse, because the failure now occurs earlier (at solving time rather than at testing time).

  By including the fact that a plan failure is expected in the error message, hopefully users will be more confident that setting `tests: True` was the right move, so they will be able to focus on the true cause of the problem: the fact that no plan including the tests exists.

- Better support for scripts [#5508](https://github.com/haskell/cabal/issues/5508) [#5698](https://github.com/haskell/cabal/issues/5698) [#6149](https://github.com/haskell/cabal/issues/6149) [#6354](https://github.com/haskell/cabal/issues/6354) [#7073](https://github.com/haskell/cabal/issues/7073) [#7842](https://github.com/haskell/cabal/issues/7842) [#7851](https://github.com/haskell/cabal/pull/7851) [#7925](https://github.com/haskell/cabal/pull/7925) [#7938](https://github.com/haskell/cabal/pull/7938) [#7990](https://github.com/haskell/cabal/pull/7990) [#7997](https://github.com/haskell/cabal/pull/7997)

  - Script support improved or added across relevant commands.
  - `cabal run script` will now cache results and will not do a fresh build every time.
  - `cabal build script` added: It will build the cache for script.
  - `cabal repl script` added: It will open a repl for script using the cache if available.
  - `cabal clean script` added: It will clean the cache for script.
  - `cabal clean` will now remove script caches for which there is no marching script.
  - `cabal list-bin` now works with scripts
  - The name of the generated script executable has been changed from "script" to
    "cabal-script-<your-sanitized-script-name>" for easier process management.
  - Reduce the default verbosity of scripts, so that the build output doesn't interfere with the script output.
  - Scripts now support a project metadata block that allows them to use options
    that would normally be set in a cabal.project file.

- Remove `Distribution.Client.Compat.FilePerms` [#7948](https://github.com/haskell/cabal/pull/7948)

  - Remove the module `Distribution.Client.Compat.FilePerms`, since it's
    both dead code and duplicates code from `Distribution.Compat.CopyFile`.

- Avoid malformed range requests [#5952](https://github.com/haskell/cabal/issues/5952) [#7970](https://github.com/haskell/cabal/pull/7970)

  - Don't send malformed range requests. Should make fetching from head.hackage and other "unstable" overlays more reliable.

- Apply local options only to local packages [#7998](https://github.com/haskell/cabal/issues/7998) [#7973](https://github.com/haskell/cabal/pull/7973)

  - Command-line `ghc-options` only applies to local packages
  - `program-options` stanza only applies to local packages

- Improve error message for empty `--allow-newer=`  [#7740](https://github.com/haskell/cabal/issues/7740) [#8140](https://github.com/haskell/cabal/pull/8140)

  Instead of internal error, the message now explains that empty argument for
  `--allow-newer=` is not allowed and reminds what `--allow-newer` (with the empty
  argument) means.

- `cabal check` now fails when no upper bounds for `base` or `Cabal` are present in setup dependencies [#4683](https://github.com/haskell/cabal/issues/4683) [#5370](https://github.com/haskell/cabal/pull/5370) [#7409](https://github.com/haskell/cabal/pull/7409)

- Fix `v2-haddock` results in "unexpected status" [#6679](https://github.com/haskell/cabal/issues/6679) [#7843](https://github.com/haskell/cabal/pull/7843)

- Fix that cabal v2 commands (`install`, `repl`, `run`) use duplicate global config and rebuild libs twice in the store [#6907](https://github.com/haskell/cabal/issues/6907) [#7753](https://github.com/haskell/cabal/pull/7753) [#7759](https://github.com/haskell/cabal/pull/7759)

- `--repl-no-load` option skips startup modules load in REPL [#7541](https://github.com/haskell/cabal/issues/7541) [#7578](https://github.com/haskell/cabal/pull/7578)

- Fix post-checkout-command crash when 0 exit status bug [#7641](https://github.com/haskell/cabal/issues/7641) [#7847](https://github.com/haskell/cabal/pull/7847)

- Fix `list-bin` to only choose the selected component [#7679](https://github.com/haskell/cabal/issues/7679) [#7791](https://github.com/haskell/cabal/pull/7791)

- `cabal sdist` works in projects with `extra-packages` [#7698](https://github.com/haskell/cabal/issues/7698)

- Add `-c` alias for `--constraint` command line flag [#7765](https://github.com/haskell/cabal/issues/7765) [#7766](https://github.com/haskell/cabal/pull/7766)

- Ensure that `v2-sdist` command respects the `--ignore-project` flag [#7965](https://github.com/haskell/cabal/issues/7965) [#8109](https://github.com/haskell/cabal/pull/8109)

- Make enable/disable nix flags easier to read [#8036](https://github.com/haskell/cabal/issues/8036) [#8054](https://github.com/haskell/cabal/pull/8054)

- Add "prompt" strategy when symlinking binaries. [#5672](https://github.com/haskell/cabal/pull/5672)

- Add a verbosity flag `+nowarn`, to suppress all warnings [#7286](https://github.com/haskell/cabal/issues/7286) [#7470](https://github.com/haskell/cabal/pull/7470)

- Lazily decode cache files for checking invalidation [#7466](https://github.com/haskell/cabal/issues/7466) [#7516](https://github.com/haskell/cabal/pull/7516)

- Add `--open` flag to `cabal haddock` [#7366](https://github.com/haskell/cabal/issues/7366) [#7550](https://github.com/haskell/cabal/pull/7550)

- Defer `build-tools-depends` choices as well as setup choices [#7532](https://github.com/haskell/cabal/pull/7532) [#7561](https://github.com/haskell/cabal/pull/7561)

- Fix running GHCJS executables [#6175](https://github.com/haskell/cabal/issues/6175) [#6361](https://github.com/haskell/cabal/issues/6361) [#7575](https://github.com/haskell/cabal/pull/7575)

- Support Git submodules in `source-package-repository` [#5536](https://github.com/haskell/cabal/issues/5536) [#7625](https://github.com/haskell/cabal/pull/7625)

- Add support for `--package-db` flags to v2 commands [#5773](https://github.com/haskell/cabal/issues/5773) [#7676](https://github.com/haskell/cabal/pull/7676)

- `cabal init` now takes an optional argument and treats it as root directory for the new package [#7871](https://github.com/haskell/cabal/issues/7871) [#7873](https://github.com/haskell/cabal/pull/7873)

- Silence warning about `world`-file field being unrecognised for flat config files [#7894](https://github.com/haskell/cabal/issues/7894) [#7903](https://github.com/haskell/cabal/pull/7903)

- Fix Ctrl-C handling during package download [#6322](https://github.com/haskell/cabal/issues/6322) [#7929](https://github.com/haskell/cabal/pull/7929)

- Fix the timestamp shown during `cabal update` [#7934](https://github.com/haskell/cabal/pull/7934)

- Sync darcs repos [#7137](https://github.com/haskell/cabal/pull/7137)

- `cabal-install-solver`: Provide more context in error messages [#7468](https://github.com/haskell/cabal/issues/7468) [#7473](https://github.com/haskell/cabal/pull/7473)

  - Error messages for misspelled extensions and languages now provide additional context.
  - The error messages will provide a suggested fix if the misspelled extension or language is similar enough
    to known languages and extensions.

### Internal changes

- Remove bootstrapping plan files from version control, and simplify bootstrap update `Makefile` targets. [#7949](https://github.com/haskell/cabal/pull/7949)

- CI setup

  - Remove Travis scripts [#6959](https://github.com/haskell/cabal/pull/6959)
