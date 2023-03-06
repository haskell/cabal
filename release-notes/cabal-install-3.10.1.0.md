cabal-install and cabal-install-solver 3.10.1.0 changelog and release notes
---

Release 3.10.1.0 of cabal now sets the `--enable-documentation` option by default
when running `cabal haddock` (https://github.com/haskell/cabal/issues/7462).
If the new default does not fit your workflow, pass an explicit `--disable-documentation`
option or consider (helping to implement) the other workarounds proposed
in https://github.com/haskell/cabal/issues/8725.

This version of cabal introduces support for JS cross-compilation (https://github.com/haskell/cabal/pull/8636).
We've seen related Windows segfaults on CI, so please kindly report if you can reproduce either
cabal or GHC segfaults when cross-compiling for JS or compiling normally but with any files
listed in the `js-sources` field of a .cabal file.

This release of cabal also fixes a lot of bugs in cabal 3.8.1.0 and not all of the fixes
are listed here.


### Significant changes

- `Cabal-3.10.1.0` is used, which brings [its own set of changes](./Cabal-3.10.1.0.md),
  many of which are significant. The Cabal and Cabal-syntax changes that touch
  the cabal-install or cabal-install-solver codebases are pasted here again in full.

- Add support for the XDG Base Directory Specification [#680](https://github.com/haskell/cabal/issues/680) [#7386](https://github.com/haskell/cabal/pull/7386)

  Cabal/cabal-install now uses the XDG Base Directory Specification to
  store configuration, caches, and the store.  Specifically,
  `$XDG_CONFIG_HOME/cabal` stores the configuration file,
  `$XDG_CACHE_HOME/cabal` stores downloaded packages and similar, and
  `$XDG_STATE_HOME/cabal` mainly contains the store of compiled
  packages.  Further, `cabal install` will put executables in
  `~/.local/bin` by default.

  The `dist`/`dist-newstyle` directories are not affected.

  On Windows, these XDG paths are mapped to other (hopefully)
  appropriate locations.  See the Cabal User Guide for information.

  If the `CABAL_DIR` environment variable is set, the indicated
  directory will be used to store all Cabal-related files, as in
  previous versions.

  **Backwards compatibility**: If `~/.cabal` already exists, this will be
  interpreted as `CABAL_DIR=~/.cabal`.  This means that upgrading on an
  existing system already using cabal-install should not cause any
  change in behaviour.  An existing system can be migrated by deleting
  `~/.cabal` (possibly copying `~/.cabal/config` to
  `~/.config/cabal/config` first).

- Add `installDirs` (`datadir`, etc) to v2-build and remove them from v2-install [#8556](https://github.com/haskell/cabal/pull/8556)

  - Passes through `--prefix`, `--datadir` and other `installdir`-arguments properly in v2-build, allowing its use in preparing somewhat more relocatable compilation targets (no worse than v1-build).
  - Removes these flags (which never worked) from v2-install

- No global packages auto written to environment files, allow `--force-reinstalls` [#5559](https://github.com/haskell/cabal/issues/5559) [#6165](https://github.com/haskell/cabal/issues/6165) [#8607](https://github.com/haskell/cabal/pull/8607)

  - When v2-install runs on a library, it does not pin global packages in the env file, only those directly necessary for the library.

- Add support for GHC 9.4+ `-fprof-late` flag [#8528](https://github.com/haskell/cabal/pull/8528)

  - This adds the new `late-toplevel` (and its alias `late`) profiling-detail option which enables `-fprof-late`-based automatic cost centre annotations for GHCs that support it ([earliest is GHC 9.4.1](https://downloads.haskell.org/ghc/9.4.1/docs/users_guide/profiling.html#ghc-flag--fprof-late)).

- Added `haddock-project` command [#7669](https://github.com/haskell/cabal/issues/7669) [#8162](https://github.com/haskell/cabal/pull/8162)

  The `haddock-project` command can be used to build documentation of multiple
  packages.  By passing `--local` option the directory will be self contained,
  by passing `--hackage` links to dependencies will link to `hackage`.  Both
  `--local` and `--hackage` options imply `--quickfix`, `--gen-index`,
  `--gen-contents`, and `--hyperlinked-source`.

  Building self contained directory is the default, unless `--hackage` or
  `--html-location`.

  The PR #8162 also fixes the `--with-haddock` option.


### Other changes

- Pretty-print run targets on failure [#8189](https://github.com/haskell/cabal/issues/8189) [#8234](https://github.com/haskell/cabal/pull/8234)

  - Targets of the `run` command are pretty-printed when failing due to multiple targets.
  - Duplicate targets are removed in the output.

- Expose `BuildFlags` and `ClientHaddockFlags` [#8351](https://github.com/haskell/cabal/issues/8351) [#8353](https://github.com/haskell/cabal/pull/8353)

  Some types are present in public functions in cabal-install, but they are not exported.
  This PR fix it for BuildFlags and ClientHaddockFlags.

- `cabal man` handles `$PAGER` containing arguments now [#8405](https://github.com/haskell/cabal/issues/8405) [#8353](https://github.com/haskell/cabal/pull/8353)

  Things like `PAGER="less -FX" cabal man` work now.

  There's a slight change in the default behavior. We still use `less -R` as the default,
  but if the user passes `PAGER=less`, we won't add -R to that, unlike before. This is
  reasonable, as the user should be able to set the variable as they see fit.

- Fix issue with "nix" config option [#8452](https://github.com/haskell/cabal/issues/8452) [#8522](https://github.com/haskell/cabal/pull/8522)

  Nix option in config file was broken with [#8054](https://github.com/haskell/cabal/pull/8054), this should fix it.

- "Build profile" message now reflects optimization level set in global config [#8487](https://github.com/haskell/cabal/issues/8487) [#8488](https://github.com/haskell/cabal/pull/8488)

  Imagine you have `optimization: 2` in your `~/.cabal/config`, and you call `cabal build`
  in a project that doesn't have optimization level explicitly set in its project file.
  You will still see 'Build profile: -w ghc-<VER> -O1'. This is incorrect and was fixed
  in this patch: now you'll see '-O2'.

- `ghc`, `ghc-boot`, `ghci` packages should be non-upgradable [#8489](https://github.com/haskell/cabal/issues/8489) [#8501](https://github.com/haskell/cabal/pull/8501)

  - Changed both `Distribution.Client.Dependency`'s `dontUpgradeNonUpgradeablePackages`
    and `Distribution.Solver.Modular.Solver`'s `nonInstallable` to be in sync.

- Apply command line flags to install packages [#8637](https://github.com/haskell/cabal/issues/8637) [#8779](https://github.com/haskell/cabal/pull/8779)

  - Command line flags usually only apply to "local" packages (packages specified
    in the cabal.project). This change causes the v2-install command to ignore
    that distinction to better match the expected behavior for packages specified
    directly in the command.

- Print a warning when assertions are enabled [#4377](https://github.com/haskell/cabal/issues/4377) [#8240](https://github.com/haskell/cabal/pull/8240)

  - Now cabal-install executable will print a warning if assertions are enabled

- Implement `--prefer-oldest` [#8261](https://github.com/haskell/cabal/pull/8261)

  - The new flag makes Cabal solver try to find a build plan with the oldest versions possible. This is useful to establish lower bounds.

- Update and sdist now only check for compiler when project file has conditionals [#8352](https://github.com/haskell/cabal/issues/8352) [#8550](https://github.com/haskell/cabal/issues/8550) [#8589](https://github.com/haskell/cabal/issues/8589) [#8358](https://github.com/haskell/cabal/pull/8358) [#8627](https://github.com/haskell/cabal/pull/8627)

  - Cabal update and sdist will not require a ghc in path unless the project has conditionals that require it

- Allow offline bootstrapping of cabal-install [#8368](https://github.com/haskell/cabal/pull/8368)

  - The bootstrap script for cabal-install now supports fetching the sources of the dependencies in a separate step.
    One can then copy over the resulting archive and perform offline bootstrapping of cabal-install.

- Pass some haddock flags to dependencies [#395](https://github.com/haskell/cabal/issues/395) [#8104](https://github.com/haskell/cabal/issues/8104) [#8414](https://github.com/haskell/cabal/pull/8414)

  - Pass `--haddock-hoogle`, `--haddock-html`, `--haddock-internal`, `--haddock-quickjump`, `--haddock-hyperlinked-source`
    to all the dependencies if they are specified as command line args

- build pkgconfig db individually when bulk fails [#8494](https://github.com/haskell/cabal/issues/8494) [#8496](https://github.com/haskell/cabal/pull/8496)

  - When pkg-config fails to get versions for all packages in bulk, falls back to querying one-by-one.

- improve install target handling logic regarding of project files [#8094](https://github.com/haskell/cabal/issues/8094) [#8352](https://github.com/haskell/cabal/issues/8352) [#8498](https://github.com/haskell/cabal/pull/8498)

  - fixes an issue where `cabal install pkg-x.y.z` fails unless `--ignore-project` is given
  - fixes an issue where `cabal install --ignore-project` crashes when a project file is present

- Redownload packages when source hash verification fails [#7541](https://github.com/haskell/cabal/issues/7541) [#8500](https://github.com/haskell/cabal/pull/8500)

  - Cabal-install will verify source hashes on cached downloads against the current index, and redownload on mismatch. (Which can occur with e.g. head.hackage)

- `cabal init -i` should sanitize package name guessed from the directory name [#8404](https://github.com/haskell/cabal/issues/8404) [#8561](https://github.com/haskell/cabal/pull/8561)

  If the current directory name has any non-alphanumeric symbols in its name, the symbols will be replaced with a dash. Also, will make sure that the resulting package name starts with a letter.

  This worked for cabal init -n already, and this PR only moves code around so that cabal init -i also benefits from this logic.

- Support `js-sources` with GHC, not only with GHCJS [#8636](https://github.com/haskell/cabal/pull/8636)

  - Take into account `js-sources` when building library components with GHC
  - Missing support for `js-sources` in executable components is tracked in #8639

- Fix `extra-source-file` rebuild tracking when run in a multi-package project [#8632](https://github.com/haskell/cabal/issues/8632) [#8634](https://github.com/haskell/cabal/issues/8634) [#8640](https://github.com/haskell/cabal/pull/8640)

  - Fixes an issue where glob expansion of `extra-source-files` for rebuild tracking purposes was not occurring correctly when run in a multi-package setting (i.e. when the globs needed to be expanded relative to something other than ".").

- No up-front warning that assertions are on for special commands (help, version, numeric-version) [#8645](https://github.com/haskell/cabal/issues/8645) [#8647](https://github.com/haskell/cabal/pull/8647)

  - When compiled with ghc option `-fno-ignore-assert`, `cabal-install` will issue a generic warning that assertions are on.
    This warning will no longer be emitted for special modes of operation like `cabal --numeric-version`, `--version` and help,
    and also not if `cabal` could not parse its command line.
    Further, the warning now goes to `stderr` rather than `stdout`.

- Fix and improve list parser for `cabal init` arguments [#8659](https://github.com/haskell/cabal/issues/8659) [#8663](https://github.com/haskell/cabal/pull/8663)

  Occurrences of 'Flag [a]' seem to behave in an unexpected way. The monoid
  instance of 'Flag' is right associative and discard the value on the
  left, but we want to merge the contents of 'Flag'.

  Permits:
  - `cabal init -d base -d vector -d containers`

  Fixes for all Flag '[a]' the cli parser in cabal init. Adds cli parser tests.

  Adds the feature to specify a comma-separated list of dependencies:
  - `cabal init -d base,vector,containers`

- Fix resolution of imports by relative paths in cabal.project [#8686](https://github.com/haskell/cabal/pull/8686)

  Fix bug where cabal tries to resolve imports by relative paths against
  the directory cabal executable was invoked in rather than directory of
  cabal.project file that does the import.

- Fix `cabal repl` discarding `--build-depends` [#6859](https://github.com/haskell/cabal/issues/6859) [#7081](https://github.com/haskell/cabal/issues/7081) [#8732](https://github.com/haskell/cabal/pull/8732)

  - Fix `repl` command discarding `--build-depends` argument when using
    `allow-newer` or `allow-older`.

- Add `cabal get --only-package-description` [#1954](https://github.com/haskell/cabal/issues/1954) [#1977](https://github.com/haskell/cabal/pull/1977) [#5162](https://github.com/haskell/cabal/pull/5162) [#8263](https://github.com/haskell/cabal/pull/8263)

- `cabal haddock` now implies `--enable-documentation` [#7462](https://github.com/haskell/cabal/issues/7462) [#8259](https://github.com/haskell/cabal/pull/8259)

- Include `extra-lib-dirs-static` into `PackageHash` [#6935](https://github.com/haskell/cabal/issues/6935) [#7794](https://github.com/haskell/cabal/pull/7794)

- Document `cabal check` in the user guide [#8237](https://github.com/haskell/cabal/pull/8237)

- Add documentation (or stubs) for cabal-install commands: `fetch`, `get`, `info`, `init`, `list`, `report`, `unpack`, `upload`, `user-config` [#7884](https://github.com/haskell/cabal/issues/7884) [#7978](https://github.com/haskell/cabal/issues/7978) [#8308](https://github.com/haskell/cabal/issues/8308) [#8309](https://github.com/haskell/cabal/pull/8309)

- Add check for upper bound on any dependency in `cabal check` [#8291](https://github.com/haskell/cabal/issues/8291) [#8339](https://github.com/haskell/cabal/pull/8339)
