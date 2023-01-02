### Significant changes

- Add cabal-version: 3.4 [!6663](https://github.com/haskell/cabal/pull/6663)

  See various changes in https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-3-4

- Support GHC-8.12 [#6903](https://github.com/haskell/cabal/issues/6903) [#6904](https://github.com/haskell/cabal/issues/6904) [#6905](https://github.com/haskell/cabal/issues/6905) [!6545](https://github.com/haskell/cabal/pull/6545) [!6552](https://github.com/haskell/cabal/pull/6552) [!6553](https://github.com/haskell/cabal/pull/6553) [!6563](https://github.com/haskell/cabal/pull/6563) [!6735](https://github.com/haskell/cabal/pull/6735) [!6844](https://github.com/haskell/cabal/pull/6844) [!6848](https://github.com/haskell/cabal/pull/6848) [!6865](https://github.com/haskell/cabal/pull/6865) [!6908](https://github.com/haskell/cabal/pull/6908) [!6947](https://github.com/haskell/cabal/pull/6947)

  - Support for `-this-package-key` deprecation
  - Use process `createPipe`
  - Add support for WINIO to Cabal
  - Adopt to simplified subsumption changes

  There are no magical `IO = WithCallStack Prelude.IO` alias in Cabal,
  as it is hard to get working with GHC-8.12's simplified subsumption.

- Treat `pkg:sublib` dependency syntax as is in `cabal-version: 3.4` [#6083](https://github.com/haskell/cabal/issues/6083) [!6893](https://github.com/haskell/cabal/pull/6893) [!6907](https://github.com/haskell/cabal/pull/6907)

  In `cabal-version: 3.4` cabal files the dependency definition

  ```cabal
  build-depends: somesublib
  ```

  is not using in-package sublibraries. You have to be explicit and write

  ```cabal
  build-depends: thispkg:somesublib
  ```

  This fixes an issue, where it was impossible to refer to external
  library, if you had sublibrary of the same name.

- Add active-repositories configuration [#6819](https://github.com/haskell/cabal/issues/6819) [!6724](https://github.com/haskell/cabal/pull/6724) [!6868](https://github.com/haskell/cabal/pull/6868)

  New `active-repositories` `cabal.project` fields allows
  specifying an order and ways various package repositories are combined.

  The active `active-repositories` is saved in `cabal.project.freeze`.

- Various `cabal init` improvements [#6150](https://github.com/haskell/cabal/issues/6150) [#6675](https://github.com/haskell/cabal/issues/6675) [!6603](https://github.com/haskell/cabal/pull/6603) [!6607](https://github.com/haskell/cabal/pull/6607) [!6619](https://github.com/haskell/cabal/pull/6619) [!6632](https://github.com/haskell/cabal/pull/6632) [!6641](https://github.com/haskell/cabal/pull/6641) [!6650](https://github.com/haskell/cabal/pull/6650) [!6661](https://github.com/haskell/cabal/pull/6661) [!6676](https://github.com/haskell/cabal/pull/6676) [!6677](https://github.com/haskell/cabal/pull/6677) [!6678](https://github.com/haskell/cabal/pull/6678) [!6690](https://github.com/haskell/cabal/pull/6690) [!6705](https://github.com/haskell/cabal/pull/6705) [!6723](https://github.com/haskell/cabal/pull/6723)

  - Default to `cabal-version: 2.4`
  - `cabal` doesn't force a default license choice anymore
  - Licenses are always asked using SPDX expression
  - Fix an infinite loop when invalid license was passed on command line
  - `Setup.hs` is not written anymore
  - Default to --source-dir=src and --application-dir=app
  - Add `FileCreators.generateCabalFile` unit tests.
  - Default cabal init application-dir to `app`, and source-dir to `src`.
  - Default to SPDX.NONE license in cabal init interactive mode.

- Rework `v2-sdist` command [#2940](https://github.com/haskell/cabal/issues/2940) [#5813](https://github.com/haskell/cabal/issues/5813) [#6514](https://github.com/haskell/cabal/issues/6514) [#6611](https://github.com/haskell/cabal/issues/6611) [#6635](https://github.com/haskell/cabal/issues/6635) [!6454](https://github.com/haskell/cabal/pull/6454) [!6637](https://github.com/haskell/cabal/pull/6637) [!6640](https://github.com/haskell/cabal/pull/6640) [!6660](https://github.com/haskell/cabal/pull/6660) [!6666](https://github.com/haskell/cabal/pull/6666) [!6884](https://github.com/haskell/cabal/pull/6884) [!6916](https://github.com/haskell/cabal/pull/6916)

  `v2-sdist` marks all files as readonly in resulting tarballs.

  The #2940 "cabal sdist should touch preprocessed .hs files" issue
  is fixed by virtue of not doing any preprocessing anymore.
  It's responsibility of packager.

  The #6611 "v2-sdist includes a file twice in the tarball " issue
  is fixed as we don't consider only one file list,
  not two separate executable and ordinary file lists.

  The #6514 "unpack doesn't preserve (executable) permissions" issue
  is partially resolved,  as there shouldn't be executable permissions
  in the tar files.

  The rationale for above simplification is simple. The only file
  Cabal machinery would run is `configure` script. These
  are run with explicit `sh` program. For package internal scripts,
  maintainers should also use interpreters explicitly as well.
  Scripts with shebangs are not executable on Windows.

  The `v1-sdist` command is removed, as its functionality is completely
  superseded by `v2-sdist`.

- `source-repository-package` directories aren't local [#5586](https://github.com/haskell/cabal/issues/5586) [!6915](https://github.com/haskell/cabal/pull/6915) [!6917](https://github.com/haskell/cabal/pull/6917)

  Concretely these means that compiled `source-repository-package` entries
  used in different projects are stored in the global nix-style store.

- Add list-bin command [#6807](https://github.com/haskell/cabal/issues/6807) [!6931](https://github.com/haskell/cabal/pull/6931)

  `cabal list-bin executable-component` tells the path to the build artifact.

- Remove sandboxes [#6445](https://github.com/haskell/cabal/issues/6445) [!6747](https://github.com/haskell/cabal/pull/6747)

  Sandbox functionality is remove in favour of now default nix-style builds.

- `cabal list` accepts regular expression [#4267](https://github.com/haskell/cabal/issues/4267) [#6683](https://github.com/haskell/cabal/issues/6683) [!6618](https://github.com/haskell/cabal/pull/6618) [!6806](https://github.com/haskell/cabal/pull/6806)
- More rich `index-state` syntax. `v2-freeze` saves `index-state`. [#6728](https://github.com/haskell/cabal/issues/6728) [!6581](https://github.com/haskell/cabal/pull/6581) [!6591](https://github.com/haskell/cabal/pull/6591) [!6596](https://github.com/haskell/cabal/pull/6596) [!6597](https://github.com/haskell/cabal/pull/6597) [!6682](https://github.com/haskell/cabal/pull/6682) [!6733](https://github.com/haskell/cabal/pull/6733)
### Other changes

- Tests additions [#6409](https://github.com/haskell/cabal/issues/6409) [!6753](https://github.com/haskell/cabal/pull/6753) [!6759](https://github.com/haskell/cabal/pull/6759) [!6802](https://github.com/haskell/cabal/pull/6802) [!6842](https://github.com/haskell/cabal/pull/6842) [!6872](https://github.com/haskell/cabal/pull/6872)

  - Add shrinker, so writing big non-generic product shrinkers is easier
  - Add `hs-source-dirs: NL .` test-case
  - Add public multilib test(s)

- Code organization [!6599](https://github.com/haskell/cabal/pull/6599) [!6642](https://github.com/haskell/cabal/pull/6642) [!6734](https://github.com/haskell/cabal/pull/6734) [!6745](https://github.com/haskell/cabal/pull/6745) [!6746](https://github.com/haskell/cabal/pull/6746) [!6817](https://github.com/haskell/cabal/pull/6817) [!6818](https://github.com/haskell/cabal/pull/6818) [!6821](https://github.com/haskell/cabal/pull/6821) [!6867](https://github.com/haskell/cabal/pull/6867)

  - Split `Distribution.Client.Types` module
  - Move cabal-testsuite code into `src/`
  - Add `ProjectFlags`
  - Add `NixStyleOptions`
  - Internal refactorings to install command.
  - Make own modules for `InstallMethod` and `OverwritePolicy`
  - Make cabal-install compilable with `NoImplicitPrelude`
  - Refactor shared `TargetProblem` data types into their own module.
  - Template `cabal-install.cabal` using zinza

- The grammar of most package description fields is formally documented [!6591](https://github.com/haskell/cabal/pull/6591) [!6593](https://github.com/haskell/cabal/pull/6593) [!6704](https://github.com/haskell/cabal/pull/6704) [!6766](https://github.com/haskell/cabal/pull/6766) [!6778](https://github.com/haskell/cabal/pull/6778) [!6780](https://github.com/haskell/cabal/pull/6780) [!6781](https://github.com/haskell/cabal/pull/6781) [!6791](https://github.com/haskell/cabal/pull/6791) [!6800](https://github.com/haskell/cabal/pull/6800)

  See https://cabal.readthedocs.io/en/latest/buildinfo-fields-reference.html

- Add -fexpose-all-unfoldings to parsec and Cabal in release project [!6708](https://github.com/haskell/cabal/pull/6708)

  This makes parser faster with acceptable code-size increase.

- Fix ghci being launched before other sources are built. [!6923](https://github.com/haskell/cabal/pull/6923)

  Related to foreign calls to C-sources.

- Use default install directory if not specified [#5973](https://github.com/haskell/cabal/issues/5973) [!6624](https://github.com/haskell/cabal/pull/6624)

  Cabal 3.0.0.0 added the --installdir option to specify the location that
  binaries should be installed in. Running a cabal user-config update would
  populate the config file with the default value, but the cabal install program
  would error if it wasn't set. This change uses the default value that would be
  written to the config if its unset, and outputs a warning.

- Update GHC environment location [#6565](https://github.com/haskell/cabal/issues/6565) [!6822](https://github.com/haskell/cabal/pull/6822)

  cabal-install currently assumes that the GHC environment files are always located in `$HOME/.ghc/`.
  However, GHC itself doesn't query the home directory directly.
  Instead, it uses `getAppUserDataDirectory "ghc"` which happens to coincide with `$HOME/.ghc`/ on UNIX systems.
  On Windows, however, GHC ends up looking in `%APPDATA%/ghc/`.

- Remove `-any` and `-none` syntax for version ranges in cabal-version: 3.4 [#6589](https://github.com/haskell/cabal/issues/6589) [!6786](https://github.com/haskell/cabal/pull/6786)

  Use `>=0` (or empty in dependencies) and `<0` respectively.

- Add pijul to known repository type [#6610](https://github.com/haskell/cabal/issues/6610) [!6684](https://github.com/haskell/cabal/pull/6684)

  Pijul is now explicitly recognized version control system.
  However `cabal-install` isn't yet able to use it in
  `source-repository-package`.

- Remove upgrade, uninstall and win32selfupgrade commands [#6691](https://github.com/haskell/cabal/issues/6691) [!6707](https://github.com/haskell/cabal/pull/6707)

  This commands were not implemented or are special purpose.
  The removal of `win32selfupgrade` might break self upgrade on windows,
  when `cabal-install` tries to rewrite the binary of itself.
  This shouldn't be a problem when symlinking is used.

- Remove local-repo [#6729](https://github.com/haskell/cabal/issues/6729) [!6730](https://github.com/haskell/cabal/pull/6730)

  This functionality was mainly used by removed sandboxes.
  `file+noindex` repositories are better variant for local package repositories.

- Add support for multiple public libraries to the solver [#6039](https://github.com/haskell/cabal/issues/6039) [!6047](https://github.com/haskell/cabal/pull/6047) [!6812](https://github.com/haskell/cabal/pull/6812) [!6836](https://github.com/haskell/cabal/pull/6836)

  cabal-install can now take into account the presence and visibility of
  sublibraries when solving. This also means that, when using cabal-install,
  multiple public libraries support is extended to GHCs older than 8.8.
  The support is limited to source packages for now.

- Remove text type-class [!6764](https://github.com/haskell/cabal/pull/6764) [!6777](https://github.com/haskell/cabal/pull/6777) [!6784](https://github.com/haskell/cabal/pull/6784) [!6785](https://github.com/haskell/cabal/pull/6785) [!6793](https://github.com/haskell/cabal/pull/6793) [!6794](https://github.com/haskell/cabal/pull/6794)

  `Text` type-class was removed from `Cabal`, but it still lived in `cabal-install`.
  Now it is finally removed.
  `cabal-install` is still using `ReadP` parsers for various things though.

- Windows install symlinks [!5684](https://github.com/haskell/cabal/pull/5684) [!6506](https://github.com/haskell/cabal/pull/6506) [!6519](https://github.com/haskell/cabal/pull/6519)

  `cabal-install` will try to infer whether symlinking is supported on Windows,
  and make symlinks in `install`. Otherwise it will fallback to copying files.
  Either way can be forced with `--install-method`.

- Use process jobs when spawning subprocesses [!6529](https://github.com/haskell/cabal/pull/6529) [!6536](https://github.com/haskell/cabal/pull/6536)

  Many toolchain tools written for POSIX systems rely on the exec system
  call. Unfortunately, it is not possible to implement exec in a
  POSIX-compliant manner on Windows. In particular, the semantics of the
  exec implementation provided by the widely-used msvcrt C runtime
  will cause process's waiting on the exec'ing process to incorrectly
  conclude that the process has successfully terminated when in fact it is
  still running in another process.

  For this reason, the process library exposes the use_process_jobs
  flag to use a more strict (although still not POSIX-compliant) mechanism
  for tracking process completion.
  This is explained in [the Process.hs comment](https://github.com/haskell/process/blob/master/System/Process.hs#L399)

  Unfortunately, job support in the process library is currently quite
  broken and was only recently fixed. Consequently, we only enable job
  object support for process releases >= 1.6.8.

- Change `BuildReports` parse/pretty to use FieldGrammar framework [!6783](https://github.com/haskell/cabal/pull/6783)
- Tell users about `cabal.project.local~` in `cabal v2-configure` [!6877](https://github.com/haskell/cabal/pull/6877)
- Improvements to cabal-testsuite framework [!6643](https://github.com/haskell/cabal/pull/6643)
- Refactor cabalInstallVersions, add make doctest-cli [!6720](https://github.com/haskell/cabal/pull/6720)
- Update copyright years to 2020 [!6644](https://github.com/haskell/cabal/pull/6644)
- Documentation updates and typo-fixes [!6504](https://github.com/haskell/cabal/pull/6504) [!6550](https://github.com/haskell/cabal/pull/6550) [!6560](https://github.com/haskell/cabal/pull/6560) [!6613](https://github.com/haskell/cabal/pull/6613) [!6660](https://github.com/haskell/cabal/pull/6660) [!6668](https://github.com/haskell/cabal/pull/6668) [!6669](https://github.com/haskell/cabal/pull/6669) [!6686](https://github.com/haskell/cabal/pull/6686) [!6697](https://github.com/haskell/cabal/pull/6697) [!6761](https://github.com/haskell/cabal/pull/6761) [!6928](https://github.com/haskell/cabal/pull/6928) [!6940](https://github.com/haskell/cabal/pull/6940) [!6951](https://github.com/haskell/cabal/pull/6951) [!6953](https://github.com/haskell/cabal/pull/6953) [!6954](https://github.com/haskell/cabal/pull/6954)
- Flag assignment parsing refactorings [#6853](https://github.com/haskell/cabal/issues/6853) [!6854](https://github.com/haskell/cabal/pull/6854) [!6858](https://github.com/haskell/cabal/pull/6858)
- Remove new- command from `--help` output (there are v2-) [!6930](https://github.com/haskell/cabal/pull/6930)
- Installing (copy or symlink) executable message prints destination. [!6582](https://github.com/haskell/cabal/pull/6582) [!6590](https://github.com/haskell/cabal/pull/6590)
- Check sha256 if `#sha256=...` fragments are given to URIs [!6576](https://github.com/haskell/cabal/pull/6576)
- all extra-source-files are change-tracked [#4746](https://github.com/haskell/cabal/issues/4746) [!6889](https://github.com/haskell/cabal/pull/6889)
- `upload --help` now includes `password-command` as a config file option (#5224) [#5224](https://github.com/haskell/cabal/issues/5224) [!6313](https://github.com/haskell/cabal/pull/6313) [!6609](https://github.com/haskell/cabal/pull/6609) [!6680](https://github.com/haskell/cabal/pull/6680)
- Use PrettyField to format cabal file in `cabal init` [#5555](https://github.com/haskell/cabal/issues/5555) [!6718](https://github.com/haskell/cabal/pull/6718)
- Default to 'NoReports' for remote build reporting [#6210](https://github.com/haskell/cabal/issues/6210) [!6625](https://github.com/haskell/cabal/pull/6625)
- Add foo:bar syntax to mixins [#6281](https://github.com/haskell/cabal/issues/6281) [!6912](https://github.com/haskell/cabal/pull/6912)
- default-language field is optional in cabal-version: 3.4 [#6288](https://github.com/haskell/cabal/issues/6288) [!6924](https://github.com/haskell/cabal/pull/6924) [!6926](https://github.com/haskell/cabal/pull/6926)
- Allow cabal v2-install pkgname:exename [#6369](https://github.com/haskell/cabal/issues/6369) [!6576](https://github.com/haskell/cabal/pull/6576)
- Allow cabal v2-install http:// [#6393](https://github.com/haskell/cabal/issues/6393) [!6576](https://github.com/haskell/cabal/pull/6576)
- Include component name in unit-id [#6485](https://github.com/haskell/cabal/issues/6485) [!6547](https://github.com/haskell/cabal/pull/6547)
- cabal v2-install prints copy/symlink destination [#6575](https://github.com/haskell/cabal/issues/6575) [!6582](https://github.com/haskell/cabal/pull/6582) [!6890](https://github.com/haskell/cabal/pull/6890)
- Prepend hs-source-dir to match-component, fixes `cabal repl file` [#6622](https://github.com/haskell/cabal/issues/6622) [!6623](https://github.com/haskell/cabal/pull/6623) [!6826](https://github.com/haskell/cabal/pull/6826) [!6875](https://github.com/haskell/cabal/pull/6875)
- Change 'optional-packages' default to empty, when cabal.project is missing. [#4797](https://github.com/haskell/cabal/issues/4797) [#6739](https://github.com/haskell/cabal/issues/6739) [!6740](https://github.com/haskell/cabal/pull/6740)
- v2-update reports new index-state [#6804](https://github.com/haskell/cabal/issues/6804) [!6810](https://github.com/haskell/cabal/pull/6810)
- Fix rpmvercmp (trailing non-digit characters) [#6805](https://github.com/haskell/cabal/issues/6805) [!6808](https://github.com/haskell/cabal/pull/6808)
- Add `-z` / `--ignore-project` flag to `cabal v2-update` [#6809](https://github.com/haskell/cabal/issues/6809) [!6814](https://github.com/haskell/cabal/pull/6814)
- Adjust message indicating `--lib` is likely desired [#6856](https://github.com/haskell/cabal/issues/6856) [!6857](https://github.com/haskell/cabal/pull/6857)
- cxx-sources, asm-sources and cmm-sources are change-tracked [#6869](https://github.com/haskell/cabal/issues/6869) [!6870](https://github.com/haskell/cabal/pull/6870)
- Change manpage command to man [!6548](https://github.com/haskell/cabal/pull/6548)
- other changes [!6556](https://github.com/haskell/cabal/pull/6556) [!6598](https://github.com/haskell/cabal/pull/6598) [!6910](https://github.com/haskell/cabal/pull/6910)
- Package description parser benchmark [!6594](https://github.com/haskell/cabal/pull/6594)
- Add `+stderr` modifier to `Verbosity` [!6929](https://github.com/haskell/cabal/pull/6929)
- Mark public-libs as experimental feature [!6605](https://github.com/haskell/cabal/pull/6605)
- Add SPDX License List 3.9 [#6879](https://github.com/haskell/cabal/issues/6879) [!6662](https://github.com/haskell/cabal/pull/6662) [!6944](https://github.com/haskell/cabal/pull/6944)
- More checks in version range parser [!6586](https://github.com/haskell/cabal/pull/6586)
- Require cabal-versions `>=1.25` to be exact [!6654](https://github.com/haskell/cabal/pull/6654)
- Add and use weeder [!6779](https://github.com/haskell/cabal/pull/6779) [!6790](https://github.com/haskell/cabal/pull/6790)
