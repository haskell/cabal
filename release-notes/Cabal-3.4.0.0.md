### Significant changes

- Add cabal-version: 3.4 [!6663](https://github.com/haskell/cabal/pull/6663)
  
  See various changes in https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-3-4
  
- Support GHC-9.0 [#6903](https://github.com/haskell/cabal/issues/6903) [#6904](https://github.com/haskell/cabal/issues/6904) [#6905](https://github.com/haskell/cabal/issues/6905) [!6545](https://github.com/haskell/cabal/pull/6545) [!6552](https://github.com/haskell/cabal/pull/6552) [!6553](https://github.com/haskell/cabal/pull/6553) [!6563](https://github.com/haskell/cabal/pull/6563) [!6735](https://github.com/haskell/cabal/pull/6735) [!6844](https://github.com/haskell/cabal/pull/6844) [!6848](https://github.com/haskell/cabal/pull/6848) [!6865](https://github.com/haskell/cabal/pull/6865) [!6908](https://github.com/haskell/cabal/pull/6908) [!6947](https://github.com/haskell/cabal/pull/6947)
  
  - Support for `-this-package-key` deprecation
  - Use process `createPipe`
  - Add support for WINIO to Cabal
  - Adopt to simplfied subsumption changes
  
  There are no magical `IO = WithCallStack Prelude.IO` alias in Cabal,
  as it is hard to get working with GHC-9.0's simplified subsumption.
  
- Treat `pkg:sublib` dependency syntax as is in `cabal-version: 3.4` [#6083](https://github.com/haskell/cabal/issues/6083) [!6893](https://github.com/haskell/cabal/pull/6893) [!6907](https://github.com/haskell/cabal/pull/6907)
  
  In `cabal-version: 3.4` cabal files the dependency definition
  
  ```cabal
  build-depends: somesublib
  ```
  
  does not use in-package sublibraries. You have to be explicit and write
  
  ```cabal
  build-depends: thispkg:somesublib
  ```
  
  This fixes an issue where it was impossible to refer to an external
  library if you had a sublibrary of the same name.
  
- Dependency type refactorings [#5570](https://github.com/haskell/cabal/issues/5570) [#6894](https://github.com/haskell/cabal/issues/6894) [!6768](https://github.com/haskell/cabal/pull/6768) [!6798](https://github.com/haskell/cabal/pull/6798) [!6895](https://github.com/haskell/cabal/pull/6895) [!6896](https://github.com/haskell/cabal/pull/6896) [!6897](https://github.com/haskell/cabal/pull/6897) [!6898](https://github.com/haskell/cabal/pull/6898)
  
  With additions of (public) sublibraries, `Dependency` type use for multiple needs become painful.
  Therefore a new type `PackageVersionConstraint` was added to serve *constraint on package* use-case.
  `Dependency` type is *dependency on a library component*, i.e. representing `build-depends` entry.
  
  - Use PackageVersionConstraint more
  - Add NonEmptySet and use it in Dependency
  
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
  
- Fix ghci being launched before other sources are built. [!6923](https://github.com/haskell/cabal/pull/6923)
  
  Related to foreign calls to C-sources.
  
- Remove `-any` and `-none` syntax for version ranges in cabal-version: 3.4 [#6589](https://github.com/haskell/cabal/issues/6589) [!6786](https://github.com/haskell/cabal/pull/6786)
  
  Use `>=0` (or empty in dependencies) and `<0` respectively.
  
- Add pijul to known repository type [#6610](https://github.com/haskell/cabal/issues/6610) [!6684](https://github.com/haskell/cabal/pull/6684)
  
  Pijul is now explicitly recognized version control system.
  However `cabal-install` isn't yet able to use it in
  `source-repository-package`.
  
- Rename Flag to CompilerFlag and PackageFlag [#6710](https://github.com/haskell/cabal/issues/6710) [!6725](https://github.com/haskell/cabal/pull/6725)
  
  There was three Flag's. Now they are
  
  - `Flag` (cli parsing)
  - `PackageFlag`
  - `CompilerFlag`
  
  This allows wild-imports without `hiding (Flag)`.
  
- Accept "linux-androideabi" as an alias for Android for determining buildOS [!6301](https://github.com/haskell/cabal/pull/6301) [!6949](https://github.com/haskell/cabal/pull/6949)
  
  `Cabal` will able to parse `linux-androideabi` from the value of `System.Info.os`.
  
- Refactor Distribution.PackageDescription module [!6855](https://github.com/haskell/cabal/pull/6855)
  
  Now it re-exports a collection of modules, not individual symbols.
  
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
  
- Remove `AnyVersion` and `WildcardVersion` constructors from `Version` [!6742](https://github.com/haskell/cabal/pull/6742)
  
  This changes makes `Version` type less "syntactic",
  i.e. contains less constructors for semantically same version ranges.
  
- Cabal-QuickCheck package with `Arbitrary` instances [#6882](https://github.com/haskell/cabal/issues/6882) [!6557](https://github.com/haskell/cabal/pull/6557) [!6891](https://github.com/haskell/cabal/pull/6891)
- Create Cabal-tree-diff package with `ToExpr` instances [!6789](https://github.com/haskell/cabal/pull/6789)
- `Cabal.Distribution.Compiler`: add `Traversable` instance for `PerCompilerFlavor` [!6763](https://github.com/haskell/cabal/pull/6763)
- Improvements to cabal-testsuite framework [!6643](https://github.com/haskell/cabal/pull/6643)
- Update copyright years to 2020 [!6644](https://github.com/haskell/cabal/pull/6644)
- Documentation updates and typo-fixes [!6504](https://github.com/haskell/cabal/pull/6504) [!6550](https://github.com/haskell/cabal/pull/6550) [!6560](https://github.com/haskell/cabal/pull/6560) [!6613](https://github.com/haskell/cabal/pull/6613) [!6660](https://github.com/haskell/cabal/pull/6660) [!6668](https://github.com/haskell/cabal/pull/6668) [!6669](https://github.com/haskell/cabal/pull/6669) [!6686](https://github.com/haskell/cabal/pull/6686) [!6697](https://github.com/haskell/cabal/pull/6697) [!6761](https://github.com/haskell/cabal/pull/6761) [!6928](https://github.com/haskell/cabal/pull/6928) [!6940](https://github.com/haskell/cabal/pull/6940) [!6951](https://github.com/haskell/cabal/pull/6951) [!6953](https://github.com/haskell/cabal/pull/6953) [!6954](https://github.com/haskell/cabal/pull/6954)
- Flag assignment parsing refactorings [#6853](https://github.com/haskell/cabal/issues/6853) [!6854](https://github.com/haskell/cabal/pull/6854) [!6858](https://github.com/haskell/cabal/pull/6858)
- Add foo:bar syntax to mixins [#6281](https://github.com/haskell/cabal/issues/6281) [!6912](https://github.com/haskell/cabal/pull/6912)
- default-language field is optional in cabal-version: 3.4 [#6288](https://github.com/haskell/cabal/issues/6288) [!6924](https://github.com/haskell/cabal/pull/6924) [!6926](https://github.com/haskell/cabal/pull/6926)
- Split `KnownRepoType` out of `RepoType` [#6432](https://github.com/haskell/cabal/issues/6432) [!6612](https://github.com/haskell/cabal/pull/6612)
- Include component name in unit-id [#6485](https://github.com/haskell/cabal/issues/6485) [!6547](https://github.com/haskell/cabal/pull/6547)
- Change 'optional-packages' default to empty, when cabal.project is mising. [#4797](https://github.com/haskell/cabal/issues/4797) [#6739](https://github.com/haskell/cabal/issues/6739) [!6740](https://github.com/haskell/cabal/pull/6740)
- Fix rpmvercmp (trailing non-digit characters) [#6805](https://github.com/haskell/cabal/issues/6805) [!6808](https://github.com/haskell/cabal/pull/6808)
- other changes [!6556](https://github.com/haskell/cabal/pull/6556) [!6598](https://github.com/haskell/cabal/pull/6598) [!6910](https://github.com/haskell/cabal/pull/6910)
- Package description parser benchmark [!6594](https://github.com/haskell/cabal/pull/6594)
- Add `licenceIsFsfLibre` [!6878](https://github.com/haskell/cabal/pull/6878)
- Add `+stderr` modifier to `Verbosity` [!6929](https://github.com/haskell/cabal/pull/6929)
- Mark public-libs as experimental feature [!6605](https://github.com/haskell/cabal/pull/6605)
- Add rewriteFileLBS and use it to write setup wrapper [!6574](https://github.com/haskell/cabal/pull/6574)
- Add SPDX License List 3.9 [#6879](https://github.com/haskell/cabal/issues/6879) [!6662](https://github.com/haskell/cabal/pull/6662) [!6944](https://github.com/haskell/cabal/pull/6944)
- Change specVersion to have CabalSpecVersion type [!6653](https://github.com/haskell/cabal/pull/6653)
- Better UTF8 handling, parsed `ShortText` is now valid. [!6588](https://github.com/haskell/cabal/pull/6588)
- More checks in version range parser [!6586](https://github.com/haskell/cabal/pull/6586)
- Require cabal-versions `>=1.25` to be exact [!6654](https://github.com/haskell/cabal/pull/6654)
- Add and use weeder [!6779](https://github.com/haskell/cabal/pull/6779) [!6790](https://github.com/haskell/cabal/pull/6790)
