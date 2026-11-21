---
synopsis: Run the compiler and hc-pkg from the programs found when configuring the compiler
packages: [Cabal, cabal-install]
prs: 12624
---

Querying and updating package databases, starting an interpreter and checking
which build options the compiler supports used to look `ghc` and `ghc-pkg` up by
name in a `ProgramDb`. They now take the `CompilerPrograms` found when
configuring the compiler, and run the programs it records with the arguments
and environment from the `ProgramDb`:

- `Distribution.Simple.Configure`: `getInstalledPackages`,
  `getPackageDBContents`, `getInstalledPackagesMonitorFiles`,
  `computePackageInfo`, `adjustBuildOptions` and `adjustBuildOptionsAndWarn`
  take a `CompilerPrograms`; `configCompilerAuxEx` also returns one;
- `Distribution.Simple.Register`: `initPackageDB`, `createPackageDB`,
  `invokeHcPkg` and `registerPackage` take a `CompilerPrograms`;
- `Distribution.Simple.Build.startInterpreter` takes a `CompilerPrograms`;
- `Distribution.Simple.GHC`: `getInstalledPackages`, `getPackageDBContents`,
  `getInstalledPackagesMonitorFiles`, `startInterpreter` and `registerPackage`
  take a `CompilerPrograms`; `hcPkgInfo` takes a `Verbosity` and a
  `CompilerPrograms` and runs in `IO`;
- `Distribution.Simple.UHC`: `getInstalledPackages` and `registerPackage` take
  a `CompilerPrograms`;
- `Distribution.Simple.Program` exports `compilerSupports`, which tells whether
  the configured compiler supports a GHC feature.
