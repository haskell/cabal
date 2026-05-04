---
synopsis: Make Cabal-hooks library more self-sufficient
packages: [Cabal-hooks]
prs: 11772
issues:
---

The `Distribution.Simple.SetupHooks` module from `Cabal-hooks` now re-exports
a lot of the functionality that is commonly needed when writing `SetupHooks`:

  - File-path related functionality from `Distribution.Utils.Path`.
  - Functionality related to the program database: `lookupProgram`, `runProgramCwd`.
  - IO utilities such as `warn`, `createDirectoryIfMissingVerbose`, and `rewriteFileEx`.
  - Various types frequently used in pre-build rules, such as `Binary`,
    `ModuleName`.
  - Functions that extract information from `LocalBuildInfo` such as
    `localPkgDescr`, `mbWorkDirLBI`, `withPrograms`, `interpretSymbolicPathLBI`
    and `componentBuildInfo`.


In addition, new file monitoring helper functions `findAndMonitorDirFileGlob`
and `findAndMonitorSourceDirsFileExt` have been added. These make it very
simple and convenient to search for a file glob or files with a particular
extension in the source directories, for pre-build rules.
