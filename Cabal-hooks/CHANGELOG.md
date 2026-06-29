# Changelog for `Cabal-hooks`

## 3.18.0.0 — July 2026

- Stop exposing constructors of `RuleCommands` [#11461](https://github.com/haskell/cabal/issues/11461) [#11771](https://github.com/haskell/cabal/pull/11771)

  The constructors of the `SetupHooks` `RuleCommands` are no longer exposed
  from `Distribution.Simple.SetupHooks`. These were rather gnarly internal
  constructors; the intended public API is via `staticRule` and `dynamicRule`.

- Make Cabal-hooks library more self-sufficient [#11772](https://github.com/haskell/cabal/pull/11772)

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
  and `findAndMonitorSourceDirsFileExts` have been added. These make it very
  simple and convenient to search for a file glob or files with a particular
  extension in the source directories, for pre-build rules.

- Recompilation checking for SetupHooks pre-build rules [#11730](https://github.com/haskell/cabal/issues/11730) [#11731](https://github.com/haskell/cabal/pull/11731)

  Pre-build rules are now only re-run when stale, according to the conditions
  described in the [SetupHooks
  API](https://hackage.haskell.org/package/Cabal-hooks/docs/Distribution-Simple-SetupHooks.html).
  That is, a rule is re-run if any of the following conditions are satisfied:

    - The rule is new, or
    - A dependency of the rule is stale.
      That is, either we have re-run another rule that this rule depends on,
      or one of the file inputs to the rule is newer than the oldest output of the
      rule (or the rule output doesn't exist at all), or
    - The rule itself has changed, e.g. the parameters stored in `RuleData`
      have changed.

  In particular, Cabal will now write per-component caches of pre-build rules
  in order to compute which rules have changed between runs, with file name
  "setup-hooks-rules.cache".

- Pre-build rules can generate extra sources and bundled libraries [#10791](https://github.com/haskell/cabal/issues/10791) [#11607](https://github.com/haskell/cabal/issues/11607) [#11573](https://github.com/haskell/cabal/pull/11573)

  It is now possible to write pre-build rules that generate source files other
  than Haskell files, as well as extra bundled library files.

  Because there is no counterpart to `autogen-modules` for non-Haskell source
  files, you will need to proceed in two steps:

    1. In a per-component pre-configure hook, add the files you want to generate
       to the relevant fields, e.g. the `cSources` field of `BuildInfo` for a
       C source file.

       These files must be relative to `autogenCompModulesDir`.

    2. Pre-build rules generating these files will now be demanded. This avoids
       getting an error message "The following pre-build rules are not demanded
       and will not be run".

  Note that include files (such as `.h` files) are a bit different: any files
  listed under `includes`/`autogen-includes` are required at **configure**
  time.  This gives `SetupHooks` authors two choices: either list the include
  files in `autogen-includes` but generate them in a pre-configure hook, or
  don't list them there and generate them in a pre-build rule, relying on the
  files getting picked up from included directories (this may be brittle).

## 3.16.1 – December 2025
  * No changes

## 3.16.0 – July 2025
  * No changes

## 3.14.2 – April 2025
  * No changes

## 3.14 – November 2024

  * Initial release of the `Hooks` API.

