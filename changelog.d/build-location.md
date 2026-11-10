---
synopsis: Don't serialise package location
packages: [Cabal, Cabal-hooks]
prs: 12562
issues: 12538
---

The location of a package (its working directory and dist directory) is no
longer part of its configuration: one needs to know where a package is in
order to read its configuration from disk.

Changes to the Cabal library:

  - `LocalBuildInfo` no longer stores the `CommonSetupFlags` of the
    `configure` invocation. Its location is a `BuildLocation`, supplied by
    whoever creates the `LocalBuildInfo` (`configure`) or reads it
    (`getPersistBuildConfig`), and not written to disk.
  - `LocalBuildInfo` is no longer serialisable. Use `PersistedBuildInfo`
    instead which strips off the `BuildLocation`.
  - The `LocalBuildInfo` pattern synonym no longer has a `configFlags` field.
    It has `packageConfigFlags` and `componentConfigFlags` instead, as well as
    `buildLocation`. Likewise, `PackageBuildDescr` has `packageConfigFlags` and
    `componentConfigFlags` fields instead of `configFlags`.
    A deprecated read-only `configFlags` function is provided for migration,
    although note the verbosity it computes is the default one, not the
    verbosity of the `configure` invocation.
  - `writePersistBuildConfig` no longer takes the working directory and dist
    directory: it writes to the location of the `LocalBuildInfo`.
    `getConfigStateFile` and `tryGetConfigStateFile` are removed; use
    `getPersistBuildConfig` and `tryGetPersistBuildConfig`.
  - `buildDirPBD` is removed; use `buildDir` or `setupFlagsBuildDir`.
  - `PostConfPackageInputs` and `PreConfComponentInputs` gain a
    `commonFlags :: CommonSetupFlags` field, with the common flags of the
    `configure` invocation.
  - `repl` and `haddock` use the verbosity and temporary file options of their
    own invocation, rather than those of `configure`.
