---
synopsis: Separate package-level and component-level configure flags
packages: [Cabal, cabal-install]
prs: 12562
issues: 12538
---

`ConfigFlags` is now split up into whole package `PackageConfigFlags` and
per-component `ComponentConfigFlags`.

`ConfigFlags` is now a bidirectional pattern synonym with all the previous
fields, so code which constructs, updates or reads `ConfigFlags` using these
field names keeps working.

The `Eq` instance of `ConfigFlags` is now derived, so it now also compares
`configInstantiateWith` and `configExtraFrameworkDirs`,
which the previous hand-written instance omitted.

The unused `configScratchDir` field is removed.
