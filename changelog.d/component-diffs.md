---
synopsis: Store component diffs instead of updating package description
packages: [Cabal, Cabal-hooks]
prs: 12562
issues: 12538
---

`PackageBuildDescr` now keeps the package description as configured for the
package as a whole. The output of the per-component pre-configure hooks is
stored separately in the new `componentDiffs` field of `ComponentBuildDescr`.

The `LocalBuildInfo` pattern synonym has `packageLevelPkgDescr` and
`componentDiffs` fields instead of `localPkgDescr`. `localPkgDescr` is now a
function which combines the two; it can no longer be used in record
construction or update.
