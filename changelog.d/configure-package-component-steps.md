---
synopsis: Split configure into whole package and per-component
packages: [Cabal, Cabal-hooks]
prs: 12562
issues: 12538
---

The Cabal configure function has been split up into two:

  - `configurePackage` configures the package as a whole.
  - `configureComponents` configures the components, given the result of
    `configurePackage`.

These replace `configureFinal`.

`PreConfPackageInputs` no longer has a `configFlags` field: it has
`commonFlags` and `packageConfigFlags` instead, as package-wide hooks only
receive package-level flags.

The order some steps happen in `configure` has changed, which affects packages
with `build-type: Hooks`:

  - The pre-configure hook now runs after resolving the flag assignment (i.e.
    after finalising the package description and settling its conditionals).
  - The post-configure hook no longer sees build tools or `pkg-config` programs
    in the `ProgramDb`, because they depend on which components have been
    enabled. The per-component pre-configure hooks continue to see them.
    What the `pkg-config` dependencies add to each component is now stored in
    the `componentDiffs` of `ComponentBuildDescr`, alongside the output of the
    per-component pre-configure hooks.
