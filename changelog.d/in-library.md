---
synopsis: Directly call in-library functions to build packages
packages: [Cabal, cabal-install]
prs: 11703
significance: significant
---

The way `cabal-install` builds packages has been significantly overhauled. In
most circumstances, `cabal-install` will directly call `Cabal` library functions
to build packages:

  - We no longer need `cabal-install` to act as a Setup (the `--act-as-setup`
    flag). We used to need this to set the working directory and to redirect
    logging output, but that can now be done via `Cabal` library functions.
  - Packages with `build-type: Hooks` are now also built via `Cabal` library
    functions instead of the `Setup.hs` interface. `cabal-install` achieves this
    by building an external hooks executable with which it communicates to
    run `SetupHooks`.

The main upside of this change is that we waste less time re-running the entire
`Cabal` `configure` step; instead `cabal-install` directly starts off with
the information it already knows (compiler, versions of dependencies given by
the solver, flag assignment, etc). This necessitated refactoring the `Cabal`
`configure` code in order to skip running the unnecessary initial steps that
are made redundant by the information from `cabal-install`'s `ElaboratedReadyPackage`.

There should be no outward-facing change in behaviour beside speeding up the
`configure` step.
