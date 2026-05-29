---
synopsis: Pre-configure compiler program database
packages: [Cabal, cabal-install]
prs: 11768
---

The compiler program database, containing `ghc`, `ghc-pkg`, `haddock` and various
toolchain programs (such as `ar`, `ld`) is now configured ahead of time within
`cabal-install`, so that we don't have to re-configure all of those programs
once for every package.

See the pre-existing Note [Caching the result of configuring the compiler] in
Distribution.Client.ProjectPlanning and Note [Constructing the ProgramDb]
in Distribution.Client.SetupWrapper for additional details.

This required a tiny change to `Cabal` to define and expose
`clearUnconfiguredPrograms :: ProgramDb -> ProgramDb` for use in `cabal-install`.
