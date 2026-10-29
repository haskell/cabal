---
synopsis: Key the elaborated install plan by build machine
packages: [cabal-install]
prs: 12356
---

- Each elaborated package now records the machine it is built for, and the
  elaborated install plan is keyed by `WithMachine UnitId` rather than a bare
  `UnitId`. Under cross-compilation the host and build copies of a package
  can share a `UnitId`; keying by machine keeps them as distinct nodes in the
  plan graph.
- Plan edges carry the machine the solver resolved each dependency at:
  library dependencies are on the package's own machine, while executable
  (`build-tool-depends`) and setup dependencies point at the machine of the plan
  node they were resolved to (the build machine when cross-compiling, the
  host machine otherwise).
- The toolchain used to elaborate a package is selected by that package's
  machine, and both toolchains are carried in `ElaboratedSharedConfig`.
- The build and monitoring subsystems, and the `plan.json` output, remain
  keyed by plain `UnitId`; the machine tag is projected away at those
  boundaries, which is lossless when there is only one machine. Under
  cross-compilation, a package present on both machines under one `UnitId`
  appears once in those views (the host copy) and twice, under the same
  `id`, in `plan.json`; making those consumers machine-aware is left for a
  follow-up.
- Behaviour-preserving for ordinary (non-cross) builds, which have a single
  machine. Cross-compiling a package with a custom `Setup.hs` now builds its
  setup dependencies with the build compiler, as separate nodes in the plan.

Builds on the generalised install plan key (#12092) and is part of
cross-compilation support (#11179).
