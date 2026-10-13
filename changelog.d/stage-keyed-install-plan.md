---
synopsis: Key the elaborated install plan by build stage
packages: [cabal-install]
prs: 12356
---

- Each elaborated package now records the stage it is built for, and the
  elaborated install plan is keyed by `WithStage UnitId` rather than a bare
  `UnitId`. Under cross-compilation the host and build copies of a package
  can share a `UnitId`; keying by stage keeps them as distinct nodes in the
  plan graph.
- Plan edges carry the stage the solver resolved each dependency at:
  library dependencies are on the package's own stage, while executable
  (`build-tools`) and setup dependencies point at the stage of the plan
  node they were resolved to (the build stage when cross-compiling, the
  host stage otherwise).
- The toolchain used to elaborate a package is selected by that package's
  stage, and both toolchains are carried in `ElaboratedSharedConfig`.
- The build and monitoring subsystems, and the `plan.json` output, remain
  keyed by plain `UnitId`; the stage tag is projected away at those
  boundaries, which is lossless when there is only one stage. Under
  cross-compilation, a package present at both stages under one `UnitId`
  appears once in those views (the host copy) and twice, under the same
  `id`, in `plan.json`; making those consumers stage-aware is left for a
  follow-up.
- Behaviour-preserving for ordinary (non-cross) builds, which have a single
  stage. Cross-compiling a package with a custom `Setup.hs` now builds its
  setup dependencies with the build compiler, as separate nodes in the plan.

Builds on the generalised install plan key (#12092) and is part of
cross-compilation support (#11179).
