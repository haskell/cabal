---
synopsis: Generalise the install plan to arbitrary node keys
packages: [cabal-install]
prs: 12092
significance:
---

- `Distribution.Client.InstallPlan` no longer hardcodes `UnitId` as the key of
  its nodes. The `UnitId`-specific `IsUnit` constraint is replaced by

  ```haskell
  type IsGraph ipkg srcpkg = (IsNode ipkg, IsNode srcpkg, Key ipkg ~ Key srcpkg)
  ```

  and the shared key type is threaded through the operations that previously
  mentioned `UnitId` directly, so `Processing` and `BuildOutcomes` are now
  parameterised over the key. Functions that print or report on keys gained
  `Pretty`/`Show (Key ipkg)` constraints, and helpers became more honest about
  what they need — e.g. `depends :: IsNode a => a -> [Key a]` rather than
  `depends :: IsUnit a => a -> [UnitId]`.
- This is a behaviour-preserving refactor. The sole instantiation,
  `GenericPlanPackage InstalledPackageInfo (ConfiguredPackage UnresolvedPkgLoc)`,
  remains `UnitId`-keyed, so there is no functional change. It prepares for
  cross-compilation (#11179), where install plans are keyed by a stage-qualified
  key rather than a bare `UnitId`.
