---
synopsis: Store Setup targets in each command's flags, not in CommonSetupFlags
packages: [Cabal, cabal-install]
prs: 12562
issues: 12538
---

The targets of a `Setup` invocation are no longer stored in the `setupTargets`
field of `CommonSetupFlags`. Each command's flags now have their own targets
field (e.g. `configTargets`, `buildTargets`).

Breaking changes:

  - `setupTargets` is removed from `CommonSetupFlags`;
  - the `*CommonFlags` pattern synonyms no longer have a targets field;
  - `extraConfigArgs` is removed from `LocalBuildConfig` and `LocalBuildInfo`
    (it duplicated `configTargets`).

The command line interface of `Setup` is unchanged.
