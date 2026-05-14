---
synopsis: Recompilation checking for SetupHooks pre-build rules
packages: [Cabal, Cabal-hooks]
prs: 11731
issues: 11730
---

Pre-build rules are now only re-run when stale, according to the conditions
described in the [SetupHooks API](https://hackage.haskell.org/package/Cabal-hooks/docs/Distribution-Simple-SetupHooks.html). That is, a rule is re-run if any of the following conditions are
satisfied:

  - The rule is new, or
  - A dependency of the rule is stale.  
    That is, either we have re-run another rule that this rule depends on,
    or one of the file inputs to the rule is newer than the oldest output of the
    rule (or the rule output doesn't exist at all), or
  - The rule itself has changed, e.g. the parameters stored in `RuleData`
    have changed.

In particular, Cabal will now write per-component caches of pre-build rules
in order to compute which rules have changed between runs, with file name
"setup-hooks-rules.cache".
