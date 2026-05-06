---
synopsis: Stop exposing constructors of RuleCommands
packages: [Cabal-hooks]
prs: 11771
issues: 11461
---

The constructors of the `SetupHooks` `RuleCommands` are no longer exposed from
`Distribution.Simple.SetupHooks`. These were rather gnarly internal constructors;
the intended public API is via `staticRule` and `dynamicRule`.
