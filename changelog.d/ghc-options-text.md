---
synopsis: Use Text instead of String in the fields of GhcOptions
packages: [Cabal, cabal-install]
prs: 12588
---

All string-typed fields of `GhcOptions` now hold `Text` instead of `String`.
This entails `renderGhcOptions` and `packageDbArgsDb` returning `[Text]`
instead of `[String]`; callers that run GHC still pass `[String]` arguments.
