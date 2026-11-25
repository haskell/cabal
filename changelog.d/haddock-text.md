---
synopsis: "Use Text instead of String in the Haddock command rendering"
packages: [Cabal, cabal-install]
prs: 12643
---

`Distribution.Simple.Haddock` now builds the `haddock` invocation as `[Text]`,
converting to `String` at the process boundary. The path-like
`HaddockFlags`/`HaddockProjectFlags` fields use `FilePath`, including the
resources directory.
