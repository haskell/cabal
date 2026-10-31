---
synopsis: Compute Haddock library paths from package and library names
packages: [Cabal]
prs: [12491]
---

`Distribution.Simple.BuildPaths` exports `haddockLibraryDirPath'` and
`haddockLibraryPath'`, which compute the location of a library's Haddock
documentation from its package identifier or name and its library name, for
example for an installed package. `haddockDirName` accepts any `Package`.
