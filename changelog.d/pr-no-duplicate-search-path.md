---
synopsis: Search for each program once in each directory of the search path
packages: [Cabal]
prs: 0000
issues: 12544
---

Since Cabal 3.14 the search path that `configure` builds listed the initial
search path twice. Every search for a program that is not installed went
through `PATH` twice, and every program cabal configured, ghc included, ran
with a `PATH` that had each directory twice.
