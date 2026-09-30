---
synopsis: Build GHCi libraries with `ghc --merge-objs`
packages: [Cabal]
prs: 12358
issues: [7828, 9301, 12332]
---

To build a GHCi library (`--enable-library-for-ghci`), Cabal used to run
`ld -r` itself, finding `ld` from the GHC settings and probing it for the
flags it supports. When GHC advertises the `--merge-objs` mode (GHC 9.4 and
later), Cabal now delegates the merging to GHC, which uses the merge tool and
flags it was configured with. Whether a GHCi library can be built at all is
decided by whether GHC has a merge tool configured, instead of by probing
`ld --help`, and `ld` is no longer probed at configure time on such GHCs.
Older GHCs keep the previous behaviour.
