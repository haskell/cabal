---
synopsis: Fix dead documentation link in the help text of `v1-` commands
packages: [cabal-install]
prs: 12549
issues: 12550
---

The legacy note printed by `cabal v1-*` commands pointed to
`nix-local-build-overview.html`, which no longer exists on the Cabal user
guide. It now points to `nix-local-build.html`.
