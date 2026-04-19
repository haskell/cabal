---
synopsis: "Improve Backpack broken-package error to show unfilled signatures"
packages: [Cabal]
prs: 11669
---

When an indefinite Backpack package is installed separately (e.g. via
nix `callCabal2nix`), configuring a consumer that depends on it would
show an opaque hashed UnitId like
`framework-0.1.0.0+95RTb42ZWxa9J13cUStM0q`. The error now shows the
package name, which signatures are unfilled, and advises rebuilding
in the same cabal project so cabal can fill them.
