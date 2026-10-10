---
synopsis: Do not pass `-optJSP` to GHC older than 9.10.2
packages: [Cabal]
prs: 12398
significance:
---

The `jspp-options` field is rendered as `ghc -optJSP<opt>`, but that flag only
exists since GHC 9.10.2. Passing it to an older GHC made the invocation fail, so
a package using `jspp-options` could not be built at all with GHC < 9.10.2. The
options are now omitted when the compiler is older than 9.10.2.
