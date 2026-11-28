---
synopsis: Do not reconfigure packages when enabling documentation
packages: [cabal-install]
prs: [12667]
---

Enabling or disabling documentation no longer reconfigures local packages.
With Cabal 3.19 or later, the GHC flags that documentation implies, such as
`-haddock`, are passed to each build and Haddock invocation instead of being
persisted in `setup-config`. They precede explicit arguments, which therefore
take precedence, and which remain part of the package configuration: for
example, `-haddock` in `ghc-options` still reconfigures. The repl gets
`-haddock` the same way, so that `:doc` finds the documentation of the
project's own definitions. Older custom Setup versions retain the
configure-time behaviour.

The build monitor records the flags used by the last successful compilation:
enabling documentation compiles again to produce the missing artifacts, while
disabling it reuses the existing ones.
