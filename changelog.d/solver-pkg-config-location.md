---
synopsis: Make --with-pkg-config apply to the solver's pkg-config database
packages: [cabal-install]
prs: 12356
---

`--with-pkg-config` and `pkg-config-location` were honoured by Setup configure
but ignored by the solver, which read the pkg-config database through the
`pkg-config` found on the search path. A package whose `pkgconfig-depends`
only the user-specified pkg-config knew was rejected while planning. The
user-specified pkg-config is now the one the solver asks too.
