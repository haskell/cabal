---
synopsis: Stage-qualified package stanzas in project files
packages: [cabal-install, Cabal-syntax]
prs: 0
---

A project file can configure every package built for one build stage of a
cross-compilation with `package build:*` and `package host:*`, in addition to
`package *` (both stages) and `package <name>`. These sit between the local
package layer and the named layer. To allow the `build:*` spelling, section
arguments in the fields parser may now contain colons; no consumer of a
`.cabal` file accepts an argument with a colon, so `.cabal` files do not gain
new syntax.

Program locations in a stage stanza also select the tools `cabal` itself runs
for that stage: `package host:*` with `pkg-config-location:` makes the host
stage's pkg-config database, and so the solver's view of `pkgconfig-depends`
for host packages, come from a cross toolchain's pkg-config.
