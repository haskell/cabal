---
synopsis: Link to dependency documentation in the Haddock output directory
packages: [Cabal]
prs: [12491]
---

With `--haddock-output-dir=DIR`, Haddock now links against the documentation
of a dependency found at its usual location in `DIR`, that is `DIR/<package>`,
or `DIR/<package>/<sublibrary>` for a sublibrary, instead of its registered
documentation. Packages documented into the same directory, for example with
`cabal haddock all --haddock-output-dir=DIR`, now link to each other there,
with relative links, so that `DIR` can be moved or served over HTTP. A
`<library>.haddock.unit-id` file next to each interface in `DIR` records the
unit it was generated for, and documentation left there for another unit, for
example an earlier version of the dependency, is not used.
Previously, only the registered documentation in each package's build
directory was used: links were missing when it had not been generated, and
otherwise pointed to documentation possibly generated with different settings.
