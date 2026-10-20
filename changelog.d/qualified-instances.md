---
synopsis: "Let build tools and setup scripts have their own instance of a package version"
packages: [cabal-install-solver, cabal-install]
---

A build tool (`build-tool-depends`) and a setup script (`setup-depends`) are
solved in their own scope, so the solver could already pick for them a version
of a package that differs from the one used by the rest of the plan. It could
not pick the same version with different dependencies or flags: the single
instance restriction made every goal that chose a version share one instance,
and the solve failed when they could not agree.

The restriction now applies within a scope only. When a build tool or a setup
script cannot share the instance of a package version used elsewhere in the
plan, it gets an instance of its own. For example, a project that builds new
`Win32` and `process` packages can now use `hsc2hs` as a build tool of `Win32`,
building `hsc2hs` against the installed `process` and `Win32`, even though the
project's own `hsc2hs` uses the new ones. The solver still prefers sharing an
instance; where sharing fails, a build tool or setup script that used to fall
back to another version of the package may now get its own instance of the
same version instead.

A second instance of a package of the project, or of a package depending on
one, is not supported yet, because such packages are built inplace under their
package id; the planner reports an error instead.
