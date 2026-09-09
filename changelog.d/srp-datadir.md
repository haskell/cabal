---
synopsis: Fix the data-dir environment of in-place packages
packages: [cabal-install]
prs: 0
---

cabal-install exports a `<pkg>_datadir` variable for every package built in
place, so that its data files are found through `Paths_<pkg>` without an
install step. Two fixes to how that environment is built:

- For a `source-repository-package` the variable pointed at the tarball
  cabal-install packs the checkout into, not at the directory the package is
  unpacked to, so such a package could not find its data files when run
  (`cabal run`) or used as a build tool.

- When building a package, only the variables of the packages it depends on
  are exported now, instead of those of every in-place package in the plan.
  The variables are keyed by package name and override the data dir compiled
  into any executable of that name, so the plan-wide environment redirected
  tools that were not part of the build at all: a project that builds `hsc2hs`
  and preprocesses other packages with the `hsc2hs` found next to the compiler
  pointed that external `hsc2hs` at the not-yet-built in-place package.
