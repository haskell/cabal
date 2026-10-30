---
synopsis: "`cabal bench` runs benchmarks one at a time, once everything is built"
packages: [cabal-install]
prs: 12407
issues: 7557
significance: significant
---

`cabal bench` used to run each benchmark as soon as it was built. When building
in parallel (e.g. with `-j`), several benchmarks could thus run at the same time,
and while other components were still being built, so that they competed for
resources, which skewed their results.

Now, `cabal bench` first builds everything that is needed (still in parallel),
and only then runs the benchmarks, one at a time, in the order of the build
plan. Consequently:

- The first benchmark starts only once everything is built, so its results
  come later than before, and all the build output now precedes the output of
  the benchmarks.
- Without `--keep-going`, no benchmark is run if anything fails to build, and
  no further benchmark is run once one of them has failed. With
  `--keep-going`, the benchmarks of everything that was built successfully are
  run.
