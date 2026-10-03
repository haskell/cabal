---
synopsis: "`cabal bench` runs benchmarks one at a time, once everything is built"
packages: [cabal-install]
prs: 0000
issues: 7557
---

`cabal bench` used to run each benchmark as soon as it was built. When building
in parallel (e.g. with `-j`), several benchmarks could thus run at the same time,
and while other components were still being built, so that they competed for
resources, which skewed their results.

Now, `cabal bench` first builds everything (still in parallel), and only then
runs the benchmarks, one at a time. Without `--keep-going`, no benchmark is run
if the build fails.
