---
synopsis: "`cabal bench` no longer runs benchmarks in parallel"
packages: [cabal-install]
prs: 0000
issues: 7557
---

When building in parallel (e.g. with `-j`), `cabal bench` used to run several
benchmarks at the same time, so that they competed for resources and skewed each
other's results. Benchmarks are now run one at a time, while components are
still built in parallel.

Note that other components may still be built while a benchmark is running.
Use `-j1` to avoid that.
