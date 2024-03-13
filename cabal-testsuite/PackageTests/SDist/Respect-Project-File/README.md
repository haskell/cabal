# Tests of sdist with `--project-file`

```
$ tree -P '*.project|*.test.hs' --prune
.
├── cabal.ignore-project.test.hs
├── cabal.ignore-project.v2.test.hs
├── cabal.no-project.test.hs
├── cabal.no-project.v2.test.hs
├── cabal.project
├── Projects-Default-No
│   ├── cabal.dot-uv.project
│   ├── cabal.dot-uv.test.hs
│   ├── cabal.dot-uv.v2.test.hs
│   ├── cabal.ignore-project.test.hs
│   ├── cabal.ignore-project.v2.test.hs
│   ├── cabal.no-project.test.hs
│   ├── cabal.no-project.v2.test.hs
│   ├── cabal.sub-pq.project
│   ├── cabal.sub-pq.test.hs
│   ├── cabal.sub-pq.v2.test.hs
│   ├── cabal.sub-rs.project
│   ├── cabal.sub-rs.test.hs
│   └── cabal.sub-rs.v2.test.hs
└── Projects-Default-Yes
    ├── cabal.dot-uv.project
    ├── cabal.dot-uv.test.hs
    ├── cabal.dot-uv.v2.test.hs
    ├── cabal.ignore-project.test.hs
    ├── cabal.ignore-project.v2.test.hs
    ├── cabal.no-project.test.hs
    ├── cabal.no-project.v2.test.hs
    ├── cabal.project
    ├── cabal.project.test.hs
    ├── cabal.project.v2.test.hs
    ├── cabal.sub-pq.project
    ├── cabal.sub-pq.test.hs
    ├── cabal.sub-pq.v2.test.hs
    ├── cabal.sub-rs.project
    ├── cabal.sub-rs.test.hs
    └── cabal.sub-rs.v2.test.hs

3 directories, 34 files
```

There are of the two subdirectories, one has a `cabal.project` and the other
doesn't. This is the default project. There are three important things to notice
with these tests.

1. All the tests with a supplied `--project-file` option pick up a default
   `cabal.project` instead; either the one one in the current directory or the
   one from the parent directory, one level up. I think this behaviour is wrong
   and the supplied `--project-file` option should be respected.

   Before I'd put a project there, one level up, the project probing had gone
   all the way up to Cabal's own `cabal.project` as can be seen by this diff
   after that change:

    ```diff
    $ git diff
    ...
    --- a/cabal-testsuite/PackageTests/SDist/Respect-Project-File/Projects-Default-No/cabal.sub-rs.out
    +++ b/cabal-testsuite/PackageTests/SDist/Respect-Project-File/Projects-Default-No/cabal.sub-rs.out
    @@ -1,12 +1,2 @@
    # cabal sdist
    -Wrote tarball sdist to <ROOT>/cabal.sub-rs.dist/work/./dist/sdist/Cabal-3.11.0.0.tar.gz
    -Wrote tarball sdist to <ROOT>/cabal.sub-rs.dist/work/./dist/sdist/cabal-testsuite-3.tar.gz
    -Wrote tarball sdist to <ROOT>/cabal.sub-rs.dist/work/./dist/sdist/Cabal-syntax-3.11.0.0.tar.gz
    -Wrote tarball sdist to <ROOT>/cabal.sub-rs.dist/work/./dist/sdist/cabal-install-3.11.0.0.tar.gz
    -Wrote tarball sdist to <ROOT>/cabal.sub-rs.dist/work/./dist/sdist/cabal-install-solver-3.11.0.0.tar.gz
    -Wrote tarball sdist to <ROOT>/cabal.sub-rs.dist/work/./dist/sdist/solver-benchmarks-3.tar.gz
    -Wrote tarball sdist to <ROOT>/cabal.sub-rs.dist/work/./dist/sdist/Cabal-QuickCheck-3.11.0.0.tar.gz
    -Wrote tarball sdist to <ROOT>/cabal.sub-rs.dist/work/./dist/sdist/Cabal-tree-diff-3.11.0.0.tar.gz
    -Wrote tarball sdist to <ROOT>/cabal.sub-rs.dist/work/./dist/sdist/Cabal-described-3.11.0.0.tar.gz
    -Wrote tarball sdist to <ROOT>/cabal.sub-rs.dist/work/./dist/sdist/Cabal-tests-3.tar.gz
    -Wrote tarball sdist to <ROOT>/cabal.sub-rs.dist/work/./dist/sdist/cabal-benchmarks-3.tar.gz
    +Wrote tarball sdist to <ROOT>/cabal.sub-rs.dist/work/./dist/sdist/p-0.1.tar.gz
    ```

2. The `--ignore-project` option works, as witnessed by each
   `cabal.ignore-project.test.hs` when the package in the same directory as the
   test is used.

3. The `*.v2.test.hs` are the same as their `*.test.hs` sibling tests but they
   exercise the `v2-sdist` command instead of the `sdist` command.
