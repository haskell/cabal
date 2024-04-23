Cabal-tests
===========

The test suites of `Cabal-tests` are ordinary unit tests.  If you're looking for
the package tests, they live in `cabal-testsuite` now.

If running test suites `parser-tests` and `check-tests` to update the expected
output, do so from this directory, the `Cabal-tests` directory. These test
suites look for packages to test in relative locations.

```
$ cabal run Cabal-tests:parser-tests -- --accept

$ cabal run Cabal-tests:check-tests -- --accept

$ tree -P '*.cabal' --prune
.
├── Cabal-tests.cabal
└── tests
    └── ParserTests
        ├── errors
        │   ├── anynone.cabal
        │   ├── big-version.cabal
        │   ├── common1.cabal
        │   ├── common2.cabal
        │   ├── common3.cabal
        │   ├── forward-compat2.cabal
        │   ├── forward-compat3.cabal
        │   ├── forward-compat.cabal
        │   ├── issue-5055-2.cabal
        │   ├── issue-5055.cabal
        │   ├── leading-comma-2b.cabal
        │   ├── leading-comma-2.cabal
        │   ├── leading-comma-2c.cabal
        │   ├── leading-comma.cabal
        │   ├── libpq1.cabal
        │   ├── libpq2.cabal
        │   ├── MiniAgda.cabal
        │   ├── mixin-1.cabal
        │   ├── mixin-2.cabal
        │   ├── multiple-libs.cabal
        │   ├── noVersion2.cabal
        │   ├── noVersion.cabal
        │   ├── range-ge-wild.cabal
        │   ├── removed-fields.cabal
        │   ├── spdx-1.cabal
        │   ├── spdx-2.cabal
        │   ├── spdx-3.cabal
        │   ├── undefined-flag.cabal
        │   ├── version-sets-1.cabal
        │   ├── version-sets-2.cabal
        │   ├── version-sets-3.cabal
        │   └── version-sets-4.cabal
        ├── ipi
        │   ├── Includes2.cabal
        │   ├── internal-preprocessor-test.cabal
        │   ├── issue-2276-ghc-9885.cabal
        │   └── transformers.cabal
        ├── regressions
        │   ├── all-upper-bound.cabal
        │   ├── anynone.cabal
        │   ├── assoc-cpp-options.cabal
        │   ├── bad-glob-syntax.cabal
        │   ├── big-version.cabal
        │   ├── cc-options-with-optimization.cabal
        │   ├── common2.cabal
        │   ├── common3.cabal
        │   ├── common.cabal
        │   ├── common-conditional.cabal
        │   ├── cxx-options-with-optimization.cabal
        │   ├── decreasing-indentation.cabal
        │   ├── denormalised-paths.cabal
        │   ├── elif2.cabal
        │   ├── elif.cabal
        │   ├── encoding-0.8.cabal
        │   ├── extensions-paths-5054.cabal
        │   ├── generics-sop.cabal
        │   ├── ghc-option-j.cabal
        │   ├── globstar-literal.cabal
        │   ├── haddock-api-2.18.1-check.cabal
        │   ├── hasktorch.cabal
        │   ├── hidden-main-lib.cabal
        │   ├── indentation2.cabal
        │   ├── indentation3.cabal
        │   ├── indentation.cabal
        │   ├── issue-5055.cabal
        │   ├── issue-5846.cabal
        │   ├── issue-6083-a.cabal
        │   ├── issue-6083-b.cabal
        │   ├── issue-6083-c.cabal
        │   ├── issue-6083-pkg-pkg.cabal
        │   ├── issue-6288-a.cabal
        │   ├── issue-6288-b.cabal
        │   ├── issue-6288-c.cabal
        │   ├── issue-6288-d.cabal
        │   ├── issue-6288-e.cabal
        │   ├── issue-6288-f.cabal
        │   ├── issue-774.cabal
        │   ├── issue-7776-a.cabal
        │   ├── issue-7776-b.cabal
        │   ├── issue-7776-c.cabal
        │   ├── issue-8646.cabal
        │   ├── jaeger-flamegraph.cabal
        │   ├── leading-comma-2.cabal
        │   ├── leading-comma.cabal
        │   ├── libpq1.cabal
        │   ├── libpq2.cabal
        │   ├── mixin-1.cabal
        │   ├── mixin-2.cabal
        │   ├── mixin-3.cabal
        │   ├── monad-param.cabal
        │   ├── multiple-libs-2.cabal
        │   ├── nothing-unicode.cabal
        │   ├── noVersion.cabal
        │   ├── Octree-0.5.cabal
        │   ├── pre-1.6-glob.cabal
        │   ├── pre-2.4-globstar.cabal
        │   ├── pre-3.8-globstar-literal.cabal
        │   ├── public-multilib-1.cabal
        │   ├── public-multilib-2.cabal
        │   ├── shake.cabal
        │   ├── spdx-1.cabal
        │   ├── spdx-2.cabal
        │   ├── spdx-3.cabal
        │   ├── th-lift-instances.cabal
        │   ├── version-sets.cabal
        │   └── wl-pprint-indef.cabal
        └── warnings
            ├── bom.cabal
            ├── bool.cabal
            ├── deprecatedfield.cabal
            ├── doubledash.cabal
            ├── extratestmodule.cabal
            ├── gluedop.cabal
            ├── multiplesingular.cabal
            ├── nbsp.cabal
            ├── newsyntax.cabal
            ├── oldsyntax.cabal
            ├── operator.cabal
            ├── specversion-a.cabal
            ├── specversion-b.cabal
            ├── specversion-c.cabal
            ├── subsection.cabal
            ├── tab.cabal
            ├── trailingfield.cabal
            ├── unknownfield.cabal
            ├── unknownsection.cabal
            ├── utf8.cabal
            ├── versiontag.cabal
            └── wildcard.cabal

7 directories, 127 files
```
