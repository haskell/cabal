Writing package tests
=====================

The tests under the [PackageTests] directory define and build packages
that exercise various components of Cabal. Each test case is an [HUnit]
test. The entry point for the test suite, where all the test cases are
listed, is [PackageTests.hs]. There are utilities for calling the stages
of Cabal's build process in [PackageTests/PackageTester.hs]; have a look
at an existing test case to see how they are used.

It is important that package tests use the in-place version of Cabal
rather than the system version. Several long-standing bugs in the test
suite were caused by testing the system (rather than the newly compiled)
version of Cabal. There are two places where the system Cabal can
accidentally be invoked:

1. Compiling `Setup.hs`. `runghc` needs to be told about the in-place
   package database. This issue should be solved for all future package
   tests; see `compileSetup` in [PackageTests/PackageTester.hs].

2. Compiling a package which depends on Cabal. In particular, packages
   with the [detailed]-type test suites depend on the Cabal library
   directly, so it is important that they are configured to use the
   in-place package database. The test suite already creates a stub
   `PackageSpec` for this case; see
   [PackageTests/BuildTestSuiteDetailedV09/Check.hs] to see how it is
   used.

[PackageTests]: PackageTests
[HUnit]: http://hackage.haskell.org/package/HUnit
[PackageTests.hs]: PackageTests.hs
[PackageTests/PackageTester.hs]: PackageTests/PackageTester.hs
[detailed]: ../Distribution/TestSuite.hs
[PackageTests/BuildTestSuiteDetailedV09/Check.hs]: PackageTests/BuildTestSuiteDetailedV09/Check.hs