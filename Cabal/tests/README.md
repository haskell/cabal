Writing package tests
=====================

The tests under the [PackageTests] directory define and build packages
that exercise various components of Cabal. Each test case is an [HUnit]
test. The entry point for the test suite, where all the test cases are
listed, is [PackageTests.hs]. There are utilities for calling the stages
of Cabal's build process in [PackageTests/PackageTester.hs]; have a look
at an existing test case to see how they are used.

In order to run the tests, `PackageTests` needs to know where the inplace
copy of Cabal being tested is, as well as some information which was
used to configure it.  By default, `PackageTests` tries to look at the
`LocalBuildInfo`, but if the format of `LocalBuildInfo` has changed
between the version of Cabal which ran the configure step, and the
version of Cabal we are testing against, this may fail.  In that
case, you can manually specify the information we need using
the following environment variables:

* `CABAL_PACKAGETESTS_GHC` is the path to the GHC you compiled Cabal with
* `CABAL_PACKAGETESTS_WITH_GHC` is the path for the GHC you want to have
  Cabal use when running tests; i.e., you can change this to a different
  version of GHC to see how Cabal handles that version.  If omitted,
  it defaults to `CABAL_PACKAGETESTS_GHC`.
* `CABAL_PACKAGETESTS_DB_STACK` is a PATH-style list of package database paths,
  `clear`, `global` and `user`.  Each component of the list is
  interpreted the same way as Cabal's `-package-db` flag.  This list
  must contain the copy of Cabal you are planning to test against
  (as well as its transitive dependencies).  By default, we guess
  that it is just the global and user database.  However, if some of
  Cabal's dependencies were installed in a sandbox or other non-standard
  location, you will need to add it.  Most commonly, if you are are
  using 'new-build' you'll need to add
  dist-newstyle/packagedb/ghc-VERSION and $HOME/.cabal/store/ghc-VERSION/package.db
  to your database stack.  (In most situations, the actual inplace
  database Cabal was registered into is automatically detected.)

There are a few extra options to toggle (e.g. `CABAL_PACKAGETESTS_GHC_PKG`
lets you explicitly set ghc-pkg in case Cabal can't autodetect it) but
the three above.

If you can successfully run the test suite, we'll print out examples
of all of these values for you under "Environment".

[PackageTests]: PackageTests
[HUnit]: http://hackage.haskell.org/package/HUnit
[PackageTests.hs]: PackageTests.hs
[PackageTests/PackageTester.hs]: PackageTests/PackageTester.hs
[detailed]: ../Distribution/TestSuite.hs
[PackageTests/BuildTestSuiteDetailedV09/Check.hs]: PackageTests/BuildTestSuiteDetailedV09/Check.hs
