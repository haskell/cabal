cabal-testsuite is a suite of integration tests for Cabal-based
frameworks.

How to run
----------

1. Build `cabal-tests` (`cabal new-build cabal-tests`)
2. Run the `cabal-tests` executable. It will scan for all tests
   in your current directory and subdirectories and run them.
   To run a specific set of tests, use `cabal-tests PATH ...`.  You can
   control parallelism using the `-j` flag.

There are a few useful flags which can handle some special cases:

* `--builddir DIR` can be used to manually specify the dist directory
  that was used to build `cabal-tests`; this can be used if
  the autodetection doesn't work correctly (which may be the
  case for old versions of GHC.)

* `--with-ghc PATH` can be used to specify an alternate version of
  GHC to ask the tests to compile with.

* `--with-cabal PATH` can be used to specify the path of a
  `cabal-install` executable.  In this case, tests involving
  this executable will also get run.

How to write
------------

1. Create the package(s) that you need for your test in a
   new directory.  (Currently, tests are stored in `PackageTests`
   and `tests`; we might reorganize this soon.)

2. Create one or more `.test.hs` scripts in your directory, using
   the template:
   ```
   import Test.Cabal.Prelude
   main = setupAndCabalTest $ do
       -- your test code here
   ```

   The general API is that the test is considered to succeed if
   it returns exit 0, and failed if it returned exit code 1.
   Standard output/error are purely for diagnostic purposes.
   `setupAndCabal` test indicates that invocations of `setup`
   should work both for a raw `Setup` script, as well as
   `cabal-install` (if your test works only for one or the
   other, use `setupTest` or `cabalTest`).

   Code runs in the `TestM` monad, which manages some administrative
   environment (e.g., the test that is running, etc.)
   `Test.Cabal.Prelude` contains a number of useful functions
   for testing implemented in this monad, including ways to invoke
   `Setup`, `ghc-pkg`, and other important programs.  For other
   ideas of how to write tests, look at existing `.test.hs`
   scripts.  If you don't see something anywhere, that's probably
   because it isn't implemented. Implement it!

3. Run your tests using `cabal-tests` (no need to rebuild when
   you add or modify a test; it is automatically picked up.)

We also support a `.multitest.hs` prefix; eventually this will
allow multiple tests to be defined in one file but run in parallel;
at the moment, these just indicate long running tests that should
be run early (to avoid straggling.)

Design notes
------------

This is the second rewrite of the integration testing framework.  The
primary goal was to use Haskell as the test language (letting us take
advantage of a real programming language, and use utilities provided to
us by the Cabal library itself), while at the same time compensating for
two perceived problems of pure-Haskell test suites:

* Haskell test suites are generally compiled before they run
  (for example, this is the modus operandi of `cabal test`).
  In practice, this results in a long edit-recompile cycle
  when working on tests. This hurts a lot when you would
  like to experimentally edit a test when debugging an issue.

* Haskell's metaprogramming facilities (e.g., Template Haskell)
  can't handle dynamically loading modules from the file system;
  thus, there ends up being a considerable amount of boilerplate
  needed to "wire" up test cases to the central test runner.

Our approach to address these issues is to maintain Haskell test scripts
as self-contained programs which are run by the GHCi interpreter.
This is not altogether trivial, and so there are a few important
technical innovations to make this work:

* Unlike a traditional test program which can be built by the Cabal
  build system, these test scripts must be interpretable at
  runtime (outside of the build system.)  Our approach to handle
  this is to link against the same version of Cabal that was
  used to build the top-level test program (by way of a Custom
  setup linked against the Cabal library under test) and then
  use this library to compute the necessary GHC flags to pass
  to these scripts.

* The startup latency of `runghc` can be quite high, which adds up
  when you have many tests.  To solve this, in `Test.Cabal.Server`
  we have an implementation an GHCi server, for which we can reuse
  a GHCi instance as we are running test scripts.  It took some
  technical ingenuity to implement this, but the result is that
  running scripts is essentially free.

Here is the general outline of how the `cabal-tests` program operates:

1. It first loads the cached `LocalBuildInfo` associated with the
   host build system (which was responsible for building `cabal-tests`
   in the first place.)  This information lets us compute the
   flags that we will use to subsequently invoke GHC.

2. We then recursively scan the current working directory, looking
   for files suffixed `.test.hs`; these are the test scripts we
   will run.

3. For every thread specified via the `-j`, we spawn a GHCi
   server, and then use these to run the test scripts until all
   test scripts have been run.

The new `cabal-tests` runner doesn't use Tasty because I couldn't
figure out how to get out the threading setting, and then spawn
that many GHCi servers to service the running threads.  Improvements
welcome.

Non-goals
---------

Here are some things we do not plan on supporting:

* A file format for specifying multiple packages and source files.
  While in principle there is nothing wrong with making it easier
  to write tests, tests stored in this manner are more difficult
  to debug with, as they must first be "decompressed" into a full
  folder hierarchy before they can be interacted with.

* An accept output mode, which allows users to accept new test output.
  The trouble is that Cabal output is complex, often interspersed with
  output from GHC, and whatever this test output is, must be consistent
  across platforms, versions of GHC, etc.  Experience has shown that
  Cabal's command line output is not consistent enough for tests of
  this manner to work well.
