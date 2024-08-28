cabal-testsuite is a suite of integration tests for Cabal-based
frameworks.

How to run
----------

1. Build `cabal-testsuite` (`cabal build cabal-testsuite:cabal-tests`)
2. Run the `cabal-tests` executable. It will scan for all tests
   in your current directory and subdirectories and run them.
   To run a specific set of tests, use `cabal-tests --with-cabal=CABALBIN PATH ...`.
   (e.g. `cabal run cabal-testsuite:cabal-tests -- --with-cabal=cabal cabal-testsuite/PackageTests/TestOptions/setup.test.hs`)
   You can control parallelism using the `-j` flag.

There are a few useful flags:

* `--with-cabal PATH` can be used to specify the path of a
  `cabal-install` executable.  IF YOU DO NOT SPECIFY THIS FLAG,
  CABAL INSTALL TESTS WILL NOT RUN.

* `--with-ghc PATH` can be used to specify an alternate version of
  GHC to ask the tests to compile with.

* `--builddir DIR` can be used to manually specify the dist directory
  that was used to build `cabal-tests`; this can be used if
  the autodetection doesn't work correctly (which may be the
  case for old versions of GHC.)

* `--keep-tmp-files` can be used to keep the temporary directories that tests
  are run in.

## Which Cabal library version do cabal-install tests use?

By default the `cabal-install` tests will use the `Cabal` library which comes with
the boot compiler when it needs to build a custom `Setup.hs`.

This can be very confusing if you are modifying the Cabal library, writing a test
which relies on a custom setup script and you are wondering why the test is not
responding at all to your changes.

There are some flags which allow you to instruct `cabal-install` to use a different
`Cabal` library version.

1. `--boot-cabal-lib` specifies to use the Cabal library bundled with the
   test compiler, this is the default.
2. `--intree-cabal-lib=<root_dir>` specifies to use Cabal and Cabal-syntax
   from a specific directory, and `--test-tmp` indicates where to put
   the package database they are built into.
3. `--specific-cabal-lib=<VERSION>` specifies to use a specific Cabal
   version from hackage (ie 3.10.2.0) and installs the package database
   into `--test-tmp=<DIR>`

The CI scripts use the `--intree-cabal-lib` option for the most part but in
the future there should be a variety of jobs which test `cabal-install` built
against newer `Cabal` versions but forced to interact with older `Cabal` versions.

### How to run the doctests

You need to install the `doctest` tool. Make sure it's compiled with your current
GHC, and don't forget to reinstall it every time you switch GHC version:

``` shellsession
cabal install doctest --overwrite-policy=always --ignore-project
```

After that you can run doctests for a component of your choice via the following command:

``` shellsession
cabal repl --with-ghc=doctest --build-depends=QuickCheck --build-depends=template-haskell --repl-options="-w" --project-file="cabal.validate.project" Cabal-syntax
```

In this example we have run doctests in `Cabal-syntax`. Notice, that some
components have broken doctests
([#8734](https://github.com/haskell/cabal/issues/8734));
our CI currently checks that `Cabal-syntax` and `Cabal` doctests pass via
`make doctest-install && make doctest` (you can use this `make`-based workflow too).

How to write
------------

If you learn better by example, just look at the tests that live
in `cabal-testsuite/PackageTests`; if you `git log -p`, you can
see the full contents of various commits which added a test for
various functionality.  See if you can find an existing test that
is similar to what you want to test.

Tests are all run in temporary system directories. At the start of a test
all the files which are in the same folder as the test script are copied into
a system temporary directory and then the rest of the script operates in this
directory.

**NOTE:** only files which are known to git are copied, so you have to
`git add` any files which are part of a test before running the test.
You can use the `--keep-tmp-files` flag to keep the temporary directories in
order to inspect the result of running a test.

Otherwise, here is a walkthrough:

1. Create the package(s) that you need for your test in a
   new directory.
   (Currently (2021-10-06), tests are stored in `PackageTests`,
   with the exception of one test stored in `tests`.)

2. Create one or more `.test.hs` scripts in your directory, using
   the template:
   ```haskell
   import Test.Cabal.Prelude
   main = setupAndCabalTest $ do
       -- your test code here
   ```

   `setupAndCabal` test indicates that invocations of `setup`
   should work both for a raw `Setup` script, as well as
   `cabal-install` (if your test works only for one or the
   other, use `setupTest` or `cabalTest`).

   Code runs in the `TestM` monad, which manages some administrative
   environment (e.g., the test that is running, etc.).
   `Test.Cabal.Prelude` contains a number of useful functions
   for testing implemented in this monad, including the functions `cabal`
   and `setup` which let you invoke those respective programs.  You should
   read through that file to get a sense for what capabilities
   are possible (grep for use-sites of functions to see how they
   are used).  If you don't see something anywhere, that's probably
   because it isn't implemented. Implement it!

   To include parts that are supposed to fail (in the sense that a
   non-zero exit code is returned), there is the `fails` combinator,
   e.g.:
   ```haskell
   main = cabalTest $ do
     fails $ cabal "bad-command" [ "bad", "args" ]
     cabal "good-command" [ "good", "args" ]
     fails $ cabal "another-bad-one" [ ... ]
     ...
   ```

   The dependencies which your test is allowed to use are listed in the
   cabal file under the `test-runtime-deps` executable. At compile-time there is
   a custom Setup.hs script which inspects this list and records the versions of
   each package in a generated file. These are then used when `cabal-tests` runs
   when it invokes `runghc` to run each test.
   We ensure they are built and available by listing `test-runtime-deps` in the
   build-tool-depends section of the cabal-tests executable.


3. Run your tests using `cabal-tests` (no need to rebuild when
   you add or modify a test; it is automatically picked up).
   The first time you run a test, assuming everything else is
   in order, it will complain that the actual output doesn't match
   the expected output.  Use the `--accept` flag to accept the
   output if it makes sense!

We also support a `.multitest.hs` prefix; eventually this will
allow multiple tests to be defined in one file but run in parallel;
at the moment, these just indicate long running tests that should
be run early (to avoid straggling).

Frequently asked questions
--------------------------

For all of these answers, to see examples of the functions in
question, grep the test suite.

**Why isn't some output I added to Cabal showing up in the recorded
test output?** Only "marked" output is picked up by Cabal; currently,
only `notice`, `warn` and `die` produce marked output.  Use those
combinators for your output.

**How can I add a dependency on a package from Hackage in a test?**
By default, the test suite is completely independent of the contents
of Hackage, to ensure that it keeps working across all GHC versions.
If possible, define the package locally.  If the package needs
to be from Hackage (e.g., you are testing the global store code
in new-build), use `withRepo "repo"` to initialize a "fake" Hackage with
the packages placed in the `repo` directory.

**How do I run an executable that my test built?** The specific
function you should use depends on how you built the executable:

* If you built it using `Setup build`, use `runExe`
* If you installed it using `Setup install` or `cabal install`, use
  `runInstalledExe`.
* If you built it with `cabal build`, use `runPlanExe`; note
  that you will need to run this inside of a `withPlan` that is
  placed *after* you have invoked `build`. (Grep for an example!)

**How do I turn off accept tests? My test output wobbles too much.**
Use `recordMode DoNotRecord`.  This should be a last resort; consider
modifying Cabal so that the output is stable.  If you must do this, make
sure you add extra, manual tests to ensure the output looks like what
you expect.

**How can I manually test for a string in output?**  Use the primed
variants of a command (e.g., `cabal'` rather than `cabal`) and use
`assertOutputContains`.  Note that this will search over BOTH stdout
and stderr.

**How do I skip running a test in some environments?**  Use the
`skipIf` and `skipUnless` combinators.  Useful parameters to test
these with include `hasSharedLibraries`, `hasProfiledLibraries`,
`hasCabalShared`, `isGhcVersion`, `isWindows`, `isLinux`, `isOSX`.

**I programmatically modified a file in my test suite, but Cabal/GHC
doesn't seem to be picking it up.**  You need to sleep sufficiently
long before editing a file, in order for file system timestamp
resolution to pick it up.  Use `withDelay` and `delay` prior to
making a modification.

**How do I mark a test as broken?**  Use `expectBroken`, which takes
the ticket number as its first argument.  Note that this does NOT
handle accept-test brokenness, so you will have to add a manual
string output test, if that is how your test is "failing."

Hermetic tests
--------------

Tests are run in a fresh temporary system directory. This attempts to isolate the
tests from anything specific to do with your directory structure. In particular

* You must be running the test inside a valid Git checkout of the test
  suite (`withSourceCopy` uses Git to determine which files should be copied.)

* You must `git add` all files which are relevant to the test, otherwise
  they will not be copied.

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
  when you have many tests.  To solve this, our `Test.Cabal.Server`
  GHCi server implementation can reuse
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

Expect tests
------------

An expect test (aka _golden test_)
is a test where we read out the output of the test
and compare it directly against a saved copy of the test output.
When test output changes, you can ask the test suite to "accept"
the new output, which automatically overwrites the old expected
test output with the new.

Supporting expect tests with Cabal is challenging, because Cabal
interacts with multiple versions of external components (most
prominently GHC) with different variants of their output, and no
one wants to rerun a test on four different versions of GHC to make
sure we've picked up the correct output in all cases.

Still, we'd like to take advantage of expect tests for Cabal's error
reporting.  So here's our strategy:

1. We have a new verbosity flag `+markoutput` which lets you toggle the emission
   of `-----BEGIN CABAL OUTPUT-----` and  `-----END CABAL OUTPUT-----`
   stanzas.

2. When someone requests an expect test, we ONLY consider output between
   these flags.

The expectation is that Cabal will only enclose output it controls
between these stanzas.  In practice, this just means we wrap `die`,
`warn` and `notice` with these markers.

An added benefit of this strategy is that we can continue operating
at high verbosity by default (which is very helpful for having useful
diagnostic information immediately, e.g. in CI.)

We also need to deal with nondeterminism in test output in some
situations.  Here are the most common ones:

* Dependency solving output on failure is still non-deterministic, due to
  its dependence on the global package database.  We're tracking this
  in https://github.com/haskell/cabal/issues/4332 but for now, we're
  not running expect tests on this output.

* Tests against Custom setup will build against the Cabal that shipped with
  GHC, so you need to be careful NOT to record this output (since we
  don't control that output.)

* We have some munging on the output, to remove common sources of
  non-determinism: paths, GHC versions, boot package versions, etc.
  Check `normalizeOutput` to see what we do.  Note that we save
  *normalized* output, so if you modify the normalizer you will
  need to rerun the test suite accepting everything.

* The Setup interface gets a `--enable-deterministic` flag which we
  pass by default.  The intent is to make Cabal more deterministic;
  for example, with this flag we no longer compute a hash when
  computing IPIDs, but just use the tag `-inplace`.  You can manually
  disable this using `--disable-deterministic` (as is the case with
  `UniqueIPID`.)

Some other notes:

* It's good style to put `default-language` in all your stanzas, so
  Cabal doesn't complain about it (that warning is marked!).  Ditto
  with `cabal-version` at the top of your Cabal file.

* If you can't get the output of a test to be deterministic, no
  problem: just exclude it from recording and do a manual test
  on the output for the string you're looking for.  Try to be
  deterministic, but sometimes it's not (easily) possible.

Non-goals
---------

Here are some things we do not currently plan on supporting:

* A file format for specifying multiple packages and source files.
  While in principle there is nothing wrong with making it easier
  to write tests, tests stored in this manner are more difficult
  to debug with, as they must first be "decompressed" into a full
  folder hierarchy before they can be interacted with.  (But some
  of our tests need substantial setup; for example, tests that
  have to setup a package repository.  In this case, because there
  already is a setup necessary, we might consider making things easier here.)
