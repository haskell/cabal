# Cabal [![Hackage version](https://img.shields.io/hackage/v/Cabal.svg?label=Hackage)](https://hackage.haskell.org/package/Cabal) [![Stackage version](https://www.stackage.org/package/Cabal/badge/lts?label=Stackage)](https://www.stackage.org/package/Cabal) [![Build Status](https://secure.travis-ci.org/haskell/cabal.svg?branch=master)](http://travis-ci.org/haskell/cabal) [![Windows build status](https://ci.appveyor.com/api/projects/status/yotutrf4i4wn5d9y/branch/master?svg=true)](https://ci.appveyor.com/project/23Skidoo/cabal) [![Documentation Status](http://readthedocs.org/projects/cabal/badge/?version=wip-sphinx-doc)](http://cabal.readthedocs.io/en/wip-sphinx-doc/?badge=wip-sphinx-doc)

This Cabal Git repository contains the following packages:

 * [Cabal](Cabal/README.md): the Cabal library package ([license](Cabal/LICENSE))
 * [cabal-install](cabal-install/README.md): the package containing the `cabal` tool ([license](cabal-install/LICENSE))

The canonical upstream repository is located at
https://github.com/haskell/cabal.

Installing Cabal
----------------

Assuming that you have a pre-existing, older version of `cabal-install`,
run:

~~~~
cabal install cabal-install
~~~~

To get the latest version of `cabal-install`. (You may want to `cabal
update` first.)

To install the latest version from the Git repository, clone the
Git repository and then run:

~~~~
(cd Cabal; cabal install)
(cd cabal-install; cabal install)
~~~~

Building Cabal for hacking
--------------------------

The current recommended way of developing Cabal is to use the
`new-build` feature which [shipped in cabal-install-1.24](http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/).  Assuming
that you have a sufficiently recent cabal-install (see above),
it is sufficient to run:

~~~~
cabal new-build cabal-install
~~~~

To build a local, development copy of cabal-install.  The binary
will be located at
`dist-newstyle/build/cabal-install-$VERSION/build/cabal/cabal`;
you can determine the `$VERSION` of cabal-install by looking at
[cabal-install/cabal-install.cabal](cabal-install/cabal-install.cabal).

Here are some other useful variations on the commands:

~~~~
cabal new-build Cabal # build library only
cabal new-build Cabal:package-tests # build Cabal's package test suite
cabal new-build cabal-install:integration-tests # etc...
~~~~

Running tests
-------------

**Using Travis and AppVeyor.**
The easiest way to run tests on Cabal is to make a branch on GitHub
and then open a pull request; our continuous integration service on
Travis and AppVeyor will build and test your code.  Title your PR
with WIP so we know that it does not need code review.  Alternately,
you can enable Travis on your fork in your own username and Travis
should build your local branches.

Some tips for using Travis effectively:

* Watch over your jobs on the [Travis website](http://travis-ci.org).
  If you know a build of yours is going to fail (because one job has
  already failed), be nice to others and cancel the rest of the jobs,
  so that other commits on the build queue can be processed.

* If you want realtime notification when builds of your PRs finish, we have a [Slack team](https://haskell-cabal.slack.com/). To get issued an invite, fill in your email at [this sign up page](https://haskell-cabal.herokuapp.com).

* If you enable Travis for the fork of Cabal in your local GitHub, you
  can have builds done automatically for your local branch seperate
  from Cabal. This is an alternative to opening a PR.

**Running tests locally.**
To run tests locally with `new-build`, you will need to know the
name of the test suite you want.  Cabal and cabal-install have
several.  In general, the test executable for
`{Cabal,cabal-install}:$TESTNAME` will be stored at
`dist-newstyle/build/{Cabal,cabal-install}-$VERSION/build/$TESTNAME/$TESTNAME`.

To run a single test, use `-p` which applies a regex filter to the test names.

* `Cabal:package-tests` are out-of-process integration tests on the top-level `Setup`
  command line interface.  If you are hacking on the Cabal library you
  want to run this test suite.  It must be run from the `Cabal` subdirectory
  (ugh!)  This test suite can be a bit touchy; see
  [Cabal/tests/README.md](Cabal/tests/README.md) for more information.
  Build products and test logs are generated and stored in
  `Cabal/tests/PackageTests` under folders named `dist-test` and
  `dist-test.$subname`.

  Handy command line spell to find test logs is:
  ```sh
  find . -name test.log|grep test-name
  ```

  `test.sh` in the same directory as `test.log` is intended to let you rerun
  the test without running the actual test driver.


* `Cabal:unit-tests` are small, quick-running unit tests
  on small pieces of functionality in Cabal.  If you are working
  on some utility functions in the Cabal library you should run this
  test suite.

* `cabal-install:unit-tests` are small, quick-running unit tests on
  small pieces of functionality in cabal-install.  If you are working
  on some utility functions in cabal-install you should run this test
  suite.

* `cabal-install:solver-quickcheck` are QuickCheck tests on
  cabal-install's dependency solver.  If you are working
  on the solver you should run this test suite.

* `cabal-install:integration-tests` are out-of-process integration tests on the
  top-level `cabal` command line interface.  The coverage is not
  very good but it attempts to exercise most of cabal-install.

* `cabal-install:integration-tests2` are integration tests on some
  top-level API functions inside the `cabal-install` source code.
  You should also run this test suite.

Conventions
-----------

* Spaces, not tabs.

* Try to follow style conventions of a file you are modifying, and
  avoid gratuitous reformatting (it makes merges harder!)

* A lot of Cabal does not have top-level comments.  We are trying to
  fix this.  If you add new top-level definitions, please Haddock them;
  and if you spend some time understanding what a function does, help
  us out and add a comment.  We'll try to remind you during code review.

* If you do something tricky or non-obvious, add a comment.

* For local imports (Cabal module importing Cabal module), import lists
  are NOT required (although you may use them at your discretion.)  For
  third-party and standard library imports, please use explicit import
  lists.

* You can use basically any GHC extension supported by a GHC in our
  support window, except Template Haskell, which would cause
  bootstrapping problems in the GHC compilation process.

* Our GHC support window is three years: that is, the Cabal library
  must be buildable out-of-the-box with the dependencies that shipped
  with GHC for at least three years.  The Travis CI checks this, so
  most developers submit a PR to see if their code works on all
  these versions of Haskell.  cabal-install must also be buildable
  on all these GHCs, although it does not have to be buildable
  out-of-the-box. Instead, the `cabal-install/bootstrap.sh` script
  must be able to download and install all of the dependencies.
  (This is also checked by CI!)

* `Cabal` has its own Prelude, in `Distribution.Compat.Prelude`,
  that provides a compatibility layer and exports some commonly
  used additional functions. Use it in all new modules.

We like [this style guide][guide].

[guide]: https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md

Communicating
-------------

There are a few main venues of communication:

* Most developers subscribe to receive messages from [all issues](https://github.com/haskell/cabal/issues); issues can be used to [open discussion](https://github.com/haskell/cabal/issues?q=is%3Aissue+is%3Aopen+custom+label%3A%22type%3A+discussion%22).  If you know someone who should hear about a message, CC them explicitly using the @username GitHub syntax.

* For more organizational concerns, the [mailing
  list](http://www.haskell.org/mailman/listinfo/cabal-devel) is used.

* Many developers idle on `#hackage` on `irc.freenode.net`.  `#ghc` is
  also a decently good bet.

Releases
--------

Notes for how to make a release are at the
wiki page ["Making a release"](https://github.com/haskell/cabal/wiki/Making-a-release).
Currently, @23Skidoo, @rthomas, @tibbe and @dcoutts have access to
`haskell.org/cabal`, and @davean is the point of contact for getting
permissions.

API Documentation
-----------------

Auto-generated API documentation for the `master` branch of Cabal is automatically uploaded here: http://haskell.github.io/cabal-website/doc/html/Cabal/.
