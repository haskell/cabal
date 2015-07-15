Contributing to Cabal
=====================

If you want to hack on Cabal, don't be intimidated!

* Read the [guide to the source
  code](https://github.com/haskell/cabal/wiki/Source-Guide).

* Subscribe to the [mailing
  list](http://www.haskell.org/mailman/listinfo/cabal-devel).

* Browse the [list of open issues](https://github.com/haskell/cabal/issues).

* There are other resources listed on the [development
  wiki](https://github.com/haskell/cabal/wiki).

* See [Cabal/tests/README.md] for information about writing package tests.

Of particular value are the open issues list and the cabal-devel mailing
list, which is a good place to ask questions.

[Cabal/tests/README.md]: Cabal/tests/README.md

Building Cabal from git cloned sources and running the tests
------------------------------------------------------------

_The steps below will make use of sandboxes for building. The process might be
somewhat different when you do not want to use sandboxes._

Building Cabal from from source requires the following:

* Glorious/Glasgow Haskell Compiler (ghc).
* An existing (relatively recent) `cabal` binary (e.g. obtained as part of the
Haskell Platform, bootstrapped from the
[source tarball on Hackage](http://hackage.haskell.org/package/cabal-install) or
installed from your Linux vendor).
* The sources. For example, you might want to

    ~~~~
    cd ~/MyHaskellCode
    git clone https://github.com/haskell/cabal.git
    cd cabal
    ~~~~

    to download the git repository to ~/MyHaskellCode/cabal.

To build and test the `Cabal` library, do:

1. Move to `Cabal` directory:

    ~~~~
    cd Cabal
    ~~~~

2. Create a sandbox, and fill it with the necessary dependencies:

    ~~~~
    cabal sandbox init
    cabal install --only-dependencies --enable-tests
    ~~~~

3. Unfortunately, because of way the bootstrapping works for cabal,
    we cannot use `cabal` for the next steps;
    we need to use Setup instead.
    So, compile Setup.hs:
    
    ~~~~
    ghc --make -threaded Setup.hs
    ~~~~

4. However, we _do_ want to use the sandbox package database that was created
    by cabal.
    We need its path later, so we have to find out where it is,
    for example with:

    ~~~~
    cabal exec -- sh -c "echo \$GHC_PACKAGE_PATH" | sed 's/:.*//'
    ~~~~

    the result should be something like

    ~~~~
    ~/MyHaskellCode/cabal/Cabal/.cabal-sandbox/$SOMESTUFF-packages.conf.d
    ~~~~
    
    (or, as a relative path with my setup:)

    ~~~~
    .cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d
    ~~~~

    We will refer to this as `PACKAGEDB`.

5. Configure and build Cabal, and run all tests:

    ~~~~
    ./Setup configure --enable-tests --package-db=$PACKAGEDB
    ./Setup build
    ./Setup test
    ~~~~

The steps for building and testing the `cabal-install` executable are almost
identical; only the first two steps are different:

1. Move to the `cabal-install` directory:

    ~~~~
    cd cabal-install
    ~~~~

2. Create a sandbox, and fill it with the necessary dependencies.
    For this, we need to add the Cabal library from the repository as an
    add-source dependency:

    ~~~~
    cabal sandbox init
    cabal sandbox add-source ../Cabal/
    cabal install --only-dependencies --enable-tests
    ~~~~

(In addition, the absolute sandbox path will be slightly different
because we have to use the `cabal-install` sandbox, not the Cabal one. If you
use the relative path, you are set.)

Dependencies policy
-------------------

Cabal's policy is to support being built by versions of GHC that are up
to 3 years old.

The Cabal library must be buildable out-of-the-box, i.e., the
dependency versions required by Cabal must have shipped with GHC for
at least 3 years. Cabal may use newer libraries if they are available,
as long as there is a suitable fallback when only older versions
exist.

cabal-install must be buildable by versions of GHC that are up to 3
years old. It need not be buildable out-of-the-box, so cabal-install
may depend on newer versions of libraries if they can still be
compiled by 3-year-old versions of GHC.
