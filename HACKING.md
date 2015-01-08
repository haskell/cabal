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

Building Cabal from from source requires the following:

* Glorious/Glasgow Haskell Compiler (ghc).
* An existing (relatively recent) `cabal` binary (e.g. obtained as part of the
Haskell Platform, bootstrapped from the
[source tarball on Hackage](http://hackage.haskell.org/package/cabal-install) or
installed from your Linux vendor).

Once you have these, the steps are:

1. Change into the directory where you want to stash the cabal sources, eg:
    ```
    cd ~/MyHaskellCode
    ```

2. Clone the repo and change into the `cabal-install` directory:

    ```
    git clone https://github.com/haskell/cabal.git
    cd cabal/cabal-install/
    ```

3. If you are hacking on Cabal you probaly don't want your development version
to interfere with the `cabal` executable you actually use, so we'll set up and
use a cabal sandbox:

    ```
    cabal sandbox init
    ```

4. Now add the `Cabal` library and install all the dependencies into the sandbox:

    ```
    cabal sandbox add-source ../Cabal
    cabal --enable-tests install --dependencies-only
    ```
    Since you used `add-source`, any changes to `Cabal/` will automatically be
    picked up when building inside `cabal-install/` from now on.


5. Build cabal-install and run the tests:

    ```
    cabal build
    cabal test
    ```

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
