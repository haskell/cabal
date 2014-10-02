# Building Cabal from git cloned sources and running the tests

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
