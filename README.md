# Cabal

[![Hackage version](https://img.shields.io/hackage/v/Cabal.svg?label=Hackage)](https://hackage.haskell.org/package/Cabal)
[![Stackage version](https://www.stackage.org/package/Cabal/badge/lts?label=Stackage)](https://www.stackage.org/package/Cabal)
[![Documentation Status](http://readthedocs.org/projects/cabal/badge/?version=latest)](http://cabal.readthedocs.io/en/latest/?badge=latest)
[![IRC chat](https://img.shields.io/badge/chat-via%20libera-brightgreen.svg)](https://web.libera.chat/#hackage)
[![Matrix chat](https://img.shields.io/badge/chat-via%20matrix-brightgreen.svg)](https://matrix.to/#/#hackage:libera.chat)

<img src="https://www.haskell.org/cabal/images/Cabal-light.png" align="right">

This Cabal Git repository contains the following main packages:

 * [Cabal](Cabal/README.md): the Cabal library package ([license](Cabal/LICENSE))
 * [Cabal-syntax](Cabal-syntax/README.md): the `.cabal` file format library ([license](Cabal-syntax/LICENSE))
 * [cabal-install](cabal-install/README.md): the package containing the `cabal` tool ([license](cabal-install/LICENSE))
 * [cabal-install-solver](cabal-install-solve): the package containing the solver component of the `cabal` tool ([license](cabal-install-solver/LICENSE))

The canonical upstream repository is located at
https://github.com/haskell/cabal.

Ways to get the `cabal-install` binary
--------------------------------

1. _GHCup (**preferred**)_: get GHCup using [the directions on its website](https://www.haskell.org/ghcup/) and run:

    ```
    ghcup install --set cabal latest
    ```

2. _[Download from official website](https://www.haskell.org/cabal/download.html)_:
    the `cabal-install` binary download for your platform should contain the `cabal` executable.

Ways to build `cabal-install` for everyday use
--------------------------------------------

1. _With cabal-install_:
    if you have a pre-existing version of `cabal-install`, run:

    ```
    cabal install cabal-install
    ```

    to get the latest version of `cabal-install`. (You may want to `cabal update` first.)

2. _From Git_:
    again with a pre-existing version of `cabal-install`,
    you can install the latest version from the Git repository. Clone the
    Git repository, move to its root, and run:

    ```
    cabal install --project-file=cabal.project.release cabal-install
    ```

3. _Bootstrapping_:
    if you don't have a pre-existing version of `cabal-install`,
    look into the [`bootstrap`](bootstrap) directory.

Build for hacking and contributing to cabal
-------------------------------------------

Refer to [CONTRIBUTING.md](CONTRIBUTING.md).
