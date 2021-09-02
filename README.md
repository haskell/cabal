# Cabal [![Hackage version](https://img.shields.io/hackage/v/Cabal.svg?label=Hackage)](https://hackage.haskell.org/package/Cabal) [![Stackage version](https://www.stackage.org/package/Cabal/badge/lts?label=Stackage)](https://www.stackage.org/package/Cabal) [![Artifacts](https://github.com/haskell/cabal/actions/workflows/artifacts.yml/badge.svg?branch=master)](https://github.com/haskell/cabal/actions/workflows/artifacts.yml?query=branch%3Amaster) [![Documentation Status](http://readthedocs.org/projects/cabal/badge/?version=latest)](http://cabal.readthedocs.io/en/latest/?badge=latest)

<img src="https://www.haskell.org/cabal/images/Cabal-light.png" align="right">

This Cabal Git repository contains the following packages:

 * [Cabal](Cabal/README.md): the Cabal library package ([license](Cabal/LICENSE))
 * [cabal-install](cabal-install/README.md): the package containing the `cabal` tool ([license](cabal-install/LICENSE))

The canonical upstream repository is located at
https://github.com/haskell/cabal.

Installing cabal-install (by downloading the binary)
----------------------------------------------------

Prebuilt binary releases can be obtained from https://www.haskell.org/cabal/download.html.
The `cabal-install` binary download for your platform should contain the `cabal` executable.

Installing cabal-install (preferred, with cabal-install)
--------------------------------------------------------

Assuming that you have a pre-existing, recent version of `cabal-install`, run:

~~~~
cabal install cabal-install
~~~~

To get the latest version of `cabal-install`. (You may want to `cabal update` first.)

To install the latest version from the Git repository, clone the
Git repository and then run:

~~~~
cabal install --project-file=cabal.project.release cabal-install
~~~~

Installing cabal-install without cabal-install
----------------------------------------------

Assuming you don't have a pre-existing copy of `cabal-install`,
look into `bootstrap` directory.
