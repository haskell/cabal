# Cabal

[![Hackage version](https://img.shields.io/hackage/v/Cabal.svg?label=Hackage)](https://hackage.haskell.org/package/Cabal)
[![Stackage version](https://www.stackage.org/package/Cabal/badge/lts?label=Stackage)](https://www.stackage.org/package/Cabal)
[![Documentation Status](http://readthedocs.org/projects/cabal/badge/?version=latest)](http://cabal.readthedocs.io/en/latest/?badge=latest)
[![IRC chat](https://img.shields.io/badge/chat-via%20libera-brightgreen.svg)](https://web.libera.chat/#hackage)
[![Matrix chat](https://img.shields.io/badge/chat-via%20matrix-brightgreen.svg)](https://matrix.to/#/#hackage:libera.chat)

<img src="https://www.haskell.org/cabal/images/Cabal-light.png" align="right">

This Cabal Git repository contains the following packages:

 * [Cabal](Cabal/README.md): the Cabal library package ([license](Cabal/LICENSE))
 * [Cabal-syntax](Cabal-syntax/README.md): the `.cabal` file format library ([license](Cabal-syntax/LICENSE))
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

```
cabal install cabal-install
```

To get the latest version of `cabal-install`. (You may want to `cabal update` first.)

To install the latest version from the Git repository, clone the
Git repository and then run:

```
cabal install --project-file=cabal.project.release cabal-install
```

Installing cabal-install without cabal-install
----------------------------------------------

Assuming you don't have a pre-existing copy of `cabal-install`,
look into [`bootstrap`](bootstrap) directory.
