# Cabal [![Hackage version](https://img.shields.io/hackage/v/Cabal.svg?label=Hackage)](https://hackage.haskell.org/package/Cabal) [![Stackage version](https://www.stackage.org/package/Cabal/badge/lts?label=Stackage)](https://www.stackage.org/package/Cabal) [![Build Status](https://secure.travis-ci.org/haskell/cabal.svg?branch=master)](http://travis-ci.org/haskell/cabal) [![Windows build status](https://ci.appveyor.com/api/projects/status/github/haskell/cabal?branch=master&svg=true)](https://ci.appveyor.com/project/23Skidoo/cabal) [![Documentation Status](http://readthedocs.org/projects/cabal/badge/?version=latest)](http://cabal.readthedocs.io/en/latest/?badge=latest)

<img src="https://www.haskell.org/cabal/images/Cabal-light.png" align="right">

This Cabal Git repository contains the following packages:

 * [Cabal](Cabal/README.md): the Cabal library package ([license](Cabal/LICENSE))
 * [cabal-install](cabal-install/README.md): the package containing the `cabal` tool ([license](cabal-install/LICENSE))

The canonical upstream repository is located at
https://github.com/haskell/cabal.

Installing Cabal (by downloading the binary)
--------------------------------------------

Prebuilt binary releases can be obtained from https://www.haskell.org/cabal/download.html.
The `cabal-install` binary download for your platform should contain the `cabal` executable.

Installing Cabal (with cabal)
-----------------------------

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

Installing Cabal (without cabal)
--------------------------------

Assuming you don't have a pre-existing copy of `cabal-install`, run:

~~~~
cabal-install $ ./bootstrap.sh # running ./bootstrap.sh from within in cabal-install folder.
~~~~

For more details, and non-unix like systems, see the [README.md in cabal-install](cabal-install/README.md) and [Contributing Guidelines](CONTRIBUTING.md).
