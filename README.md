# Cabal

[![Hackage version](https://img.shields.io/hackage/v/Cabal.svg?label=Hackage)](https://hackage.haskell.org/package/Cabal)
[![Stackage version](https://www.stackage.org/package/Cabal/badge/lts?label=Stackage)](https://www.stackage.org/package/Cabal)
[![Documentation Status](http://readthedocs.org/projects/cabal/badge/?version=latest)](http://cabal.readthedocs.io/en/latest/?badge=latest)
[![IRC chat](https://img.shields.io/badge/chat-via%20libera-brightgreen.svg)](https://web.libera.chat/#hackage)
[![Matrix chat](https://img.shields.io/badge/chat-via%20matrix-brightgreen.svg)](https://matrix.to/#/#hackage:matrix.org)
[![GitLab pipeline status](https://gitlab.haskell.org/haskell/cabal/badges/cabal-head/pipeline.svg?key_text=Release%20CI%20Early%20Warning&key_width=150)](https://gitlab.haskell.org/haskell/cabal/-/commits/cabal-head)

<img src="https://www.haskell.org/cabal/images/Cabal-light.png" align="right">

This Cabal Git repository contains the following main packages:

 * [Cabal](Cabal/README.md): the Cabal library package ([license](Cabal/LICENSE))
 * [Cabal-syntax](Cabal-syntax/README.md): the `.cabal` file format library ([license](Cabal-syntax/LICENSE))
 * [cabal-install](cabal-install/README.md): the package containing the `cabal` tool ([license](cabal-install/LICENSE))
 * [cabal-install-solver](cabal-install-solver): the package containing the solver component of the `cabal` tool ([license](cabal-install-solver/LICENSE))

The canonical upstream repository is located at
https://github.com/haskell/cabal.

Ways to get the `cabal-install` binary
--------------------------------

1. _GHCup_ (**preferred**): get GHCup using [the directions on its website](https://www.haskell.org/ghcup/) and run:

    ```
    ghcup install --set cabal latest
    ```

2. _[Download from official website](https://www.haskell.org/cabal/download.html)_:
    the `cabal-install` binary download for your platform should contain the `cabal` executable.

#### Preview Releases

_Getting unreleased versions of `cabal-install`_: gives you a chance to try out yet-unreleased features.
Currently, we only provide binaries for `x86_64` platforms.

1. _GitHub preview release built from the tip of the `master` branch_: [download from GitHub](https://github.com/haskell/cabal/releases/tag/cabal-head) or use this GHCup command to install:

    ```
    ghcup install cabal -u https://github.com/haskell/cabal/releases/download/cabal-head/cabal-head-Linux-x86_64.tar.gz head
    ```

    Replace "Linux" with "Windows" or "macOS" as appropriate.

    The default Linux build is dynamically linked against `zlib`, `gmp` and `glibc`.
    You will need to have appropriate versions of these libraries installed to use it.
    Alternatively a statically linked "Linux-static" binary is also provided.

    You might need to add the following to your `cabal.project` file
    if your build fails because of an out-of-date `Cabal` library:
    ```
    allow-newer:
      *:Cabal,
      *:Cabal-syntax

    source-repository-package
        type: git
        location: https://github.com/haskell/cabal.git
        subdir: Cabal Cabal-syntax
    ```


2. Even more cutting-edge binaries built from pull requests are always available
   from the `Validate` worklow page on GitHub, at the very bottom of the page,
   or from the `build-alpine` workflow for statically linked Linux builds.

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

Learn how to use `cabal` and get support
----------------------------------------

`cabal` comes with a thorough [User Manual](https://cabal.readthedocs.io).
If you are new to `cabal` and want to quickly learn the basics, check
[Getting Started With Haskell and Cabal](https://cabal.readthedocs.io/en/latest/getting-started.html).

Got questions? Ask in [Haskell Matrix](https://matrix.to/#/#haskell:matrix.org)
(online chat) or [Haskell Discourse](https://discourse.haskell.org).

Build for hacking and contributing to cabal
-------------------------------------------

Refer to [CONTRIBUTING.md](CONTRIBUTING.md).
