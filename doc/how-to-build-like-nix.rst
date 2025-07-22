.. _nix-style-builds:

How to build locally like in Nix
================================

Nix-style local builds are a new build system implementation inspired by Nix.
The Nix-style local build system is commonly called "v2-build" for short
after the ``cabal v2-*`` family of commands that control it. However, those
names are only temporary now that Nix-style local builds have become the
default. For those who do not wish to use the new
functionality, the classic project style will not be removed immediately,
but these legacy commands will require the usage of the ``v1-`` prefix as of
Cabal 3.0 and will be removed in a future release. For a future-proof
way to use these commands in a script or tutorial that anticipates the
possibility of another UI paradigm being devised in the future, there
are also ``v2-`` prefixed versions that will reference the same functionality
until such a point as it is completely removed from Cabal.

Nix-style local builds combine the best of non-sandboxed and sandboxed Cabal:

1. Like sandboxed Cabal previously, we build sets of independent local
   packages deterministically and independent of any global state.
   v2-build will never tell you that it can't build your package
   because it would result in a "dangerous reinstall." Given a
   particular state of the Hackage index, your build is completely
   reproducible. For example, you no longer need to compile packages
   with profiling ahead of time; just request profiling and v2-build
   will rebuild all its dependencies with profiling automatically.

2. Like non-sandboxed Cabal today, builds of external packages are
   cached in a global store, so that a package can be built once,
   and then reused anywhere else it is also used. No need to continually
   rebuild dependencies whenever you make a new sandbox: dependencies
   which can be shared, are shared.

Nix-style local builds were first released as beta in cabal-install 1.24.
They currently work with all versions of GHC supported by that release: GHC 7.0 and later.

Some features described in this manual are not implemented. If you need
them, please give us a shout and we'll prioritize accordingly.



.. toctree::
   nix-local-build
