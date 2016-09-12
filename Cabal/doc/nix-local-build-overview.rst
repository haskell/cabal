Nix-style Local Builds
======================

``cabal new-build``, also known as Nix-style local builds, is a new
command inspired by Nix that comes with cabal-install 1.24. Nix-style
local builds combine the best of non-sandboxed and sandboxed Cabal:

1. Like sandboxed Cabal today, we build sets of independent local
   packages deterministically and independent of any global state.
   new-build will never tell you that it can't build your package
   because it would result in a "dangerous reinstall." Given a
   particular state of the Hackage index, your build is completely
   reproducible. For example, you no longer need to compile packages
   with profiling ahead of time; just request profiling and new-build
   will rebuild all its dependencies with profiling automatically.

2. Like non-sandboxed Cabal today, builds of external packages are
   cached in ``~/.cabal/store``, so that a package can be built once,
   and then reused anywhere else it is also used. No need to continually
   rebuild dependencies whenever you make a new sandbox: dependencies
   which can be shared, are shared.

Nix-style local builds work with all versions of GHC supported by
cabal-install 1.24, which currently is GHC 7.0 and later.

Some features described in this manual are not implemented. If you need
them, please give us a shout and we'll prioritize accordingly.



.. toctree::
   nix-local-build
