**tl;dr** With the soon-to-be released cabal-install 1.24, try ``cabal
new-build`` instead of sandboxes or Stack; you'll be pleasantly
surprised.

``cabal new-build``, also known as “Nix-style local builds”, is a new
command that will be tech-previewed with cabal-install 1.24. Nix-style
local builds combine the best of non-sandboxed and sandboxed Cabal:

1. Like sandboxed Cabal today, we build sets of independent local
   packages deterministically and independent of any global state.
   new-build will never tell you that it can't build your package
   because it would result in a “dangerous reinstall.”  Given a
   particular state of the Hackage index, your build is completely
   reproducible.

2. Like non-sandboxed Cabal today, builds of external packages are
   cached globally, so that a package can be built once, and then reused
   anywhere else it is also used.  No need to continually rebuild
   dependencies whenever you make a new sandbox: dependencies which can
   be shared, are shared.

I’ve been using Nix-style local builds exclusively to do my Haskell
development, and it's hard to understate my enthusiasm for this new
feature.  It achieves much of the same user experience as Stack, but
without positing the existence of a distribution of blessed,
version-pegged packages to build against (e.g., Stackage). There is
still lots of polish needed (it's missing commands and no user interface
for freezing dependency solver results), but it is working well enough
to be useful for early adopters.

How to use it
-------------

As of writing, cabal-install 1.24 has not yet been uploaded to Hackage,
but you can download a `prerelease off of GitHub.
<https://github.com/haskell/cabal/tree/1.24>`_ You should install Cabal
and cabal-install in the usual way (e.g., ``(cd Cabal; cabal install);
(cd cabal-install; cabal install``).

To use ``cabal new-build`` on a single Cabal file, no configuration is
necessary: in the same directory as the Cabal file, just run ``cabal
new-build`` (no configure necessary) and your project will start
building.  ``cabal new-build`` takes the same arguments as ``cabal
install``.

To build a multiple package project, in your root directory create a
``cabal.project`` file, as so::

  packages: Cabal/Cabal.cabal
            cabal-install/cabal-install.cabal

In the ``packages`` field, list the Cabal file of every package that you
would like to build locally.  This field supports abbreviations: you can
glob using ``*``, and the use of braces to denote alternatives is also
supported.  So a compact version of the same ``cabal.project`` file is::

  packages: {Cabal,cabal-install}

By default, ``cabal new-build`` builds every component in the package
that is your current working directory.  If there is no package in your
current working directory (e.g., you're in the root of the folder
containing your packages), ``cabal new-build`` builds nothing. You can
instead specify what packages you would like to be built as arguments.
For example, ``cabal new-build Cabal:package-tests exe:cabal
--enable-tests`` requests two components be built: the component
``package-tests`` from the package ``Cabal``, and the executable named
``cabal`` (which happens to be from the package ``cabal-install``).

Known issues
------------

As a tech preview, the code is still `a little rough around the edges
<https://github.com/haskell/cabal/labels/nix-local-build>`_. Here are
some more major issues you might run into:

* Although dependency resolution is deterministic, if you update your
  Hackage index with ``cabal update``, `dependency resolution will
  change too <https://github.com/haskell/cabal/issues/2996>`_. ``cabal
  freeze`` does not currently work, so you'll have to manually construct
  the set of desired constraints.

* A new feature of new-build is that it avoids rebuilding packages when
  there have been no changes to them, by tracking the hashes of their
  contents.  However, this dependency tracking is not 100% accurate
  (specifically, it relies on your Cabal file accurately reporting all
  file dependencies ala ``sdist``, and it doesn't know about search
  paths).  There's currently no UI for forcing a package to be
  recompiled; however you can induce a recompilation fairly easily by
  removing an appropriate cache file: specifically, for the package
  named ``p-1.0``, delete the file
  ``dist-newstyle/build/p-1.0/cache/build``.
