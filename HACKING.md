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

Dependencies policy
-------------------

Cabal's policy is to support being built by versions of GHC that are up
to 3 years old.

The development branch of the Cabal library must not depend on any library,
or any version of any library, outside those that ship with GHC HEAD. All
dependencies must be buildable with versions of GHC up to 3 years old
(see above), but they need not ship with older versions of GHC.
