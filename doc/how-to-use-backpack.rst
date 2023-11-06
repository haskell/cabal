.. _Backpack:

How to use Backpack modules
===========================

Cabal and GHC jointly support Backpack, an extension to Haskell's module
system which makes it possible to parametrize a package over some
modules, which can be instantiated later arbitrarily by a user.  This
means you can write a library to be agnostic over some data
representation, and then instantiate it several times with different
data representations.  Like C++ templates, instantiated packages are
recompiled for each instantiation, which means you do not pay any
runtime cost for parametrizing packages in this way.  Backpack modules
are somewhat experimental; while fully supported by cabal-install, they are currently
`not supported by Stack <https://github.com/commercialhaskell/stack/issues/2540>`__.

A Backpack package is defined by use of the
:pkg-field:`library:signatures` field, or by (transitive) dependency on
a package that defines some requirements.  To define a parametrized
package, define a signature file (file extension ``hsig``) that
specifies the signature of the module you want to parametrize over, and
add it to your Cabal file in the :pkg-field:`library:signatures` field.

.. code-block:: haskell
    :caption: .hsig

    signature Str where

    data Str

    concat :: [Str] -> Str

.. code-block:: cabal
    :caption: parametrized.cabal

    cabal-version: 2.2
    name: parametrized

    library
      build-depends: base
      signatures: Str
      exposed-modules: MyModule

You can define any number of regular modules (e.g., ``MyModule``) that
import signatures and use them as regular modules.

If you are familiar with ML modules, you might now expect there to be
some way to apply the parametrized package with an implementation of
the ``Str`` module to get a concrete instantiation of the package.
Backpack operates slightly differently with a concept of *mix-in
linking*, where you provide an implementation of ``Str`` simply by
bringing another module into scope with the same name as the
requirement.  For example, if you had a package ``str-impl`` that provided a
module named ``Str``, instantiating ``parametrized`` is as simple as
just depending on both ``str-impl`` and ``parametrized``:

.. code-block:: cabal
    :caption: combined.cabal

    cabal-version: 2.2
    name: combined

    library
      build-depends: base, str-impl, parametrized

Note that due to technical limitations, you cannot directly define
``Str`` in the ``combined`` library; it must be placed in its own
library (you can use :ref:`Sublibraries <sublibs>` to conveniently
define a sub-library).

However, a more common situation is that your names don't match up
exactly.  The :pkg-field:`library:mixins` field can be used to rename
signatures and modules to line up names as necessary.  If you have
a requirement ``Str`` and an implementation ``Data.Text``, you can
line up the names in one of two ways:

* Rename the requirement to match the implementation:
  ``mixins: parametrized requires (Str as Data.Text)``
* Rename the implementation to match the requirement:
  ``mixins: text (Data.Text as Str)``

The :pkg-field:`library:mixins` field can also be used to disambiguate
between multiple instantiations of the same package; for each
instantiation of the package, give it a separate entry in mixins with
the requirements and provided modules renamed to be distinct.

.. code-block:: cabal
    :caption: .cabal

    cabal-version: 2.2
    name: double-combined

    library
      build-depends: base, text, bytestring, parametrized
      mixins:
        parametrized (MyModule as MyModule.Text) requires (Str as Data.Text),
        parametrized (MyModule as MyModule.BS) requires (Str as Data.ByteString)

Intensive use of Backpack sometimes involves creating lots of small
parametrized libraries; :ref:`Sublibraries <sublibs>` can be used
to define all of these libraries in a single package without having to
create many separate Cabal packages.  You may also find it useful to use
:pkg-field:`library:reexported-modules` to reexport instantiated
libraries to Backpack-unware users (e.g., Backpack can be used entirely
as an implementation detail.)

Backpack imposes a limitation on Template Haskell that goes beyond the usual TH
stage restriction: it's not possible to splice TH code imported from a
compilation unit that is still "indefinite", that is, a unit for which some
module signatures still haven't been matched with implementations. The reason
is that indefinite units are typechecked, but not compiled, so there's no
actual TH code to run while splicing. Splicing TH code from a definite
compilation unit into an indefinite one works normally.

For more information about Backpack, check out the
`GHC wiki page <https://gitlab.haskell.org/ghc/ghc/-/wikis/backpack>`__.

