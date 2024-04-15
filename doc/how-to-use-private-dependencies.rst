.. _Private dependencies:

Private dependencies
====================

Historically, Cabal has enforced the restriction that a library must only link
against one version of each package it depends on. This ensures that all of the
dependencies in the build plan work together. In your application you use
different libraries together, so it's of paramount importance that they all
agree on what ``Text`` means or what a ``ByteString`` is.

However, sometimes it's desirable to allow multiple versions of the same
library into a build plan. In this case, it's desirable to allow a library
author to specify a **private** dependency with the promise that its existence
will not leak from the interface of the library which uses it.

The two main use cases of private dependencies are:

* Writing benchmarks and testsuites for your library which test new versions of
  your library against old versions.
* Writing libraries which can communicate with processes built against
  a range of different library versions (such as ``cabal`-install` calling `./`Setup``).

Private dependencies are a new feature in ``Cabal`` and ``cabal-install`` which
allows the Cabal solver to pick versions for private dependencies independently
from the package versions chosen for public dependencies, enabling the scenarios above.

Private dependencies are an advanced feature which should only be used if you
know what you are doing and need a build plan which involves linking a library against
multiple versions of the same library.

The examples used in this blog post are available in its "complete" form in
this `github repository <https://github.com/well-typed/private-dependencies-examples>`__.

Using private dependencies
--------------------------

To explore the syntax and properties of private dependencies, consider a
testsuite which will have a private dependency on ``text-1.2`` and a
separate private dependency on ``text-2``:

::

        ┌──────────┐
      ┌─┤bench-text├─┐
      │ └──────────┘ │
      │              │
   ┌──▼───┐     ┌────▼─┐
   │text-1│     │text-2│
   └──────┘     └──────┘

This benchmark/testsuite will be able to run two different versions of
the same function from the package in the same executable, making it
much easier to e.g. detect performance regressions or improvements
across versions.

A private dependency can be introduced in a cabal file in the
``private-build-depends`` field. The specification starts with the name
of the private dependency *scope* and then contains a list of normal
dependency specifications which dictate the packages included in that
private scope. Our example needs two private scopes, each with a
specific version of text:

::

   private-build-depends: TEXT1 with (text == 1.2.*), TEXT2 with (text == 2.*)

The package **goals in a private scope are solved independently of all
other scopes**. In this example, the ``TEXT1`` scope can choose a
version of ``text`` in the ``1.2.x`` range and the ``TEXT2`` scope can
choose a version of ``text`` in the ``2.*`` range.

However, **private scopes do not apply transitively**, so the
dependencies of ``text`` will be solved in the normal top-level scope.
If your program contains a value of type ``Bool`` from the ``base``
package, which ``text`` also depends on, only if the scopes are not
applied transitively can the same ``Bool`` value can be passed to
functions from the ``TEXT1`` scope and ``TEXT2`` scope. Visually, the
“complete” dependency graph would look like:

::

        ┌──────────┐
      ┌─┤bench-text├─┐
      │ └──────────┘ │
      │              │
   ┌──▼───┐     ┌────▼─┐
   │text-1│     │text-2│
   └──┬───┘     └───┬──┘
      │             │
      │   ┌──────┐  │
      └───►base-4◄──┘
          └──────┘

Dependencies introduced privately can be imported into modules in the
project by prefixing the name of the private scope to an exposed module
name:

::

   import qualified TEXT1.Data.Text as T1
   import qualified TEXT2.Data.Text as T2

Now, obviously, ``T1.Text`` and ``T2.Text`` are distinct types, and it’s
the programmer’s responsibility to ensure that their program does not
mix them together. The typechecker will tell you if you attempt to use
incompatible types with each other.

Further to this, the private dependencies contract states that you are
**not allowed to expose anything from a private scope in the public API
of your library**. Again, it’s the programmer’s responsibility to ensure
that their public API doesn’t expose any of these types. This isn’t
checked by the compiler; if you don’t ensure the public API doesn’t
expose types imported from a private scope, your package is going to be
broken.

Once the types are imported, they are used just like any other qualified
imports. In a simple test, we check that ``text-2.*`` and ``text-1.2.*``
agree in the length of ``"abc"``:

::

   main = print $ T1.length (T1.pack "abc") == T2.length (T2.pack "abc")

Closure of Private Dependencies
-------------------------------

A private dependency scope may contain multiple packages, and **packages
in the same private scope are solved together**. That is, you have to
pick one version per package in the scope that satisfies all constraints
of all packages in that scope. For instance, having
``private-build-depends: S0 with (pkg-a == 0.1, pkg-b == 0.2)`` will
fail if ``pkg-a`` has ``build-depends: pkg-b == 0-1`` in its cabal file.

Specifying two (or more) packages in the same scope can be particularly
useful if these two packages are tightly coupled and you need each to
use a version compatible with the other, but still want them to be
solved independently of the top-level scope.

A crucial property must hold true of the packages in a private scope:
**the transitive closure of the packages in the private scope must be
closed**. A scope is **closed** if, whenever we have a dependency chain
``P1 -> Q -> P2``, in which ``P1`` and ``P2`` are both in a given
private scope ``S``, then ``Q`` also belongs to the private scope ``S``.
The Cabal solver checks this property and guarantees no scope violates
this property, however, it doesn’t implicit add packages into a private
scope, so you may need to add packages violating the closure property
manually to the scope.

Apartness of Private Dependencies
---------------------------------

A library using private dependencies must **assume that a privately
scoped dependency is apart from the top-level scope and from other
private scopes**.

By coincidence (and heuristics), the solver might choose the same
version of a package to satisfy different private scopes. Conversely, it
might also not choose the same version. Therefore, in order to robustly
write a program using private dependencies, the programmer must assume
that modules in separate scopes are incompatible with other modules of
the same name, even if those modules come from the same package.

Benchmarking using Private Dependencies
---------------------------------------

Using private dependencies, it is straightforward to write a benchmark
suite which tests different versions of the same library against each
other.

Consider the task of benchmarking ``text-1.2.*`` vs ``text-2.*`` again:
private dependencies make it possible to benchmark the two versions
directly against each other in the same benchmark run.

Working through a complete benchmarking example, let’s introduce two new
private scopes to encapsulate the different ``text`` versions which we
want to test:

::

   benchmark testing-text-benchmarks
       import:           warnings
       default-language: Haskell2010
       type:             exitcode-stdio-1.0
       hs-source-dirs:   bench/
       main-is:          Main.hs
       build-depends:    base ^>=4.18.0.0, tasty-bench, tasty, deepseq
       private-build-depends: TEXT1 with (text == 1.2.*), TEXT2 with (text == 2.*)

The different versions of ``text`` are made available with the
respective prefixes in ``Main.hs``:

::

   import qualified TEXT1.Data.Text as TEXT1
   import qualified TEXT1.Data.Text.IO as TEXT1


   import qualified TEXT2.Data.Text as TEXT2
   import qualified TEXT2.Data.Text.IO as TEXT2

The ``InputEnv`` type allows us to wrap ``TEXT1.Text`` and
``TEXT2.Text`` to provide an ``NFData`` instance for types which we just
have to force to WHNF to be sure of having evaluated fully:

::

   data InputEnv a = InputEnv { lorem :: !a }

   instance NFData (InputEnv a) where
     rnf InputEnv{} = ()

   type T1Env = InputEnv TEXT1.Text
   type T2Env = InputEnv TEXT2.Text

We declare a helper function (``mkBench``) which takes the two prepared
``Text`` values and functions from the relevant library versions, and
proceed with benchmarking as is typically done for other benchmarks
regardless of private scopes:

::

   mkBench :: T1Env -> T2Env -> (TEXT1.Text -> a) -> (TEXT2.Text -> b) -> [Benchmark]
   mkBench t1env t2env t1 t2 =
     [ bench "t1" $ whnf t1 (lorem t1env)
     , bench "t2" $ whnf t2 (lorem t2env) ]

   main =
     defaultMain . (:[]) $
       env (InputEnv <$> TEXT1.readFile "lorem.txt") $ \t1env ->
       env (InputEnv <$> TEXT2.readFile "lorem.txt") $ \t2env ->
         let mk_bench = mkBench t1env t2env
         in bgroup "text"
             [ bgroup "Reverse" (mk_bench TEXT1.reverse TEXT2.reverse)
             , bgroup "Length"  (mk_bench TEXT1.length TEXT2.length)
             ]

Running the benchmarks we can see that ``text-2.*`` performs much better
than ``text-1.2``, at least in the case of reversing and length.

