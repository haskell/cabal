.. highlight:: console

Quickstart
==========

Suppose that you are in a directory containing a single Cabal package
which you wish to build (if you haven't set up a package yet check
out :doc:`How to package Haskell code <how-to-package-haskell-code>` for
instructions). You can configure and build it using Nix-style
local builds with this command (configuring is not necessary):

::

    $ cabal build

To open a GHCi shell with this package, use this command:

::

    $ cabal repl

To run an executable defined in this package, use this command:

::

    $ cabal run <executable name> [executable args]

Developing multiple packages
----------------------------

Many Cabal projects involve multiple packages which need to be built
together. To build multiple Cabal packages, you need to first create a
``cabal.project`` file which declares where all the local package
directories live. For example, in the Cabal repository, there is a root
directory with a folder per package, e.g., the folders ``Cabal`` and
``cabal-install``. The ``cabal.project`` file specifies each folder as
part of the project:

.. code-block:: cabal

    packages: Cabal/
              cabal-install/

The expectation is that a ``cabal.project`` is checked into your source
control, to be used by all developers of a project. If you need to make
local changes, they can be placed in ``cabal.project.local`` (which
should not be checked in.)

Then, to build every component of every package, from the top-level
directory, run the command: (using cabal-install-2.0 or greater.)

::

    $ cabal build all

To build a specific package, you can either run ``build`` from the
directory of the package in question:

::

    $ cd cabal-install
    $ cabal build

or you can pass the name of the package as an argument to
``cabal build`` (this works in any subdirectory of the project):

::

    $ cabal build cabal-install

You can also specify a specific component of the package to build. For
example, to build a test suite named ``package-tests``, use the command:

::

    $ cabal build package-tests

Targets can be qualified with package names. So to request
``package-tests`` *from* the ``Cabal`` package, use
``Cabal-tests:package-tests``.

Unlike sandboxes, there is no need to setup a sandbox or ``add-source``
projects; just check in ``cabal.project`` to your repository and
``build`` will just work.

Cookbook
========

How can I profile my library/application?
-----------------------------------------

Create or edit your ``cabal.project.local``, adding the following
line::

    profiling: True

Now, ``cabal build`` will automatically build all libraries and
executables with profiling.  You can fine-tune the profiling settings
for each package using :cfg-field:`profiling-detail`::

    package p
        profiling-detail: toplevel-functions

Alternately, you can call ``cabal build --enable-profiling`` to
temporarily build with profiling.

How can I have a reproducible set of versions for my dependencies?
------------------------------------------------------------------

You can use ``cabal freeze`` to save the solver results to a file.

Since Cabal 3.8, an alternative approach is to use a :ref:`remote project
configuration file<conditionals and imports>`: to specify a set of versions for
packages.

One provider of such package sets is Stackage_, and its package sets are called
snapshots. The Stackage snapshots contain a set of packages from Hackage that
have all been verified to build with a given version of GHC.

For example, the snapshot named lts-19.2 contains versioned packages which all
compile on GHC 9.0.2. You can conveniently review the `versions of packages in
lts-19.2`_. Using the following ``cabal.project`` file, Cabal will use the
versions of packages that the this snapshot specifies:

::

    packages: .
    import: https://www.stackage.org/lts-19.2/cabal.config

Please note that project files do not get bundled in Cabal package tarballs,
made using e.g. ``cabal sdist``. Project files are intended for use in local
development environments.

.. _Stackage: https://stackage.org/
.. _versions of packages in lts-19.2: https://www.stackage.org/lts-19.2

How it works
============

Local versus external packages
------------------------------

One of the primary innovations of Nix-style local builds is the
distinction between local packages, which users edit and recompile and
must be built per-project, versus external packages, which can be cached
across projects. To be more precise:

1. A **local package** is one that is listed explicitly in the
   ``packages``, ``optional-packages`` or ``extra-packages`` fields of a
   project. Packages in the former two fields will usually have their
   source code stored in a folder in your project, while ``extra-packages`` lists
   packages residing on Hackage that are treated as being local anyway.

Local packages, as well as the external packages (below) which depend on
them, are built **inplace**, meaning that they are always built
specifically for the project and are not installed globally. Inplace
packages are not cached and not given unique hashes, which makes them
suitable for packages which you want to edit and recompile.

2. An **external package** is any package which is not listed in the
   ``packages``, ``optional-packages`` and ``extra-packages`` fields.
   The source code for external packages is usually retrieved from Hackage.

When an external package does not depend on an inplace package, it can
be built and installed to a **global** store, which can be shared across
projects. These build products are identified by a hash based on all of
the inputs which influence the compilation of a package (flags,
dependency selection, etc.). Just as in Nix, these hashes uniquely
identify the result of a build; if we compute this identifier and we
find that we already have this ID built, we can just use the already
built version.

Use ``cabal path --store-dir`` to show where your global package store is located.
This is configurable via the global ``store-dir`` option.
If you need to clear your store for
whatever reason (e.g., to reclaim disk space or because the global
store is corrupted), deleting this directory is safe (``build``
will just rebuild everything it needs on its next invocation).

This split motivates some of the UI choices for Nix-style local build
commands. For example, flags passed to ``cabal build`` are only
applied to *local* packages, so that adding a flag to
``cabal build`` doesn't necessitate a rebuild of *every* transitive
dependency in the global package store.

In cabal-install 2.0 and above, Nix-style local builds also take advantage of a
new Cabal library feature, `per-component
builds <https://github.com/ezyang/ghc-proposals/blob/master/proposals/0000-componentized-cabal.rst>`__,
where each component of a package is configured and built separately.
This can massively speed up rebuilds of packages with lots of components
(e.g., a package that defines multiple executables), as only one
executable needs to be rebuilt. Packages that use Custom setup scripts
are not currently built on a per-component basis.

Where are my build products?
----------------------------

A major deficiency in the current implementation of ``cabal build`` is that
there is no programmatic way to access the location of build products.
The location of the build products is intended to be an internal
implementation detail of ``cabal build``, but we also understand that many
unimplemented features can only be reasonably worked around by
accessing build products directly.

The location where build products can be found varies depending on the
version of cabal-install:

-  In cabal-install-1.24, the dist directory for a package ``p-0.1`` is
   stored in ``dist-newstyle/build/p-0.1``. For example, if you built an
   executable or test suite named ``pexe``, it would be located at
   ``dist-newstyle/build/p-0.1/build/pexe/pexe``.

-  In cabal-install-2.0, the dist directory for a package ``p-0.1``
   defining a library built with GHC 8.0.1 on 64-bit Linux is
   ``dist-newstyle/build/x86_64-linux/ghc-8.0.1/p-0.1``. When
   per-component builds are enabled (any non-Custom package), a
   subcomponent like an executable or test suite named ``pexe`` will be
   stored at
   ``dist-newstyle/build/x86_64-linux/ghc-8.0.1/p-0.1/c/pexe``; thus,
   the full path of the executable is
   ``dist-newstyle/build/x86_64-linux/ghc-8.0.1/p-0.1/c/pexe/build/pexe/pexe``
   (you can see why we want this to be an implementation detail!)

-  In cabal-install-2.2 and above, the ``/c/`` part of the above path
   is replaced with one of ``/l/``, ``/x/``, ``/f/``, ``/t/``, or
   ``/b/``, depending on the type of component (sublibrary,
   executable, foreign library, test suite, or benchmark
   respectively). So the full path to an executable named ``pexe``
   compiled with GHC 8.0.1 on a 64-bit Linux is now
   ``dist-newstyle/build/x86_64-linux/ghc-8.0.1/p-0.1/x/pexe/build/pexe/pexe``;
   for a benchmark named ``pbench`` it now is
   ``dist-newstyle/build/x86_64-linux/ghc-8.0.1/p-0.1/b/pbench/build/pbench/pbench``;


The paths are a bit longer in 2.0 and above but the benefit is that you can
transparently have multiple builds with different versions of GHC. We
plan to add the ability to create aliases for certain build
configurations, and more convenient paths to access particularly useful
build products like executables.

Caching
-------

Nix-style local builds support a robust caching system which helps to reduce
the time it takes to execute a rebuild cycle. While the details of how
``cabal-install`` does caching are an implementation detail and may
change in the future, knowing what gets cached is helpful for
understanding the performance characteristics of invocations to
``build``. The cached intermediate results are stored in
``dist-newstyle/cache``; this folder can be safely deleted to clear the
cache.

The following intermediate results are cached in the following files in
this folder (the most important two are first):

``solver-plan`` (binary)
    The result of calling the dependency solver, assuming that the
    Hackage index, local ``cabal.project`` file, and local ``cabal``
    files are unmodified. (Notably, we do NOT have to dependency solve
    again if new build products are stored in the global store; the
    invocation of the dependency solver is independent of what is
    already available in the store.)
``source-hashes`` (binary)
    The hashes of all local source files. When all local source files of
    a local package are unchanged, ``cabal build`` will skip
    invoking ``setup build`` entirely (saving us from a possibly
    expensive call to ``ghc --make``). The full list of source files
    participating in compilation is determined using
    ``cabal sdist --list-only``. Thus if you do not list all your
    source files in a Cabal file, Cabal may fail to recompile when you
    edit them.
``config`` (binary)
    The full project configuration, merged from ``cabal.project`` (and
    friends) as well as the command line arguments.
``compiler`` (binary)
    The configuration of the compiler being used to build the project.
``improved-plan`` (binary)
    Like ``solver-plan``, but with all non-inplace packages improved
    into pre-existing copies from the store.
``plan.json`` (JSON)
    A JSON serialization of the computed install plan intended
    for integrating ``cabal`` with external tooling.
    The `cabal-plan <http://hackage.haskell.org/package/cabal-plan>`__
    package provides a library for parsing ``plan.json`` files into a
    Haskell data structure as well as an example tool showing possible
    applications.

    .. todo::

        Document JSON schema (including version history of schema)


Note that every package also has a local cache managed by the Cabal
build system, e.g., in ``$distdir/cache``.
