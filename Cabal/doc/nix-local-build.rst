.. highlight:: console

Quickstart
==========

Suppose that you are in a directory containing a single Cabal package
which you wish to build (if you haven't set up a package yet check
out `developing packages <developing-packages.html>`__ for
instructions). You can configure and build it using Nix-style
local builds with this command (configuring is not necessary):

::

    $ cabal v2-build

To open a GHCi shell with this package, use this command:

::

    $ cabal v2-repl

To run an executable defined in this package, use this command:

::

    $ cabal v2-run <executable name> [executable args]

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

    $ cabal v2-build

To build a specific package, you can either run ``v2-build`` from the
directory of the package in question:

::

    $ cd cabal-install
    $ cabal v2-build

or you can pass the name of the package as an argument to
``cabal v2-build`` (this works in any subdirectory of the project):

::

    $ cabal v2-build cabal-install

You can also specify a specific component of the package to build. For
example, to build a test suite named ``package-tests``, use the command:

::

    $ cabal v2-build package-tests

Targets can be qualified with package names. So to request
``package-tests`` *from* the ``Cabal`` package, use
``Cabal:package-tests``.

Unlike sandboxes, there is no need to setup a sandbox or ``add-source``
projects; just check in ``cabal.project`` to your repository and
``v2-build`` will just work.

Cookbook
========

How can I profile my library/application?
-----------------------------------------

Create or edit your ``cabal.project.local``, adding the following
line::

    profiling: True

Now, ``cabal v2-build`` will automatically build all libraries and
executables with profiling.  You can fine-tune the profiling settings
for each package using :cfg-field:`profiling-detail`::

    package p
        profiling-detail: toplevel-functions

Alternately, you can call ``cabal v2-build --enable-profiling`` to
temporarily build with profiling.

How it works
============

Local versus external packages
------------------------------

One of the primary innovations of Nix-style local builds is the
distinction between local packages, which users edit and recompile and
must be built per-project, versus external packages, which can be cached
across projects. To be more precise:

1. A **local package** is one that is listed explicitly in the
   ``packages``, ``optional-packages`` or ``extra-packages`` field of a
   project. Usually, these refer to packages whose source code lives
   directly in a folder in your project (although, you can list an
   arbitrary Hackage package in ``extra-packages`` to force it to be
   treated as local).

Local packages, as well as the external packages (below) which depend on
them, are built **inplace**, meaning that they are always built
specifically for the project and are not installed globally. Inplace
packages are not cached and not given unique hashes, which makes them
suitable for packages which you want to edit and recompile.

2. An **external package** is any package which is not listed in the
   ``packages`` field. The source code for external packages is usually
   retrieved from Hackage.

When an external package does not depend on an inplace package, it can
be built and installed to a **global** store, which can be shared across
projects. These build products are identified by a hash that over all of
the inputs which would influence the compilation of a package (flags,
dependency selection, etc.). Just as in Nix, these hashes uniquely
identify the result of a build; if we compute this identifier and we
find that we already have this ID built, we can just use the already
built version.

The global package store is ``~/.cabal/store`` (configurable via
global `store-dir` option); if you need to clear your store for
whatever reason (e.g., to reclaim disk space or because the global
store is corrupted), deleting this directory is safe (``v2-build``
will just rebuild everything it needs on its next invocation).

This split motivates some of the UI choices for Nix-style local build
commands. For example, flags passed to ``cabal v2-build`` are only
applied to *local* packages, so that adding a flag to
``cabal v2-build`` doesn't necessitate a rebuild of *every* transitive
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

A major deficiency in the current implementation of v2-build is that
there is no programmatic way to access the location of build products.
The location of the build products is intended to be an internal
implementation detail of v2-build, but we also understand that many
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

- In cabal-install-2.2 and above, the ``/c/`` part of the above path
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

Nix-style local builds sport a robust caching system which help reduce
the time it takes to execute a rebuild cycle. While the details of how
``cabal-install`` does caching are an implementation detail and may
change in the future, knowing what gets cached is helpful for
understanding the performance characteristics of invocations to
``v2-build``. The cached intermediate results are stored in
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
    a local package are unchanged, ``cabal v2-build`` will skip
    invoking ``setup build`` entirely (saving us from a possibly
    expensive call to ``ghc --make``). The full list of source files
    participating in compilation are determined using
    ``setup sdist --list-sources`` (thus, if you do not list all your
    source files in a Cabal file, you may fail to recompile when you
    edit them.)
``config`` (same format as ``cabal.project``)
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

There is another useful file in ``dist-newstyle/cache``,
``plan.json``, which is a JSON serialization of the computed install
plan and is intended for integrating with external tooling.




Commands
========

We now give an in-depth description of all the commands, describing the
arguments and flags they accept.

cabal v2-configure
-------------------

``cabal v2-configure`` takes a set of arguments and writes a
``cabal.project.local`` file based on the flags passed to this command.
``cabal v2-configure FLAGS; cabal new-build`` is roughly equivalent to
``cabal v2-build FLAGS``, except that with ``new-configure`` the flags
are persisted to all subsequent calls to ``v2-build``.

``cabal v2-configure`` is intended to be a convenient way to write out
a ``cabal.project.local`` for simple configurations; e.g.,
``cabal v2-configure -w ghc-7.8`` would ensure that all subsequent
builds with ``cabal v2-build`` are performed with the compiler
``ghc-7.8``. For more complex configuration, we recommend writing the
``cabal.project.local`` file directly (or placing it in
``cabal.project``!)

``cabal v2-configure`` inherits options from ``Cabal``. semantics:

-  Any flag accepted by ``./Setup configure``.

-  Any flag accepted by ``cabal configure`` beyond
   ``./Setup configure``, namely ``--cabal-lib-version``,
   ``--constraint``, ``--preference`` and ``--solver.``

-  Any flag accepted by ``cabal install`` beyond ``./Setup configure``.

-  Any flag accepted by ``./Setup haddock``.

The options of all of these flags apply only to *local* packages in a
project; this behavior is different than that of ``cabal install``,
which applies flags to every package that would be built. The motivation
for this is to avoid an innocuous addition to the flags of a package
resulting in a rebuild of every package in the store (which might need
to happen if a flag actually applied to every transitive dependency). To
apply options to an external package, use a ``package`` stanza in a
``cabal.project`` file.

cabal v2-update
----------------

``cabal v2-update`` updates the state of the package index. If the
project contains multiple remote package repositories it will update
the index of all of them (e.g. when using overlays).

Some examples:

::

    $ cabal v2-update                  # update all remote repos
    $ cabal v2-update head.hackage     # update only head.hackage

cabal v2-build
---------------

``cabal v2-build`` takes a set of targets and builds them. It
automatically handles building and installing any dependencies of these
targets.

A target can take any of the following forms:

-  A package target: ``package``, which specifies that all enabled
   components of a package to be built. By default, test suites and
   benchmarks are *not* enabled, unless they are explicitly requested
   (e.g., via ``--enable-tests``.)

-  A component target: ``[package:][ctype:]component``, which specifies
   a specific component (e.g., a library, executable, test suite or
   benchmark) to be built.

-  All packages: ``all``, which specifies all packages within the project.

-  Components of a particular type: ``package:ctypes``, ``all:ctypes``:
   which specifies all components of the given type. Where valid
   ``ctypes`` are:
     - ``libs``, ``libraries``,
     - ``flibs``, ``foreign-libraries``,
     - ``exes``, ``executables``,
     - ``tests``,
     - ``benches``, ``benchmarks``.

In component targets, ``package:`` and ``ctype:`` (valid component types
are ``lib``, ``flib``, ``exe``, ``test`` and ``bench``) can be used to
disambiguate when multiple packages define the same component, or the
same component name is used in a package (e.g., a package ``foo``
defines both an executable and library named ``foo``). We always prefer
interpreting a target as a package name rather than as a component name.

Some example targets:

::

    $ cabal v2-build lib:foo-pkg       # build the library named foo-pkg
    $ cabal v2-build foo-pkg:foo-tests # build foo-tests in foo-pkg

(There is also syntax for specifying module and file targets, but it
doesn't currently do anything.)

Beyond a list of targets, ``cabal v2-build`` accepts all the flags that
``cabal v2-configure`` takes. Most of these flags are only taken into
consideration when building local packages; however, some flags may
cause extra store packages to be built (for example,
``--enable-profiling`` will automatically make sure profiling libraries
for all transitive dependencies are built and installed.)

In addition ``cabal v2-build`` accepts these flags:

- ``--only-configure``: When given we will forgoe performing a full build and
  abort after running the configure phase of each target package.


cabal v2-repl
--------------

``cabal v2-repl TARGET`` loads all of the modules of the target into
GHCi as interpreted bytecode. In addition to ``cabal v2-build``'s flags,
it takes an additional ``--repl-options`` flag.

To avoid ``ghci`` specific flags from triggering unneeded global rebuilds these
flags are now stripped from the internal configuration. As a result
``--ghc-options`` will no longer (reliably) work to pass flags to ``ghci`` (or
other repls). Instead, you should use the new ``--repl-options`` flag to
specify these options to the invoked repl. (This flag also works on ``cabal
repl`` and ``Setup repl`` on sufficiently new versions of Cabal.)

Currently, it is not supported to pass multiple targets to ``v2-repl``
(``v2-repl`` will just successively open a separate GHCi session for
each target.)

It also provides a way to experiment with libraries without needing to download
them manually or to install them globally.

This command opens a REPL with the current default target loaded, and a version
of the ``vector`` package matching that specification exposed.

::

    $ cabal v2-repl --build-depends "vector >= 0.12 && < 0.13"

Both of these commands do the same thing as the above, but only exposes ``base``,
``vector``, and the ``vector`` package's transitive dependencies even if the user
is in a project context.

::

    $ cabal v2-repl --ignore-project --build-depends "vector >= 0.12 && < 0.13"
    $ cabal v2-repl --project='' --build-depends "vector >= 0.12 && < 0.13"

This command would add ``vector``, but not (for example) ``primitive``, because
it only includes the packages specified on the command line (and ``base``, which
cannot be excluded for technical reasons).

::

    $ cabal v2-repl --build-depends vector --no-transitive-deps

cabal v2-run
-------------

``cabal v2-run [TARGET [ARGS]]`` runs the executable specified by the
target, which can be a component, a package or can be left blank, as
long as it can uniquely identify an executable within the project.
Tests and benchmarks are also treated as executables.

See `the v2-build section <#cabal-new-build>`__ for the target syntax.

Except in the case of the empty target, the strings after it will be
passed to the executable as arguments.

If one of the arguments starts with ``-`` it will be interpreted as
a cabal flag, so if you need to pass flags to the executable you
have to separate them with ``--``.

::

    $ cabal v2-run target -- -a -bcd --argument

'v2-run' also supports running script files that use a certain format. With
a script that looks like:

::

    #!/usr/bin/env cabal
    {- cabal:
    build-depends: base ^>= 4.11
                , shelly ^>= 1.8.1
    -}

    main :: IO ()
    main = do
        ...

It can either be executed like any other script, using ``cabal`` as an
interpreter, or through this command:

::

    $ cabal v2-run script.hs
    $ cabal v2-run script.hs -- --arg1 # args are passed like this

cabal v2-freeze
----------------

``cabal v2-freeze`` writes out a **freeze file** which records all of
the versions and flags which that are picked by the solver under the
current index and flags.  Default name of this file is
``cabal.project.freeze`` but in combination with a
``--project-file=my.project`` flag (see :ref:`project-file
<cmdoption-project-file>`)
the name will be ``my.project.freeze``.
A freeze file has the same syntax as ``cabal.project`` and looks
something like this:

.. highlight:: cabal

::

    constraints: HTTP ==4000.3.3,
                 HTTP +warp-tests -warn-as-error -network23 +network-uri -mtl1 -conduit10,
                 QuickCheck ==2.9.1,
                 QuickCheck +templatehaskell,
                 -- etc...


For end-user executables, it is recommended that you distribute the
``cabal.project.freeze`` file in your source repository so that all
users see a consistent set of dependencies. For libraries, this is not
recommended: users often need to build against different versions of
libraries than what you developed against.

cabal v2-bench
---------------

``cabal v2-bench [TARGETS] [OPTIONS]`` runs the specified benchmarks
(all the benchmarks in the current package by default), first ensuring
they are up to date.

cabal v2-test
--------------

``cabal v2-test [TARGETS] [OPTIONS]`` runs the specified test suites
(all the test suites in the current package by default), first ensuring
they are up to date.

cabal v2-haddock
-----------------

``cabal v2-haddock [FLAGS] [TARGET]`` builds Haddock documentation for
the specified packages within the project.

If a target is not a library :cfg-field:`haddock-benchmarks`,
:cfg-field:`haddock-executables`, :cfg-field:`haddock-internal`,
:cfg-field:`haddock-tests` will be implied as necessary.

cabal v2-exec
---------------

``cabal v2-exec [FLAGS] [--] COMMAND [--] [ARGS]`` runs the specified command
using the project's environment. That is, passing the right flags to compiler
invocations and bringing the project's executables into scope.

cabal v2-install
-----------------

``cabal v2-install [FLAGS] PACKAGES`` builds the specified packages and
symlinks/copies their executables in ``installdir`` (usually ``~/.cabal/bin``).

For example this command will build the latest ``cabal-install`` and symlink
its ``cabal`` executable:

::

    $ cabal v2-install cabal-install

In addition, it's possible to use ``cabal v2-install`` to install components
of a local project. For example, with an up-to-date Git clone of the Cabal
repository, this command will build cabal-install HEAD and symlink the
``cabal`` executable:

::

    $ cabal v2-install exe:cabal

Where symlinking is not possible (eg. on Windows), ``--install-method=copy``
can be used:

::

    $ cabal v2-install exe:cabal --install-method=copy --installdir=~/bin

Note that copied executables are not self-contained, since they might use
data-files from the store.

It is also possible to "install" libraries using the ``--lib`` flag. For
example, this command will build the latest Cabal library and install it:

::

    $ cabal v2-install --lib Cabal

This works by managing GHC environments. By default, it is writing to the
global environment in ``~/.ghc/$ARCH-$OS-$GHCVER/environments/default``.
``v2-install`` provides the ``--package-env`` flag to control which of
these environments is modified.

This command will modify the environment file in the current directory:

::

    $ cabal v2-install --lib Cabal --package-env .

This command will modify the environment file in the ``~/foo`` directory:

::

    $ cabal v2-install --lib Cabal --package-env foo/

Do note that the results of the previous two commands will be overwritten by
the use of other v2-style commands, so it is not recommended to use them inside
a project directory.

This command will modify the environment in the "local.env" file in the
current directory:

::

    $ cabal v2-install --lib Cabal --package-env local.env

This command will modify the ``myenv`` named global environment:

::

    $ cabal v2-install --lib Cabal --package-env myenv

If you wish to create a named environment file in the current directory where
the name does not contain an extension, you must reference it as ``./myenv``.

You can learn more about how to use these environments in `this section of the
GHC manual <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#package-environments>`_.

cabal v2-clean
---------------

``cabal v2-clean [FLAGS]`` cleans up the temporary files and build artifacts
stored in the ``dist-newstyle`` folder.

By default, it removes the entire folder, but it can also spare the configuration
and caches if the ``--save-config`` option is given, in which case it only removes
the build artefacts (``.hi``, ``.o`` along with any other temporary files generated
by the compiler, along with the build output).

cabal v2-sdist
---------------

``cabal v2-sdist [FLAGS] [TARGETS]`` takes the crucial files needed to build ``TARGETS``
and puts them into an archive format ready for upload to Hackage. These archives are stable
and two archives of the same format built from the same source will hash to the same value.

``cabal v2-sdist`` takes the following flags:

- ``-l``, ``--list-only``: Rather than creating an archive, lists files that would be included.
  Output is to ``stdout`` by default. The file paths are relative to the project's root
  directory.

- ``-o``, ``--output-dir``: Sets the output dir, if a non-default one is desired. The default is
  ``dist-newstyle/sdist/``. ``--output-dir -`` will send output to ``stdout``
  unless multiple archives are being created.

- ``-z``, ``--null``: Only used with ``--list-only``. Separates filenames with a NUL
  byte instead of newlines.

``v2-sdist`` is inherently incompatible with sdist hooks, not due to implementation but due
to fundamental core invariants (same source code should result in the same tarball, byte for
byte) that must be satisfied for it to function correctly in the larger v2-build ecosystem.
``autogen-modules`` is able to replace uses of the hooks to add generated modules, along with
the custom publishing of Haddock documentation to Hackage.

Configuring builds with cabal.project
=====================================

``cabal.project`` files support a variety of options which configure the
details of your build. The general syntax of a ``cabal.project`` file is
similar to that of a Cabal file: there are a number of fields, some of
which live inside stanzas:

::

    packages: */*.cabal
    with-compiler: /opt/ghc/8.0.1/bin/ghc

    package cryptohash
      optimization: False

In general, the accepted field names coincide with the accepted command
line flags that ``cabal install`` and other commands take. For example,
``cabal v2-configure --enable-profiling`` will write out a project
file with ``profiling: True``.

The full configuration of a project is determined by combining the
following sources (later entries override earlier ones):

1. ``~/.cabal/config`` (the user-wide global configuration)

2. ``cabal.project`` (the project configuration)

3. ``cabal.project.freeze`` (the output of ``cabal v2-freeze``)

4. ``cabal.project.local`` (the output of ``cabal v2-configure``)


Specifying the local packages
-----------------------------

The following top-level options specify what the local packages of a
project are:

.. cfg-field:: packages: package location list (space or comma separated)
    :synopsis: Project packages.

    :default: ``./*.cabal``

    Specifies the list of package locations which contain the local
    packages to be built by this project. Package locations can take the
    following forms:

    1. They can specify a Cabal file, or a directory containing a Cabal
       file, e.g., ``packages: Cabal cabal-install/cabal-install.cabal``.

    2. They can specify a glob-style wildcards, which must match one or
       more (a) directories containing a (single) Cabal file, (b) Cabal
       files (extension ``.cabal``), or (c) tarballs which contain Cabal
       packages (extension ``.tar.gz``).
       For example, to match all Cabal files in all
       subdirectories, as well as the Cabal projects in the parent
       directories ``foo`` and ``bar``, use
       ``packages: */*.cabal ../{foo,bar}/``

    3. They can specify an ``http``, ``https`` or ``file``
       URL, representing the path to a remote tarball to be downloaded
       and built.

    There is no command line variant of this field; see :issue:`3585`.

.. cfg-field:: optional-packages: package location list (space or comma-separated)
    :synopsis: Optional project packages.

    :default: ``./*/*.cabal``

    Like :cfg-field:`packages`, specifies a list of package locations
    containing local packages to be built. Unlike :cfg-field:`packages`,
    if we glob for a package, it is permissible for the glob to match against
    zero packages. The intended use-case for :cfg-field:`optional-packages`
    is to make it so that vendored packages can be automatically picked up if
    they are placed in a subdirectory, but not error if there aren't any.

    There is no command line variant of this field.

.. cfg-field:: extra-packages: package list with version bounds (comma separated)
    :synopsis: Adds external pacakges as local

    [STRIKEOUT:Specifies a list of external packages from Hackage which
    should be considered local packages.] (Not implemented)

    There is no command line variant of this field.



All local packages are *vendored*, in the sense that if other packages
(including external ones from Hackage) depend on a package with the name
of a local package, the local package is preferentially used.  This
motivates the default settings::

    packages: ./*.cabal
    optional-packages: ./*/*.cabal

...any package can be vendored simply by making a checkout in the
top-level project directory, as might be seen in this hypothetical
directory layout::

    foo.cabal
    foo-helper/     # local package
    unix/           # vendored external package

All of these options support globs. ``cabal v2-build`` has its own glob
format:

-  Anywhere in a path, as many times as you like, you can specify an
   asterisk ``*`` wildcard. E.g., ``*/*.cabal`` matches all ``.cabal``
   files in all immediate subdirectories. Like in glob(7), asterisks do
   not match hidden files unless there is an explicit period, e.g.,
   ``.*/foo.cabal`` will match ``.private/foo.cabal`` (but
   ``*/foo.cabal`` will not).

-  You can use braces to specify specific directories; e.g.,
   ``{vendor,pkgs}/*.cabal`` matches all Cabal files in the ``vendor``
   and ``pkgs`` subdirectories.

Formally, the format described by the following BNF:

.. todo::
    convert globbing grammar to proper ABNF_ syntax

.. code-block:: abnf

    FilePathGlob    ::= FilePathRoot FilePathGlobRel
    FilePathRoot    ::= {- empty -}        # relative to cabal.project
                      | "/"                # Unix root
                      | [a-zA-Z] ":" [/\\] # Windows root
                      | "~"                # home directory
    FilePathGlobRel ::= Glob "/"  FilePathGlobRel # Unix directory
                      | Glob "\\" FilePathGlobRel # Windows directory
                      | Glob         # file
                      | {- empty -}  # trailing slash
    Glob      ::= GlobPiece *
    GlobPiece ::= "*"            # wildcard
                | [^*{},/\\] *   # literal string
                | "\\" [*{},]    # escaped reserved character
                | "{" Glob "," ... "," Glob "}" # union (match any of these)


Specifying Packages from Remote Version Control Locations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Starting with Cabal 2.4, there is now a stanza
``source-repository-package`` for specifying packages from an external
version control which supports the following fields:

- :pkg-field:`source-repository:type`
- :pkg-field:`source-repository:location`
- :pkg-field:`source-repository:tag`
- :pkg-field:`source-repository:subdir`

A simple example is shown below:

.. code-block:: cabal

    packages: .

    source-repository-package
        type: git
        location: https://github.com/hvr/HsYAML.git
        tag: e70cf0c171c9a586b62b3f75d72f1591e4e6aaa1

    source-repository-package
        type: git
        location: https://github.com/well-typed/cborg
        tag: 3d274c14ca3077c3a081ba7ad57c5182da65c8c1
        subdir: cborg

Global configuration options
----------------------------

The following top-level configuration options are not specific to any
package, and thus apply globally:


.. cfg-field:: verbose: nat
               --verbose=n, -vn
    :synopsis: Build verbosity level.

    :default: 1

    Control the verbosity of ``cabal`` commands, valid values are from 0
    to 3.

    The command line variant of this field is ``--verbose=2``; a short
    form ``-v2`` is also supported.

.. cfg-field:: jobs: nat or $ncpus
               --jobs=n, -jn, --jobs=$ncpus
    :synopsis: Number of builds running in parallel.

    :default: 1

    Run *nat* jobs simultaneously when building. If ``$ncpus`` is
    specified, run the number of jobs equal to the number of CPUs.
    Package building is often quite parallel, so turning on parallelism
    can speed up build times quite a bit!

    The command line variant of this field is ``--jobs=2``; a short form
    ``-j2`` is also supported; a bare ``--jobs`` or ``-j`` is equivalent
    to ``--jobs=$ncpus``.

.. cfg-field::  keep-going: boolean
                --keep-going
    :synopsis: Try to continue building on failure.

    :default: False

    If true, after a build failure, continue to build other unaffected
    packages.

    The command line variant of this field is ``--keep-going``.

.. option:: --builddir=DIR

    Specifies the name of the directory where build products for
    build will be stored; defaults to ``dist-newstyle``.  If a
    relative name is specified, this directory is resolved relative
    to the root of the project (i.e., where the ``cabal.project``
    file lives.)

    This option cannot be specified via a ``cabal.project`` file.

.. _cmdoption-project-file:
.. option:: --project-file=FILE

    Specifies the name of the project file used to specify the
    rest of the top-level configuration; defaults to ``cabal.project``.
    This name not only specifies the name of the main project file,
    but also the auxiliary project files ``cabal.project.freeze``
    and ``cabal.project.local``; for example, if you specify
    ``--project-file=my.project``, then the other files that will
    be probed are ``my.project.freeze`` and ``my.project.local``.

    If the specified project file is a relative path, we will
    look for the file relative to the current working directory,
    and then for the parent directory, until the project file is
    found or we have hit the top of the user's home directory.

    This option cannot be specified via a ``cabal.project`` file.

.. option:: --store-dir=DIR

    Specifies the name of the directory of the global package store.

Solver configuration options
----------------------------

The following settings control the behavior of the dependency solver:

.. cfg-field:: constraints: constraints list (comma separated)
               --constraint="pkg >= 2.0"
    :synopsis: Extra dependencies constraints.

    Add extra constraints to the version bounds, flag settings,
    and other properties a solver can pick for a
    package. For example:

    ::

        constraints: bar == 2.1

    A package can be specified multiple times in ``constraints``, in
    which case the specified constraints are intersected. This is
    useful, since the syntax does not allow you to specify multiple
    constraints at once. For example, to specify both version bounds and
    flag assignments, you would write:

    ::

        constraints: bar == 2.1,
                     bar +foo -baz

    Valid constraints take the same form as for the `constraint
    command line option
    <installing-packages.html#cmdoption-setup-configure-constraint>`__.

.. cfg-field:: preferences: preference (comma separated)
               --preference="pkg >= 2.0"
    :synopsis: Prefered dependency versions.

    Like :cfg-field:`constraints`, but the solver will attempt to satisfy
    these preferences on a best-effort basis. The resulting install is locally
    optimal with respect to preferences; specifically, no single package
    could be replaced with a more preferred version that still satisfies
    the hard constraints.

    Operationally, preferences can cause the solver to attempt certain
    version choices of a package before others, which can improve
    dependency solver runtime.

    One way to use :cfg-field:`preferences` is to take a known working set of
    constraints (e.g., via ``cabal v2-freeze``) and record them as
    preferences. In this case, the solver will first attempt to use this
    configuration, and if this violates hard constraints, it will try to
    find the minimal number of upgrades to satisfy the hard constraints
    again.

    The command line variant of this field is
    ``--preference="pkg >= 2.0"``; to specify multiple preferences, pass
    the flag multiple times.

.. cfg-field:: allow-newer: none, all or list of scoped package names (space or comma separated)
               --allow-newer, --allow-newer=[none,all,[scope:][^]pkg]
    :synopsis: Lift dependencies upper bound constaints.

    :default: ``none``

    Allow the solver to pick an newer version of some packages than
    would normally be permitted by than the :pkg-field:`build-depends` bounds
    of packages in the install plan. This option may be useful if the
    dependency solver cannot otherwise find a valid install plan.

    For example, to relax ``pkg``\ s :pkg-field:`build-depends` upper bound on
    ``dep-pkg``, write a scoped package name of the form:

    ::

        allow-newer: pkg:dep-pkg

    If the scope shall be limited to specific releases of ``pkg``, the
    extended form as in

    ::

        allow-newer: pkg-1.2.3:dep-pkg, pkg-1.1.2:dep-pkg

    can be used to limit the relaxation of dependencies on
    ``dep-pkg`` by the ``pkg-1.2.3`` and ``pkg-1.1.2`` releases only.

    The scoped syntax is recommended, as it is often only a single package
    whose upper bound is misbehaving. In this case, the upper bounds of
    other packages should still be respected; indeed, relaxing the bound
    can break some packages which test the selected version of packages.

    The syntax also allows to prefix the dependee package with a
    modifier symbol to modify the scope/semantic of the relaxation
    transformation in a additional ways. Currently only one modifier
    symbol is defined, i.e. ``^`` (i.e. caret) which causes the
    relaxation to be applied only to ``^>=`` operators and leave all other
    version operators untouched.

    However, in some situations (e.g., when attempting to build packages
    on a new version of GHC), it is useful to disregard *all*
    upper-bounds, with respect to a package or all packages. This can be
    done by specifying just a package name, or using the keyword ``all``
    to specify all packages:

    ::

        -- Disregard upper bounds involving the dependencies on
        -- packages bar, baz. For quux only, relax
        -- 'quux ^>= ...'-style constraints only.
        allow-newer: bar, baz, ^quux

        -- Disregard all upper bounds when dependency solving
        allow-newer: all

        -- Disregard all `^>=`-style upper bounds when dependency solving
        allow-newer: ^all


    For consistency, there is also the explicit wildcard scope syntax
    ``*`` (or its alphabetic synonym ``all``). Consequently, the
    examples above are equivalent to the explicitly scoped variants:

    ::

        allow-newer: all:bar, *:baz, *:^quux

        allow-newer: *:*
        allow-newer: all:all

        allow-newer: *:^*
        allow-newer: all:^all

    In order to ignore all bounds specified by a package ``pkg-1.2.3``
    you can combine scoping with a right-hand-side wildcard like so

    ::

        -- Disregard any upper bounds specified by pkg-1.2.3
        allow-newer: pkg-1.2.3:*

        -- Disregard only `^>=`-style upper bounds in pkg-1.2.3
        allow-newer: pkg-1.2.3:^*


    :cfg-field:`allow-newer` is often used in conjunction with a constraint
    (in the cfg-field:`constraints` field) forcing the usage of a specific,
    newer version of a package.

    The command line variant of this field is e.g. ``--allow-newer=bar``. A
    bare ``--allow-newer`` is equivalent to ``--allow-newer=all``.

.. cfg-field:: allow-older: none, all, list of scoped package names (space or comma separated)
               --allow-older, --allow-older=[none,all,[scope:][^]pkg]
    :synopsis: Lift dependency lower bound constaints.
    :since: 2.0

    :default: ``none``

    Like :cfg-field:`allow-newer`, but applied to lower bounds rather than
    upper bounds.

    The command line variant of this field is ``--allow-older=all``. A
    bare ``--allow-older`` is equivalent to ``--allow-older=all``.


.. cfg-field:: index-state: HEAD, unix-timestamp, ISO8601 UTC timestamp.
   :synopsis: Use source package index state as it existed at a previous time.
   :since: 2.0

   :default: ``HEAD``

   This allows to change the source package index state the solver uses
   to compute install-plans. This is particularly useful in
   combination with freeze-files in order to also freeze the state the
   package index was in at the time the install-plan was frozen.

   ::

      -- UNIX timestamp format example
      index-state: @1474739268

      -- ISO8601 UTC timestamp format example
      -- This format is used by 'cabal v2-configure'
      -- for storing `--index-state` values.
      index-state: 2016-09-24T17:47:48Z


.. cfg-field:: reject-unconstrained-dependencies: all, none
               --reject-unconstrained-dependencies=[all|none]
   :synopsis: Restrict the solver to packages that have constraints on them.

   :default: none
   :since: 2.6

   By default, the dependency solver can include any package that it's
   aware of in a build plan. If you wish to restrict the build plan to
   a closed set of packages (e.g., from a freeze file), use this flag.

   When set to `all`, all non-local packages that aren't goals must be
   explicitly constrained. When set to `none`, the solver will
   consider all packages.


Package configuration options
-----------------------------

Package options affect the building of specific packages. There are three
ways a package option can be specified:

-  They can be specified at the top-level, in which case they apply only
   to **local package**, or

-  They can be specified inside a ``package`` stanza, in which case they
   apply to the build of the package, whether or not it is local or
   external.

-  They can be specified inside an ``package *`` stanza, in which case they
   apply to all packages, local ones from the project and also external
   dependencies.


For example, the following options specify that :cfg-field:`optimization`
should be turned off for all local packages, and that ``bytestring`` (possibly
an external dependency) should be built with ``-fno-state-hack``::

    optimization: False

    package bytestring
        ghc-options: -fno-state-hack

``ghc-options`` is not specifically described in this documentation,
but is one of many fields for configuring programs.  They take the form
``progname-options`` and ``progname-location``, and
can only be set inside package stanzas.  (TODO: They are not supported
at top-level, see :issue:`3579`.)

At the moment, there is no way to specify an option to apply to all
external packages or all inplace packages. Additionally, it is only
possible to specify these options on the command line for all local
packages (there is no per-package command line interface.)

Some flags were added by more recent versions of the Cabal library. This
means that they are NOT supported by packages which use Custom setup
scripts that require a version of the Cabal library older than when the
feature was added.

.. cfg-field:: flags: list of +flagname or -flagname (space separated)
               --flags="+foo -bar", -ffoo, -f-bar
    :synopsis: Enable or disable package flags.

    Force all flags specified as ``+flagname`` to be true, and all flags
    specified as ``-flagname`` to be false. For example, to enable the
    flag ``foo`` and disable ``bar``, set:

    ::

        flags: +foo -bar

    If there is no leading punctuation, it is assumed that the flag
    should be enabled; e.g., this is equivalent:

    ::

        flags: foo -bar

    Flags are *per-package*, so it doesn't make much sense to specify
    flags at the top-level, unless you happen to know that *all* of your
    local packages support the same named flags. If a flag is not
    supported by a package, it is ignored.

    See also the solver configuration field :cfg-field:`constraints`.

    The command line variant of this flag is ``--flags``. There is also
    a shortened form ``-ffoo -f-bar``.

    A common mistake is to say ``cabal v2-build -fhans``, where
    ``hans`` is a flag for a transitive dependency that is not in the
    local package; in this case, the flag will be silently ignored. If
    ``haskell-tor`` is the package you want this flag to apply to, try
    ``--constraint="haskell-tor +hans"`` instead.

.. cfg-field:: with-compiler: executable
               --with-compiler=executable
    :synopsis: Path to compiler executable.

    Specify the path to a particular compiler to be used. If not an
    absolute path, it will be resolved according to the :envvar:`PATH`
    environment. The type of the compiler (GHC, GHCJS, etc) must be
    consistent with the setting of the :cfg-field:`compiler` field.

    The most common use of this option is to specify a different version
    of your compiler to be used; e.g., if you have ``ghc-7.8`` in your
    path, you can specify ``with-compiler: ghc-7.8`` to use it.

    This flag also sets the default value of :cfg-field:`with-hc-pkg`, using
    the heuristic that it is named ``ghc-pkg-7.8`` (if your executable name
    is suffixed with a version number), or is the executable named
    ``ghc-pkg`` in the same directory as the ``ghc`` directory. If this
    heuristic does not work, set :cfg-field:`with-hc-pkg` explicitly.

    For inplace packages, ``cabal v2-build`` maintains a separate build
    directory for each version of GHC, so you can maintain multiple
    build trees for different versions of GHC without clobbering each
    other.

    At the moment, it's not possible to set :cfg-field:`with-compiler` on a
    per-package basis, but eventually we plan on relaxing this
    restriction. If this is something you need, give us a shout.

    The command line variant of this flag is
    ``--with-compiler=ghc-7.8``; there is also a short version
    ``-w ghc-7.8``.

.. cfg-field:: with-hc-pkg: executable
               --with-hc-pkg=executable
    :synopsis: Specifies package tool.

    Specify the path to the package tool, e.g., ``ghc-pkg``. This
    package tool must be compatible with the compiler specified by
    :cfg-field:`with-compiler` (generally speaking, it should be precisely
    the tool that was distributed with the compiler). If this option is
    omitted, the default value is determined from :cfg-field:`with-compiler`.

    The command line variant of this flag is
    ``--with-hc-pkg=ghc-pkg-7.8``.

.. cfg-field:: optimization: nat
               --enable-optimization
               --disable-optimization
    :synopsis: Build with optimization.

    :default: ``1``

    Build with optimization. This is appropriate for production use,
    taking more time to build faster libraries and programs.

    The optional *nat* value is the optimisation level. Some compilers
    support multiple optimisation levels. The range is 0 to 2. Level 0
    disables optimization, level 1 is the default. Level 2 is higher
    optimisation if the compiler supports it. Level 2 is likely to lead
    to longer compile times and bigger generated code. If you are not
    planning to run code, turning off optimization will lead to better
    build times and less code to be rebuilt when a module changes.

    When optimizations are enabled, Cabal passes ``-O2`` to the C compiler.

    We also accept ``True`` (equivalent to 1) and ``False`` (equivalent
    to 0).

    Note that as of GHC 8.0, GHC does not recompile when optimization
    levels change (see :ghc-ticket:`10923`), so if
    you change the optimization level for a local package you may need
    to blow away your old build products in order to rebuild with the
    new optimization level.

    The command line variant of this flag is ``-O2`` (with ``-O1``
    equivalent to ``-O``). There are also long-form variants
    ``--enable-optimization`` and ``--disable-optimization``.

.. cfg-field:: configure-options: args (space separated)
               --configure-option=arg
    :synopsis: Options to pass to configure script.

    A list of extra arguments to pass to the external ``./configure``
    script, if one is used. This is only useful for packages which have
    the ``Configure`` build type. See also the section on
    `system-dependent
    parameters <developing-packages.html#system-dependent-parameters>`__.

    The command line variant of this flag is ``--configure-option=arg``,
    which can be specified multiple times to pass multiple options.

.. cfg-field:: compiler: ghc, ghcjs, jhc, lhc, uhc or haskell-suite
               --compiler=compiler
    :synopsis: Compiler to build with.

    :default: ``ghc``

    Specify which compiler toolchain to be used. This is independent of
    ``with-compiler``, because the choice of toolchain affects Cabal's
    build logic.

    The command line variant of this flag is ``--compiler=ghc``.

.. cfg-field:: tests: boolean
               --enable-tests
               --disable-tests
    :synopsis: Build tests.

    :default: ``False``

    Force test suites to be enabled. For most users this should not be
    needed, as we always attempt to solve for test suite dependencies,
    even when this value is ``False``; furthermore, test suites are
    automatically enabled if they are requested as a built target.

    The command line variant of this flag is ``--enable-tests`` and
    ``--disable-tests``.

.. cfg-field:: benchmarks: boolean
               --enable-benchmarks
               --disable-benchmarks
    :synopsis: Build benchmarks.

    :default: ``False``

    Force benchmarks to be enabled. For most users this should not be
    needed, as we always attempt to solve for benchmark dependencies,
    even when this value is ``False``; furthermore, benchmarks are
    automatically enabled if they are requested as a built target.

    The command line variant of this flag is ``--enable-benchmarks`` and
    ``--disable-benchmarks``.

.. cfg-field:: extra-prog-path: paths (newline or comma separated)
               --extra-prog-path=PATH
    :synopsis: Add directories to program search path.
    :since: 1.18

    A list of directories to search for extra required programs. Most
    users should not need this, as programs like ``happy`` and ``alex``
    will automatically be installed and added to the path. This can be
    useful if a ``Custom`` setup script relies on an exotic extra
    program.

    The command line variant of this flag is ``--extra-prog-path=PATH``,
    which can be specified multiple times.

.. cfg-field:: run-tests: boolean
               --run-tests
    :synopsis: Run package test suite upon installation.

    :default: ``False``

    Run the package test suite upon installation. This is useful for
    saying "When this package is installed, check that the test suite
    passes, terminating the rest of the build if it is broken."

    .. warning::

      One deficiency: the :cfg-field:`run-tests` setting of a package is NOT
      recorded as part of the hash, so if you install something without
      :cfg-field:`run-tests` and then turn on ``run-tests``, we won't
      subsequently test the package. If this is causing you problems, give
      us a shout.

    The command line variant of this flag is ``--run-tests``.

Object code options
^^^^^^^^^^^^^^^^^^^

.. cfg-field:: debug-info: integer
               --enable-debug-info=<n>
               --disable-debug-info
    :synopsis: Build with debug info enabled.
    :since: 1.22

    :default: False

    If the compiler (e.g., GHC 7.10 and later) supports outputing OS
    native debug info (e.g., DWARF), setting ``debug-info: True`` will
    instruct it to do so. See the GHC wiki page on :ghc-wiki:`DWARF`
    for more information about this feature.

    (This field also accepts numeric syntax, but until GHC 8.2 this didn't
    do anything.)

    The command line variant of this flag is ``--enable-debug-info`` and
    ``--disable-debug-info``.

.. cfg-field:: split-sections: boolean
               --enable-split-sections
               --disable-split-sections
    :synopsis: Use GHC's split sections feature.
    :since: 2.2

    :default: False

    Use the GHC ``-split-sections`` feature when building the library. This
    reduces the final size of the executables that use the library by
    allowing them to link with only the bits that they use rather than
    the entire library. The downside is that building the library takes
    longer and uses a bit more memory.

    This feature is supported by GHC 8.0 and later.

    The command line variant of this flag is ``--enable-split-sections`` and
    ``--disable-split-sections``.

.. cfg-field:: split-objs: boolean
               --enable-split-objs
               --disable-split-objs
    :synopsis: Use GHC's split objects feature.

    :default: False

    Use the GHC ``-split-objs`` feature when building the library. This
    reduces the final size of the executables that use the library by
    allowing them to link with only the bits that they use rather than
    the entire library. The downside is that building the library takes
    longer and uses considerably more memory.

    It is generally recommend that you use ``split-sections`` instead
    of ``split-objs`` where possible.

    The command line variant of this flag is ``--enable-split-objs`` and
    ``--disable-split-objs``.

.. cfg-field:: executable-stripping: boolean
               --enable-executable-stripping
               --disable-executable-stripping
    :synopsis: Strip installed programs.

    :default: True

    When installing binary executable programs, run the ``strip``
    program on the binary. This can considerably reduce the size of the
    executable binary file. It does this by removing debugging
    information and symbols.

    Not all Haskell implementations generate native binaries. For such
    implementations this option has no effect.

    (TODO: Check what happens if you combine this with ``debug-info``.)

    The command line variant of this flag is
    ``--enable-executable-stripping`` and
    ``--disable-executable-stripping``.

.. cfg-field:: library-stripping: boolean
               --enable-library-stripping
               --disable-library-stripping
    :synopsis: Strip installed libraries.
    :since: 1.20

    When installing binary libraries, run the ``strip`` program on the
    binary, saving space on the file system. See also
    ``executable-stripping``.

    The command line variant of this flag is
    ``--enable-library-stripping`` and ``--disable-library-stripping``.

Executable options
^^^^^^^^^^^^^^^^^^

.. cfg-field:: program-prefix: prefix
               --program-prefix=prefix
    :synopsis: Prepend prefix to program names.

    [STRIKEOUT:Prepend *prefix* to installed program names.] (Currently
    implemented in a silly and not useful way. If you need this to work
    give us a shout.)

    *prefix* may contain the following path variables: ``$pkgid``,
    ``$pkg``, ``$version``, ``$compiler``, ``$os``, ``$arch``, ``$abi``,
    ``$abitag``

    The command line variant of this flag is ``--program-prefix=foo-``.

.. cfg-field:: program-suffix: suffix
               --program-suffix=suffix
    :synopsis: Append refix to program names.

    [STRIKEOUT:Append *suffix* to installed program names.] (Currently
    implemented in a silly and not useful way. If you need this to work
    give us a shout.)

    The most obvious use for this is to append the program's version
    number to make it possible to install several versions of a program
    at once: ``program-suffix: $version``.

    *suffix* may contain the following path variables: ``$pkgid``,
    ``$pkg``, ``$version``, ``$compiler``, ``$os``, ``$arch``, ``$abi``,
    ``$abitag``

    The command line variant of this flag is
    ``--program-suffix='$version'``.

Dynamic linking options
^^^^^^^^^^^^^^^^^^^^^^^

.. cfg-field:: shared: boolean
               --enable-shared
               --disable-shared
    :synopsis: Build shared library.

    :default: False

    Build shared library. This implies a separate compiler run to
    generate position independent code as required on most platforms.

    The command line variant of this flag is ``--enable-shared`` and
    ``--disable-shared``.

.. cfg-field:: executable-dynamic: boolean
               --enable-executable-dynamic
               --disable-executable-dynamic
    :synopsis: Link executables dynamically.

    :default: False

    Link executables dynamically. The executable's library dependencies
    should be built as shared objects. This implies ``shared: True``
    unless ``shared: False`` is explicitly specified.

    The command line variant of this flag is
    ``--enable-executable-dynamic`` and
    ``--disable-executable-dynamic``.

.. cfg-field:: library-for-ghci: boolean
               --enable-library-for-ghci
               --disable-library-for-ghci
    :synopsis: Build libraries suitable for use with GHCi.

    :default: True

    Build libraries suitable for use with GHCi. This involves an extra
    linking step after the build.

    Not all platforms support GHCi and indeed on some platforms, trying
    to build GHCi libs fails. In such cases, consider setting
    ``library-for-ghci: False``.

    The command line variant of this flag is
    ``--enable-library-for-ghci`` and ``--disable-library-for-ghci``.

.. cfg-field:: relocatable:
               --relocatable
    :synopsis: Build relocatable package.
    :since: 1.22

    :default: False

    [STRIKEOUT:Build a package which is relocatable.] (TODO: It is not
    clear what this actually does, or if it works at all.)

    The command line variant of this flag is ``--relocatable``.

Static linking options
^^^^^^^^^^^^^^^^^^^^^^

.. cfg-field:: static: boolean
               --enable-static
               --disable-static
    :synopsis: Build static library.


    :default: False

    Roll this and all dependent libraries into a combined ``.a`` archive.
    This uses GHCs ``-staticlib`` flag, which is available for iOS and with
    GHC 8.4 and later for other platforms as well.

.. cfg-field:: executable-static: boolean
               --enable-executable-static
               --disable-executable-static
    :synopsis: Build fully static executables.


    :default: False

    Build fully static executables.
    This link all dependent libraries into executables statically,
    including libc.
    This passes ``-static`` and ``-optl=-static`` to GHC.

Foreign function interface options
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. cfg-field:: extra-include-dirs: directories (comma or newline separated list)
               --extra-include-dirs=DIR
    :synopsis: Adds C header search path.

    An extra directory to search for C header files. You can use this
    flag multiple times to get a list of directories.

    You might need to use this flag if you have standard system header
    files in a non-standard location that is not mentioned in the
    package's ``.cabal`` file. Using this option has the same affect as
    appending the directory *dir* to the :pkg-field:`include-dirs` field in each
    library and executable in the package's ``.cabal`` file. The
    advantage of course is that you do not have to modify the package at
    all. These extra directories will be used while building the package
    and for libraries it is also saved in the package registration
    information and used when compiling modules that use the library.

    The command line variant of this flag is
    ``--extra-include-dirs=DIR``, which can be specified multiple times.

.. cfg-field:: extra-lib-dirs: directories (comma or newline separated list)
               --extra-lib-dirs=DIR
    :synopsis: Adds library search directory.

    An extra directory to search for system libraries files.

    The command line variant of this flag is ``--extra-lib-dirs=DIR``,
    which can be specified multiple times.

.. cfg-field:: extra-framework-dirs: directories (comma or newline separated list)
               --extra-framework-dirs=DIR
    :synopsis: Adds framework search directory (OS X only).

    An extra directory to search for frameworks (OS X only).

    You might need to use this flag if you have standard system
    libraries in a non-standard location that is not mentioned in the
    package's ``.cabal`` file. Using this option has the same affect as
    appending the directory *dir* to the :cfg-field:`extra-lib-dirs` field in
    each library and executable in the package's ``.cabal`` file. The
    advantage of course is that you do not have to modify the package at
    all. These extra directories will be used while building the package
    and for libraries it is also saved in the package registration
    information and used when compiling modules that use the library.

    The command line variant of this flag is
    ``--extra-framework-dirs=DIR``, which can be specified multiple
    times.

Profiling options
^^^^^^^^^^^^^^^^^

.. cfg-field:: profiling: boolean
               --enable-profiling
               --disable-profiling
    :synopsis: Enable profiling builds.
    :since: 1.22

    :default: False

    Build libraries and executables with profiling enabled (for
    compilers that support profiling as a separate mode). It is only
    necessary to specify :cfg-field:`profiling` for the specific package you
    want to profile; ``cabal v2-build`` will ensure that all of its
    transitive dependencies are built with profiling enabled.

    To enable profiling for only libraries or executables, see
    :cfg-field:`library-profiling` and :cfg-field:`executable-profiling`.

    For useful profiling, it can be important to control precisely what
    cost centers are allocated; see :cfg-field:`profiling-detail`.

    The command line variant of this flag is ``--enable-profiling`` and
    ``--disable-profiling``.

.. cfg-field:: profiling-detail: level
               --profiling-detail=level
    :synopsis: Profiling detail level.
    :since: 1.24

    Some compilers that support profiling, notably GHC, can allocate
    costs to different parts of the program and there are different
    levels of granularity or detail with which this can be done. In
    particular for GHC this concept is called "cost centers", and GHC
    can automatically add cost centers, and can do so in different ways.

    This flag covers both libraries and executables, but can be
    overridden by the ``library-profiling-detail`` field.

    Currently this setting is ignored for compilers other than GHC. The
    levels that cabal currently supports are:

    default
        For GHC this uses ``exported-functions`` for libraries and
        ``toplevel-functions`` for executables.
    none
        No costs will be assigned to any code within this component.
    exported-functions
        Costs will be assigned at the granularity of all top level
        functions exported from each module. In GHC, this
        is for non-inline functions.  Corresponds to ``-fprof-auto-exported``.
    toplevel-functions
        Costs will be assigned at the granularity of all top level
        functions in each module, whether they are exported from the
        module or not. In GHC specifically, this is for non-inline
        functions.  Corresponds to ``-fprof-auto-top``.
    all-functions
        Costs will be assigned at the granularity of all functions in
        each module, whether top level or local. In GHC specifically,
        this is for non-inline toplevel or where-bound functions or
        values.  Corresponds to ``-fprof-auto``.

    The command line variant of this flag is
    ``--profiling-detail=none``.

.. cfg-field:: library-profiling-detail: level
               --library-profiling-detail=level
    :synopsis: Libraries profiling detail level.
    :since: 1.24

    Like :cfg-field:`profiling-detail`, but applied only to libraries

    The command line variant of this flag is
    ``--library-profiling-detail=none``.

.. cfg-field:: library-vanilla: boolean
               --enable-library-vanilla
               --disable-library-vanilla
    :synopsis: Build libraries without profiling.

    :default: True

    Build ordinary libraries (as opposed to profiling libraries).
    Mostly, you can set this to False to avoid building ordinary
    libraries when you are profiling.

    The command line variant of this flag is
    ``--enable-library-vanilla`` and ``--disable-library-vanilla``.

.. cfg-field:: library-profiling: boolean
               --enable-library-profiling
               --disable-library-profiling
    :synopsis: Build libraries with profiling enabled.
    :since: 1.22

    :default: False

    Build libraries with profiling enabled.  You probably want
    to use :cfg-field:`profiling` instead.

    The command line variant of this flag is
    ``--enable-library-profiling`` and ``--disable-library-profiling``.

.. cfg-field:: executable-profiling: boolean
               --enable-executable-profiling
               --disable-executable-profiling
    :synopsis: Build executables with profiling enabled.
    :since: 1.22

    :default: False

    Build executables with profiling enabled. You probably want
    to use :cfg-field:`profiling` instead.

    The command line variant of this flag is
    ``--enable-executable-profiling`` and
    ``--disable-executable-profiling``.

Coverage options
^^^^^^^^^^^^^^^^

.. cfg-field:: coverage: boolean
               --enable-coverage
               --disable-coverage
    :synopsis: Build with coverage enabled.
    :since: 1.22

    :default: False

    Build libraries and executables (including test suites) with Haskell
    Program Coverage enabled. Running the test suites will automatically
    generate coverage reports with HPC.

    The command line variant of this flag is ``--enable-coverage`` and
    ``--disable-coverage``.

.. cfg-field:: library-coverage: boolean
               --enable-library-coverage
               --disable-library-coverage
    :since: 1.22
    :deprecated:

    :default: False

    Deprecated, use :cfg-field:`coverage`.

    The command line variant of this flag is
    ``--enable-library-coverage`` and ``--disable-library-coverage``.

Haddock options
^^^^^^^^^^^^^^^

.. cfg-field:: documentation: boolean
               --enable-documentation
               --disable-documentation
    :synopsis: Enable building of documentation.

    :default: False

    Enables building of Haddock documentation

    The command line variant of this flag is ``--enable-documentation``
    and ``--disable-documentation``.

    `documentation: true` does not imply :cfg-field:`haddock-benchmarks`,
    :cfg-field:`haddock-executables`, :cfg-field:`haddock-internal` or
    :cfg-field:`haddock-tests`. These need to be enabled separately if
    desired.

.. cfg-field:: doc-index-file: templated path
               --doc-index-file=TEMPLATE
    :synopsis: Path to haddock templates.

    A central index of Haddock API documentation (template cannot use
    ``$pkgid``), which should be updated as documentation is built.

    The command line variant of this flag is
    ``--doc-index-file=TEMPLATE``

The following commands are equivalent to ones that would be passed when
running ``setup haddock``. (TODO: Where does the documentation get put.)

.. cfg-field:: haddock-hoogle: boolean
    :synopsis: Generate Hoogle file.

    :default: False

    Generate a text file which can be converted by Hoogle_
    into a database for searching. This is equivalent to running ``haddock``
    with the ``--hoogle`` flag.

    The command line variant of this flag is ``--hoogle`` (for the
    ``haddock`` command).

.. cfg-field:: haddock-html: boolean
    :synopsis: Build HTML documentation.

    :default: True

    Build HTML documentation.

    The command line variant of this flag is ``--html`` (for the
    ``haddock`` command).

.. cfg-field:: haddock-html-location: templated path
    :synopsis: Haddock HTML templates location.

    Specify a template for the location of HTML documentation for
    prerequisite packages. The substitutions are applied to the template
    to obtain a location for each package, which will be used by
    hyperlinks in the generated documentation. For example, the
    following command generates links pointing at [Hackage] pages:

    ::

        html-location: 'http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html'

    Here the argument is quoted to prevent substitution by the shell. If
    this option is omitted, the location for each package is obtained
    using the package tool (e.g. ``ghc-pkg``).

    The command line variant of this flag is ``--html-location`` (for
    the ``haddock`` subcommand).

.. cfg-field:: haddock-executables: boolean
    :synopsis: Generate documentation for executables.

    :default: False

    Run haddock on all executable programs.

    The command line variant of this flag is ``--executables`` (for the
    ``haddock`` subcommand).

.. cfg-field:: haddock-tests: boolean
    :synopsis: Generate documentation for tests.

    :default: False

    Run haddock on all test suites.

    The command line variant of this flag is ``--tests`` (for the
    ``haddock`` subcommand).

.. cfg-field:: haddock-benchmarks: boolean
    :synopsis: Generate documentation for benchmarks.

    :default: False

    Run haddock on all benchmarks.

    The command line variant of this flag is ``--benchmarks`` (for the
    ``haddock`` subcommand).

.. cfg-field:: haddock-all: boolean
    :synopsis: Generate documentation for everything

    :default: False

    Run haddock on all components.

    The command line variant of this flag is ``--all`` (for the
    ``haddock`` subcommand).

.. cfg-field:: haddock-internal: boolean
    :synopsis: Generate documentation for internal modules

    :default: False

    Build haddock documentation which includes unexposed modules and
    symbols.

    The command line variant of this flag is ``--internal`` (for the
    ``haddock`` subcommand).

.. cfg-field:: haddock-css: path
    :synopsis: Location of Haddoc CSS file.

    The CSS file that should be used to style the generated
    documentation (overriding haddock's default.)

    The command line variant of this flag is ``--css`` (for the
    ``haddock`` subcommand).

.. cfg-field:: haddock-hyperlink-source: boolean
    :synopsis: Generate hyperlinked source code for documentation

    :default: False

    Generated hyperlinked source code using `HsColour`_, and have
    Haddock documentation link to it.

    The command line variant of this flag is ``--hyperlink-source`` (for
    the ``haddock`` subcommand).

.. cfg-field:: haddock-hscolour-css: path
    :synopsis: Location of CSS file for HsColour

    The CSS file that should be used to style the generated hyperlinked
    source code (from `HsColour`_).

    The command line variant of this flag is ``--hscolour-css`` (for the
    ``haddock`` subcommand).

.. cfg-field:: haddock-contents-location: URL
    :synopsis: URL for contents page.

    A baked-in URL to be used as the location for the contents page.

    The command line variant of this flag is ``--contents-location``
    (for the ``haddock`` subcommand).

.. cfg-field:: haddock-keep-temp-files: boolean
    :synopsis: Keep temporary Haddock files.

    Keep temporary files.

    The command line variant of this flag is ``--keep-temp-files`` (for
    the ``haddock`` subcommand).

Advanced global configuration options
-------------------------------------

.. cfg-field:: write-ghc-environment-files: always, never, or ghc8.4.4+
               --write-ghc-environment-files=policy
    :synopsis: Whether a ``.ghc.environment`` should be created after a successful build.

    :default: ``never``

    Whether a `GHC package environment file <https://downloads.haskell.org/~ghc/master/users-guide/packages.html#package-environments>`_
    should be created after a successful build.

    Since Cabal 3.0, defaults to ``never``. Before that, defaulted to
    creating them only when compiling with GHC 8.4.4 and older (GHC
    8.4.4 `is the first version
    <https://ghc.haskell.org/trac/ghc/ticket/13753>`_ that supports
    the ``-package-env -`` option that allows ignoring the package
    environment files).


.. cfg-field:: http-transport: curl, wget, powershell, or plain-http
               --http-transport=transport
    :synopsis: Transport to use with http(s) requests.

    :default: ``curl``

    Set a transport to be used when making http(s) requests.

    The command line variant of this field is ``--http-transport=curl``.

.. cfg-field:: ignore-expiry: boolean
               --ignore-expiry
    :synopsis: Ignore Hackage expiration dates.

    :default: False

    If ``True``, we will ignore expiry dates on metadata from Hackage.

    In general, you should not set this to ``True`` as it will leave you
    vulnerable to stale cache attacks. However, it may be temporarily
    useful if the main Hackage server is down, and we need to rely on
    mirrors which have not been updated for longer than the expiry
    period on the timestamp.

    The command line variant of this field is ``--ignore-expiry``.

.. cfg-field:: remote-repo-cache: directory
               --remote-repo-cache=DIR
    :synopsis: Location of packages cache.

    :default: ``~/.cabal/packages``

    [STRIKEOUT:The location where packages downloaded from remote
    repositories will be cached.] Not implemented yet.

    The command line variant of this flag is
    ``--remote-repo-cache=DIR``.

.. cfg-field:: logs-dir: directory
               --logs-dir=DIR
    :synopsis: Directory to store build logs.

    :default: ``~/.cabal/logs``

    [STRIKEOUT:The location where build logs for packages are stored.]
    Not implemented yet.

    The command line variant of this flag is ``--logs-dir=DIR``.

.. cfg-field:: build-summary: template filepath
               --build-summary=TEMPLATE
    :synopsis: Build summaries location.

    :default: ``~/.cabal/logs/build.log``

    [STRIKEOUT:The file to save build summaries. Valid variables which
    can be used in the path are ``$pkgid``, ``$compiler``, ``$os`` and
    ``$arch``.] Not implemented yet.

    The command line variant of this flag is
    ``--build-summary=TEMPLATE``.

.. cfg-field:: local-repo: directory
               --local-repo=DIR
    :deprecated:

    [STRIKEOUT:The location of a local repository.] Deprecated. See
    "Legacy repositories."

    The command line variant of this flag is ``--local-repo=DIR``.

.. cfg-field:: world-file: path
               --world-file=FILE
    :deprecated:

    [STRIKEOUT:The location of the world file.] Deprecated.

    The command line variant of this flag is ``--world-file=FILE``.

Undocumented fields: ``root-cmd``, ``symlink-bindir``, ``build-log``,
``remote-build-reporting``, ``report-planned-failure``, ``one-shot``,
``offline``.

Advanced solver options
^^^^^^^^^^^^^^^^^^^^^^^

Most users generally won't need these.

.. cfg-field:: solver: modular
               --solver=modular
    :synopsis: Which solver to use.

    This field is reserved to allow the specification of alternative
    dependency solvers. At the moment, the only accepted option is
    ``modular``.

    The command line variant of this field is ``--solver=modular``.

.. cfg-field:: max-backjumps: nat
               --max-backjumps=N
    :synopsis: Maximum number of solver backjumps.

    :default: 4000

    Maximum number of backjumps (backtracking multiple steps) allowed
    while solving. Set -1 to allow unlimited backtracking, and 0 to
    disable backtracking completely.

    The command line variant of this field is ``--max-backjumps=4000``.

.. cfg-field:: reorder-goals: boolean
               --reorder-goals
               --no-reorder-goals
    :synopsis: Allow solver to reorder goals.

    :default: False

    When enabled, the solver will reorder goals according to certain
    heuristics. Slows things down on average, but may make backtracking
    faster for some packages. It's unlikely to help for small projects,
    but for big install plans it may help you find a plan when otherwise
    this is not possible. See :issue:`1780` for more commentary.

    The command line variant of this field is ``--(no-)reorder-goals``.

.. cfg-field:: count-conflicts: boolean
               --count-conflicts
               --no-count-conflicts
    :synopsis: Solver prefers versions with less conflicts.

    :default: True

    Try to speed up solving by preferring goals that are involved in a
    lot of conflicts.

    The command line variant of this field is
    ``--(no-)count-conflicts``.

.. cfg-field:: minimize-conflict-set: boolean
               --minimize-conflict-set
               --no-minimize-conflict-set
    :synopsis: Try to improve the solver error message when there is no
	       solution.

    :default: False

    When there is no solution, try to improve the solver error message
    by finding a minimal conflict set. This option may increase run
    time significantly, so it is off by default.

    The command line variant of this field is
    ``--(no-)minimize-conflict-set``.

.. cfg-field:: strong-flags: boolean
               --strong-flags
               --no-strong-flags
    :synopsis: Do not defer flag choices when solving.

    :default: False

    Do not defer flag choices. (TODO: Better documentation.)

    The command line variant of this field is ``--(no-)strong-flags``.

.. cfg-field:: allow-boot-library-installs: boolean
               --allow-boot-library-installs
               --no-allow-boot-library-installs
    :synopsis: Allow cabal to install or upgrade any package.

    :default: False

    By default, the dependency solver doesn't allow ``base``,
    ``ghc-prim``, ``integer-simple``, ``integer-gmp``, and
    ``template-haskell`` to be installed or upgraded. This flag
    removes the restriction.

    The command line variant of this field is
    ``--(no-)allow-boot-library-installs``.

.. cfg-field:: cabal-lib-version: version
               --cabal-lib-version=version
    :synopsis: Version of Cabal library used to build package.

    This field selects the version of the Cabal library which should be
    used to build packages. This option is intended primarily for
    internal development use (e.g., forcing a package to build with a
    newer version of Cabal, to test a new version of Cabal.) (TODO:
    Specify its semantics more clearly.)

    The command line variant of this field is
    ``--cabal-lib-version=1.24.0.1``.

.. include:: references.inc
