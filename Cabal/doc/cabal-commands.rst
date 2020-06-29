cabal-install Commands
======================

We now give an in-depth description of all the commands, describing the
arguments and flags they accept.

cabal v2-configure
-------------------

``cabal v2-configure`` takes a set of arguments and writes a
``cabal.project.local`` file based on the flags passed to this command.
``cabal v2-configure FLAGS; cabal v2-build`` is roughly equivalent to
``cabal v2-build FLAGS``, except that with ``v2-configure`` the flags
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

-  A module target: ``[package:][ctype:]module``, which specifies that the
   component of which the given module is a part of will be built.

-  A filepath target: ``[package:][ctype:]filepath``, which specifies that the
   component of which the given filepath is a part of will be built.

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
    $ cabal v2-build src/Lib.s         # build the library component to
                                       # which "src/Lib.hs" belongs
    $ cabal v2-build app/Main.hs       # build the executable component of
                                       # "app/Main.hs"
    $ cabal v2-build Lib               # build the library component to
                                       # which the module "Lib" belongs

Beyond a list of targets, ``cabal v2-build`` accepts all the flags that
``cabal v2-configure`` takes. Most of these flags are only taken into
consideration when building local packages; however, some flags may
cause extra store packages to be built (for example,
``--enable-profiling`` will automatically make sure profiling libraries
for all transitive dependencies are built and installed.)

In addition ``cabal v2-build`` accepts these flags:

- ``--only-configure``: When given we will forego performing a full build and
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

See `the v2-build section <#cabal-v2-build>`__ for the target syntax.

Except in the case of the empty target, the strings after it will be
passed to the executable as arguments.

If one of the arguments starts with ``-`` it will be interpreted as
a cabal flag, so if you need to pass flags to the executable you
have to separate them with ``--``.

::

    $ cabal v2-run target -- -a -bcd --argument

``v2-run`` also supports running script files that use a certain format. With
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
the versions and flags that are picked by the solver under the
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

Where symlinking is not possible (eg. on some Windows versions) the ``copy``
method is used by default. You can specify the install method
by using ``--install-method`` flag:

::

    $ cabal v2-install exe:cabal --install-method=copy --installdir=$HOME/bin

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

This command will modify the environment in the ``local.env`` file in the
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

- ``-o``, ``--output-directory``: Sets the output dir, if a non-default one is desired. The default is
  ``dist-newstyle/sdist/``. ``--output-directory -`` will send output to ``stdout``
  unless multiple archives are being created.

- ``--null-sep``: Only used with ``--list-only``. Separates filenames with a NUL
  byte instead of newlines.

``v2-sdist`` is inherently incompatible with sdist hooks (which were removed in `Cabal-3.0`),
not due to implementation but due to fundamental core invariants
(same source code should result in the same tarball, byte for byte)
that must be satisfied for it to function correctly in the larger v2-build ecosystem.
``autogen-modules`` is able to replace uses of the hooks to add generated modules, along with
the custom publishing of Haddock documentation to Hackage.

.. warning::

  Packages that use Backpack will stop working if uploaded to
  Hackage, due to `issue #6005 <https://github.com/haskell/cabal/issues/6005>`_.
  While this is happening, we recommend not uploading these packages
  to Hackage (and instead referencing the package directly
  as a ``source-repository-package``).
