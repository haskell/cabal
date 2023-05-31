cabal.project Reference
=======================

``cabal.project`` files support a variety of options which configure the
details of your build. The general syntax of a ``cabal.project`` file is
similar to that of a Cabal file: there are a number of fields, some of
which live inside stanzas (groups of fields that apply to only part of a
project or can be referenced as a unit):

::

    packages: */*.cabal
    with-compiler: /opt/ghc/8.0.1/bin/ghc

    package cryptohash
      optimization: False

In general, the accepted field names coincide with the accepted command
line flags that ``cabal install`` and other commands take. For example,
``cabal configure --enable-profiling`` will write out a project
file with ``profiling: True``.

The full configuration of a project is determined by combining the
following sources (later entries override earlier ones, except for appendable
options):

1. :ref:`The user-wide global configuration <config-file-discovery>` (default: ``~/.config/cabal/config``)

2. ``cabal.project`` (the project configuration)

3. ``cabal.project.freeze`` (the output of ``cabal freeze``)

4. ``cabal.project.local`` (the output of ``cabal configure``)

Any call to ``cabal build`` will consider ``cabal.project*`` files from parent
directories when there is none in the current directory.

.. _conditionals and imports:

Conditionals and imports
------------------------

As of ``cabal-install`` version 3.8, cabal supports conditional logic
and imports in ``cabal.project`` files. :ref:`conditions` in cabal
may case on operating system, architecture, and
compiler (i.e. there is no support for a notion of custom flags in
project files). Imports may specify local filepaths or remote urls,
and may reference either cabal.project files or v1-style cabal.config
freeze files. As a usage example:

::

    if(os(darwin))
      optimization: False
    elif(os(freebsd))
      packages: freebsd/*.cabal
    else
      optimization: True

    import: https://some.remote.source/subdir/cabal.config

    import: relativepath/extra-project.project

    import: /absolutepath/some-project.project

Using conditionals will force cabal to find a ghc to derive
architecture and version information from, which will force some
commands (update, sdist) to require ghc present where otherwise it
would not be necessitated.

Specifying the local packages
-----------------------------

The following top-level options specify what the local packages of a
project are:

.. cfg-field:: packages: package location list (space or comma separated)
    :synopsis: Project packages.

    :default: ``./*.cabal``

    .. warning::

      The default value ``./*.cabal`` only takes effect if there is no explicit
      ``cabal.project`` file.
      If you use such explicit file you *must* fill the field.

    Specifies the list of package locations which contain the local
    packages to be built by this project. Package locations can take the
    following forms:

    1. They can specify a Cabal file, or a directory containing a Cabal
       file, e.g., ``packages: Cabal cabal-install/cabal-install.cabal``.

    2. They can specify glob-style wildcards, which must match one or
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
    Note that the default value is only included if there is no
    ``cabal.project`` file. The field is appendable which means there would be
    no way to drop the default value if it was included.

.. cfg-field:: optional-packages: package location list (space or comma-separated)
    :synopsis: Optional project packages.

    :default: empty

    Like :cfg-field:`packages`, specifies a list of package locations
    containing local packages to be built. Unlike :cfg-field:`packages`,
    if we glob for a package, it is permissible for the glob to match against
    zero packages. The intended use-case for :cfg-field:`optional-packages`
    is to make it so that vendored packages can be automatically picked up if
    they are placed in a subdirectory, but not error if there aren't any.

    There is no command line variant of this field.

.. cfg-field:: extra-packages: package list with version bounds (comma separated)
    :synopsis: Adds external packages as local

    Specifies a list of external packages from Hackage, which
    should be considered local packages. The motivation for
    :cfg-field:`extra-packages` is making libraries that are not
    dependencies of any package in the project available for use in ghci.

    There is no command line variant of this field.



All local packages are *vendored*, in the sense that if other packages
(including external ones from Hackage) depend on a package with the name
of a local package, the local package is preferentially used.
For subdirectories to be considered local packages, the following setting
can be used::

    packages: ./*.cabal
    optional-packages: ./*/*.cabal

...then any package can be vendored simply by making a checkout in the
top-level project directory, as might be seen in this hypothetical
directory layout::

    foo.cabal
    foo-helper/     # local package
    unix/           # vendored external package

All of these options support globs. ``cabal build`` has its own glob
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

Formally, the format is described by the following BNF:

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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Starting with Cabal 2.4, there is now a stanza
``source-repository-package`` for specifying packages from an external
version control.

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

    source-repository-package
        type: git
        location: https://github.com/haskell/network.git
        tag: e76fdc753e660dfa615af6c8b6a2ad9ddf6afe70
        post-checkout-command: autoreconf -i

cabal-install 3.4 sdists the ``source-repository-package`` repositories and uses resulting tarballs as project packages.
This allows sharing of packages across different projects.

.. cfg-field:: type: VCS kind

.. cfg-field:: location: VCS location (usually URL)

.. cfg-field:: tag: VCS tag

.. cfg-field:: subdir: subdirectory list

    Use one or more subdirectories of the repository.

.. cfg-field:: post-checkout-command: command

    Run command in the checked out repository, prior sdisting.

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

    This option can only be specified from the command line.

.. _cmdoption-project-dir:
.. option:: --project-dir=DIR

    Specifies the path of the project directory. If a relative
    :ref:`project-file<cmdoption-project-file>` path is also specified,
    it will be resolved relative to this directory.

    The project directory does not need to contain a ``cabal.project`` file.

    This option can only be specified from the command line.

.. _cmdoption-project-file:
.. option:: --project-file=FILE

    Specifies the path and name of the project file used to specify the
    rest of the top-level configuration; defaults to ``cabal.project``.
    This name not only specifies the name of the main project file,
    but also the auxiliary project files ``cabal.project.freeze``
    and ``cabal.project.local``; for example, if you specify
    ``--project-file=my.project``, then the other files that will
    be probed are ``my.project.freeze`` and ``my.project.local``.

    If :ref:`project-dir<cmdoption-project-dir>` is not specified,
    and the path is relative, we will
    look for the file relative to the current working directory,
    and then for the parent directory, until the project file is
    found or we have hit the top of the user's home directory.

    This option can only be specified from the command line.

.. option:: --ignore-project

    Ignores the local ``cabal.project`` file and uses the default
    configuration with the local ``foo.cabal`` file. Note that
    this flag will be ignored if either of the ``--project-dir`` or
    ``--project-file`` flags are also set.

.. option:: --store-dir=DIR

    Specifies the name of the directory of the global package store.

.. cfg-field:: package-dbs: package DB stack (comma separated)
               --package-db=[clear, global, user, PATH]
    :synopsis: PackageDB stack manipulation
    :since: 3.7

    There are three package databases involved with most builds:

    global
        Compiler installation of rts, base, etc.
    store
        Nix-style local build cache
    in-place
        Project-specific build directory

    By default, the package stack you will have with v2 commands is:

    ::

        -- [global, store]

    So all remote packages required by your project will be
    registered in the store package db (because it is last).

    When cabal starts building your local projects, it appends the in-place db
    to the end:

    ::

        -- [global, store, in-place]

    So your local packages get put in ``dist-newstyle`` instead of the store.

    This flag manipulates the default prefix: ``[global, store]`` and accepts
    paths, the special value ``global`` referring to the global package db, and
    ``clear`` which removes all prior entries. For example,

    ::

        -- [global, store, foo]
        package-dbs: foo

        -- [foo]
        package-dbs: clear, foo

        -- [bar, baz]
        package-dbs: clear, foo, clear, bar, baz

    The command line variant of this flag is ``--package-db=DB`` which can be
    specified multiple times.

Phase control
-------------

The following settings apply to commands that result in build actions
(``build``, ``run``, ``repl``, ``test``...), and control which phases of the
build are executed.

.. option:: --dry-run

    Do not download, build, or install anything, only print what would happen.

.. option:: --only-configure

    Instead of performing a full build just run the configure step.
    Only accepted by the ``build`` command.

.. option:: --only-download

    Do not build anything, only fetch the packages.

.. option:: --only-dependencies

    Install only the dependencies necessary to build the given packages.
    Not accepted by the ``repl`` command.

Solver configuration options
----------------------------

The following settings control the behavior of the dependency solver:

.. cfg-field:: constraints: constraints list (comma separated)
               --constraint="pkg >= 2.0", -c "pkg >= 2.0"
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

    Valid constraints take the same form as for the
    :option:`runhaskell Setup.hs configure --constraint`
    command line option.

.. cfg-field:: preferences: preference (comma separated)
               --preference="pkg >= 2.0"
    :synopsis: Preferred dependency versions.

    Like :cfg-field:`constraints`, but the solver will attempt to satisfy
    these preferences on a best-effort basis. The resulting install is locally
    optimal with respect to preferences; specifically, no single package
    could be replaced with a more preferred version that still satisfies
    the hard constraints.

    Operationally, preferences can cause the solver to attempt certain
    version choices of a package before others, which can improve
    dependency solver runtime.

    One way to use :cfg-field:`preferences` is to take a known working set of
    constraints (e.g., via ``cabal freeze``) and record them as
    preferences. In this case, the solver will first attempt to use this
    configuration, and if this violates hard constraints, it will try to
    find the minimal number of upgrades to satisfy the hard constraints
    again.

    The command line variant of this field is
    ``--preference="pkg >= 2.0"``; to specify multiple preferences, pass
    the flag multiple times.

.. cfg-field:: allow-newer: none, all or list of scoped package names (space or comma separated)
               --allow-newer, --allow-newer=[none,all,[scope:][^]pkg]
    :synopsis: Lift dependencies upper bound constraints.

    :default: ``none``

    Allow the solver to pick more recent version of some packages than
    would normally be permitted by the :pkg-field:`build-depends` bounds
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
    (in the :cfg-field:`constraints` field) forcing the usage of a specific,
    newer version of a package.

    The command line variant of this field is e.g. ``--allow-newer=bar``. A
    bare ``--allow-newer`` is equivalent to ``--allow-newer=all``.

.. cfg-field:: allow-older: none, all, list of scoped package names (space or comma separated)
               --allow-older, --allow-older=[none,all,[scope:][^]pkg]
    :synopsis: Lift dependency lower bound constraints.
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
      -- This format is used by 'cabal configure'
      -- for storing `--index-state` values.
      index-state: 2016-09-24T17:47:48Z

      -- Specify different index-states per package repository
      -- Supported since 3.4
      index-state:
        , hackage.haskell.org 2020-05-06T22:33:27Z
        , head.hackage 2020-04-29T04:11:05Z

.. cfg-field:: active-repositories: reponame1, reponame2

    :synopsis: Specify active package repositories
    :since: 3.4

    :default: ``:rest``

    Specifies which of the package repositories defined in the configuration
    should be active. It's also useful for specifying the order and the way
    active repositories are merged.

    When searching for a certain version of a certain package name, the list of
    active repositories is searched last-to-first.

    For example, suppose hackage.haskell.org has versions 1.0 and 2.0 of
    package X, and my-repository has version 2.0 of a similarly named package.
    Then, with the following configuration:

    ::

      -- Force my-repository to be the first repository considered
      active-repositories:
        , hackage.haskell.org
        , my-repository

    version 2.0 of X will come from my-repository, and version 1.0 will come
    from hackage.haskell.org.

    If we want to make a repository the sole provider of certain packages, we
    can put it last in the active repositories list, and add the :override
    modifier.

    For example, if we modify the previous example like this:

    ::

      active-repositories:
        , hackage.haskell.org
        , my-repository:override

    then version 1.0 of package X won't be found in any case, because X is
    present in my-repository only in version 2.0, and the :override forbids
    searching for other versions of X further up the list.

    :override has no effect for package names that aren't present in the
    overriding repository.

    The special repository reference :rest stands for "all the other repositories"
    and can be useful to avoid lengthy lists of repository names:

    ::

      -- Force my-repository to be the first repository considered
      active-repositories: :rest, my-repository

    The special repository reference :none disables all repositories, effectively
    putting cabal in "offline" mode:

    ::

      active-repositories: :none


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

``ghc-options`` is not specifically described in this documentation, but is one
of many fields for configuring programs.  They take the form
``progname-options`` and ``progname-location``, and can be set for all local
packages in a ``program-options`` stanza or under a package stanza.

On the command line, these options are applied to all local packages.
There is no per-package command line interface.

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

    Exactly one of + or - is required before each flag.

    Flags are *per-package*, so it doesn't make much sense to specify
    flags at the top-level, unless you happen to know that *all* of your
    local packages support the same named flags. If a flag is not
    supported by a package, it is ignored.

    See also the solver configuration field :cfg-field:`constraints`.

    The command line variant of this flag is ``--flags``. There is also
    a shortened form ``-ffoo -f-bar``.

    A common mistake is to say ``cabal build -fhans``, where
    ``hans`` is a flag for a transitive dependency that is not in the
    local package; in this case, the flag will be silently ignored. If
    ``haskell-tor`` is the package you want this flag to apply to, try
    ``--constraint="haskell-tor +hans"`` instead.

.. cfg-field:: with-compiler: executable
               --with-compiler=executable
    :synopsis: Path to compiler executable.

    Specify the path to a particular compiler to be used. If not an
    absolute path, it will be resolved according to the ``PATH``
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

    For inplace packages, ``cabal build`` maintains a separate build
    directory for each version of GHC, so you can maintain multiple
    build trees for different versions of GHC without clobbering each
    other.

    It's not possible to set :cfg-field:`with-compiler` on a
    per-package basis.

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
    :ref:`system-dependent parameters`.

    The command line variant of this flag is ``--configure-option=arg``,
    which can be specified multiple times to pass multiple options.

.. cfg-field:: compiler: ghc, ghcjs, jhc, lhc, uhc or haskell-suite
               --compiler=compiler
    :synopsis: Compiler to build with.

    :default: ``ghc``

    Specify the compiler toolchain to be used. This is independent of
    ``with-compiler``, because the choice of toolchain affects Cabal's
    build logic.

    The command line variant of this flag is ``--compiler=ghc``.

    It's not possible to set :cfg-field:`compiler` on a
    per-package basis.

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

.. _cmdoption-extra-prog-path:
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

    When specifying :ref:`--http-transport<cmdoption-http-transport>` from the
    command line, only extra-prog-path from the command line are added to the
    program search path.

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

    If the compiler (e.g., GHC 7.10 and later) supports outputting OS
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

    If ``debug-info`` is set explicitly then ``executable-stripping`` is set
    to ``False`` as otherwise all the debug symbols will be stripped.

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

    If ``debug-info`` is set explicitly then ``library-stripping`` is set
    to ``False`` as otherwise all the debug symbols will be stripped.

    The command line variant of this flag is
    ``--enable-library-stripping`` and ``--disable-library-stripping``.

Executable options
^^^^^^^^^^^^^^^^^^

.. cfg-field:: program-prefix: prefix
               --program-prefix=prefix
    :synopsis: Prepend prefix to program names.

    :strike:`Prepend *prefix* to installed program names.` (Currently
    implemented in a silly and not useful way. If you need this to work
    give us a shout.)

    *prefix* may contain the following path variables: ``$pkgid``,
    ``$pkg``, ``$version``, ``$compiler``, ``$os``, ``$arch``, ``$abi``,
    ``$abitag``

    The command line variant of this flag is ``--program-prefix=foo-``.

.. cfg-field:: program-suffix: suffix
               --program-suffix=suffix
    :synopsis: Append refix to program names.

    :strike:`Append *suffix* to installed program names.` (Currently
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

    :strike:`Build a package which is relocatable.` (TODO: It is not
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
    This links all dependent libraries into executables statically,
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
    want to profile; ``cabal build`` will ensure that all of its
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
    late-toplevel
        Like top-level but costs will be assigned to top level definitions after
        optimization. This lowers profiling overhead massively while giving similar
        levels of detail as toplevle-functions. However it means functions introduced
        by GHC during optimization will show up in profiles as well.
        Corresponds to ``-fprof-late`` if supported and ``-fprof-auto-top`` otherwise.
    late
        Currently an alias for late-toplevel

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

    Enables building of Haddock documentation.
    Implied when calling ``cabal haddock``.

    The command line variant of this flag is ``--enable-documentation``
    and ``--disable-documentation``.

    ``documentation: true`` does not imply
    :cfg-field:`haddock-all`,
    :cfg-field:`haddock-benchmarks`,
    :cfg-field:`haddock-executables`,
    :cfg-field:`haddock-internal` or
    :cfg-field:`haddock-tests`.
    These need to be enabled separately if desired.

.. cfg-field:: doc-index-file: templated path
               --doc-index-file=TEMPLATE
    :synopsis: Path to haddock templates.

    A central index of Haddock API documentation (template cannot use
    ``$pkgid``), which should be updated as documentation is built.

The following commands are equivalent to ones that would be passed when
running ``setup haddock``.

.. cfg-field:: haddock-hoogle: boolean
               --haddock-hoogle
    :synopsis: Generate Hoogle file.

    :default: False

    Generate a text file which can be converted by Hoogle_
    into a database for searching.
    This is equivalent to running ``haddock`` with the ``--hoogle`` flag.

.. cfg-field:: haddock-html: boolean
               --haddock-html
    :synopsis: Build HTML documentation.

    :default: True

    Build HTML documentation.

.. cfg-field:: haddock-quickjump: boolean
               --haddock-quickjump
    :synopsis: Generate Quickjump file.

    :default: False

    Generate an index for interactive documentation navigation.
    This is equivalent to running ``haddock`` with the ``--quickjump`` flag.

.. cfg-field:: haddock-html-location: templated path
               --haddock-html-location=TEMPLATE
    :synopsis: Haddock HTML templates location.

    Specify a template for the location of HTML documentation for
    prerequisite packages. The substitutions are applied to the template
    to obtain a location for each package, which will be used by
    hyperlinks in the generated documentation. For example, the
    following command generates links pointing at Hackage pages:

    ::

        html-location: http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html

    If passed on the command line,
    the argument may be quoted to prevent substitution by the shell.

    ::

        --html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html'

    If this option is omitted, the location for each package is obtained
    using the package tool (e.g. ``ghc-pkg``).

.. cfg-field:: haddock-executables: boolean
               --haddock-executables
    :synopsis: Generate documentation for executables.

    :default: False

    Run haddock on all executable programs.

.. cfg-field:: haddock-tests: boolean
               --haddock-tests
    :synopsis: Generate documentation for tests.

    :default: False

    Run haddock on all test suites.

.. cfg-field:: haddock-benchmarks: boolean
               --haddock-benchmarks
    :synopsis: Generate documentation for benchmarks.

    :default: False

    Run haddock on all benchmarks.

.. cfg-field:: haddock-internal: boolean
               --haddock-internal
    :synopsis: Generate documentation for internal modules

    :default: False

    Build haddock documentation which includes unexposed modules and
    symbols.

.. cfg-field:: haddock-all: boolean
               --haddock-all
    :synopsis: Generate documentation for everything

    :default: False

    Run haddock on all components.

.. cfg-field:: haddock-css: path
               --haddock-css=PATH
    :synopsis: Location of Haddock CSS file.

    The CSS file that should be used to style the generated
    documentation (overriding haddock's default).

.. cfg-field:: haddock-hyperlink-source: boolean
               --haddock-hyperlink-source
    :synopsis: Generate hyperlinked source code for documentation

    :default: False

    Generated hyperlinked source code using `HsColour`_, and have
    Haddock documentation link to it.
    This is equivalent to running ``haddock`` with the ``--hyperlinked-source`` flag.

.. cfg-field:: haddock-hscolour-css: path
               --haddock-hscolour-css=PATH
    :synopsis: Location of CSS file for HsColour

    The CSS file that should be used to style the generated hyperlinked
    source code (from `HsColour`_).

.. cfg-field:: haddock-contents-location: URL
               --haddock-contents-location=URL
    :synopsis: URL for contents page.

    A baked-in URL to be used as the location for the contents page.

.. cfg-field:: haddock-keep-temp-files: boolean
    :synopsis: Keep temporary Haddock files.

    Keep temporary files.

    There is no command line variant of this flag.

.. cfg-field:: haddock-output-dir: path
               --haddock-output-dir=PATH
    :synopsis: Generate haddock documentation into this directory.

    Generate haddock documentation into this directory instead of the default
    location next to other build products.

    This flag is provided as a technology preview and is subject to change in the
    next releases.

.. cfg-field:: open: boolean
               --open
    :synopsis: Open generated documentation in-browser.

    When generating HTML documentation, attempt to open it in a browser
    when complete. This will use ``xdg-open`` on Linux and BSD systems,
    ``open`` on macOS, and ``start`` on Windows.

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
    <https://gitlab.haskell.org/ghc/ghc/-/issues/13753>`_ that supports
    the ``-package-env -`` option that allows ignoring the package
    environment files).

.. cfg-field:: build-info: True, False
               --enable-build-info
               --disable-build-info
    :synopsis: Whether build information for each individual component should be
               written in a machine readable format.

    :default: ``False``

    Enable generation of build information for Cabal components. Contains very
    detailed information on how to build an individual component, such as
    compiler version, modules of a component and how to compile the component.

    The output format is in json, and the exact location can be discovered from
    ``plan.json``, where it is identified by ``build-info`` within the items in
    the ``install-plan``.
    Note, that this field in ``plan.json`` can be ``null``, if and only if
    ``build-type: Custom`` is set, and the ``Cabal`` version is too
    old (i.e. ``< 3.7``).
    If the field is missing entirely, the component is not a local one, thus,
    no ``build-info`` exists for that particular component within the
    ``install-plan``.

    .. note::
        The format and fields of the generated build information is currently experimental,
        in the future we might add or remove fields, depending on the needs of other tooling.

.. _cmdoption-http-transport:
.. cfg-field:: http-transport: curl, wget, powershell, or plain-http
               --http-transport=transport
    :synopsis: Transport to use with http(s) requests.

    :default: ``curl``

    Set a transport to be used when making http(s) requests.

    The command line variant of this field is ``--http-transport=curl``.

    If the project configuration imports remote urls, the user can only specify
    the http-transport option from the command line.

    When specifying the http-transport from the command line, the program
    search path can only be influenced using :ref:`--extra-prog-path<cmdoption-extra-prog-path>`.

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

    The location where packages downloaded from remote repositories will be
    cached.

    The command line variant of this flag is
    ``--remote-repo-cache=DIR``.

.. cfg-field:: logs-dir: directory
               --logs-dir=DIR
    :synopsis: Directory to store build logs.

    :default: ``~/.cabal/logs``

    :strike:`The location where build logs for packages are stored.`
    Not implemented yet.

    The command line variant of this flag is ``--logs-dir=DIR``.

.. cfg-field:: build-summary: template filepath
               --build-summary=TEMPLATE
    :synopsis: Build summaries location.

    :default: ``~/.cabal/logs/build.log``

    :strike:`The file to save build summaries.` Not implemented yet.

    Valid variables which can be used in the path are ``$pkgid``,
    ``$compiler``, ``$os`` and ``$arch``.

    The command line variant of this flag is
    ``--build-summary=TEMPLATE``.

Undocumented fields: ``root-cmd``, ``symlink-bindir``, ``build-log``,
``remote-build-reporting``, ``report-planned-failure``, ``offline``.

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

.. cfg-field:: fine-grained-conflicts: boolean
               --fine-grained-conflicts
               --no-fine-grained-conflicts
    :synopsis: Skip a version of a package if it does not resolve any conflicts
               encountered in the last version (solver optimization).

    :default: True

    When enabled, the solver will skip a version of a package if it does not
    resolve any of the conflicts encountered in the last version of that
    package. For example, if ``foo-1.2`` depended on ``bar``, and the solver
    couldn't find consistent versions for ``bar``'s dependencies, then the
    solver would skip ``foo-1.1`` if it also depended on ``bar``.

    The command line variant of this field is
    ``--(no-)fine-grained-conflicts``.

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

.. cfg-field:: prefer-oldest: boolean
               --prefer-oldest
               --no-prefer-oldest
    :synopsis: Prefer the oldest versions of packages available.
    :since:    3.10

    :default:  False

    By default, when solver has a choice of multiple versions of the same
    package, it will first try to derive a build plan with the latest
    version. This flag switches the behaviour, making the solver
    to prefer the oldest packages available.

    The primary use case is to help users in establishing lower bounds
    of upstream dependencies.

    The command line variant of this field is ``--(no-)prefer-oldest``.

.. include:: references.inc
