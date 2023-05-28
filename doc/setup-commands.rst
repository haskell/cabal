.. _setup-commands:

Setup.hs Commands
=================

.. highlight:: console

GHC provides the commands ``runhaskell`` and ``runghc`` (they are equivalent)
to allow you to run Haskell programs without first having to compile them
(scripts). The low-level Cabal interface is implemented using ``Setup.hs``
scripts. You should prefer using higher level interface provided by nix-style
builds. However, the documentation of the low level interface below may be helpful
to high level interface users as well, because it delves into internal details
common to both and omitted elsewhere.

::

    $ runhaskell Setup.hs [command] [option...]

For the summary of the ``Setup.hs`` script's command syntax, run:

::

    $ runhaskell Setup.hs --help

Building and installing a system package
----------------------------------------

::

    $ runhaskell Setup.hs configure --ghc
    $ runhaskell Setup.hs build
    $ runhaskell Setup.hs install

The first line readies the system to build the tool using GHC; for
example, it checks that GHC exists on the system. The second line
performs the actual building, while the last both copies the build
results to some permanent place and registers the package with GHC.

.. note ::

    Global installing of packages is not recommended.
    The :ref:`nix-style-builds` is the preferred way of building and installing
    packages.

Creating a binary package
-------------------------

When creating binary packages (e.g. for Red Hat or Debian) one needs to
create a tarball that can be sent to another system for unpacking in the
root directory:

::

    $ runhaskell Setup.hs configure --prefix=/usr
    $ runhaskell Setup.hs build
    $ runhaskell Setup.hs copy --destdir=/tmp/mypkg
    $ tar -czf mypkg.tar.gz /tmp/mypkg/

If the package contains a library, you need two additional steps:

::

    $ runhaskell Setup.hs register --gen-script
    $ runhaskell Setup.hs unregister --gen-script

This creates shell scripts ``register.sh`` and ``unregister.sh``, which
must also be sent to the target system. After unpacking there, the
package must be registered by running the ``register.sh`` script. The
``unregister.sh`` script would be used in the uninstall procedure of the
package. Similar steps may be used for creating binary packages for
Windows.

The following options are understood by all commands:

.. program:: setup

.. option:: --help, -h or -?

    List the available options for the command.

.. option:: --verbose=n or -v n

    Set the verbosity level (0-3). The normal level is 1; a missing *n*
    defaults to 2.

    There is also an extended version of this command which can be
    used to fine-tune the verbosity of output.  It takes the
    form ``[silent|normal|verbose|debug]``\ *flags*, where *flags*
    is a list of ``+`` flags which toggle various aspects of
    output.  At the moment, only ``+callsite`` and ``+callstack``
    are supported, which respectively toggle call site and call
    stack printing (these are only supported if Cabal
    is built with a sufficiently recent GHC.)

The various commands and the additional options they support are
described below. In the simple build infrastructure, any other options
will be reported as errors.

.. _setup-configure:

runhaskell Setup.hs configure
-----------------------------

.. program:: runhaskell Setup.hs configure

Prepare to build the package. Typically, this step checks that the
target platform is capable of building the package, and discovers
platform-specific features that are needed during the build.

The user may also adjust the behaviour of later stages using the options
listed in the following subsections. In the simple build infrastructure,
the values supplied via these options are recorded in a private file
read by later stages.

If a user-supplied ``configure`` script is run (see the section on
:ref:`system-dependent parameters` or
on :ref:`more-complex-packages`), it is
passed the :option:`--with-hc-pkg`, :option:`--prefix`, :option:`--bindir`,
:option:`--libdir`, :option:`--dynlibdir`, :option:`--datadir`, :option:`--libexecdir` and
:option:`--sysconfdir` options. In addition the value of the
:option:`--with-compiler` option is passed in a :option:`--with-hc-pkg` option
and all options specified with :option:`--configure-option` are passed on.

.. note::
   `GNU autoconf places restrictions on paths, including the directory
   that the package is built from.
   <https://www.gnu.org/software/autoconf/manual/autoconf.html#File-System-Conventions>`_
   The errors produced when this happens can be obscure; Cabal attempts to
   detect and warn in this situation, but it is not perfect.

In Cabal 2.0, support for a single positional argument was added to
``runhaskell Setup.hs configure`` This makes Cabal configure the specific component to
be configured. Specified names can be qualified with ``lib:`` or
``exe:`` in case just a name is ambiguous (as would be the case for a
package named ``p`` which has a library and an executable named ``p``.)
This has the following effects:

-  Subsequent invocations of ``cabal build``, ``register``, etc. operate only
   on the configured component.

-  Cabal requires all "internal" dependencies (e.g., an executable
   depending on a library defined in the same package) must be found in
   the set of databases via :option:`--package-db` (and related flags): these
   dependencies are assumed to be up-to-date. A dependency can be
   explicitly specified using :option:`--dependency` simply by giving the name
   of the internal library; e.g., the dependency for an internal library
   named ``foo`` is given as
   ``--dependency=pkg-internal=pkg-1.0-internal-abcd``.

-  Only the dependencies needed for the requested component are
   required. Similarly, when :option:`--exact-configuration` is specified,
   it's only necessary to specify :option:`--dependency` for the component.
   (As mentioned previously, you *must* specify internal dependencies as
   well.)

-  Internal ``build-tool-depends`` and ``build-tools`` dependencies are expected
   to be in the ``PATH`` upon subsequent invocations of ``setup``.

Full details can be found in the `Componentized Cabal
proposal <https://github.com/ezyang/ghc-proposals/blob/master/proposals/0000-componentized-cabal.rst>`__.

Programs used for building
^^^^^^^^^^^^^^^^^^^^^^^^^^

The following options govern the programs used to process the source
files of a package:

.. option:: --ghc or -g, --jhc, --lhc, --uhc

    Specify which Haskell implementation to use to build the package. At
    most one of these flags may be given. If none is given, the
    implementation under which the setup script was compiled or
    interpreted is used.

.. option:: --with-compiler=path or -w *path*

    Specify the path to a particular compiler. If given, this must match
    the implementation selected above. The default is to search for the
    usual name of the selected implementation.

    This flag also sets the default value of the :option:`--with-hc-pkg`
    option to the package tool for this compiler. Check the output of
    ``runhaskell Setup.hs configure -v`` to ensure that it finds the right package
    tool (or use :option:`--with-hc-pkg` explicitly).

.. option:: --with-hc-pkg=path

    Specify the path to the package tool, e.g. ``ghc-pkg``. The package
    tool must be compatible with the compiler specified by
    :option:`--with-compiler`. If this option is omitted, the default value is
    determined from the compiler selected.

.. option:: --with-prog=path

    Specify the path to the program *prog*. Any program known to Cabal
    can be used in place of *prog*. It can either be a fully path or the
    name of a program that can be found on the program search path. For
    example: ``--with-ghc=ghc-6.6.1`` or
    ``--with-cpphs=/usr/local/bin/cpphs``. The full list of accepted
    programs is not enumerated in this user guide. Rather, run
    ``cabal install --help`` to view the list.

.. option:: --prog-options=options

    Specify additional options to the program *prog*. Any program known
    to Cabal can be used in place of *prog*. For example:
    ``--alex-options="--template=mytemplatedir/"``. The *options* is
    split into program options based on spaces. Any options containing
    embedded spaced need to be quoted, for example
    ``--foo-options='--bar="C:\Program File\Bar"'``. As an alternative
    that takes only one option at a time but avoids the need to quote,
    use :option:`--prog-option` instead.

.. option:: --prog-option=option

    Specify a single additional option to the program *prog*. For
    passing an option that contains embedded spaces, such as a file name
    with embedded spaces, using this rather than :option:`--prog-options`
    means you do not need an additional level of quoting. Of course if you
    are using a command shell you may still need to quote, for example
    ``--foo-options="--bar=C:\Program File\Bar"``.

All of the options passed with either :option:`--prog-options`
or :option:`--prog-option` are passed in the order they were
specified on the configure command line.

Installation paths
^^^^^^^^^^^^^^^^^^

The following options govern the location of installed files from a
package:

.. option:: --prefix=dir

    The root of the installation. For example for a global install you
    might use ``/usr/local`` on a Unix system, or ``C:\Program Files``
    on a Windows system. The other installation paths are usually
    subdirectories of *prefix*, but they don't have to be.

    In the simple build system, *dir* may contain the following path
    variables: ``$pkgid``, ``$pkg``, ``$version``, ``$compiler``,
    ``$os``, ``$arch``, ``$abi``, ``$abitag``

.. option:: --bindir=dir

    Executables that the user might invoke are installed here.

    In the simple build system, *dir* may contain the following path
    variables: ``$prefix``, ``$pkgid``, ``$pkg``, ``$version``,
    ``$compiler``, ``$os``, ``$arch``, ``$abi``, ``$abitag``

.. option:: --libdir=dir

    Object-code libraries are installed here.

    In the simple build system, *dir* may contain the following path
    variables: ``$prefix``, ``$bindir``, ``$pkgid``, ``$pkg``,
    ``$version``, ``$compiler``, ``$os``, ``$arch``, ``$abi``,
    ``$abitag``

.. option:: --dynlibdir=dir

    Dynamic libraries are installed here.

    By default, this is set to `$libdir/$abi`, which is usually not equal to
    `$libdir/$libsubdir`.

    In the simple build system, *dir* may contain the following path
    variables: ``$prefix``, ``$bindir``, ``$libdir``, ``$pkgid``, ``$pkg``,
    ``$version``, ``$compiler``, ``$os``, ``$arch``, ``$abi``,
    ``$abitag``

.. option:: --libexecdir=dir

    Executables that are not expected to be invoked directly by the user
    are installed here.

    In the simple build system, *dir* may contain the following path
    variables: ``$prefix``, ``$bindir``, ``$libdir``, ``$libsubdir``,
    ``$pkgid``, ``$pkg``, ``$version``, ``$compiler``, ``$os``,
    ``$arch``, ``$abi``, ``$abitag``

.. option:: --datadir=dir

    Architecture-independent data files are installed here.

    In the simple build system, *dir* may contain the following path
    variables: ``$prefix``, ``$bindir``, ``$libdir``, ``$libsubdir``,
    ``$pkgid``, ``$pkg``, ``$version``, ``$compiler``, ``$os``,
    ``$arch``, ``$abi``, ``$abitag``

.. option:: --sysconfdir=dir

    Installation directory for the configuration files.

    In the simple build system, *dir* may contain the following path
    variables: ``$prefix``, ``$bindir``, ``$libdir``, ``$libsubdir``,
    ``$pkgid``, ``$pkg``, ``$version``, ``$compiler``, ``$os``,
    ``$arch``, ``$abi``, ``$abitag``

In addition the simple build system supports the following installation
path options:

.. option:: --libsubdir=dir

    A subdirectory of *libdir* in which libraries are actually installed. For
    example, in the simple build system on Unix, the default *libdir* is
    ``/usr/local/lib``, and *libsubdir* contains the compiler ABI and package
    identifier,
    e.g. ``x86_64-linux-ghc-8.0.2/mypkg-0.1.0-IxQNmCA7qrSEQNkoHSF7A``, so
    libraries would be installed in
    ``/usr/local/lib/x86_64-linux-ghc-8.0.2/mypkg-0.1.0-IxQNmCA7qrSEQNkoHSF7A/``.

    *dir* may contain the following path variables: ``$pkgid``,
    ``$pkg``, ``$version``, ``$compiler``, ``$os``, ``$arch``, ``$abi``,
    ``$abitag``

.. option:: --libexecsubdir=dir

    A subdirectory of *libexecdir* in which private executables are
    installed. For example, in the simple build system on Unix, the default
    *libexecdir* is ``/usr/local/libexec``, and *libsubdir* is
    ``x86_64-linux-ghc-8.0.2/mypkg-0.1.0``, so private executables would be
    installed in ``/usr/local/libexec/x86_64-linux-ghc-8.0.2/mypkg-0.1.0/``

    *dir* may contain the following path variables: ``$pkgid``,
    ``$pkg``, ``$version``, ``$compiler``, ``$os``, ``$arch``, ``$abi``,
    ``$abitag``

.. option:: --datasubdir=dir

    A subdirectory of *datadir* in which data files are actually
    installed.

    *dir* may contain the following path variables: ``$pkgid``,
    ``$pkg``, ``$version``, ``$compiler``, ``$os``, ``$arch``, ``$abi``,
    ``$abitag``

.. option:: --docdir=dir

    Documentation files are installed relative to this directory.

    *dir* may contain the following path variables: ``$prefix``,
    ``$bindir``, ``$libdir``, ``$libsubdir``, ``$datadir``,
    ``$datasubdir``, ``$pkgid``, ``$pkg``, ``$version``, ``$compiler``,
    ``$os``, ``$arch``, ``$abi``, ``$abitag``

.. option:: --htmldir=dir

    HTML documentation files are installed relative to this directory.

    *dir* may contain the following path variables: ``$prefix``,
    ``$bindir``, ``$libdir``, ``$libsubdir``, ``$datadir``,
    ``$datasubdir``, ``$docdir``, ``$pkgid``, ``$pkg``, ``$version``,
    ``$compiler``, ``$os``, ``$arch``, ``$abi``, ``$abitag``

.. option:: --program-prefix=prefix

    Prepend *prefix* to installed program names.

    *prefix* may contain the following path variables: ``$pkgid``,
    ``$pkg``, ``$version``, ``$compiler``, ``$os``, ``$arch``, ``$abi``,
    ``$abitag``

.. option:: --program-suffix=suffix

    Append *suffix* to installed program names. The most obvious use for
    this is to append the program's version number to make it possible
    to install several versions of a program at once:
    ``--program-suffix='$version'``.

    *suffix* may contain the following path variables: ``$pkgid``,
    ``$pkg``, ``$version``, ``$compiler``, ``$os``, ``$arch``, ``$abi``,
    ``$abitag``

Path variables in the simple build system
"""""""""""""""""""""""""""""""""""""""""

For the simple build system, there are a number of variables that can be
used when specifying installation paths. The defaults are also specified
in terms of these variables. A number of the variables are actually for
other paths, like ``$prefix``. This allows paths to be specified
relative to each other rather than as absolute paths, which is important
for building relocatable packages (see :ref:`prefix independence`).

$prefix
    The path variable that stands for the root of the installation. For
    an installation to be relocatable, all other installation paths must
    be relative to the ``$prefix`` variable.
$bindir
    The path variable that expands to the path given by the :option:`--bindir`
    configure option (or the default).
$libdir
    As above but for :option:`--libdir`
$libsubdir
    As above but for :option:`--libsubdir`
$dynlibdir
    As above but for :option:`--dynlibdir`
$datadir
    As above but for :option:`--datadir`
$datasubdir
    As above but for :option:`--datasubdir`
$docdir
    As above but for :option:`--docdir`
$pkgid
    The name and version of the package, e.g. ``mypkg-0.2``
$pkg
    The name of the package, e.g. ``mypkg``
$version
    The version of the package, e.g. ``0.2``
$compiler
    The compiler being used to build the package, e.g. ``ghc-6.6.1``
$os
    The operating system of the computer being used to build the
    package, e.g. ``linux``, ``windows``, ``osx``, ``freebsd`` or
    ``solaris``
$arch
    The architecture of the computer being used to build the package,
    e.g. ``i386``, ``x86_64``, ``ppc`` or ``sparc``
$abitag
    An optional tag that a compiler can use for telling incompatible
    ABI's on the same architecture apart. GHCJS encodes the underlying
    GHC version in the ABI tag.
$abi
    A shortcut for getting a path that completely identifies the
    platform in terms of binary compatibility. Expands to the same value
    as ``$arch-$os-compiler-$abitag`` if the compiler uses an abi tag,
    ``$arch-$os-$compiler`` if it doesn't.

Paths in the simple build system
""""""""""""""""""""""""""""""""

For the simple build system, the following defaults apply:

.. list-table:: Default installation paths

    * - Option
      - Unix Default
      - Windows Default
    * - :option:`--prefix` (global)
      - ``/usr/local``
      - ``%PROGRAMFILES%\Haskell``
    * - :option:`--prefix` (per-user)
      - ``$HOME/.cabal``
      - ``%APPDATA%\cabal``
    * - :option:`--bindir`
      - ``$prefix/bin``
      - ``$prefix\bin``
    * - :option:`--libdir`
      - ``$prefix/lib``
      - ``$prefix``
    * - :option:`--libsubdir` (others)
      - ``$pkgid/$compiler``
      - ``$pkgid\$compiler``
    * - :option:`--dynlibdir`
      - ``$libdir/$abi``
      - ``$libdir\$abi``
    * - :option:`--libexecdir`
      - ``$prefix/libexec``
      - ``$prefix\$pkgid``
    * - :option:`--datadir` (executable)
      - ``$prefix/share``
      - ``$prefix``
    * - :option:`--datadir` (library)
      - ``$prefix/share``
      - ``%PROGRAMFILES%\Haskell``
    * - :option:`--datasubdir`
      - ``$pkgid``
      - ``$pkgid``
    * - :option:`--docdir`
      - ``$datadir/doc/$pkgid``
      - ``$prefix\doc\$pkgid``
    * - :option:`--sysconfdir`
      - ``$prefix/etc``
      - ``$prefix\etc``
    * - :option:`--htmldir`
      - ``$docdir/html``
      - ``$docdir\html``
    * - :option:`--program-prefix`
      - (empty)
      - (empty)
    * - :option:`--program-suffix`
      - (empty)
      - (empty)

.. _prefix independence:

Prefix independence
"""""""""""""""""""

On Windows it is possible to obtain the pathname of the running program.
This means that we can construct an installable executable package that
is independent of its absolute install location. The executable can find
its auxiliary files by finding its own path and knowing the location of
the other files relative to ``$bindir``. Prefix independence is
particularly useful: it means the user can choose the install location
(i.e. the value of ``$prefix``) at install-time, rather than having to
bake the path into the binary when it is built.

In order to achieve this, we require that for an executable on Windows,
all of ``$bindir``, ``$libdir``, ``$dynlibdir``, ``$datadir`` and ``$libexecdir`` begin
with ``$prefix``. If this is not the case then the compiled executable
will have baked-in all absolute paths.

The application need do nothing special to achieve prefix independence.
If it finds any files using ``getDataFileName`` and the :ref:`other functions
provided for the purpose <accessing-data-files>`,
the files will be accessed relative to the location of the current
executable.

A library cannot (currently) be prefix independent, because it will be
linked into an executable whose file system location bears no relation
to the library package.

.. _controlling flag assignments:

Controlling Flag Assignments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Flag assignments (see :ref:`resolution-of-conditions-and-flags`)
can be controlled with the following command line options.

.. option:: -f flagname or -f -flagname

    Force the specified flag to ``true`` or ``false`` (if preceded with
    a ``-``). Later specifications for the same flags will override
    earlier, i.e., specifying ``-fdebug -f-debug`` is equivalent to
    ``-f-debug``

.. option:: --flags=flagspecs

    Same as ``-f``, but allows specifying multiple flag assignments at
    once. The parameter is a space-separated list of flag names (to
    force a flag to ``true``), optionally preceded by a ``-`` (to force
    a flag to ``false``). For example,
    ``--flags="debug -feature1 feature2"`` is equivalent to
    ``-fdebug -f-feature1 -ffeature2``.

Building Test Suites
^^^^^^^^^^^^^^^^^^^^

.. option:: --enable-tests

    Build the test suites defined in the package description file during
    the ``build`` stage. Check for dependencies required by the test
    suites. If the package is configured with this option, it will be
    possible to run the test suites with the ``test`` command after the
    package is built.

.. option:: --disable-tests

    (default) Do not build any test suites during the ``build`` stage.
    Do not check for dependencies required only by the test suites. It
    will not be possible to invoke the ``test`` command without
    reconfiguring the package.

.. option:: --enable-coverage

    Build libraries and executables (including test suites) with Haskell
    Program Coverage enabled. Running the test suites will automatically
    generate coverage reports with HPC.

.. option:: --disable-coverage

    (default) Do not enable Haskell Program Coverage.

Miscellaneous options
^^^^^^^^^^^^^^^^^^^^^

.. option:: --user

    Does a per-user installation. This changes the `default installation
    prefix <#paths-in-the-simple-build-system>`__. It also allow
    dependencies to be satisfied by the user's package database, in
    addition to the global database. This also implies a default of
    ``--user`` for any subsequent ``install`` command, as packages
    registered in the global database should not depend on packages
    registered in a user's database.

.. option:: --global

    (default) Does a global installation. In this case package
    dependencies must be satisfied by the global package database. All
    packages in the user's package database will be ignored. Typically
    the final installation step will require administrative privileges.

.. option:: --package-db=db

    Allows package dependencies to be satisfied from this additional
    package database *db* in addition to the global package database.
    All packages in the user's package database will be ignored. The
    interpretation of *db* is implementation-specific. Typically it will
    be a file or directory. Not all implementations support arbitrary
    package databases.

    This pushes an extra db onto the db stack. The :option:`--global` and
    :option:`--user` mode switches add the respective [Global] and [Global,
    User] dbs to the initial stack. There is a compiler-implementation
    constraint that the global db must appear first in the stack, and if
    the user one appears at all, it must appear immediately after the
    global db.

    To reset the stack, use ``--package-db=clear``.

.. option:: --ipid=ipid

    Specifies the *installed package identifier* of the package to be
    built; this identifier is passed on to GHC and serves as the basis
    for linker symbols and the ``id`` field in a ``ghc-pkg``
    registration. When a package has multiple components, the actual
    component identifiers are derived off of this identifier. E.g., an
    internal library ``foo`` from package ``p-0.1-abcd`` will get the
    identifier ``p-0.1-abcd-foo``.

.. option:: --cid=cid

    Specifies the *component identifier* of the component being built;
    this is only valid if you are configuring a single component.

.. option:: --enable-optimization[=n] or -O [n]

    (default) Build with optimization flags (if available). This is
    appropriate for production use, taking more time to build faster
    libraries and programs.

    The optional *n* value is the optimisation level. Some compilers
    support multiple optimisation levels. The range is 0 to 2. Level 0
    is equivalent to :option:`--disable-optimization`, level 1 is the
    default if no *n* parameter is given. Level 2 is higher optimisation
    if the compiler supports it. Level 2 is likely to lead to longer
    compile times and bigger generated code.

    When optimizations are enabled, Cabal passes ``-O2`` to the C compiler.

.. option:: --disable-optimization

    Build without optimization. This is suited for development: building
    will be quicker, but the resulting library or programs will be
    slower.

.. option:: --enable-profiling

    Build libraries and executables with profiling enabled (for
    compilers that support profiling as a separate mode). For this to
    work, all libraries used by this package must also have been built
    with profiling support. For libraries this involves building an
    additional instance of the library in addition to the normal
    non-profiling instance. For executables it changes the single
    executable to be built in profiling mode.

    This flag covers both libraries and executables, but can be
    overridden by the :option:`--enable-library-profiling` flag.

    See also the :option:`--profiling-detail` flag below.

.. option:: --disable-profiling

    (default) Do not enable profiling in generated libraries and
    executables.

.. option:: --enable-library-profiling or -p

    As with :option:`--enable-profiling` above, but it applies only for
    libraries. So this generates an additional profiling instance of the
    library in addition to the normal non-profiling instance.

    The :option:`--enable-profiling` flag controls the profiling mode for both
    libraries and executables, but if different modes are desired for
    libraries versus executables then use :option:`--enable-library-profiling`
    as well.

.. option:: --disable-library-profiling

    (default) Do not generate an additional profiling version of the library.

.. option:: --profiling-detail[=level]

    Some compilers that support profiling, notably GHC, can allocate
    costs to different parts of the program and there are different
    levels of granularity or detail with which this can be done. In
    particular for GHC this concept is called "cost centers", and GHC
    can automatically add cost centers, and can do so in different ways.

    This flag covers both libraries and executables, but can be
    overridden by the :option:`--library-profiling-detail` flag.

    Currently this setting is ignored for compilers other than GHC. The
    levels that cabal currently supports are:

    default
        For GHC this uses ``exported-functions`` for libraries and
        ``toplevel-functions`` for executables.
    none
        No costs will be assigned to any code within this component.
    exported-functions
        Costs will be assigned at the granularity of all top level
        functions exported from each module. In GHC specifically, this
        is for non-inline functions.
    toplevel-functions
        Costs will be assigned at the granularity of all top level
        functions in each module, whether they are exported from the
        module or not. In GHC specifically, this is for non-inline
        functions.
    all-functions
        Costs will be assigned at the granularity of all functions in
        each module, whether top level or local. In GHC specifically,
        this is for non-inline toplevel or where-bound functions or
        values.

    This flag is new in Cabal-1.24. Prior versions used the equivalent
    of ``none`` above.

.. option:: --library-profiling-detail[=level]

    As with :option:`--profiling-detail` above, but it applies only for
    libraries.

    The level for both libraries and executables is set by the
    :option:`--profiling-detail` flag, but if different levels are desired
    for libraries versus executables then use
    :option:`--library-profiling-detail` as well.

.. option:: --enable-library-vanilla

    (default) Build ordinary libraries (as opposed to profiling
    libraries). This is independent of the
    :option:`--enable-library-profiling` option. If you enable both, you get
    both.

.. option:: --disable-library-vanilla

    Do not build ordinary libraries. This is useful in conjunction with
    :option:`--enable-library-profiling` to build only profiling libraries,
    rather than profiling and ordinary libraries.

.. option:: --enable-library-for-ghci

    (default) Build libraries suitable for use with GHCi.

.. option:: --disable-library-for-ghci

    Not all platforms support GHCi and indeed on some platforms, trying
    to build GHCi libs fails. In such cases this flag can be used as a
    workaround.

.. option:: --enable-split-objs

    Use the GHC ``-split-objs`` feature when building the library. This
    reduces the final size of the executables that use the library by
    allowing them to link with only the bits that they use rather than
    the entire library. The downside is that building the library takes
    longer and uses considerably more memory.

.. option:: --disable-split-objs

    (default) Do not use the GHC ``-split-objs`` feature. This makes
    building the library quicker but the final executables that use the
    library will be larger.

.. option:: --enable-executable-stripping

    (default) When installing binary executable programs, run the
    ``strip`` program on the binary. This can considerably reduce the
    size of the executable binary file. It does this by removing
    debugging information and symbols. While such extra information is
    useful for debugging C programs with traditional debuggers it is
    rarely helpful for debugging binaries produced by Haskell compilers.

    Not all Haskell implementations generate native binaries. For such
    implementations this option has no effect.

.. option:: --disable-executable-stripping

    Do not strip binary executables during installation. You might want
    to use this option if you need to debug a program using gdb, for
    example if you want to debug the C parts of a program containing
    both Haskell and C code. Another reason is if your are building a
    package for a system which has a policy of managing the stripping
    itself (such as some Linux distributions).

.. option:: --enable-shared

    Build shared library. This implies a separate compiler run to
    generate position independent code as required on most platforms.

.. option:: --disable-shared

    (default) Do not build shared library.

.. option:: --enable-static

   Build a static library. This passes ``-staticlib`` to GHC (available
   for iOS, and with 8.4 more platforms).  The result is an archive ``.a``
   containing all dependent haskell libraries combined.

.. option:: --disable-static

    (default) Do not build a static library.

.. option:: --enable-executable-dynamic

    Link dependent Haskell libraries into executables dynamically.
    The executable's library dependencies must have been
    built as shared objects. This implies :option:`--enable-shared`
    unless :option:`--disable-shared` is explicitly specified.

.. option:: --disable-executable-dynamic

   (default) Link dependent Haskell libraries into executables statically.
   Non-Haskell (C) libraries are still linked dynamically, including libc,
   so the result is still not a fully static executable
   unless :option:`--enable-executable-static` is given.

.. option:: --enable-executable-static

    Build fully static executables.
    This links all dependent libraries into executables statically,
    including libc.

.. option:: --disable-executable-static

   (default) Do not build fully static executables.

.. option:: --configure-option=str

    An extra option to an external ``configure`` script, if one is used
    (see the section on :ref:`system-dependent parameters`).
    There can be several of these options.

.. option:: --extra-include-dirs[=dir]

    An extra directory to search for C header files. You can use this
    flag multiple times to get a list of directories.

    You might need to use this flag if you have standard system header
    files in a non-standard location that is not mentioned in the
    package's ``.cabal`` file. Using this option has the same effect as
    appending the directory *dir* to the ``include-dirs`` field in each
    library and executable in the package's ``.cabal`` file. The
    advantage of course is that you do not have to modify the package at
    all. These extra directories will be used while building the package
    and for libraries it is also saved in the package registration
    information and used when compiling modules that use the library.

.. option:: --extra-lib-dirs[=dir]

    An extra directory to search for system libraries files. You can use
    this flag multiple times to get a list of directories.

.. option:: --extra-framework-dirs[=dir]

    An extra directory to search for frameworks (OS X only). You can use
    this flag multiple times to get a list of directories.

    You might need to use this flag if you have standard system
    libraries in a non-standard location that is not mentioned in the
    package's ``.cabal`` file. Using this option has the same affect as
    appending the directory *dir* to the ``extra-lib-dirs`` field in
    each library and executable in the package's ``.cabal`` file. The
    advantage of course is that you do not have to modify the package at
    all. These extra directories will be used while building the package
    and for libraries it is also saved in the package registration
    information and used when compiling modules that use the library.

.. option:: --dependency[=pkgname=ipid]

    Specify that a particular dependency should used for a particular
    package name. In particular, it declares that any reference to
    *pkgname* in a :pkg-field:`build-depends` should be resolved to
    *ipid*.

.. option:: --promised-dependency[=pkgname=ipid]

    Very much like ``--dependency`` but the package doesn't need to already
    be installed. This is useful when attempting to start multiple component
    sessions with cabal's multi-repl or projects such as Haskell Language Server.

    Several checks which are enabled for ``--dependency``s are disabled for promised
    dependencies, so prefer to use ``--dependency`` if you know that the dependency
    is already installed.

.. option:: --exact-configuration

    This changes Cabal to require every dependency be explicitly
    specified using :option:`--dependency`, rather than use Cabal's (very
    simple) dependency solver. This is useful for programmatic use of
    Cabal's API, where you want to error if you didn't specify enough
    :option:`--dependency` flags.


.. option:: --constraint=constraint

    Restrict solutions involving a package to given version
    bounds, flag settings, and other properties.

    The following considers only install plans where ``bar``,
    if used, is restricted to version 2.1:

    ::

        $ cabal install --constraint="bar == 2.1"

    The following prevents ``bar`` from being used at all:

    ::

        $ cabal install --constraint="bar <0"

    Version bounds have the same syntax as :pkg-field:`build-depends`.
    Yet extra pseudo version bounds are available here in addition:

      - ``installed`` to fix a package to the already installed version.
        Often useful for GHC-supplied packages in combination with :cfg-field:`allow-newer`,
        e.g., ``--allow-newer='*:base' --constraint='base installed'``.

      - ``source`` to fix a package to the local source copy.

    ::

        # Require that a version of bar be used that is already installed in
        # the global package database.
        $ cabal install --constraint="bar installed"

        # Require the local source copy of bar to be used.
        # (Note: By default, if we have a local package we will
        # automatically use it, so it will generally not be necessary to
        # specify this.)
        $ cabal install --constraint="bar source"

    Further, we can specify flag assignments with ``+FLAG`` and ``-FLAG``
    or enable test (``test``) and benchmark (``bench``) suites:

    ::

        # Require bar to be installed with the foo flag turned on and
        # the baz flag turned off.
        $ cabal install --constraint="bar +foo -baz"

        # Require that bar have test suites and benchmarks enabled.
        $ cabal install --constraint="bar test" --constraint="bar bench"

    To specify multiple constraints, you may pass the
    ``constraint`` option multiple times.

    By default, constraints only apply to build dependencies
    (:pkg-field:`build-depends`), build dependencies of build
    dependencies, and so on. Constraints normally do not apply to
    dependencies of the ``Setup.hs`` script of any package
    (:pkg-field:`custom-setup:setup-depends`) nor do they apply to build tools
    (:pkg-field:`build-tool-depends`) or the dependencies of build
    tools. To explicitly apply a constraint to a setup or build
    tool dependency, you can add a qualifier ``setup`` or ``any``
    to the constraint as follows:

    ::

        # Example use of the 'any' qualifier. This constraint
        # applies to package bar anywhere in the dependency graph.
        $ cabal install --constraint="any.bar == 1.0"

    ::

        # Example uses of 'setup' qualifiers.

        # This constraint applies to package bar when it is a
        # dependency of any Setup.hs script.
        $ cabal install --constraint="setup.bar == 1.0"

        # This constraint applies to package bar when it is a
        # dependency of the Setup.hs script of package foo.
        $ cabal install --constraint="foo:setup.bar == 1.0"

    ..  TODO: Uncomment this example once we decide on a syntax for 'exe'.
    ..  # Example use of the 'exe' (executable build tool)
        # qualifier. This constraint applies to package baz when it
        # is a dependency of the build tool bar being used to
        # build package foo.
        $ cabal install --constraint="foo:bar:exe.baz == 1.0"

.. option:: --disable-response-files

    Enable workaround for older versions of programs such as ``ar`` or
    ``ld`` that do not support response file arguments (i.e. ``@file``
    arguments). You may want this flag only if you specify custom ar
    executable. For system ``ar`` or the one bundled with ``ghc`` on
    Windows the ``cabal`` should do the right thing and hence should
    normally not require this flag.

.. _setup-build:

runhaskell Setup.hs build
-------------------------

Perform any preprocessing or compilation needed to make this package
ready for installation.

This command takes the following options:

.. program:: runhaskell Setup.hs build

.. option:: --prog-options=options, --prog-option=option

    These are mostly the same as the `options configure
    step <#setup-configure>`__. Unlike the options specified at the
    configure step, any program options specified at the build step are
    not persistent but are used for that invocation only. The options
    specified at the build step are in addition not in replacement of
    any options specified at the configure step.

.. _setup-haddock:

runhaskell Setup.hs haddock
---------------------------

.. program:: runhaskell Setup.hs haddock

Build the documentation for the package using Haddock_.
By default, only the documentation for the exposed modules is generated
(but see the :option:`--executables` and :option:`--internal` flags below).

This command takes the following options:

.. option:: --hoogle

    Generate a file ``dist/doc/html/``\ *pkgid*\ ``.txt``, which can be
    converted by Hoogle_ into a
    database for searching. This is equivalent to running Haddock_
    with the ``--hoogle`` flag.

.. option:: --html-location=url

    Specify a template for the location of HTML documentation for
    prerequisite packages. The substitutions (`see
    listing <#paths-in-the-simple-build-system>`__) are applied to the
    template to obtain a location for each package, which will be used
    by hyperlinks in the generated documentation. For example, the
    following command generates links pointing at Hackage_ pages:

    ::

        $ runhaskell Setup.hs haddock \
        --html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html'

    Here the argument is quoted to prevent substitution by the shell. If
    this option is omitted, the location for each package is obtained
    using the package tool (e.g. ``ghc-pkg``).

.. option:: --executables

    Also run Haddock_ for the modules of all the executable programs. By default
    Haddock_ is run only on the exported modules.

.. option:: --internal

    Run Haddock_ for the all
    modules, including unexposed ones, and make
    Haddock_ generate documentation
    for unexported symbols as well.

.. option:: --css=path

    The argument *path* denotes a CSS file, which is passed to
    Haddock_ and used to set the
    style of the generated documentation. This is only needed to
    override the default style that
    Haddock_ uses.

.. option:: --hyperlink-source

    Generate Haddock_ documentation integrated with HsColour_ . First,
    HsColour_ is run to generate colourised code. Then Haddock_ is run to
    generate HTML documentation. Each entity shown in the documentation is
    linked to its definition in the colourised code.

.. option:: --hscolour-css=path

    The argument *path* denotes a CSS file, which is passed to HsColour_ as in

    ::

        $ runhaskell Setup.hs hscolour --css=*path*

.. _setup-hscolour:

runhaskell Setup.hs hscolour
----------------------------

Produce colourised code in HTML format using HsColour_. Colourised code for
exported modules is put in ``dist/doc/html/``\ *pkgid*\ ``/src``.

This command takes the following options:

.. program:: runhaskell Setup.hs hscolour

.. option:: --executables

    Also run HsColour_ on the sources of all executable programs. Colourised
    code is put in ``dist/doc/html/``\ *pkgid*/*executable*\ ``/src``.

.. option:: --css=path

    Use the given CSS file for the generated HTML files. The CSS file
    defines the colours used to colourise code. Note that this copies
    the given CSS file to the directory with the generated HTML files
    (renamed to ``hscolour.css``) rather than linking to it.

.. _setup-install:

runhaskell Setup.hs install
---------------------------

.. program:: runhaskell Setup.hs install

Copy the files into the install locations and (for library packages)
register the package with the compiler, i.e. make the modules it
contains available to programs.

Additionally for GHC the ``extra-compilation-artifacts`` directory is copied if present.
GHC plugins can store extra data in subfolders.
(e.g. *extra-compilation-artifacts/PLUGIN_NAME/HS_MODULE.txt*)

The `install locations <#installation-paths>`__ are determined by
options to `runhaskell Setup.hs configure`_.

This command takes the following options:

.. option:: --global

    Register this package in the system-wide database. (This is the
    default, unless the :option:`runhaskell Setup.hs configure --user` option was supplied
    to the ``configure`` command.)

.. option:: --user

    Register this package in the user's local package database. (This is
    the default if the :option:`runhaskell Setup.hs configure --user` option was supplied
    to the ``configure`` command.)

.. _setup-copy:

runhaskell Setup.hs copy
------------------------

Copy the files without registering them. This command is mainly of use
to those creating binary packages.

This command takes the following option:

.. program:: runhaskell Setup.hs copy

.. option:: --destdir=path

   Specify the directory under which to place installed files. If this is
   not given, then the root directory is assumed.

.. _setup-register:

runhaskell Setup.hs register
----------------------------

Register this package with the compiler, i.e. make the modules it
contains available to programs. This only makes sense for library
packages. Note that the ``install`` command incorporates this action.
The main use of this separate command is in the post-installation step
for a binary package.

This command takes the following options:

.. program:: runhaskell Setup.hs register

.. option:: --global

    Register this package in the system-wide database. (This is the
    default.)

.. option:: --user

    Register this package in the user's local package database.

.. option:: --gen-script

    Instead of registering the package, generate a script containing
    commands to perform the registration. On Unix, this file is called
    ``register.sh``, on Windows, ``register.bat``. This script might be
    included in a binary bundle, to be run after the bundle is unpacked
    on the target system.

.. option:: --gen-pkg-config[=path]

    Instead of registering the package, generate a package registration
    file (or directory, in some circumstances). This only applies to
    compilers that support package registration files which at the
    moment is only GHC. The file should be used with the compiler's
    mechanism for registering packages. This option is mainly intended
    for packaging systems. If possible use the :option:`--gen-script` option
    instead since it is more portable across Haskell implementations.
    The *path* is optional and can be used to specify a particular
    output file to generate. Otherwise, by default the file is the
    package name and version with a ``.conf`` extension.

    This option outputs a directory if the package requires multiple
    registrations: this can occur if internal/convenience libraries are
    used. These configuration file names are sorted so that they can be
    registered in order.

.. option:: --inplace

    Registers the package for use directly from the build tree, without
    needing to install it. This can be useful for testing: there's no
    need to install the package after modifying it, just recompile and
    test.

    This flag does not create a build-tree-local package database. It
    still registers the package in one of the user or global databases.

    However, there are some caveats. It only works with GHC (currently).
    It only works if your package doesn't depend on having any
    supplemental files installed --- plain Haskell libraries should be
    fine.

.. _setup-unregister:

runhaskell Setup.hs unregister
------------------------------

.. program:: runhaskell Setup.hs unregister

Deregister this package with the compiler.

This command takes the following options:

.. option:: --global

    Deregister this package in the system-wide database. (This is the
    default.)

.. option:: --user

    Deregister this package in the user's local package database.

.. option:: --gen-script

    Instead of deregistering the package, generate a script containing
    commands to perform the deregistration. On Unix, this file is called
    ``unregister.sh``, on Windows, ``unregister.bat``. This script might
    be included in a binary bundle, to be run on the target system.

.. _setup-clean:

runhaskell Setup.hs clean
-------------------------

Remove any local files created during the ``configure``, ``build``,
``haddock``, ``register`` or ``unregister`` steps, and also any files
and directories listed in the :pkg-field:`extra-tmp-files` field.

This command takes the following options:

.. program:: runhaskell Setup.hs clean

.. option:: --save-configure, -s

    Keeps the configuration information so it is not necessary to run
    the configure step again before building.

.. _setup-test:

runhaskell Setup.hs test
------------------------

Run the test suites specified in the package description file. Aside
from the following flags, Cabal accepts the name of one or more test
suites on the command line after ``test``. When supplied, Cabal will run
only the named test suites, otherwise, Cabal will run all test suites in
the package.

.. program:: runhaskell Setup.hs test

.. option:: --builddir=dir

    The directory where Cabal puts generated build files (default:
    ``dist``). Test logs will be located in the ``test`` subdirectory.

.. option:: --human-log=path

    The template used to name human-readable test logs; the path is
    relative to ``dist/test``. By default, logs are named according to
    the template ``$pkgid-$test-suite.log``, so that each test suite
    will be logged to its own human-readable log file. Template
    variables allowed are: ``$pkgid``, ``$compiler``, ``$os``,
    ``$arch``, ``$abi``, ``$abitag``, ``$test-suite``, and ``$result``.

.. option:: --machine-log=path

    The path to the machine-readable log, relative to ``dist/test``. The
    default template is ``$pkgid.log``. Template variables allowed are:
    ``$pkgid``, ``$compiler``, ``$os``, ``$arch``, ``$abi``, ``$abitag``
    and ``$result``.

.. option:: --show-details=filter

    Determines if the results of individual test cases are shown on the
    terminal. May be ``always`` (always show), ``never`` (never show),
    ``failures`` (show only failed results), ``streaming`` (show all
    results in real time) and ``direct`` (same as ``streaming`` but no log
    file and possibly prettier).

    Default value is ``direct``: it leaves test output untouched and does not
    produce a log. This allows for colored output, which is popular with testing
    frameworks. (On the other hand, ``streaming`` creates a log but looses
    coloring.)

.. option:: --test-options=options

    Give extra options to the test executables.

.. option:: --test-option=option

    Give an extra option to the test executables. There is no need to
    quote options containing spaces because a single option is assumed,
    so options will not be split on spaces.

.. option:: --test-wrapper=path

   The wrapper script/application used to setup and tear down the test
   execution context. The text executable path and test arguments are
   passed as arguments to the wrapper and it is expected that the wrapper
   will return the test's return code, as well as a copy of stdout/stderr.

.. _setup-bench:

runhaskell Setup.hs bench
-------------------------

Run the benchmarks specified in the package description file. Aside
from the following flags, Cabal accepts the name of one or more benchmarks
on the command line after ``bench``. When supplied, Cabal will run
only the named benchmarks, otherwise, Cabal will run all benchmarks in
the package.

.. option:: --benchmark-options=options

    Give extra options to the benchmark executables.

.. option:: --benchmark-option=option

    Give an extra option to the benchmark executables. There is no need to
    quote options containing spaces because a single option is assumed,
    so options will not be split on spaces.

.. _setup-sdist:

runhaskell Setup.hs sdist
-------------------------

Create a system- and compiler-independent source distribution in a file
*package*-*version*\ ``.tar.gz`` in the ``dist`` subdirectory, for
distribution to package builders. When unpacked, the commands listed in
this section will be available.

The files placed in this distribution are the package description file,
the setup script, the sources of the modules named in the package
description file, and files named in the ``license-file``, ``main-is``,
``c-sources``, ``asm-sources``, ``cmm-sources``, ``js-sources``,
``data-files``, ``extra-source-files`` and ``extra-doc-files`` fields.

This command takes the following option:

.. program:: runhaskell Setup.hs sdist

.. option:: --snapshot

    Append today's date (in "YYYYMMDD" format) to the version number for
    the generated source package. The original package is unaffected.


.. include:: references.inc
