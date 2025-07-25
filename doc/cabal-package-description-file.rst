Package Description — <package>.cabal File
==========================================

The package description file, commonly known as "the Cabal file", describes the
contents of a package. The Cabal package is the unit of distribution. When
installed, its purpose is to make available one or more:

-  Haskell programs (executables); and/or

-  libraries, exposing a number of Haskell modules.

Public library components can be depended upon by other Cabal packages and all
library components (both public and private) can be depended upon by other
components of the same package.

Internally, the package may consist of much more than a bunch of Haskell
modules: it may also have C source code and header files, source code
meant for preprocessing, documentation, test cases, auxiliary tools etc.

A package is identified by a globally-unique *package name*, which
consists of one or more alphanumeric words separated by hyphens. To
avoid ambiguity, each of these words should contain at least one letter.
Chaos will result if two distinct packages with the same name are
installed on the same system. A particular version of the package is
distinguished by a *version number*, consisting of a sequence of one or
more integers separated by dots. These can be combined to form a single
text string called the *package ID*, using a hyphen to separate the name
from the version, e.g. "``HUnit-1.1``".

.. Note::

   Packages are not part of the Haskell language; they simply
   populate the hierarchical space of module names. In GHC 6.6 and later a
   program may contain multiple modules with the same name if they come
   from separate packages; in all other current Haskell systems packages
   may not overlap in the modules they provide, including hidden modules.

Creating a package
------------------

Suppose you have a directory hierarchy containing the source files that
make up your package. You will need to add two more files to the root
directory of the package:

:file:`{package-name}.cabal`
    a Unicode UTF-8 text file containing a package description. For
    details of the syntax of this file, see the section on
    `package descriptions`_.

:file:`Setup.hs`
    a single-module Haskell program to perform various setup tasks (with
    the interface described in the section on :ref:`setup-commands`).
    This module should import only modules that will be present in all Haskell
    implementations, including modules of the Cabal library. The content of
    this file is determined by the :pkg-field:`build-type` setting in the
    ``.cabal`` file. In most cases it will be trivial, calling on the Cabal
    library to do most of the work.

Once you have these, you can create a source bundle of this directory
for distribution. Building of the package is demonstrated in the section
:ref:`building-packages`.

One of the purposes of Cabal is to make it easier to build a package
with different Haskell implementations. So it provides abstractions of
features present in different Haskell implementations and wherever
possible it is best to take advantage of these to increase portability.
Where necessary however it is possible to use specific features of
specific implementations. For example one of the pieces of information a
package author can put in the package's ``.cabal`` file is what language
extensions the code uses. This is far preferable to specifying flags for
a specific compiler as it allows Cabal to pick the right flags for the
Haskell implementation that the user picks. It also allows Cabal to
figure out if the language extension is even supported by the Haskell
implementation that the user picks. Where compiler-specific options are
needed however, there is an "escape hatch" available. The developer can
specify implementation-specific options and more generally there is a
configuration mechanism to customise many aspects of how a package is
built depending on the Haskell implementation, the Operating system,
computer architecture and user-specified configuration flags.

::

    name:     Foo
    version:  1.0

    library
      default-language: Haskell2010
      build-depends:    base >= 4 && < 5
      exposed-modules:  Foo
      extensions:       ForeignFunctionInterface
      ghc-options:      -Wall
      if os(windows)
        build-depends: Win32 >= 2.1 && < 2.6

Example: A package containing a simple library
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The HUnit package contains a file ``HUnit.cabal`` containing:

::

    cabal-version:  3.0
    name:           HUnit
    version:        1.1.1
    synopsis:       A unit testing framework for Haskell
    homepage:       http://hunit.sourceforge.net/
    category:       Testing
    author:         Dean Herington
    license:        BSD-3-Clause
    license-file:   LICENSE
    build-type:     Simple

    library
      build-depends:      base >= 2 && < 4
      exposed-modules:    Test.HUnit.Base, Test.HUnit.Lang,
                          Test.HUnit.Terminal, Test.HUnit.Text, Test.HUnit
      default-extensions: CPP
      default-language:   Haskell2010

and the following ``Setup.hs``:

.. code-block:: haskell

    import Distribution.Simple
    main = defaultMain

Example: A package containing executable programs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    cabal-version:  3.0
    name:           TestPackage
    version:        0.0
    synopsis:       Small package with two programs
    author:         Angela Author
    license:        BSD-3-Clause
    build-type:     Simple

    executable program1
      build-depends:    HUnit >= 1.1.1 && < 1.2
      main-is:          main.hs
      hs-source-dirs:   prog1
      default-language: Haskell2010

    executable program2
      -- A different main.hs because of hs-source-dirs.
      main-is:          main.hs
      build-depends:    HUnit >= 1.1.1 && < 1.2
      hs-source-dirs:   prog2
      other-modules:    Utils
      default-language: Haskell2010

with ``Setup.hs`` the same as above.

Example: A package containing a library and executable programs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    cabal-version:   3.0
    name:            TestPackage
    version:         0.0
    synopsis:        Package with library and two programs
    license:         BSD-3-Clause
    author:          Angela Author
    build-type:      Simple

    library
      build-depends:    HUnit >= 1.1.1 && < 1.2
      hs-source-dirs:   lib
      exposed-modules:  A, B, C
      default-language: Haskell2010

    executable program1
      main-is:          main.hs
      hs-source-dirs:   prog1
      other-modules:    D, E
      default-language: Haskell2010

    executable program2
      -- A different main.hs because of hs-source-dirs.
      main-is:          main.hs
      -- No bound on a library provided by the same package.
      build-depends:    TestPackage
      hs-source-dirs:   prog2
      other-modules:    Utils
      default-language: Haskell2010

with ``Setup.hs`` the same as above. Note that any library modules
required (directly or indirectly) by an executable must be listed again.

The trivial setup script used in these examples uses the *simple build
infrastructure* provided by the Cabal library (see
`Distribution.Simple <https://hackage.haskell.org/package/Cabal/docs/Distribution-Simple.html>`__).
The simplicity lies in its interface rather that its implementation. It
automatically handles preprocessing with standard preprocessors, and
builds packages for all the Haskell implementations.

The simple build infrastructure can also handle packages where building
is governed by system-dependent parameters, if you specify a little more
(see the section on `system-dependent parameters`_).
A few packages require `more elaborate solutions <#more-complex-packages>`_.

.. _pkg-desc:

Package descriptions
--------------------

The package description file must have a name ending in "``.cabal``". It
must be a Unicode text file encoded using valid UTF-8. There must be
exactly one such file in the directory. The first part of the name is
usually the package name, and some of the tools that operate on Cabal
packages require this; specifically, Hackage rejects packages which
don't follow this rule.

In the package description file, lines whose first non-whitespace
characters are "``--``" are treated as comments and ignored.

This file should contain a number global property descriptions and
several sections.

-  The `package properties`_ describe the package
   as a whole, such as name, license, author, etc.

-  Optionally, a number of *configuration flags* can be declared. These
   can be used to enable or disable certain features of a package. (see
   the section on `configurations`_).

-  The (optional) library section specifies the `library`_ properties and
   relevant `build information`_.

-  Following is an arbitrary number of executable sections which describe
   an executable program and relevant `build information`_.

Each section consists of a number of property descriptions in the form
of field/value pairs, with a syntax roughly like mail message headers.

-  Case is not significant in field names, but is significant in field
   values.

-  To continue a field value, indent the next line relative to the field
   name.

-  Field names may be indented, but all field values in the same section
   must use the same indentation.

-  Tabs are *not* allowed as indentation characters due to a missing
   standard interpretation of tab width.

-  Before Cabal 3.0, to get a blank line in a field value, use an indented "``.``"

The syntax of the value depends on the field. Field types include:

*token*, *filename*, *directory*
    Either a sequence of one or more non-space non-comma characters, or
    a quoted string in Haskell 98 lexical syntax. The latter can be used
    for escaping whitespace, for example:
    ``ghc-options: -Wall "-with-rtsopts=-T -I1"``. Unless otherwise
    stated, relative filenames and directories are interpreted from the
    package root directory.
*freeform*, *URL*, *address*
    An arbitrary, uninterpreted string.
*identifier*
    A letter followed by zero or more alphanumerics or underscores.
*compiler*
    A compiler flavor (one of: ``GHC``, ``UHC`` or ``LHC``)
    followed by a version range. For example, ``GHC ==6.10.3``, or
    ``LHC >=0.6 && <0.8``.

Modules and preprocessors
^^^^^^^^^^^^^^^^^^^^^^^^^

Haskell module names listed in the :pkg-field:`library:exposed-modules` and
:pkg-field:`library:other-modules` fields may correspond to Haskell source
files, i.e. with names ending in "``.hs``" or "``.lhs``", or to inputs for
various Haskell preprocessors. The simple build infrastructure understands the
extensions:

-  ``.chs`` (:hackage-pkg:`c2hs`)
-  ``.hsc`` (:hackage-pkg:`hsc2hs`)
-  ``.y`` and ``.ly`` (happy_)
-  ``.x`` (alex_)
-  ``.cpphs`` (cpphs_)

When building, Cabal will automatically run the appropriate preprocessor
and compile the Haskell module it produces. For the ``c2hs`` and
``hsc2hs`` preprocessors, Cabal will also automatically add, compile and
link any C sources generated by the preprocessor (produced by
``hsc2hs``'s ``#def`` feature or ``c2hs``'s auto-generated wrapper
functions). Dependencies on pre-processors are specified via the
:pkg-field:`build-tools` or :pkg-field:`build-tool-depends` fields.

Some fields take lists of values, which are optionally separated by
commas, except for the :pkg-field:`build-depends` field, where the commas are
mandatory.

Some fields are marked as required. All others are optional, and unless
otherwise specified have empty default values.

Package properties
^^^^^^^^^^^^^^^^^^

These fields may occur in the first top-level properties section and
describe the package as a whole:

.. pkg-field:: name: package-name (required)

    The unique name of the package, without the version number.

    As pointed out in the section on `package descriptions`_, some
    tools require the package-name specified for this field to match
    the package description's file-name :file:`{package-name}.cabal`.

    A valid package name comprises an alphanumeric 'word'; or two or more
    such words separated by a hyphen character (``-``). A word cannot be
    comprised only of the digits ``0`` to ``9``.

    An alphanumeric character belongs to one of the Unicode Letter categories
    (Lu (uppercase), Ll (lowercase), Lt (titlecase), Lm (modifier), or
    Lo (other)) or Number categories (Nd (decimal), Nl (letter), or No (other)).

    Package names are case-sensitive.

    Expressed as a regular expression:

    ``[0-9]*[\p{L}\p{N}-[0-9]][\p{L}\p{N}]*(-[0-9]*[\p{L}\p{N}-[0-9]][\p{L}\p{N}]*)*``

    Expressed in ABNF_:

    .. code-block:: abnf

        package-name      = package-name-part *("-" package-name-part)
        package-name-part = *DIGIT UALPHANUM-NOT-DIGIT *UALNUM

        DIGIT = %x30-39 ; 0-9

        UALNUM = UALPHANUM-NOT-DIGIT / DIGIT
        UALPHANUM-NOT-DIGIT = ... ; set of Unicode code-points in Letter or
                                  ; Number categories, other than the DIGIT
                                  ; code-points

    .. note::

        Hackage will not accept package names that use alphanumeric characters
        other than ``A`` to ``Z``, ``a`` to ``z``, and ``0`` to ``9``
        (the ASCII subset).

.. pkg-field:: version: numbers (required)

    The package version number, usually consisting of a sequence of
    natural numbers separated by dots, i.e. as the regular
    expression ``[0-9]+([.][0-9]+)*`` or expressed in ABNF_:

    .. code-block:: abnf

        package-version = 1*DIGIT *("." 1*DIGIT)

.. pkg-field:: cabal-version: x.y[.z]

    The version of the Cabal specification that this package
    description uses. The Cabal specification does slowly evolve (see
    also :ref:`spec-history`), introducing new features and
    occasionally changing the meaning of existing features.
    Specifying which version of the specification you are using
    enables programs which process the package description to know
    what syntax to expect and what each part means.

    The version number you specify will affect both compatibility and
    behaviour. Most tools (including the Cabal library and the ``cabal``
    program) understand a range of versions of the Cabal specification.
    Older tools will of course only work with older versions of the
    Cabal specification that was known at the time. Most of the time,
    tools that are too old will recognise this fact and produce a
    suitable error message. Likewise, ``cabal check`` will tell you
    whether the version number is sufficiently high for the features
    you use in the package description.

    As for behaviour, new versions of the Cabal specification can change the
    meaning of existing syntax. This means if you want to take advantage
    of the new meaning or behaviour then you must specify the newer
    Cabal version. Tools are expected to use the meaning and behaviour
    appropriate to the version given in the package description.

    In particular, the syntax of package descriptions changed
    significantly with Cabal version 1.2 and the :pkg-field:`cabal-version`
    field is now required. Files written in the old syntax are still
    recognized, so if you require compatibility with very old Cabal
    versions then you may write your package description file using the
    old syntax. Please consult the user's guide of an older Cabal
    version for a description of that syntax.

    Starting with ``cabal-version: 2.2`` this field is only valid if
    fully contained in the very first line of a package description
    and ought to adhere to the ABNF_ grammar

    .. code-block:: abnf

        newstyle-spec-version-decl = "cabal-version" *WS ":" *WS newstyle-spec-version *WS

        newstyle-spec-version      = NUM "." NUM [ "." NUM ]

        NUM    = DIGIT0 / DIGITP 1*DIGIT0
        DIGIT0 = %x30-39
        DIGITP = %x31-39
        WS     = %20


    .. note::

        For package descriptions using a format prior to
        ``cabal-version: 1.12`` the legacy syntax resembling a version
        range syntax

        .. code-block:: cabal

            cabal-version: >= 1.10

        needs to be used.

        This legacy syntax is supported up until ``cabal-version: >=
        2.0`` it is however strongly recommended to avoid using the
        legacy syntax. See also :issue:`4899`.



.. pkg-field:: build-type: identifier

    :default: ``Custom`` or ``Simple``

    The type of build used by this package. Build types are the
    constructors of the
    `BuildType <https://hackage.haskell.org/package/Cabal-syntax/docs/Distribution-Types-BuildType.html#t:BuildType>`__
    type. This field is optional and when missing, its default value
    is inferred according to the following rules:

     - When :pkg-field:`cabal-version` is set to ``2.2`` or higher,
       the default is ``Simple`` unless a :pkg-section:`custom-setup`
       exists, in which case the inferred default is ``Custom``.

     - For lower :pkg-field:`cabal-version` values, the default is
       ``Custom`` unconditionally.

    If the build type is anything other than ``Custom``, then the
    ``Setup.hs`` file *must* be exactly the standardized content
    discussed below. This is because in these cases, ``cabal`` will
    ignore the ``Setup.hs`` file completely, whereas other methods of
    package management, such as ``runhaskell Setup.hs [CMD]``, still
    rely on the ``Setup.hs`` file.

    For build type ``Simple``, the contents of ``Setup.hs`` must be:

    .. code-block:: haskell

        import Distribution.Simple
        main = defaultMain

    For build type ``Hooks``, the contents of ``Setup.hs`` must be:

    .. code-block:: haskell

        import Distribution.Simple
        import SetupHooks (setupHooks)
        main = defaultMainWithSetupHooks setupHooks

    For build type ``Configure`` (see the section on `system-dependent
    parameters`_ below), the contents of
    ``Setup.hs`` must be:

    .. code-block:: haskell

        import Distribution.Simple
        main = defaultMainWithHooks autoconfUserHooks

    For build type ``Make`` (see the section on `more complex packages`_ below),
    the contents of ``Setup.hs`` must be:

    .. code-block:: haskell

        import Distribution.Make
        main = defaultMain

    For build type ``Custom``, the file ``Setup.hs`` can be customized,
    and will be used both by ``cabal`` and other tools.

    For most packages, the build type ``Simple`` is sufficient. For more exotic
    needs, the ``Hooks`` build type is recommended; see :ref:`setup-hooks`.

.. pkg-field:: license: SPDX expression

    :default: ``NONE``

    The type of license under which this package is distributed.

    Starting with ``cabal-version: 2.2`` the ``license`` field takes a
    (case-sensitive) SPDX expression such as

    .. code-block:: cabal

        license: Apache-2.0 AND (MIT OR GPL-2.0-or-later)

    See `SPDX IDs: How to use <https://spdx.org/ids-how>`__ for more
    examples of SPDX expressions.

    The version of the
    `list of SPDX license identifiers <https://spdx.org/licenses/>`__
    is a function of the :pkg-field:`cabal-version` value as defined
    in the following table:

    +--------------------------+--------------------+
    | Cabal specification      | SPDX license list  |
    | version                  | version            |
    |                          |                    |
    +==========================+====================+
    | ``cabal-version: 2.2``   | ``3.0 2017-12-28`` |
    +--------------------------+--------------------+
    | ``cabal-version: 2.4``   | ``3.2 2018-07-10`` |
    +--------------------------+--------------------+

    **Pre-SPDX Legacy Identifiers**

    The license identifier in the table below are defined for
    ``cabal-version: 2.0`` and previous versions of the Cabal
    specification.

    +--------------------------+-----------------+
    | :pkg-field:`license`     | Note            |
    | identifier               |                 |
    |                          |                 |
    +==========================+=================+
    | ``GPL``                  |                 |
    | ``GPL-2``                |                 |
    | ``GPL-3``                |                 |
    +--------------------------+-----------------+
    | ``LGPL``                 |                 |
    | ``LGPL-2.1``             |                 |
    | ``LGPL-3``               |                 |
    +--------------------------+-----------------+
    | ``AGPL``                 | since 1.18      |
    | ``AGPL-3``               |                 |
    +--------------------------+-----------------+
    | ``BSD2``                 | since 1.20      |
    +--------------------------+-----------------+
    | ``BSD3``                 |                 |
    +--------------------------+-----------------+
    | ``MIT``                  |                 |
    +--------------------------+-----------------+
    | ``ISC``                  | since 1.22      |
    +--------------------------+-----------------+
    | ``MPL-2.0``              | since 1.20      |
    +--------------------------+-----------------+
    | ``Apache``               |                 |
    | ``Apache-2.0``           |                 |
    +--------------------------+-----------------+
    | ``PublicDomain``         |                 |
    +--------------------------+-----------------+
    | ``AllRightsReserved``    |                 |
    +--------------------------+-----------------+
    | ``OtherLicense``         |                 |
    +--------------------------+-----------------+


.. pkg-field:: license-file: filename

    See :pkg-field:`license-files`.

.. pkg-field:: license-files: filename list
    :since: 1.20

    The name of a file(s) containing the precise copyright license for
    this package. The license file(s) will be installed with the
    package.

    If you have multiple license files then use the :pkg-field:`license-files`
    field instead of (or in addition to) the :pkg-field:`license-file` field.

.. pkg-field:: copyright: freeform

    The content of a copyright notice, typically the name of the holder
    of the copyright on the package and the year(s) from which copyright
    is claimed. For example::

      copyright: (c) 2006-2007 Joe Bloggs

.. pkg-field:: author: freeform

    The original author of the package.

    Remember that ``.cabal`` files are Unicode, using the UTF-8
    encoding.

.. pkg-field:: maintainer: address

    The current maintainer or maintainers of the package. This is an
    e-mail address to which users should send bug reports, feature
    requests and patches.

.. pkg-field:: stability: freeform

    The stability level of the package, e.g. ``alpha``,
    ``experimental``, ``provisional``, ``stable``.

.. pkg-field:: homepage: URL

    The package homepage.

.. pkg-field:: bug-reports: URL

    The URL where users should direct bug reports. This would normally
    be either:

    -  A ``mailto:`` URL, e.g. for a person or a mailing list.

    -  An ``http:`` (or ``https:``) URL for an online bug tracking
       system.

    For example Cabal itself uses a web-based bug tracking system

    ::

        bug-reports: https://github.com/haskell/cabal/issues

.. pkg-field:: package-url: URL

    The location of a source bundle for the package. The distribution
    should be a Cabal package.

.. pkg-field:: synopsis: freeform

    A very short description of the package, for use in a table of
    packages. This is your headline, so keep it short (one line) but as
    informative as possible. Save space by not including the package
    name or saying it's written in Haskell.

.. pkg-field:: description: freeform

    Description of the package. This may be several paragraphs, and
    should be aimed at a Haskell programmer who has never heard of your
    package before.

    For library packages, this field is used as prologue text by
    :ref:`setup-haddock` and thus may contain the same markup as Haddock_
    documentation comments.

.. pkg-field:: category: freeform

    A classification category for future use by the package catalogue
    Hackage_. These categories have not
    yet been specified, but the upper levels of the module hierarchy
    make a good start.

.. pkg-field:: tested-with: compiler list

    A list of compilers and versions against which the package has been
    tested (or at least built). The value of this field is not used by Cabal
    and is rather intended as extra metadata for use by third party
    tooling, such as e.g. CI tooling.

    Here's a typical usage example:

    ::

        tested-with: GHC == 9.10.1, GHC == 9.8.2, GHC == 9.6.5

    The same can be spread over several lines, for instance:

    ::

        tested-with: GHC == 9.10.1
                   , GHC == 9.8.2
                   , GHC == 9.6.5

    The separating comma can also be dropped altogether:

    ::

        tested-with:
          GHC == 9.10.1
          GHC == 9.8.2
          GHC == 9.6.5

    However, this alternative might
    `disappear <https://github.com/haskell/cabal/issues/4894#issuecomment-909008657>`__
    in the future.

    Starting with :pkg-field:`cabal-version` 3.0,
    there are further conveniences.

    1. A preceding ``,`` is allowed, so a bullet-list style
       is possible (recommended):

        ::

            tested-with:
              , GHC == 9.10.1
              , GHC == 9.8.2
              , GHC == 9.6.5


    2. A concise set notation syntax is available:

       ::

          tested-with: GHC == { 9.10.1, 9.8.2, 9.6.5 }

.. pkg-field:: data-files: filename list

    A list of files to be installed for run-time use by the package.
    This is useful for packages that use a large amount of static data,
    such as tables of values or code templates. Cabal provides a way to
    `find these files at run-time <#accessing-data-files-from-package-code>`_.

    A limited form of ``*`` wildcards in file names, for example
    ``data-files: images/*.png`` matches all the ``.png`` files in the
    ``images`` directory. ``data-files: audio/**/*.mp3`` matches all
    the ``.mp3`` files in the ``audio`` directory, including
    subdirectories.

    The specific limitations of this wildcard syntax are

    - ``*`` wildcards are only allowed in place of the file name, not
      in the directory name or file extension. It must replace the
      whole file name (e.g., ``*.html`` is allowed, but
      ``chapter-*.html`` is not). If a wildcard is used, it must be
      used with an extension, so ``data-files: data/*`` is not
      allowed.

    - Prior to Cabal 2.4, when matching a wildcard plus extension, a
      file's full extension must match exactly, so ``*.gz`` matches
      ``foo.gz`` but not ``foo.tar.gz``. This restriction has been
      lifted when ``cabal-version: 2.4`` or greater so that ``*.gz``
      does match ``foo.tar.gz``

    - ``*`` wildcards will not match if the file name is empty (e.g.,
      ``*.html`` will not match ``foo/.html``).

    - ``**`` wildcards can only appear as the final path component
      before the file name (e.g., ``data/**/images/*.jpg`` is not
      allowed).

    - Prior to Cabal 3.8, if a ``**`` wildcard is used, then
      the file name must include a ``*`` wildcard (e.g.,
      ``data/**/README.rst`` was not allowed). As of ``cabal-version:
      3.8`` or greater, this restriction is lifted.

    - A wildcard that does not match any files is an error.

    The reason for providing only a very limited form of wildcard is to
    concisely express the common case of a large number of related files
    of the same file type without making it too easy to accidentally
    include unwanted files.

    On efficiency: if you use ``**`` patterns, the directory tree will
    be walked starting with the parent directory of the ``**``. If
    that's the root of the project, this might include ``.git/``,
    ``dist-newstyle/``, or other large directories! To avoid this
    behaviour, put the files that wildcards will match against in
    their own folder.

    ``**`` wildcards are available starting in Cabal 2.4
    and `bug-free since Cabal 3.0 <https://github.com/haskell/cabal/issues/6125#issuecomment-1379878419>`_.

.. pkg-field:: data-dir: directory

    The directory where Cabal looks for data files to install, relative
    to the source directory. By default, Cabal will look in the source
    directory itself.

.. pkg-field:: extra-source-files: filename list

    A list of additional files to be included in source distributions built with :ref:`setup-sdist`.
    As with :pkg-field:`data-files` it can use a limited form of ``*`` wildcards in file names.
    Files listed here are tracked by ``cabal build``; changes in these files cause (partial) rebuilds.

.. pkg-field:: extra-doc-files: filename list
    :since: 1.18

    A list of additional files to be included in source distributions,
    and also copied to the html directory when Haddock documentation is
    generated. As with :pkg-field:`data-files` it can use a limited form of
    ``*`` wildcards in file names.

.. pkg-field:: extra-tmp-files: filename list

    A list of additional files or directories to be removed by
    :ref:`setup-clean`. These  would typically be additional files created by
    additional hooks, such as the scheme described in the section on
    `system-dependent parameters`_.

.. pkg-field:: extra-files: filename list

    A list of additional files to be included in source distributions built with :ref:`setup-sdist`.
    As with :pkg-field:`data-files` it can use a limited form of ``*`` wildcards in file names.

Library
^^^^^^^

.. pkg-section:: library name
    :synopsis: Library build information.

    Build information for libraries.

    A package can include zero or more library components. A library can be
    unnamed or named (using the ``name`` argument). It can also be depended upon
    only by components in the same package (private) or by those components and
    components in other packages (public). A package can have no more than one
    unnamed library.

    .. Note::

       The 'cabal' executable provided by the 'cabal-install' package will not
       accept dependencies on sublibraries of packages with no unnamed library.

    This guide refers to an unnamed library as the main library and a named
    library as a sublibrary (such components may be considered as subidiary, or
    ancillary, to the main library). It refers to a private sublibrary as an
    internal library.

    A sublibrary cannot have the same name as its package.

    .. Note::

       Before version 3.4 of the Cabal specification, a private sublibrary could
       shadow a dependency on the main library of another package, if their
       names clashed.

    A main library is always public and a sublibrary is private by default.
    See the :pkg-field:`library:visibility` field for setting a sublibrary as
    public.

    Being able to include more than one public library in a package allows the
    separation of the unit of distribution (the package) from the unit of
    buildable code (the library). This is useful for Haskell projects with many
    libraries that are distributed together as it avoids duplication and
    potential inconsistencies.

    .. Note::

       Before version 3.0 of the Cabal specification, all sublibraries were
       internal libraries. Before version 2.0, a package could not include
       sublibraries.

    See :ref:`Sublibraries - Examples <sublibs>` for examples.

A library section should contain the following fields:

.. pkg-field:: visibility: visibility specifiers

    :since: 3.0

    :default:
        ``private`` for sublibraries. Cannot be set for the main library, which
        is always public.

    Can be set to ``private`` or ``public``. A ``private`` library component can
    only be depended on by other components of the same package. A ``public``
    component can be depended on by those components and by components of other
    packages.

    See the :pkg-field:`build-depends` field for the syntax to specify a
    dependency on a library component.

.. pkg-field:: exposed-modules: identifier list

    :required: if this package contains a library

    A list of modules added by this package.

.. pkg-field:: virtual-modules: identifier list
    :since: 2.2

    A list of virtual modules provided by this package.  Virtual modules
    are modules without a source file.  See for example the ``GHC.Prim``
    module from the ``ghc-prim`` package.  Modules listed here will not be
    built, but still end up in the list of ``exposed-modules`` in the
    installed package info when the package is registered in the package
    database.

.. pkg-field:: exposed: boolean

    :default: ``True``

    Some Haskell compilers (notably GHC) support the notion of packages
    being "exposed" or "hidden" which means the modules they provide can
    be easily imported without always having to specify which package
    they come from. However this only works effectively if the modules
    provided by all exposed packages do not overlap (otherwise a module
    import would be ambiguous).

    Almost all new libraries use hierarchical module names that do not
    clash, so it is very uncommon to have to use this field. However it
    may be necessary to set ``exposed: False`` for some old libraries
    that use a flat module namespace or where it is known that the
    exposed modules would clash with other common modules.

.. pkg-field:: reexported-modules: exportlist
    :since: 1.22

    Supported only in GHC 7.10 and later. A list of modules to
    *reexport* from this package. The syntax of this field is
    ``orig-pkg:Name as NewName`` to reexport module ``Name`` from
    ``orig-pkg`` with the new name ``NewName``. We also support
    abbreviated versions of the syntax: if you omit ``as NewName``,
    we'll reexport without renaming; if you omit ``orig-pkg``, then we
    will automatically figure out which package to reexport from, if
    it's unambiguous.

    Reexported modules are useful for compatibility shims when a package
    has been split into multiple packages, and they have the useful
    property that if a package provides a module, and another package
    reexports it under the same name, these are not considered a
    conflict (as would be the case with a stub module.) They can also be
    used to resolve name conflicts.

.. pkg-field:: signatures: signature list
    :since: 2.0

    Supported only in GHC 8.2 and later. A list of `module signatures <https://downloads.haskell.org/~ghc/master/users-guide/separate_compilation.html#module-signatures>`__ required by this package.

    Module signatures are part of the :ref:`Backpack` extension to
    the Haskell module system.

    Packages that do not export any modules and only export required signatures
    are called "signature-only packages", and their signatures are subjected to
    `signature thinning
    <https://wiki.haskell.org/Module_signature#How_to_use_a_signature_package>`__.



The library section may also contain build information fields (see the
section on `build information`_).

.. _sublibs:

**Sublibraries - Examples**

An example of the use of a private sublibrary (an internal library) is a test
suite that needs access to some internal modules in the package's main library,
which you do not otherwise want to expose. You could put those modules in an
internal library, which the main library and the test suite
:pkg-field:`build-depends` upon. Your Cabal file might then look something like
this:

::

    cabal-version:  3.4
    name:           foo
    version:        0.1.0.0
    license:        BSD-3-Clause
    license-file:   LICENSE
    build-type:     Simple

    library foo-internal
        exposed-modules:  Foo.Internal
        -- NOTE: no explicit constraints on base needed
        --       as they're inherited from the 'library' stanza
        build-depends:    base
        default-language: Haskell2010

    library
        exposed-modules:  Foo.Public
        build-depends:    foo:foo-internal, base >= 4.3 && < 5
        default-language: Haskell2010

    test-suite test-foo
        type:             exitcode-stdio-1.0
        main-is:          test-foo.hs
        -- NOTE: no constraints on 'foo-internal' as same-package
        --       dependencies implicitly refer to the same package instance
        build-depends:    foo:foo-internal, base
        default-language: Haskell2010

Another example of the use of internal libraries is a package that includes one
or more executables but does not include a public library.

Internal libraries can be used to incorporate (vendor or bundle) an external
dependency into a package, effectively simulating *private dependencies*. Below
is an example:

::

    cabal-version: 3.4
    name: haddock-library
    version: 1.6.0
    license: BSD-3-Clause

    library
      build-depends:
        , base         ^>= 4.19.0.0
        , bytestring   ^>= 0.12.0.0
        , containers   ^>= 0.6.8 || ^>= 0.7.0
        , transformers ^>= 0.6.1.0

      hs-source-dirs:       src

      -- internal sub-lib
      build-depends:        haddock-library:attoparsec

      exposed-modules:
        Documentation.Haddock

      default-language: Haskell2010

    library attoparsec
      build-depends:
        , base         ^>= 4.19.0.0
        , bytestring   ^>= 0.12.0.0
        , deepseq      ^>= 1.5.0.0

      hs-source-dirs:       vendor/attoparsec-0.13.1.0

      -- NB: haddock-library needs only small part of lib:attoparsec
      --     internally, so we only bundle that subset here
      exposed-modules:
        Data.Attoparsec.ByteString
        Data.Attoparsec.Combinator

      other-modules:
        Data.Attoparsec.Internal

      ghc-options: -funbox-strict-fields -Wall -fwarn-tabs -O2

      default-language: Haskell2010

Executables
^^^^^^^^^^^

A package description can contain multiple executable sections.
The documentation of the `cabal run <cabal-commands.html#cabal-run>`__ command
contains detailed information on how to run an executable.

.. pkg-section:: executable name
    :synopsis: Executable build info section.

    Executable sections (if present) describe executable programs contained
    in the package and must have an argument after the section label, which
    defines the name of the executable. This is a freeform argument but may
    not contain spaces.

The executable may be described using the following fields, as well as
build information fields (see the section on `build information`_).

.. pkg-field:: main-is: filename (required)

    By convention, a Haskell program must have a module called ``Main`` which
    exports an IO action named ``main``. When the program is executed, the
    action is performed. This field specifies the name of the ``.hs`` or
    ``.lhs`` source file containing that module. It is the ``.hs`` filename that
    must be listed, even if that file is generated using a preprocessor. The
    file must be relative to one of the directories listed in
    :pkg-field:`hs-source-dirs`.

    Further, while the name of the source file may vary, if the convention is
    being followed, the module itself must be named ``Main`` and export
    ``main``.

    However, GHC's ``-main-is`` option can be used to change the name of the
    relevant IO action. For example, if source file ``MyMainSourceFile.hs``
    contains a module named ``MyMainModule`` exporting ``myMainFunc`` and that
    is to be the relevant IO action, you can specify:

    ::

        executable my-app
          main-is: MyMainSourceFile.hs
          ghc-options: -main-is MyMainModule.myMainFunc
          build-depends:
              base
          default-language: Haskell2010


    Starting with ``cabal-version: 1.18`` this field supports
    specifying a C, C++, or objC source file as the main entry point.

.. pkg-field:: scope: token
    :since: 2.0

    Whether the executable is ``public`` (default) or ``private``, i.e. meant to
    be run by other programs rather than the user. Private executables are
    installed into `$libexecdir/$libexecsubdir`.


Test suites
^^^^^^^^^^^

A package description can contain multiple test suite sections.
The documentation of the `cabal test <cabal-commands.html#cabal-test>`__ command
contains detailed information on how to run test suites.

.. pkg-section:: test-suite name
    :synopsis: Test suite build information.

    Test suite sections (if present) describe package test suites and must
    have an argument after the section label, which defines the name of the
    test suite. This is a freeform argument, but may not contain spaces. It
    should be unique among the names of the package's other test suites, the
    package's executables, and the package itself. Using test suite sections
    requires at least Cabal version 1.9.2.

The test suite may be described using the following fields, as well as
build information fields (see the section on `build information`_).

.. pkg-field:: type: interface (required until ``cabal-version`` 3.8)

    The interface type and version of the test suite. Cabal supports two
    test suite interfaces, called ``exitcode-stdio-1.0`` (default since ``cabal-version`` 3.8) and
    ``detailed-0.9``. Each of these types may require or disallow other
    fields as described below.

Test suites using the ``exitcode-stdio-1.0`` (default since ``cabal-version`` 3.8) interface are executables
that indicate test failure with a non-zero exit code when run; they may
provide human-readable log information through the standard output and
error channels. The ``exitcode-stdio-1.0`` type requires the ``main-is``
field.

.. pkg-field:: main-is: filename
    :synopsis: Module containing tests main function.

    :required: ``exitcode-stdio-1.0``
    :disallowed: ``detailed-0.9``

    The name of the ``.hs`` or ``.lhs`` file containing the ``Main``
    module. Note that it is the ``.hs`` filename that must be listed,
    even if that file is generated using a preprocessor. The source file
    must be relative to one of the directories listed in
    :pkg-field:`hs-source-dirs`. This field is analogous to the ``main-is`` field
    of an executable section; see that documentation for further information.

Test suites using the ``detailed-0.9`` interface are modules exporting
the symbol ``tests :: IO [Test]``. The ``Test`` type is exported by the
module ``Distribution.TestSuite`` provided by Cabal. For more details,
see the example below.

The ``detailed-0.9`` interface allows Cabal and other test agents to
inspect a test suite's results case by case, producing detailed human-
and machine-readable log files. The ``detailed-0.9`` interface requires
the :pkg-field:`test-module` field.

.. pkg-field:: test-module: identifier

    :required: ``detailed-0.9``
    :disallowed: ``exitcode-stdio-1.0``

    The module exporting the ``tests`` symbol.

.. pkg-field:: code-generators

    An optional list of preprocessors which can generate new modules
    for use in the test-suite.

 A list of executables (possibly brought into scope by
 :pkg-field:`build-tool-depends`) that are run after all other
 preprocessors. These executables are invoked as so: ``exe-name
 TARGETDIR [SOURCEDIRS] -- [GHCOPTIONS]``. The arguments are, in order a target dir for
 output, a sequence of all source directories with source files of
 local lib components that the given test stanza depends on, and
 following a double dash, all options cabal would pass to ghc for a
 build. They are expected to output a newline-separated list of
 generated modules which have been written to the targetdir
 (excepting, if written, the main module). This can
 be used for driving doctests and other discover-style tests generated
 from source code.


Example: Package using ``exitcode-stdio-1.0`` interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

The example package description and executable source file below
demonstrate the use of the ``exitcode-stdio-1.0`` interface.

.. code-block:: cabal
    :caption: foo.cabal

    Cabal-Version:  3.0
    Name:           foo
    Version:        1.0
    License:        BSD-3-Clause
    Build-Type:     Simple

    Test-Suite test-foo
        type:             exitcode-stdio-1.0
        main-is:          test-foo.hs
        build-depends:    base >= 4 && < 5
        default-language: Haskell2010

.. code-block:: haskell
    :caption: test-foo.hs

    module Main where

    import System.Exit (exitFailure)

    main = do
        putStrLn "This test always fails!"
        exitFailure

Example: Package using ``detailed-0.9`` interface
"""""""""""""""""""""""""""""""""""""""""""""""""

The example package description and test module source file below
demonstrate the use of the ``detailed-0.9`` interface. The test module
also develops a simple implementation of the interface set by
``Distribution.TestSuite``, but in actual usage the implementation would
be provided by the library that provides the testing facility.

.. code-block:: cabal
    :caption: bar.cabal

    Cabal-Version:  3.0
    Name:           bar
    Version:        1.0
    License:        BSD-3-Clause
    Build-Type:     Simple

    Test-Suite test-bar
        type:             detailed-0.9
        test-module:      Bar
        -- To keep this (untested) example working, we rely on the test type
        -- version rather than an upper bound on Cabal. In real code, PVP
        -- compliance may require including the upper bound.
        build-depends:    base >= 4 && < 5, Cabal >= 1.9.2
        default-language: Haskell2010


.. code-block:: haskell
    :caption: Bar.hs

    module Bar ( tests ) where

    import Distribution.TestSuite

    tests :: IO [Test]
    tests = return [ Test succeeds, Test fails ]
      where
        succeeds = TestInstance
            { run = return $ Finished Pass
            , name = "succeeds"
            , tags = []
            , options = []
            , setOption = \_ _ -> Right succeeds
            }
        fails = TestInstance
            { run = return $ Finished $ Fail "Always fails!"
            , name = "fails"
            , tags = []
            , options = []
            , setOption = \_ _ -> Right fails
            }

Benchmarks
^^^^^^^^^^

A package description can contain multiple benchmark sections.
The documentation of the `cabal bench <cabal-commands.html#cabal-bench>`__ command
contains detailed information on how to run benchmarks.

.. pkg-section:: benchmark name
    :since: 1.9.2
    :synopsis: Benchmark build information.

    Benchmark sections (if present) describe benchmarks contained in the
    package and must have an argument after the section label, which defines
    the name of the benchmark. This is a freeform argument, but may not
    contain spaces. It should be unique among the names of the package's
    other benchmarks, the package's test suites, the package's executables,
    and the package itself. Using benchmark sections requires at least Cabal
    version 1.9.2.

The benchmark may be described using the following fields, as well as
build information fields (see the section on `build information`_).

.. pkg-field:: type: interface (required until ``cabal-version`` 3.8)

    The interface type and version of the benchmark. At the moment Cabal
    only support one benchmark interface, called ``exitcode-stdio-1.0``.

Benchmarks using the ``exitcode-stdio-1.0`` (default since ``cabal-version`` 3.8) interface are executables
that indicate failure to run the benchmark with a non-zero exit code
when run; they may provide human-readable information through the
standard output and error channels.

.. pkg-field:: main-is: filename

    The name of the ``.hs`` or ``.lhs`` file containing the ``Main``
    module. Note that it is the ``.hs`` filename that must be listed,
    even if that file is generated using a preprocessor. The source file
    must be relative to one of the directories listed in
    :pkg-field:`hs-source-dirs`. This field is analogous to the ``main-is``
    field of an executable section; see that documentation for further
    information.

Example:
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

.. code-block:: cabal
    :caption: foo.cabal
    :name: foo-bench.cabal

    Cabal-Version:  3.0
    Name:           foo
    Version:        1.0
    License:        BSD-3-Clause
    Build-Type:     Simple

    Benchmark bench-foo
        type:             exitcode-stdio-1.0
        main-is:          bench-foo.hs
        build-depends:    base >= 4 && < 5, time >= 1.1 && < 1.7
        default-language: Haskell2010

.. code-block:: haskell
    :caption: bench-foo.hs

    {-# LANGUAGE BangPatterns #-}
    module Main where

    import Data.Time.Clock

    fib 0 = 1
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

    main = do
        start <- getCurrentTime
        let !r = fib 20
        end <- getCurrentTime
        putStrLn $ "fib 20 took " ++ show (diffUTCTime end start)

.. _build-info:

Build information
^^^^^^^^^^^^^^^^^
.. pkg-section:: None

The following fields may be optionally present in a library, executable,
test suite or benchmark section, and give information for the building
of the corresponding library or executable. See also the sections on
`system-dependent parameters`_ and `configurations`_ for a way to supply
system-dependent values for these fields.

.. pkg-field:: build-depends: library list

    Declares the dependencies on *library* components required to build the
    current package component. See :pkg-field:`build-tool-depends` for declaring
    dependencies on build-time *tools*. Dependencies on libraries from another
    package should be annotated with a version constraint.

    **Library Names**

    A library is identified by the name of its package, optionally followed by a
    colon and the library's name (for example, ``my-package:my-library``). If a
    library name is omitted, the package's main library will be used. To refer
    expressly to a package's main library, use the name of the package as the
    library name (for example, ``my-package:my-package``). More than one library
    from the same package can be specified with the shorthand syntax
    ``my-package:{my-library1,my-library2}``.

    .. Note::

       The :pkg-field:`build-depends` field of an executable section (from
       version 1.8 of the Cabal specification), test suite section (from
       version 1.10) or benchmark section (from version 1.14) can specify a
       library in the same package by the name of the library (without a version
       constraint). Cabal then treats the executable, test suite or benchmark as
       if it were in another package that depended on the package.

    .. Note::

       Before version 3.4 of the Cabal specification, from version 2.0, a
       private sublibrary (an internal library) was identified by only the name
       of the sublibrary. An internal library could shadow a dependency on the
       main library of another package, if the names clashed.

    See the section on :pkg-section:`library` for information about how a
    package can specify library components.

    **Version Constraints**

    Version constraints use the operators ``==, >=, >, <, <=`` and a
    version number. Multiple constraints can be combined using ``&&`` or
    ``||``.

    .. Note::

       Even though there is no ``/=`` operator, by combining operators we can
       skip over one or more versions, to skip a deprecated version or to skip
       versions that narrow the constraint solving more than we'd like.

       For example, the ``time =1.12.*`` series depends on ``base >=4.13 && <5``
       but ``time-1.12.3`` bumps the lower bound on base to ``>=4.14``.  If we
       still want to compile with a ``ghc-8.8.*`` version of GHC that ships with
       ``base-4.13`` and with later GHC versions, then we can use ``time >=1.12
       && (<1.12.3 || >1.12.3)``.

       Hackage shows deprecated and preferred versions for packages, such as for
       `containers <https://hackage.haskell.org/package/containers/preferred>`_
       and `aeson <https://hackage.haskell.org/package/aeson/preferred>`_ for
       example. Deprecating package versions is not the same deprecating a
       package as a whole, for which Hackage keeps a `deprecated packages list
       <https://hackage.haskell.org/packages/deprecated>`_.

    If no version constraint is specified, any version is assumed to be
    acceptable. For example:

    ::

        library
          build-depends:
            base >= 2,
            foo >= 1.2.3 && < 1.3,
            bar

    Dependencies like ``foo >= 1.2.3 && < 1.3`` turn out to be very
    common because it is recommended practice for package versions to
    correspond to API versions (see PVP_).

    Since Cabal 1.6, there is a special wildcard syntax to help with
    such ranges

    ::

        build-depends: foo ==1.2.*

    It is only syntactic sugar. It is exactly equivalent to
    ``foo >= 1.2 && < 1.3``.

    .. Warning::

       A potential pitfall of the wildcard syntax is that the
       constraint ``nats == 1.0.*`` doesn't match the release
       ``nats-1`` because the version ``1`` is lexicographically less
       than ``1.0``. This is not an issue with the caret-operator
       ``^>=`` described below.

    Starting with Cabal 2.0, there's a new version operator to express
    PVP_-style major upper bounds conveniently, and is inspired by similar
    syntactic sugar found in other language ecosystems where it's often
    called the "Caret" operator:

    ::

        build-depends:
          foo ^>= 1.2.3.4,
          bar ^>= 1

    This allows to assert the positive knowledge that this package is
    *known* to be semantically compatible with the releases
    ``foo-1.2.3.4`` and ``bar-1`` respectively. The information
    encoded via such ``^>=``-assertions is used by the cabal solver to
    infer version constraints describing semantically compatible
    version ranges according to the PVP_ contract (see below).

    Another way to say this is that ``foo < 1.3`` expresses *negative*
    information, i.e. "``foo-1.3`` or ``foo-1.4.2`` will *not* be
    compatible"; whereas ``foo ^>= 1.2.3.4`` asserts the *positive*
    information that "``foo-1.2.3.4`` is *known* to be compatible" and (in
    the absence of additional information) according to the PVP_
    contract we can (positively) infer right away that all versions
    satisfying ``foo >= 1.2.3.4 && < 1.3`` will be compatible as well.

    .. Note::

       More generally, the PVP_ contract implies that we can safely
       relax the lower bound to ``>= 1.2``, because if we know that
       ``foo-1.2.3.4`` is semantically compatible, then so is
       ``foo-1.2`` (if it typechecks). But we'd need to perform
       additional static analysis (i.e. perform typechecking) in order
       to know if our package in the role of an API consumer will
       successfully typecheck against the dependency ``foo-1.2``.  But
       since we cannot do this analysis during constraint solving and
       to keep things simple, we pragmatically use ``foo >= 1.2.3.4``
       as the initially inferred approximation for the lower bound
       resulting from the assertion ``foo ^>= 1.2.3.4``. If further
       evidence becomes available that e.g. ``foo-1.2`` typechecks,
       one can simply revise the dependency specification to include
       the assertion ``foo ^>= 1.2``.

    The subtle but important difference in signaling allows tooling to
    treat explicitly expressed ``<``-style constraints and inferred
    (``^>=``-style) upper bounds differently.  For instance,
    :cfg-field:`allow-newer`'s ``^``-modifier allows to relax only
    ``^>=``-style bounds while leaving explicitly stated
    ``<``-constraints unaffected.

    Ignoring the signaling intent, the default syntactic desugaring rules are

    - ``^>= x`` == ``>= x && < x.1``
    - ``^>= x.y`` == ``>= x.y && < x.(y+1)``
    - ``^>= x.y.z`` == ``>= x.y.z && < x.(y+1)``
    - ``^>= x.y.z.u`` == ``>= x.y.z.u && < x.(y+1)``
    - etc.

    .. Note::

       One might expect the desugaring to truncate all version
       components below (and including) the patch-level, i.e.
       ``^>= x.y.z.u`` == ``>= x.y.z && < x.(y+1)``,
       as the major and minor version components alone are supposed to
       uniquely identify the API according to the PVP_.  However, by
       designing ``^>=`` to be closer to the ``>=`` operator, we avoid
       the potentially confusing effect of ``^>=`` being more liberal
       than ``>=`` in the presence of patch-level versions.

    Consequently, the example declaration above is equivalent to

    ::

        build-depends:
          foo >= 1.2.3.4 && < 1.3,
          bar >= 1 && < 1.1

    .. Note::

       Prior to Cabal 1.8, ``build-depends`` specified in each
       section were global to all sections. This was unintentional, but
       some packages were written to depend on it, so if you need your
       :pkg-field:`build-depends` to be local to each section, you must specify
       at least ``Cabal-Version: >= 1.8`` in your ``.cabal`` file.

    .. Note::

       Cabal 1.20 experimentally supported module thinning and
       renaming in ``build-depends``; however, this support has since been
       removed and should not be used.

    Starting with Cabal 3.0, a set notation for the ``==`` and ``^>=`` operator
    is available. For instance,

    ::

        tested-with: GHC == 8.6.3, GHC == 8.4.4, GHC == 8.2.2, GHC == 8.0.2,
                     GHC == 7.10.3, GHC == 7.8.4, GHC == 7.6.3, GHC == 7.4.2

        build-depends: network ^>= 2.6.3.6 || ^>= 2.7.0.2 || ^>= 2.8.0.0 || ^>= 3.0.1.0

    can be then written in a more convenient and concise form

    ::

        tested-with: GHC == { 8.6.3, 8.4.4, 8.2.2, 8.0.2, 7.10.3, 7.8.4, 7.6.3, 7.4.2 }

        build-depends: network ^>= { 2.6.3.6, 2.7.0.2, 2.8.0.0, 3.0.1.0 }


.. pkg-field:: other-modules: identifier list

    A list of modules used by the component but not exposed to users.
    For a library component, these would be hidden modules of the
    library. For an executable, these would be auxiliary modules to be
    linked with the file named in the ``main-is`` field.

    .. Note::

       Every module in the package *must* be listed in one of
       :pkg-field:`other-modules`, :pkg-field:`library:exposed-modules` or
       :pkg-field:`executable:main-is` fields.

.. pkg-field:: hs-source-dir: directory list
    :deprecated: 2.0
    :removed: 3.0

    :default: ``.``

    Root directories for the module hierarchy.

    Deprecated in favor of :pkg-field:`hs-source-dirs`.

.. pkg-field:: hs-source-dirs: directory list

    :default: ``.``

    Root directories for the module hierarchy.

    .. note::

      Components can share source directories but modules found there will be
      recompiled even if other components already built them, i.e., if a
      library and an executable share a source directory and the executable
      depends on the library and imports its ``Foo`` module, ``Foo`` will be
      compiled twice, once as part of the library and again for the executable.

.. pkg-field:: default-extensions: identifier list
   :since: 1.10

    A list of Haskell extensions used by every module. These determine
    corresponding compiler options enabled for all files. Extension
    names are the constructors of the
    `Extension <https://hackage.haskell.org/package/Cabal-syntax/docs/Language-Haskell-Extension.html#t:Extension>`__
    type. For example, ``CPP`` specifies that Haskell source files are
    to be preprocessed with a C preprocessor.

.. pkg-field:: other-extensions: identifier list
   :since: 1.10

    A list of Haskell extensions used by some (but not necessarily all)
    modules. From GHC version 6.6 onward, these may be specified by
    placing a ``LANGUAGE`` pragma in the source files affected e.g.

    .. code-block:: haskell

        {-# LANGUAGE CPP, MultiParamTypeClasses #-}

    In Cabal-1.24 the dependency solver will use this and
    :pkg-field:`default-extensions` information. Cabal prior to 1.24 will abort
    compilation if the current compiler doesn't provide the extensions.

    If you use some extensions conditionally, using CPP or conditional
    module lists, it is good to replicate the condition in
    :pkg-field:`other-extensions` declarations:

    ::

        other-extensions: CPP
        if impl(ghc >= 7.5)
          other-extensions: PolyKinds

    You could also omit the conditionally used extensions, as they are
    for information only, but it is recommended to replicate them in
    :pkg-field:`other-extensions` declarations.

.. pkg-field:: default-language: identifier
   :since: 1.10

    Specifies a language standard or a group of language extensions to be activated for the project. In the case of GHC, `see here for details <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/control.html#controlling-extensions>`__.

    The possible values are:

    -  ``GHC2024`` (only available for GHC version ``9.10`` or later)
    -  ``GHC2021`` (only available for GHC version ``9.2`` or later)
    -  ``Haskell2010``
    -  ``Haskell98``

.. pkg-field:: other-languages: identifier
   :since: 1.10

    Specifies a language standard used by some (but not necessarily all) modules.

.. pkg-field:: extensions: identifier list
   :deprecated: 1.10
   :removed: 3.0

   Deprecated in favor of :pkg-field:`default-extensions`.

.. pkg-field:: build-tool-depends: package:executable list
    :since: 2.0

    A list of Haskell executables needed to build this component. Executables are provided
    during the whole duration of the component, so this field can be used for executables
    needed during :pkg-section:`test-suite` as well.

    Each is specified by the package containing the executable and the name of the
    executable itself, separated by a colon, and optionally followed by a version bound.

    All executables defined in the given Cabal file are termed as *internal* dependencies
    as opposed to the rest which are *external* dependencies.

    Each of the two is handled differently:

    1. External dependencies can (and should) contain a version bound like conventional
       :pkg-field:`build-depends` dependencies.
    2. Internal dependencies should not contain a version bound, as they will be always
       resolved within the same configuration of the package in the build plan.
       Specifically, version bounds that include the package's version will be warned for
       being extraneous, and version bounds that exclude the package's version will raise
       an error for being impossible to follow.

    For example (1) using a test-suite to make sure README.md Haskell snippets are tested using
    `markdown-unlit <http://hackage.haskell.org/package/markdown-unlit>`__:

    ::

        build-tool-depends: markdown-unlit:markdown-unlit >= 0.5.0 && < 0.6

    For example (2) using a test-suite to test executable behaviour in the same package:

    ::

        build-tool-depends: mypackage:executable

    Cabal tries to make sure that all specified programs are atomically built and prepended
    on the ``PATH`` shell variable before building the component in question, but can only do
    so for Nix-style builds. Specifically:

    a) For Nix-style local builds, both internal and external dependencies.
    b) For old-style builds, only for internal dependencies [#old-style-build-tool-depends]_.
       It's up to the user to provide needed executables in this case under ``PATH``.


    .. note::

      :pkg-field:`build-tool-depends` was added in Cabal 2.0, and it will
      be ignored (with a warning) with old versions of Cabal.  See
      :pkg-field:`build-tools` for more information about backwards
      compatibility.

.. pkg-field:: build-tools: program list
    :deprecated: 2.0
    :removed: 3.0

    Deprecated in favor of :pkg-field:`build-tool-depends`, but :ref:`see below for backwards compatibility information <buildtoolsbc>`.

    A list of Haskell programs needed to build this component.
    Each may be followed by an optional version bound.
    Confusingly, each program in the list either refer to one of three things:

      1. Another executables in the same package (supported since Cabal 1.12)

      2. Tool name contained in Cabal's :ref:`hard-coded set of common tools <buildtoolsmap>`

      3. A pre-built executable that should already be on the ``PATH``
         (supported since Cabal 2.0)

    These cases are listed in order of priority:
    an executable in the package will override any of the hard-coded packages with the same name,
    and a hard-coded package will override any executable on the ``PATH``.

    In the first two cases, the list entry is desugared into a :pkg-field:`build-tool-depends` entry.
    In the first case, the entry is desugared into a :pkg-field:`build-tool-depends` entry by prefixing with ``$pkg:``.
    In the second case, it is desugared by looking up the package and executable name in a hard-coded table.
    In either case, the optional version bound is passed through unchanged.
    Refer to the documentation for :pkg-field:`build-tool-depends` to understand the desugared field's meaning, along with restrictions on version bounds.

    .. _buildtoolsbc:

    **Backward Compatibility**

    Although this field is deprecated in favor of :pkg-field:`build-tool-depends`, there are some situations where you may prefer to use :pkg-field:`build-tools` in cases (1) and (2), as it is supported by more versions of Cabal.
    In case (3), :pkg-field:`build-tool-depends` is better for backwards-compatibility, as it will be ignored by old versions of Cabal; if you add the executable to :pkg-field:`build-tools`, a setup script built against old Cabal will choke.
    If an old version of Cabal is used, an end-user will have to manually arrange for the requested executable to be in your ``PATH``.

    .. _buildtoolsmap:

    **Set of Known Tool Names**

    Identifiers specified in :pkg-field:`build-tools` are desugared into their respective equivalent :pkg-field:`build-tool-depends` form according to the table below. Consequently, a legacy specification such as::

        build-tools: alex >= 3.2.1 && < 3.3, happy >= 1.19.5 && < 1.20

    is simply desugared into the equivalent specification::

        build-tool-depends: alex:alex >= 3.2.1 && < 3.3, happy:happy >= 1.19.5 && < 1.20

    +--------------------------+-----------------------------------+-----------------+
    | :pkg-field:`build-tools` | desugared                         | Note            |
    | identifier               | :pkg-field:`build-tool-depends`   |                 |
    |                          | identifier                        |                 |
    +==========================+===================================+=================+
    | ``alex``                 | ``alex:alex``                     |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``c2hs``                 | ``c2hs:c2hs``                     |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``cpphs``                | ``cpphs:cpphs``                   |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``haddock``              | ``haddock:haddock``               |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``happy``                | ``happy:happy``                   |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``hsc2hs``               | ``hsc2hs:hsc2hs``                 |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``hscolour``             | ``hscolour:hscolour``             |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``hspec-discover``       | ``hspec-discover:hspec-discover`` | since Cabal 2.0 |
    +--------------------------+-----------------------------------+-----------------+

    This built-in set can be programmatically extended via use of the
    :ref:`Hooks build type<setup-hooks>` .

.. pkg-field:: buildable: boolean

    :default: ``True``

    Is the component buildable? Like some of the other fields below,
    this field is more useful with the slightly more elaborate form of
    the simple build infrastructure described in the section on
    `system-dependent parameters`_.

.. pkg-field:: ghc-options: token list

    Additional options for GHC.

    If specifying extensions (via ``-X<Extension>`` flags) one can often achieve
    the same effect using the :pkg-field:`default-extensions` field, which is
    preferred.

    Options required only by one module may be specified by placing an
    ``OPTIONS_GHC`` pragma in the source file affected.

    As with many other fields, whitespace can be escaped by using
    Haskell string syntax. Example:
    ``ghc-options: -Wcompat "-with-rtsopts=-T -I1" -Wall``.

.. pkg-field:: ghc-prof-options: token list

    Additional options for GHC when the package is built with profiling
    enabled.

    Note that as of Cabal-1.24, the default profiling detail level
    defaults to ``exported-functions`` for libraries and
    ``toplevel-functions`` for executables. For GHC these correspond to
    the flags ``-fprof-auto-exported`` and ``-fprof-auto-top``. Prior to
    Cabal-1.24 the level defaulted to ``none``. These levels can be
    adjusted by the person building the package with the
    ``--profiling-detail`` and ``--library-profiling-detail`` flags.

    It is typically better for the person building the package to pick
    the profiling detail level rather than for the package author. So
    unless you have special needs it is probably better not to specify
    any of the GHC ``-fprof-auto*`` flags here. However if you wish to
    override the profiling detail level, you can do so using the
    :pkg-field:`ghc-prof-options` field: use ``-fno-prof-auto`` or one of the
    other ``-fprof-auto*`` flags.

.. pkg-field:: ghc-shared-options: token list

    Additional options for GHC when the package is built as shared
    library. The options specified via this field are combined with the
    ones specified via :pkg-field:`ghc-options`, and are passed to GHC during
    both the compile and link phases.

.. pkg-field:: ghc-prof-shared-options: token list

    Additional options for GHC when the package is built as shared profiling
    library. The options specified via this field are combined with the
    ones specified via :pkg-field:`ghc-options`, and are passed to GHC during
    both the compile and link phases.

    Note that if any :pkg-field:`ghc-shared-options` are set, the
    ``-dynamic-too` option will never be passed to GHC, leading to all modules
    being compiled twice (once to generate the ``.o`` files and another to
    generate the ``.dyn_o`` files).

.. pkg-field:: ghcjs-options: token list

   Like :pkg-field:`ghc-options` but applies to GHCJS

.. pkg-field:: ghcjs-prof-options: token list

   Like :pkg-field:`ghc-prof-options` but applies to GHCJS

.. pkg-field:: ghcjs-shared-options: token list

   Like :pkg-field:`ghc-shared-options` but applies to GHCJS

.. pkg-field:: ghcjs-prof-shared-options: token list

   Like :pkg-field:`ghc-prof-shared-options` but applies to GHCJS

.. pkg-field:: includes: filename list
    :since: 1.0
    :deprecated: 2.0

    From GHC 6.10.1, :pkg-field:`includes` has no effect when compiling with
    GHC. From Cabal 2.0, support for GHC versions before GHC 6.12 was removed.

    A list of header files to be included in any compilations via C.
    This field applies to both header files that are already installed
    on the system and to those coming with the package to be installed.
    The former files should be found in absolute paths, while the latter
    files should be found in paths relative to the top of the source
    tree or relative to one of the directories listed in
    :pkg-field:`include-dirs`.

    These files typically contain function prototypes for foreign
    imports used by the package. This is in contrast to
    :pkg-field:`install-includes`, which lists header files that are intended
    to be exposed to other packages that transitively depend on this
    library.

.. pkg-field:: install-includes: filename list

    A list of header files from this package to be installed into
    ``$libdir/includes`` when the package is installed. Files listed in
    :pkg-field:`install-includes` should be found in relative to the top of the
    source tree or relative to one of the directories listed in
    :pkg-field:`include-dirs`.

    :pkg-field:`install-includes` is typically used to name header files that
    contain prototypes for foreign imports used in Haskell code in this
    package, for which the C implementations are also provided with the
    package. For example, here is a ``.cabal`` file for a hypothetical
    ``bindings-clib`` package that bundles the C source code for ``clib``::

        include-dirs:     cbits
        c-sources:        clib.c
        install-includes: clib.h

    Now any package that depends (directly or transitively) on the
    ``bindings-clib`` library can use ``clib.h``.

    Note that in order for files listed in :pkg-field:`install-includes` to be
    usable when compiling the package itself, they need to be listed in
    the :pkg-field:`includes` field as well.

.. pkg-field:: include-dirs: directory list

    A list of directories to search for header files, when preprocessing
    with ``c2hs``, ``hsc2hs``, ``cpphs`` or the C preprocessor, and also
    when compiling via C. Directories can be absolute paths (e.g., for
    system directories) or paths that are relative to the top of the
    source tree. Cabal looks in these directories when attempting to
    locate files listed in :pkg-field:`includes` and
    :pkg-field:`install-includes`.

    Directories here will be passed as ``-I<dir>`` flags to GHC.

.. pkg-field:: c-sources: filename list

    A list of C source files to be compiled and linked with the Haskell
    files.

.. pkg-field:: cxx-sources: filename list
    :since: 2.2

    A list of C++ source files to be compiled and linked with the Haskell
    files. Useful for segregating C and C++ sources when supplying different
    command-line arguments to the compiler via the :pkg-field:`cc-options`
    and the :pkg-field:`cxx-options` fields. The files listed in the
    :pkg-field:`cxx-sources` can reference files listed in the
    :pkg-field:`c-sources` field and vice-versa. The object files will be linked
    appropriately.

.. pkg-field:: asm-sources: filename list
    :since: 3.0

    A list of assembly source files to be compiled and linked with the
    Haskell files.

.. pkg-field:: cmm-sources: filename list
    :since: 3.0

    A list of C-- source files to be compiled and linked with the Haskell
    files.

.. pkg-field:: js-sources: filename list

    A list of JavaScript source files to be linked with the Haskell
    files (only for JavaScript targets).

.. pkg-field:: extra-libraries: token list

    A list of extra libraries to link with (when not linking fully static
    executables). Libraries will be passed as ``-optl-l<lib>`` flags to GHC.

.. pkg-field:: extra-libraries-static: token list

    A list of extra libraries to link with (when linking fully static
    executables).

.. pkg-field:: extra-ghci-libraries: token list

    A list of extra libraries to be used instead of 'extra-libraries'
    when the package is loaded with GHCi.

.. pkg-field:: extra-bundled-libraries: token list
   :since: 2.2

   A list of libraries that are supposed to be copied from the build
   directory alongside the produced Haskell libraries.  Note that you
   are under the obligation to produce those libraries in the build
   directory (e.g. via a custom setup).  Libraries listed here will
   be included when ``copy``-ing packages and be listed in the
   ``hs-libraries`` of the package configuration in the package database.
   Library names must either be prefixed with "HS" or "C" and corresponding
   library file names must match:

      - Libraries with name "HS<library-name>":
         - `libHS<library-name>.a`
         - `libHS<library-name>-ghc<ghc-flavour><ghc-version>.<dyn-library-extension>*`
      - Libraries with name "C<library-name>":
         - `libC<library-name>.a`
         - `lib<library-name>.<dyn-library-extension>*`

.. pkg-field:: extra-lib-dirs: directory list

    A list of directories to search for libraries (when not linking fully static
    executables). Directories will be passed as ``-optl-L<dir>`` flags to GHC.

.. pkg-field:: extra-lib-dirs-static: directory list

    A list of directories to search for libraries (when linking fully static
    executables).

.. pkg-field:: extra-library-flavours: notsure

    TBW

.. pkg-field:: extra-dynamic-library-flavours: notsure

    TBW

.. pkg-field:: cc-options: token list

    Command-line arguments to be passed to the Haskell compiler for the C
    compiling phase (as ``-optc`` flags for GHC). Since the
    arguments are compiler-dependent, this field is more useful with the
    setup described in the section on `system-dependent parameters`_.

.. pkg-field:: jspp-options: token list

    Command-line arguments for pre-processing JS code. Applies to pre-processed
    Haskell source like .js. Flags here will be passed as ``-optJSP`` flags to GHC.

.. pkg-field:: cpp-options: token list

    Command-line arguments for pre-processing Haskell code. Applies to
    Haskell source and other pre-processed Haskell source like .hsc
    .chs. Does not apply to C code, that's what cc-options is for.
    Flags here will be passed as ``-optP`` flags to GHC.

.. pkg-field:: cxx-options: token list
    :since: 2.2

    Command-line arguments to be passed to the Haskell compiler for the C++
    compiling phase (as ``-optcxx`` flags for GHC).
    The C++ sources to which these command-line arguments
    should be applied can be specified with the :pkg-field:`cxx-sources`
    field. Command-line options for C and C++ can be passed separately to
    the compiler when compiling both C and C++ sources by segregating the C
    and C++ sources with the :pkg-field:`c-sources` and
    :pkg-field:`cxx-sources` fields respectively, and providing different
    command-line arguments with the :pkg-field:`cc-options` and the
    :pkg-field:`cxx-options` fields.

.. pkg-field:: cmm-options: token list
    :since: 3.0

    Command-line arguments to be passed to the Haskell compiler when compiling
    C-- code. See also :pkg-field:`cmm-sources`.

.. pkg-field:: asm-options: token list
    :since: 3.0

    Command-line arguments to be passed to the Haskell compiler (as ``-opta``
    flags for GHC) when compiling assembler code. See also :pkg-field:`asm-sources`.

.. pkg-field:: ld-options: token list

    Command-line arguments to be passed to the Haskell compiler (as ``-optl``
    flags for GHC) for the linking phase. Note that only executables (including
    test-suites and benchmarks) are linked so this has no effect in libraries.
    Since the arguments are compiler-dependent, this field is more useful with
    the setup described in the section on `system-dependent parameters`_.

.. pkg-field:: hsc2hs-options: token list
    :since: 3.6

    Command-line arguments to be passed to ``hsc2hs``.

.. pkg-field:: pkgconfig-depends: package list

    A list of
    `pkg-config <http://www.freedesktop.org/wiki/Software/pkg-config/>`__
    packages, needed to build this package. They can be annotated with
    versions, e.g. ``gtk+-2.0 >= 2.10, cairo >= 1.0``. If no version
    constraint is specified, any version is assumed to be acceptable.
    Cabal uses ``pkg-config`` to find if the packages are available on
    the system and to find the extra compilation and linker options
    needed to use the packages.

    If you need to bind to a C library that supports ``pkg-config`` then
    it is much preferable to use this field rather than hard code options
    into the other fields. ``pkg-config --list-all`` will show you all
    supported libraries. Depending on your system you may need to adjust
    ``PKG_CONFIG_PATH``.

.. pkg-field:: frameworks: token list

    On Darwin/MacOS X, a list of frameworks to link to. See Apple's
    developer documentation for more details on frameworks. This entry
    is ignored on all other platforms.

.. pkg-field:: extra-framework-dirs: directory list
    :since: 1.24

    On Darwin/MacOS X, a list of directories to search for frameworks.
    This entry is ignored on all other platforms.

.. pkg-field:: mixins: mixin list
    :since: 2.0

    Supported only in GHC 8.2 and later. A list of packages mentioned in the
    :pkg-field:`build-depends` field, each optionally accompanied by a list of
    module and module signature renamings.  A valid mixin obeys the
    following syntax:

    ::

        Mixin ::= PackageName IncludeRenaming
        IncludeRenaming ::= ModuleRenaming { "requires" ModuleRenaming }
        ModuleRenaming ::=
            {- empty -}
          | "(" Renaming "," ... "," Renaming ")"
          | "hiding" "(" ModuleName "," ... "," ModuleName ")"
        Renaming ::=
            ModuleName
          | ModuleName "as" ModuleName

    The simplest mixin syntax is simply the name of a package mentioned in the
    :pkg-field:`build-depends` field. For example:

    ::

        library
          build-depends:
            foo ^>= 1.2.3
          mixins:
            foo

    But this doesn't have any effect. More interesting is to use the mixin
    entry to rename one or more modules from the package, like this:

    ::

        library
          mixins:
            foo (Foo.Bar as AnotherFoo.Bar, Foo.Baz as AnotherFoo.Baz)

    Note that renaming a module like this will hide all the modules
    that are not explicitly named.

    Modules can also be hidden:

    ::

        library:
          mixins:
            foo hiding (Foo.Bar)

    Hiding modules exposes everything that is not explicitly hidden.

    .. Note::

       Cabal files with :pkg-field:`cabal-version` < 3.0 suffer from an
       infelicity in how the entries of :pkg-field:`mixins` are parsed: an
       entry will fail to parse if the provided renaming clause has whitespace
       after the opening parenthesis.

       See issues :issue:`5150`, :issue:`4864`, and :issue:`5293`.

    There can be multiple mixin entries for a given package, in effect creating
    multiple copies of the dependency:

    ::

        library
          mixins:
            foo (Foo.Bar as AnotherFoo.Bar, Foo.Baz as AnotherFoo.Baz),
            foo (Foo.Bar as YetAnotherFoo.Bar)

    The ``requires`` clause is used to rename the module signatures required by
    a package:

    ::

        library
          mixins:
            foo (Foo.Bar as AnotherFoo.Bar) requires (Foo.SomeSig as AnotherFoo.SomeSig)

    Signature-only packages don't have any modules, so only the signatures can
    be renamed, with the following syntax:

    ::

        library
          mixins:
            sigonly requires (SigOnly.SomeSig as AnotherSigOnly.SomeSig)

    See the :pkg-field:`library:signatures` field for more details.

    Mixin packages are part of the :ref:`Backpack` extension to the
    Haskell module system.

    The matching of the module signatures required by a
    :pkg-field:`build-depends` dependency with the implementation modules
    present in another dependency is triggered by a coincidence of names. When
    the names of the signature and of the implementation are already the same,
    the matching is automatic. But when the names don't coincide, or we want to
    instantiate a signature in two different ways, adding mixin entries that
    perform renamings becomes necessary.

    .. Warning::

       :ref:`Backpack` has the limitation that implementation modules that instantiate
       signatures required by a :pkg-field:`build-depends` dependency can't
       reside in the same component that has the dependency. They must reside
       in a different package dependency, or at least in a separate internal
       library.

Foreign libraries
^^^^^^^^^^^^^^^^^

Foreign libraries are system libraries intended to be linked against
programs written in C or other "foreign" languages. They
come in two primary flavours: dynamic libraries (``.so`` files on Linux,
``.dylib`` files on OSX, ``.dll`` files on Windows, etc.) are linked against
executables when the executable is run (or even lazily during
execution), while static libraries (``.a`` files on Linux/OSX, ``.lib``
files on Windows) get linked against the executable at compile time.

Foreign libraries only work with GHC 7.8 and later.

A typical stanza for a foreign library looks like

::

    foreign-library myforeignlib
      type:                native-shared
      lib-version-info:    6:3:2

      if os(Windows)
        options: standalone
        mod-def-file: MyForeignLib.def

      other-modules:       MyForeignLib.SomeModule
                           MyForeignLib.SomeOtherModule
      build-depends:       base >=4.7 && <4.9
      hs-source-dirs:      src
      c-sources:           csrc/MyForeignLibWrapper.c
      default-language:    Haskell2010


.. pkg-section:: foreign-library name
    :since: 2.0
    :synopsis: Foreign library build information.

    Build information for `foreign libraries`_.

.. pkg-field:: type: foreign library type

   Cabal recognizes ``native-static`` and ``native-shared`` here, although
   we currently only support building `native-shared` libraries.

.. pkg-field:: options: foreign library option list

   Options for building the foreign library, typically specific to the
   specified type of foreign library. Currently we only support
   ``standalone`` here. A standalone dynamic library is one that does not
   have any dependencies on other (Haskell) shared libraries; without
   the ``standalone`` option the generated library would have dependencies
   on the Haskell runtime library (``libHSrts``), the base library
   (``libHSbase``), etc. Currently, ``standalone`` *must* be used on Windows
   and *must not* be used on any other platform.

.. pkg-field:: mod-def-file: filename

   This option can only be used when creating dynamic Windows libraries
   (that is, when using ``native-shared`` and the ``os`` is ``Windows``). If
   used, it must be a path to a *module definition file*. The details of
   module definition files are beyond the scope of this document; see the
   `GHC <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/win32-dlls.html>`_
   manual for some details and some further pointers.

.. pkg-field:: lib-version-info: current:revision:age

   This field is currently only used on Linux.

   This field specifies a Libtool-style version-info field that sets
   an appropriate ABI version for the foreign library. Note that the
   three numbers specified in this field do not directly specify the
   actual ABI version: ``6:3:2`` results in library version ``4.2.3``.

   With this field set, the SONAME of the library is set, and symlinks
   are installed.

   How you should bump this field on an ABI change depends on the
   breakage you introduce:

   -  Programs using the previous version may use the new version as
      drop-in replacement, and programs using the new version can also
      work with the previous one. In other words, no recompiling nor
      relinking is needed. In this case, bump ``revision`` only, don't
      touch current nor age.
   -  Programs using the previous version may use the new version as
      drop-in replacement, but programs using the new version may use
      APIs not present in the previous one. In other words, a program
      linking against the new version may fail with "unresolved
      symbols" if linking against the old version at runtime: set
      revision to 0, bump current and age.
   -  Programs may need to be changed, recompiled, and relinked in
      order to use the new version. Bump current, set revision and age
      to 0.

   Also refer to the Libtool documentation on the version-info field.

.. pkg-field:: lib-version-linux: version

   This field is only used on Linux.

   Specifies the library ABI version directly for foreign libraries
   built on Linux: so specifying ``4.2.3`` causes a library
   ``libfoo.so.4.2.3`` to be built with SONAME ``libfoo.so.4``, and
   appropriate symlinks ``libfoo.so.4`` and ``libfoo.so`` to be
   installed.

Note that typically foreign libraries should export a way to initialize
and shutdown the Haskell runtime. In the example above, this is done by
the ``csrc/MyForeignLibWrapper.c`` file, which might look something like

.. code-block:: c

    #include <stdlib.h>
    #include "HsFFI.h"

    HsBool myForeignLibInit(void){
      int argc = 2;
      char *argv[] = { "+RTS", "-A32m", NULL };
      char **pargv = argv;

      // Initialize Haskell runtime
      hs_init(&argc, &pargv);

      // do any other initialization here and
      // return false if there was a problem
      return HS_BOOL_TRUE;
    }

    void myForeignLibExit(void){
      hs_exit();
    }

With modern ghc regular libraries are installed in directories that contain
package keys. This isn't usually a problem because the package gets registered
in ghc's package DB and so we can figure out what the location of the library
is. Foreign libraries however don't get registered, which means that we'd have
to have a way of finding out where a platform library got installed (other than by
searching the ``lib/`` directory). Instead, we install foreign libraries in
``~/.local/lib``.

Configurations
^^^^^^^^^^^^^^

Library and executable sections may include conditional blocks, which
test for various system parameters and configuration flags. The flags
mechanism is rather generic, but most of the time a flag represents a
certain feature, that can be switched on or off by the package user.
Here is an example package description file using configurations:

Example: A package containing a library and executable programs
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

::

    Cabal-Version: 3.0
    Name: Test1
    Version: 0.0.1
    License: BSD-3-Clause
    Author:  Jane Doe
    Synopsis: Test package to test configurations
    Category: Example
    Build-Type: Simple

    Flag Debug
      Description: Enable debug support
      Default:     False
      Manual:      True

    Flag WebFrontend
      Description: Include API for web frontend.
      Default:     False
      Manual:      True

    Flag NewDirectory
      description: Whether to build against @directory >= 1.2@
      -- This is an automatic flag which the solver will
      -- assign automatically while searching for a solution

    Library
      Build-Depends:      base >= 4.2 && < 4.9
      Exposed-Modules:    Testing.Test1
      Default-Extensions: CPP
      Default-Language:   Haskell2010

      GHC-Options: -Wall
      if flag(Debug)
        CPP-Options: -DDEBUG
        if !os(windows)
          CC-Options: "-DDEBUG"
        else
          CC-Options: "-DNDEBUG"

      if flag(WebFrontend)
        Build-Depends: cgi >= 0.42 && < 0.44
        Other-Modules: Testing.WebStuff
        CPP-Options: -DWEBFRONTEND

        if flag(NewDirectory)
            build-depends: directory >= 1.2 && < 1.4
            Build-Depends: time >= 1.0 && < 1.9
        else
            build-depends: directory == 1.1.*
            Build-Depends: old-time >= 1.0 && < 1.2

    Executable test1
      Main-is:          T1.hs
      Build-Depends:
        , base >= 4.2 && < 4.9
        , Test1
      Default-Language: Haskell2010

      if flag(debug)
        CC-Options: "-DDEBUG"
        CPP-Options: -DDEBUG

Layout
""""""

Flags, conditionals, library and executable sections use layout to
indicate structure. This is very similar to the Haskell layout rule.
Entries in a section have to all be indented to the same level which
must be more than the section header. Tabs are not allowed to be used
for indentation.

As an alternative to using layout you can also use explicit braces
``{}``. In this case the indentation of entries in a section does not
matter, though different fields within a block must be on different
lines. Here is a bit of the above example again, using braces:

Example: Using explicit braces rather than indentation for layout
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

::

    Cabal-Version: 3.0
    Name: Test1
    Version: 0.0.1
    License: BSD-3-Clause
    Author:  Jane Doe
    Synopsis: Test package to test configurations
    Category: Example
    Build-Type: Simple

    Flag Debug {
      Description: Enable debug support
      Default:     False
      Manual:      True
    }

    Library {
      Build-Depends:       base >= 4.2 && < 4.9
      Exposed-Modules:     Testing.Test1
      Default-Extensions:  CPP
      Default-language:    Haskell2010
      if flag(debug) {
        CPP-Options: -DDEBUG
        if !os(windows) {
          CC-Options: "-DDEBUG"
        } else {
          CC-Options: "-DNDEBUG"
        }
      }
    }

Configuration Flags
"""""""""""""""""""

.. pkg-section:: flag name
    :synopsis: Flag declaration.

    Flag section declares a flag which can be used in `conditional blocks`_.

    Flag names are case-insensitive and must match ``[[:alnum:]_][[:alnum:]_-]*``
    regular expression, or expressed as ABNF_:

    .. code-block:: abnf

       flag-name = (UALNUM / "_") *(UALNUM / "_" / "-")

       UALNUM = UALPHA / DIGIT
       UALPHA = ... ; set of alphabetic Unicode code-points

    .. note::

        Hackage accepts ASCII-only flags, ``[a-zA-Z0-9_][a-zA-Z0-9_-]*`` regexp.

.. pkg-field:: description: freeform

    The description of this flag.

.. pkg-field:: default: boolean

    :default: ``True``

    The default value of this flag.

    .. note::

      This value may be :ref:`overridden in several
      ways <controlling flag assignments>`. The
      rationale for having flags default to True is that users usually
      want new features as soon as they are available. Flags representing
      features that are not (yet) recommended for most users (such as
      experimental features or debugging support) should therefore
      explicitly override the default to False.

.. pkg-field:: manual: boolean

    :default: ``False``
    :since: 1.6

      By default, Cabal tries to resolve dependencies using the flag's ``default`` value.
      If that fails, it tries again with the negated default value. However, if the flag is
      marked as ``manual``, Cabal will only use the default value and will not retry
      with the negated default. Note that the default value can still be overridden
      using command-line flags.

.. _conditional-blocks:

Conditional Blocks
^^^^^^^^^^^^^^^^^^

Conditional blocks may appear anywhere inside a component or common
section. They have to follow rather strict formatting rules. Conditional
blocks must always be of the shape

::

      if condition
         property-descriptions-or-conditionals

or

::

      if condition
           property-descriptions-or-conditionals
      else
           property-descriptions-or-conditionals

Note that the ``if`` and the condition have to be all on the same line.

Since Cabal 2.2 conditional blocks support ``elif`` construct.

::

      if condition1
           property-descriptions-or-conditionals
      elif condition2
           property-descriptions-or-conditionals
      else
           property-descriptions-or-conditionals

.. _conditions:

Conditions
""""""""""

Conditions can be formed using boolean tests and the boolean operators
``||`` (disjunction / logical "or"), ``&&`` (conjunction / logical
"and"), or ``!`` (negation / logical "not"). The unary ``!`` takes
highest precedence, ``||`` takes lowest. Precedence levels may be
overridden through the use of parentheses. For example,
``os(darwin) && !arch(i386) || os(freebsd)`` is equivalent to
``(os(darwin) && !(arch(i386))) || os(freebsd)``.

The following tests are currently supported.

:samp:`os({name})`
    Tests if the current operating system is *name*. The argument is
    tested against ``System.Info.os`` on the target system. There is
    unfortunately some disagreement between Haskell implementations
    about the standard values of ``System.Info.os``. Cabal canonicalises
    it so that in particular ``os(windows)`` works on all
    implementations. If the canonicalised os names match, this test
    evaluates to true, otherwise false. The match is case-insensitive.
:samp:`arch({name})`
    Tests if the current architecture is *name*. *name* should be the name of
    one of the nullary constructors of ``Distribution.System.Arch`` (e.g.
    ``x86_64``, ``aarch64`` or ``i386``), otherwise it will be treated as an
    'other architecture' of the given *name*. It will be compared with
    ``Distribution.System.buildArch``, which is derived from
    ``System.Info.arch`` (certain architectures are treated as synonymous; e.g.
    ``aarch64`` / ``arm64`` or ``powerpc64`` / ``powerpc64le`` are not
    distinguished). For a match, this test evaluates to true, otherwise false.
    The match is case-insensitive.
:samp:`impl({compiler})`
    Tests for the configured Haskell implementation. An optional version
    constraint may be specified (for example ``impl(ghc >= 6.6.1)``). If
    the configured implementation is of the right type and matches the
    version constraint, then this evaluates to true, otherwise false.
    The match is case-insensitive.

    Note that including a version constraint in an ``impl`` test causes
    it to check for two properties:

    -  The current compiler has the specified name, and

    -  The compiler's version satisfied the specified version constraint

    As a result, ``!impl(ghc >= x.y.z)`` is not entirely equivalent to
    ``impl(ghc < x.y.z)``. The test ``!impl(ghc >= x.y.z)`` checks that:

    -  The current compiler is not GHC, or

    -  The version of GHC is earlier than version x.y.z.

:samp:`flag({name})`
    Evaluates to the current assignment of the flag of the given name.
    Flag names are case insensitive. Testing for flags that have not
    been introduced with a flag section is an error.
``true``
    Constant value true.
``false``
    Constant value false.

.. _resolution-of-conditions-and-flags:

Resolution of Conditions and Flags
""""""""""""""""""""""""""""""""""

If a package descriptions specifies configuration flags the package user
can :ref:`control these in several ways <controlling flag assignments>`. If the
user does not fix the value of a flag, Cabal will try to find a flag
assignment in the following way.

-  For each flag specified, it will assign its default value, evaluate
   all conditions with this flag assignment, and check if all
   dependencies can be satisfied. If this check succeeded, the package
   will be configured with those flag assignments.

-  If dependencies were missing, the last flag (as by the order in which
   the flags were introduced in the package description) is tried with
   its alternative value and so on. This continues until either an
   assignment is found where all dependencies can be satisfied, or all
   possible flag assignments have been tried.

To put it another way, Cabal does a complete backtracking search to find
a satisfiable package configuration. It is only the dependencies
specified in the :pkg-field:`build-depends` field in conditional blocks that
determine if a particular flag assignment is satisfiable
(:pkg-field:`build-tools` are not considered). The order of the declaration and
the default value of the flags determines the search order. Flags
overridden on the command line fix the assignment of that flag, so no
backtracking will be tried for that flag.

If no suitable flag assignment could be found, the configuration phase
will fail and a list of missing dependencies will be printed. Note that
this resolution process is exponential in the worst case (i.e., in the
case where dependencies cannot be satisfied). There are some
optimizations applied internally, but the overall complexity remains
unchanged.

Meaning of field values when using conditionals
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

During the configuration phase, a flag assignment is chosen, all
conditionals are evaluated, and the package description is combined into
a flat package descriptions. If the same field is declared both inside
a conditional and outside then they are combined using the following rules.

-  Boolean fields are combined using conjunction (logical "and").

-  List fields are combined by appending the inner items to the outer
   items, for example

   ::

       other-extensions: CPP
       if impl(ghc)
         other-extensions: MultiParamTypeClasses

   when compiled using GHC will be combined to

   ::

       other-extensions: CPP, MultiParamTypeClasses

   Similarly, if two conditional sections appear at the same nesting
   level, properties specified in the latter will come after properties
   specified in the former.

-  All other fields must not be specified in ambiguous ways. For example

   ::

       Main-is: Main.hs
       if flag(useothermain)
         Main-is: OtherMain.hs

   will lead to an error. Instead use

   ::

       if flag(useothermain)
         Main-is: OtherMain.hs
       else
         Main-is: Main.hs

.. _common-stanzas:

Common stanzas
^^^^^^^^^^^^^^

.. pkg-section:: common name
    :since: 2.2
    :synopsis: Common build info section

Starting with Cabal-2.2 it's possible to use common build info stanzas.

::

      common deps
        build-depends: base ^>= 4.18
        ghc-options: -Wall

      common test-deps
        build-depends: tasty ^>= 1.4

      library
        import:           deps
        exposed-modules:  Foo
        default-language: Haskell2010

      test-suite tests
        import:           deps, test-deps
        type:             exitcode-stdio-1.0
        main-is:          Tests.hs
        build-depends:    foo
        default-language: Haskell2010

-  You can use `build information`_ fields in common stanzas.

-  Common stanzas must be defined before use.

-  Common stanzas can import other common stanzas.

-  You can import multiple stanzas at once. Stanza names must be separated by commas.

-  ``import`` must be the first field in a section. Since Cabal 3.0 imports
   are also allowed inside conditionals.

.. Note::

    The name `import` was chosen, because there is ``includes`` field.

.. pkg-section:: None

.. pkg-field:: import: token-list

    TBW


.. _pkg-author-source:

*Source code* repository marker
-------------------------------

.. pkg-section:: source-repository
    :since: 1.6

A marker that points to the *source code* for this package within a
**source code repository**.

There are two kinds. You can specify one or the other or both at once:

-  The ``head`` kind refers to the latest development branch of the
   package. This may be used for example to track activity of a project
   or as an indication to outside developers what sources to get for
   making new contributions.

-  The ``this`` kind refers to the branch and tag of a repository that
   contains the sources for this version or release of a package.  For most
   source control systems this involves specifying a tag, id or hash of some
   form and perhaps a branch.

As an example, here are the repositories for the Cabal library. Note that the
``this`` kind of repository specifies a tag.

::

    source-repository head
      type:     git
      location: https://github.com/haskell/cabal

    source-repository this
      type:     git
      location: https://github.com/haskell/cabal
      tag:      1.6.1

The :ref:`cabal get<cabal-get>` command uses the kind of repository with
its ``--source-repository`` option, if provided.

.. _source-repository-fields:

The :ref:`VCS fields<vcs-fields>` of ``source-repository`` are:

..
  data SourceRepo = SourceRepo
    { repoKind :: RepoKind
    , repoType :: Maybe RepoType
    , repoLocation :: Maybe String
    , repoModule :: Maybe String
    , repoBranch :: Maybe String
    , repoTag :: Maybe String
    , repoSubdir :: Maybe FilePath
    }

.. pkg-field:: type: VCS kind

    This field is required.

    .. include:: vcs/kind.rst

.. pkg-field:: location: VCS location

    This field is required.

    .. include:: vcs/location.rst

.. pkg-field:: module: token

    This field is required for the CVS repository type and should not be
    used otherwise.

    CVS requires a named module, as each CVS server can host multiple
    named repositories.

.. pkg-field:: branch: VCS branch

    This field is optional.

    .. include:: vcs/branch.rst

.. pkg-field:: tag: VCS tag

    This field is required for the ``this`` repository kind.

    This might be used to indicate what sources to get if someone needs to fix a
    bug in an older branch that is no longer an active head branch.

    .. include:: vcs/tag.rst

.. pkg-field:: subdir: VCS subdirectory

    This field is optional but, if given, specifies a single subdirectory.

    .. include:: vcs/subdir.rst

.. _setup-hooks:

Hooks
-----
The ``Hooks`` build type allows customising the configuration and the building
of a package using a collection of **hooks** into the build system.

Introduced in Cabal 3.14, this build type provides an alternative
to :ref:`Custom setups <custom-setup>` which integrates better with the rest of the
Haskell ecosystem.

To use this build type in your package, you need to:

  * Declare a ``cabal-version`` of at least 3.14 in your ``.cabal`` file.
  * Declare ``build-type: Hooks`` in your ``.cabal`` file.
  * Include a ``custom-setup`` stanza in your ``.cabal`` file, which declares
    the version of the Hooks API your package is using.
  * Define a ``SetupHooks.hs`` module next to your ``.cabal`` file. It must
    export a value ``setupHooks :: SetupHooks``.

More specifically, your ``.cabal`` file should resemble the following:

    .. code-block:: cabal

        cabal-version: 3.14
        build-type: Hooks

        custom-setup:
          setup-depends:
            base        >= 4.18 && < 5,
            Cabal-hooks >= 0.1  && < 0.2

while a basic ``SetupHooks.hs`` file might look like the following:

    .. code-block:: haskell

        module SetupHooks where
        import Distribution.Simple.SetupHooks ( SetupHooks, noSetupHooks )

        setupHooks :: SetupHooks
        setupHooks =
         noSetupHooks
           { configureHooks = myConfigureHooks
           , buildHooks = myBuildHooks }

        -- ...

Refer to the `Hackage documentation for the Distribution.Simple.SetupHooks module <https://hackage.haskell.org/package/Cabal-hooks/docs/Distribution-Simple-SetupHooks.html>`__
for an overview of the ``Hooks`` API. Further motivation and a technical overview
of the design is available in `Haskell Tech Proposal #60 <https://github.com/haskellfoundation/tech-proposals/blob/main/rfc/060-replacing-cabal-custom-build.md>`__ .

.. _custom-setup:

Custom setup scripts
--------------------

Deprecated since Cabal 3.14: prefer using the :ref:`Hooks build type<setup-hooks>` instead.

Since Cabal 1.24, custom ``Setup.hs`` are required to accurately track
their dependencies by declaring them in the ``.cabal`` file rather than
rely on dependencies being implicitly in scope.  Please refer to
`this article <https://www.well-typed.com/blog/2015/07/cabal-setup-deps/>`__
for more details.

As of Cabal library version 3.0, ``defaultMain*`` variants implement support
for response files. Custom ``Setup.hs`` files that do not use one of these
main functions are required to implement their own support, such as by using
``GHC.ResponseFile.getArgsWithResponseFiles``.

Declaring a ``custom-setup`` stanza also enables the generation of
``MIN_VERSION_package_(A,B,C)`` CPP macros for the Setup component.

.. pkg-section:: custom-setup
   :synopsis: Build information for ``Custom`` and ``Hooks`` build types
   :since: 1.24

   A :pkg-section:`custom-setup` stanza is required for ``Custom`` and ``Hooks``
   :pkg-field:`build-type`, and will be ignored (with a warning)
   for other build types.

   The stanza contains information needed for the compilation
   of custom ``Setup.hs`` scripts, and of ``SetupHooks.hs`` hooks.
   For example:

::

    custom-setup
      setup-depends:
        base   >= 4.18 && < 5,
        Cabal  >= 3.10

.. pkg-field:: setup-depends: package list
    :since: 1.24

    The dependencies needed to compile ``Setup.hs`` or ``SetupHooks.hs``. See the
    :pkg-field:`build-depends` field for a description of the syntax expected by
    this field.

    If the field is not specified the implicit package set will be used.
    The package set contains packages bundled with GHC (i.e. ``base``,
    ``bytestring``) and specifically ``Cabal``.
    The specific bounds are put on ``Cabal`` dependency:
    lower-bound is inferred from :pkg-field:`cabal-version`,
    and the upper-bound is ``< 1.25``.

    ``Cabal`` version is additionally restricted by GHC,
    with absolute minimum being ``1.20``, and for example ``Custom``
    builds with GHC-8.10 require at least ``Cabal-3.2``.


Backward compatibility and ``custom-setup``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Versions prior to Cabal 1.24 don't recognise ``custom-setup`` stanzas,
and will behave agnostic to them (except for warning about an unknown
'section'). Consequently, versions prior to Cabal 1.24 can't ensure the
declared dependencies ``setup-depends`` are in scope, and instead
whatever is registered in the current package database environment
will become eligible (and resolved by the compiler) for the
``Setup.hs`` module.

The availability of the
``MIN_VERSION_package_(A,B,C)`` CPP macros
inside ``Setup.hs`` scripts depends on the condition that either

- a ``custom-setup`` stanza has been declared (or ``cabal build`` is being used
  which injects an implicit hard-coded ``custom-setup`` stanza if it's missing),
  or
- GHC 8.0 or later is used (which natively injects package version CPP macros)

Consequently, if you need to write backward compatible ``Setup.hs``
scripts using CPP, you should declare a ``custom-setup`` stanza and
use the pattern below:

.. code-block:: haskell

    {-# LANGUAGE CPP #-}
    import Distribution.Simple

    #if defined(MIN_VERSION_Cabal)
    -- version macros are available and can be used as usual
    # if MIN_VERSION_Cabal(a,b,c)
    -- code specific to lib:Cabal >= a.b.c
    # else
    -- code specific to lib:Cabal < a.b.c
    # endif
    #else
    # warning Enabling heuristic fall-back. Please upgrade cabal-install to 1.24 or later if Setup.hs fails to compile.

    -- package version macros not available; except for exotic environments,
    -- you can heuristically assume that lib:Cabal's version is correlated
    -- with __GLASGOW_HASKELL__, and specifically since we can assume that
    -- GHC < 8.0, we can assume that lib:Cabal is version 1.22 or older.
    #endif

    main = ...

The simplified (heuristic) CPP pattern shown below is useful if all you need
is to distinguish ``Cabal < 2.0`` from ``Cabal >= 2.0``.

.. code-block:: haskell

    {-# LANGUAGE CPP #-}
    import Distribution.Simple

    #if !defined(MIN_VERSION_Cabal)
    # define MIN_VERSION_Cabal(a,b,c) 0
    #endif

    #if MIN_VERSION_Cabal(2,0,0)
    -- code for lib:Cabal >= 2.0
    #else
    -- code for lib:Cabal < 2.0
    #endif

    main = ...



Autogenerated modules and includes
----------------------------------

.. pkg-section:: None

Modules that are built automatically at setup, created with a custom
setup script, must appear on :pkg-field:`other-modules` for the library,
executable, test-suite or benchmark stanzas or also on
:pkg-field:`library:exposed-modules` for libraries to be used, but are not
really on the package when distributed. This makes commands like sdist fail
because the file is not found.

These special modules must appear again on the :pkg-field:`autogen-modules`
field of the stanza that is using them, besides :pkg-field:`other-modules` or
:pkg-field:`library:exposed-modules`. With this there is no need to create
complex build hooks for this poweruser case.

.. pkg-field:: autogen-modules: module list
   :since: 2.0

   .. todo:: document autogen-modules field

Right now :pkg-field:`executable:main-is` modules are not supported on
:pkg-field:`autogen-modules`.

::

    Library
        default-language: Haskell2010
        build-depends: base
        exposed-modules:
            MyLibrary
            MyLibHelperModule
        other-modules:
            MyLibModule
        autogen-modules:
            MyLibHelperModule

    Executable Exe
        default-language: Haskell2010
        main-is: Dummy.hs
        build-depends: base
        other-modules:
            MyExeModule
            MyExeHelperModule
        autogen-modules:
            MyExeHelperModule

.. pkg-field:: autogen-includes: filename list
   :since: 3.0

   A list of header files from this package which are autogenerated
   (e.g. by a ``configure`` script). Autogenerated header files are not
   packaged by ``sdist`` command.


.. _accessing-data-files:

Accessing data files from package code
--------------------------------------

.. index:: Paths
.. index:: Paths_

The placement on the target system of files listed in
the :pkg-field:`data-files` field varies between systems, and in some cases
one can even move packages around after installation
(see :ref:`prefix independence`). To
enable packages to find these files in a portable way, Cabal generates a
module called :file:`Paths_{pkgname}` (with any hyphens in *pkgname*
replaced by underscores) during building, so that it may be imported by
modules of the package. This module defines a function

.. code-block:: haskell

    getDataFileName :: FilePath -> IO FilePath

If the argument is a filename, the result is the name of a corresponding file on
the system on which the program is running, if the filename were listed in the
:pkg-field:`data-files` field. No check is performed that the given filename is
listed in that field.

.. Note::

   If you decide to import the :file:`Paths_{pkgname}` module then it
   *must* be listed in the :pkg-field:`other-modules` field just like any other
   module in your package and on :pkg-field:`autogen-modules` as the file is
   autogenerated.

The :file:`Paths_{pkgname}` module is not platform independent, as any
other autogenerated module, so it does not get included in the source
tarballs generated by ``sdist``.

The :file:`Paths_{pkgname}` module also includes some other useful
functions and values, which record the version of the package and some
other directories which the package has been configured to be installed
into (e.g. data files live in ``getDataDir``):

.. code-block:: haskell

    version :: Version

    getBinDir :: IO FilePath
    getLibDir :: IO FilePath
    getDynLibDir :: IO FilePath
    getDataDir :: IO FilePath
    getLibexecDir :: IO FilePath
    getSysconfDir :: IO FilePath

The actual location of all these directories can be individually
overridden at runtime using environment variables of the form
``pkg_name_var``, where ``pkg_name`` is the name of the package with all
hyphens converted into underscores, and ``var`` is either ``bindir``,
``libdir``, ``dynlibdir``, ``datadir``, ``libexedir`` or ``sysconfdir``. For example,
the configured data directory for ``pretty-show`` is controlled with the
``pretty_show_datadir`` environment variable.

Accessing the package version
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: PackageInfo
.. index:: PackageInfo_

The auto generated :file:`PackageInfo_{pkgname}` module exports the constant
``version ::`` `Version <http://hackage.haskell.org/package/base/docs/Data-Version.html>`__
which is defined as the version of your package as specified in the
``version`` field.

Accessing package-related information
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The auto generated :file:`PackageInfo_{pkgname}` module exports the following
package-related constants:

.. code-block:: haskell

    name :: String
    version :: Version
    synopsis :: String
    copyright :: String
    homepage :: String

Unlike :file:`Paths_{pkgname}` (see <#accessing-data-files-from-package-code>),
:file:`PackageInfo_{pkgname}` is system- and path-independent. It aims to be
easier to work with for hash-based tools such as Nix.

.. _system-dependent parameters:

System-dependent parameters
---------------------------

For some packages, especially those interfacing with C libraries,
implementation details and the build procedure depend on the build
environment. The ``build-type`` ``Configure`` can be used to handle many
such situations. In this case, ``Setup.hs`` should be:

.. code-block:: haskell

    import Distribution.Simple
    main = defaultMainWithHooks autoconfUserHooks

Most packages, however, would probably do better using the ``Simple``
build type and `configurations`_.

The :pkg-field:`build-type` ``Configure`` differs from ``Simple`` in two ways:

-  The package root directory must contain a shell script called
   ``configure``. The configure step will run the script. This
   ``configure`` script may be produced by
   `autoconf <http://www.gnu.org/software/autoconf/>`__ or may be
   hand-written. The ``configure`` script typically discovers
   information about the system and records it for later steps, e.g. by
   generating system-dependent header files for inclusion in C source
   files and preprocessed Haskell source files. (Clearly this won't work
   for Windows without MSYS or Cygwin: other ideas are needed.)

-  If the package root directory contains a file called
   *package*\ ``.buildinfo`` after the configuration step, subsequent
   steps will read it to obtain additional settings for `build
   information`_ fields,to be merged with the ones
   given in the ``.cabal`` file. In particular, this file may be
   generated by the ``configure`` script mentioned above, allowing these
   settings to vary depending on the build environment.

Note that the package's ``extra-source-files`` are available to the
``configure`` script when it is executed. In typical ``autoconf`` fashion,
``--host`` flag will be passed to the ``configure`` script to indicate the host
platform when cross-compiling. Moreover, various bits of build configuration
will be passed via environment variables:

 - ``CC`` will reflect the path to the C compiler
 - ``CXX`` will reflect the path to the C++ compiler (if available, it always will be if ghc >= 9.4)
 - ``CFLAGS`` will reflect the flags to the C compiler
 - ``CXXFLAGS`` will reflect the flags to the C++ compiler (if available)
 - ``CABAL_FLAGS`` will contain the Cabal flag assignment of the current
   package using traditional Cabal flag syntax (e.g. ``+flagA -flagB``)
 - ``CABAL_FLAG_<flag>`` will be set to either ``0`` or ``1`` depending upon
   whether flag ``<flag>`` is enabled. Note that any any non-alpha-numeric
   characters in the flag name are replaced with ``_``.

The build information file should have the following structure:

    *buildinfo*

    ``executable:`` *name* *buildinfo*

    ``executable:`` *name* *buildinfo* ...

where each *buildinfo* consists of settings of fields listed in the
section on `build information`_. The first one (if
present) relates to the library, while each of the others relate to the
named executable. (The names must match the package description, but you
don't have to have entries for all of them.)

Neither of these files is required. If they are absent, this setup
script is equivalent to ``defaultMain``.

Example: Using autoconf
^^^^^^^^^^^^^^^^^^^^^^^

This example is for people familiar with the
`autoconf <http://www.gnu.org/software/autoconf/>`__ tools.

In the X11 package, the file ``configure.ac`` contains:

.. code-block:: shell

    AC_INIT([Haskell X11 package], [1.1], [libraries@haskell.org], [X11])

    # Safety check: Ensure that we are in the correct source directory.
    AC_CONFIG_SRCDIR([X11.cabal])

    # Header file to place defines in
    AC_CONFIG_HEADERS([include/HsX11Config.h])

    # Check for X11 include paths and libraries
    AC_PATH_XTRA
    AC_TRY_CPP([#include <X11/Xlib.h>],,[no_x=yes])

    # Build the package if we found X11 stuff
    if test "$no_x" = yes
    then BUILD_PACKAGE_BOOL=False
    else BUILD_PACKAGE_BOOL=True
    fi
    AC_SUBST([BUILD_PACKAGE_BOOL])

    AC_CONFIG_FILES([X11.buildinfo])
    AC_OUTPUT

Then the setup script will run the ``configure`` script, which checks
for the presence of the X11 libraries and substitutes for variables in
the file ``X11.buildinfo.in``:

::

    buildable: @BUILD_PACKAGE_BOOL@
    cc-options: @X_CFLAGS@
    ld-options: @X_LIBS@

This generates a file ``X11.buildinfo`` supplying the parameters needed
by later stages:

::

    buildable: True
    cc-options:  -I/usr/X11R6/include
    ld-options:  -L/usr/X11R6/lib

The ``configure`` script also generates a header file
``include/HsX11Config.h`` containing C preprocessor defines recording
the results of various tests. This file may be included by C source
files and preprocessed Haskell source files in the package.

.. Note::

   Packages using these features will also need to list additional
   files such as ``configure``, templates for ``.buildinfo`` files, files
   named only in ``.buildinfo`` files, header files and so on in the
   :pkg-field:`extra-source-files` field to ensure that they are included in
   source distributions. They should also list files and directories generated
   by ``configure`` in the :pkg-field:`extra-tmp-files` field to ensure that
   they are removed by ``setup clean``.

Quite often the files generated by ``configure`` need to be listed
somewhere in the package description (for example, in the
:pkg-field:`install-includes` field). However, we usually don't want generated
files to be included in the source tarball. The solution is again
provided by the ``.buildinfo`` file. In the above example, the following
line should be added to ``X11.buildinfo``:

::

    install-includes: HsX11Config.h

In this way, the generated ``HsX11Config.h`` file won't be included in
the source tarball in addition to ``HsX11Config.h.in``, but it will be
copied to the right location during the install process. Packages that
use custom ``Setup.hs`` scripts can update the necessary fields
programmatically instead of using the ``.buildinfo`` file.

Conditional compilation
-----------------------

Sometimes you want to write code that works with more than one version
of a dependency. You can specify a range of versions for the dependency
in the :pkg-field:`build-depends`, but how do you then write the code that can
use different versions of the API?

Haskell lets you preprocess your code using the C preprocessor (either
the real C preprocessor, or ``cpphs``). To enable this, add
``extensions: CPP`` to your package description. When using CPP, Cabal
provides some pre-defined macros to let you test the version of
dependent packages; for example, suppose your package works with either
version 3 or version 4 of the ``base`` package, you could select the
available version in your Haskell modules like this:

.. code-block:: cpp

    #if MIN_VERSION_base(4,0,0)
    ... code that works with base-4 ...
    #else
    ... code that works with base-3 ...
    #endif

In general, Cabal supplies a macro
``MIN_VERSION_``\ *``package``*\ ``_(A,B,C)`` for each package depended
on via :pkg-field:`build-depends`. This macro is true if the actual version of
the package in use is greater than or equal to ``A.B.C`` (using the
conventional ordering on version numbers, which is lexicographic on the
sequence, but numeric on each component, so for example 1.2.0 is greater
than 1.0.3).

Since version 1.20, the ``MIN_TOOL_VERSION_``\ *``tool``*
family of macros lets you condition on the version of build tools used to
build the program (e.g. ``hsc2hs``).

Since version 1.24, the macro ``CURRENT_COMPONENT_ID``, which
expands to the string of the component identifier that uniquely
identifies this component.  Furthermore, if the package is a library,
the macro ``CURRENT_PACKAGE_KEY`` records the identifier that was passed
to GHC for use in symbols and for type equality.

Since version 2.0, the macro ``CURRENT_PACKAGE_VERSION`` expands
to the string version number of the current package.

Cabal places the definitions of these macros into an
automatically-generated header file, which is included when
preprocessing Haskell source code by passing options to the C
preprocessor.

Cabal also allows to detect when the source code is being used for
generating documentation. The ``__HADDOCK_VERSION__`` macro is defined
only when compiling via Haddock_
instead of a normal Haskell compiler. The value of the
``__HADDOCK_VERSION__`` macro is defined as ``A*1000 + B*10 + C``, where
``A.B.C`` is the Haddock version. This can be useful for working around
bugs in Haddock or generating prettier documentation in some special
cases.

.. _more-complex-packages:

More complex packages
---------------------

For packages that don't fit the simple schemes described above, you have
a few options:

-  By using the :pkg-field:`build-type` ``Custom``, you can supply your own
   ``Setup.hs`` file, and customize the simple build infrastructure
   using *hooks*. These allow you to perform additional actions before
   and after each command is run, and also to specify additional
   preprocessors. A typical ``Setup.hs`` may look like this:

   .. code-block:: haskell

       import Distribution.Simple
       main = defaultMainWithHooks simpleUserHooks { postHaddock = posthaddock }

       posthaddock args flags desc info = ....

   See ``UserHooks`` in
   `Distribution.Simple <https://hackage.haskell.org/package/Cabal/docs/Distribution-Simple.html>`__
   for the details, but note that this interface is experimental, and
   likely to change in future releases.

   If you use a custom ``Setup.hs`` file you should strongly consider
   adding a :pkg-section:`custom-setup` stanza with a
   :pkg-field:`custom-setup:setup-depends` field to ensure that your setup
   script does not break with future dependency versions.

-  You could delegate all the work to ``make``, though this is unlikely
   to be very portable. Cabal supports this with the :pkg-field:`build-type`
   ``Make`` and a trivial setup library
   `Distribution.Make <https://hackage.haskell.org/package/Cabal/docs/Distribution-Make.html>`__,
   which simply parses the command line arguments and invokes ``make``.
   Here ``Setup.hs`` should look like this:

   .. code-block:: haskell

       import Distribution.Make
       main = defaultMain

   The root directory of the package should contain a ``configure``
   script, and, after that has run, a ``Makefile`` with a default target
   that builds the package, plus targets ``install``, ``register``,
   ``unregister``, ``clean``, ``dist`` and ``docs``. Some options to
   commands are passed through as follows:

   -  The ``--with-hc-pkg``, ``--prefix``, ``--bindir``, ``--libdir``,
      ``--dynlibdir``, ``--datadir``, ``--libexecdir`` and ``--sysconfdir`` options to
      the ``configure`` command are passed on to the ``configure``
      script. In addition the value of the ``--with-compiler`` option is
      passed in a ``--with-hc`` option and all options specified with
      ``--configure-option=`` are passed on.

   -  The ``--destdir`` option to the ``copy`` command becomes a setting
      of a ``destdir`` variable on the invocation of ``make copy``. The
      supplied ``Makefile`` should provide a ``copy`` target, which will
      probably look like this:

      .. code-block:: make

          copy :
                  $(MAKE) install prefix=$(destdir)/$(prefix) \
                                  bindir=$(destdir)/$(bindir) \
                                  libdir=$(destdir)/$(libdir) \
                                  dynlibdir=$(destdir)/$(dynlibdir) \
                                  datadir=$(destdir)/$(datadir) \
                                  libexecdir=$(destdir)/$(libexecdir) \
                                  sysconfdir=$(destdir)/$(sysconfdir) \

-  Finally, with the :pkg-field:`build-type` ``Custom``, you can also write your
   own setup script from scratch, and you may use the Cabal
   library for all or part of the work. One option is to copy the source
   of ``Distribution.Simple``, and alter it for your needs. Good luck.

.. include:: references.inc

.. rubric:: Footnotes

.. [#old-style-build-tool-depends]

  Some packages (ab)use :pkg-field:`build-depends` on old-style builds, but this has a few major drawbacks:

    - using Nix-style builds it's considered an error if you depend on a exe-only package via build-depends: the solver will refuse it.
    - it may or may not place the executable on ``PATH``.
    - it does not ensure the correct version of the package is installed, so you might end up overwriting versions with each other.
