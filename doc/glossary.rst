.. _glossary:

Glossary
========

.. glossary::
   :sorted:

   ABI hash
     A hash that :term:`GHC` computes over the interface a compiled library
     exposes, recorded when the library is registered in a :term:`package
     database`. A library compiled against one ABI hash of a dependency has to
     be rebuilt if that hash changes.

   active repositories
     The :term:`repositories <repository>` the :term:`solver` takes packages
     from, and how their package sets are combined. Set with
     :cfg-field:`active-repositories`.

   allow-newer
   allow-older
     Project settings that tell the :term:`solver` to ignore the upper
     (``allow-newer``) or lower (``allow-older``) :term:`version bounds
     <version bound>` that packages put on their dependencies. See
     :cfg-field:`allow-newer` and :cfg-field:`allow-older`.

   autogen module
     A module that Cabal generates during the build instead of reading it from
     the package's source, such as the :term:`Paths module`. It is listed in
     :pkg-field:`autogen-modules`.

   automatic flag
     A :term:`flag` that the :term:`solver` is free to set to either value in
     order to find a :term:`build plan`. Flags are automatic unless declared
     with ``manual: True``; compare :term:`manual flag`.

   backjump
     A step the :term:`solver` takes when it hits a conflict: it abandons its
     choices back to the most recent one that contributed to the conflict. The
     ``--max-backjumps`` option limits how many it may take.

   Backpack
     A system, implemented by :term:`GHC` and Cabal together, that lets a
     library leave some of its modules as :term:`signatures <signature>` to be
     filled in later by whoever depends on it. See :ref:`Backpack`.

   benchmark
     A :term:`component` that measures performance, declared in a
     :pkg-section:`benchmark` stanza and run with ``cabal bench``.

   boot package
   boot library
     A package that ships with :term:`GHC` and is registered in its global
     :term:`package database`, such as ``base`` and ``ghc-prim``. Each GHC
     comes with one version of each boot package and some of them cannot be
     replaced, so choosing a GHC fixes those versions for the :term:`solver`.

   build directory
     The directory holding a project's build products and caches. It is
     ``dist-newstyle`` unless changed with :cfg-field:`builddir`. The
     :term:`setup script` interface uses ``dist`` instead.

   build information
     The fields that every kind of :term:`component` accepts, such as
     :pkg-field:`build-depends`, :pkg-field:`other-modules` and
     :pkg-field:`default-language`. See :ref:`build-info`.

   build plan
   install plan
     Everything :term:`cabal` has decided to build for a :term:`project`: the
     exact version and :term:`flag assignment` of every package, the
     :term:`units <unit>` that result, and the order to build them in. Show it
     with ``cabal build --dry-run``; it is also written to :term:`plan.json`.

   build tool dependency
     An executable that must be available while a :term:`component` is being
     built, such as a preprocessor. It is declared in
     :pkg-field:`build-tool-depends` as ``package:executable``; :term:`cabal`
     builds it and makes it available for that build.

   build type
     How a package's build is driven, set by :pkg-field:`build-type`.
     ``Simple`` is Cabal's standard build with no custom code, ``Configure``
     also runs a ``./configure`` script, ``Hooks`` adds steps from
     :term:`SetupHooks.hs`, and ``Custom`` hands the build to an arbitrary
     :term:`setup script`.

   Cabal
      The Common Architecture for Building Applications and Libraries. Also a
      library of the same name.

   cabal
     The command line tool, ``cabal-install:exe:cabal``, the executable named
     ``cabal`` from the ``cabal-install`` package.

   cabal-install:exe:cabal
     A more explicit way to refer to the ``cabal`` command line tool.

   cabal-install
     The package that provides the ``cabal`` command line tool. Also,
     confusingly, people will say ``cabal-install`` when they want to
     disambiguate between ``cabal`` the tool and ``Cabal`` the library.

   cabal directory
     The single directory, ``~/.cabal`` on Unix, in which :term:`cabal` used
     to keep its :term:`configuration file`, downloaded packages,
     :term:`store` and installed executables. New installations spread these
     over the directories of the :term:`XDG Base Directory Specification`
     instead; an existing ``~/.cabal`` or a ``CABAL_DIR`` environment variable
     brings back the single directory. See :ref:`directories`.

   Cabal-hooks
     The package providing the API that :term:`SetupHooks.hs` is written
     against.

   cabal.project
   project file
     The file that describes a :term:`project`: which packages it contains and
     how they are to be built. See :doc:`cabal-project-description-file`.

   cabal.project.freeze
   freeze file
     A file, written by ``cabal freeze``, of :term:`constraints <constraint>`
     that pin the version and :term:`flag assignment` of every dependency in
     the current :term:`build plan`, so that later builds make the same
     choices.

   cabal.project.local
     A file of overrides that is read after :term:`cabal.project` and is meant
     for one developer's machine, not for sharing. ``cabal configure`` writes
     its settings there.

   cabal script
     A single Haskell source file that states its own dependencies in a
     ``{- cabal: -}`` comment block and is run with ``cabal run``. See
     :ref:`cabal run`.

   Cabal specification
     The definition of the :term:`package description` format. It is
     versioned, and a package states which version it is written in with the
     :pkg-field:`cabal-version` field. That is not the version of
     :term:`cabal` or of the ``Cabal`` library, though both must be recent
     enough to understand it. See :doc:`file-format-changelog`.

   Cabal-syntax
     The package that defines the types, parser and printer for
     :term:`package descriptions <package description>`. It is separate from
     the ``Cabal`` package so that tools can read ``.cabal`` files without
     depending on the build system.

   caret operator
     The ``^>=`` operator in a :term:`version range`. ``^>=1.2.3`` means
     ``>=1.2.3 && <1.3``: at least this version, and below the next major
     version as the :term:`PVP` defines it.

   common stanza
     A named group of :term:`build information` fields, declared with
     ``common name``, that :term:`component` stanzas pull in with
     :term:`import`. See :ref:`common-stanzas`.

   compiler flavor
     The name of a Haskell compiler without its version, such as ``ghc`` or
     ``ghcjs``. It appears in ``impl()`` conditions, in
     :pkg-field:`tested-with` and in the ``compiler`` project setting.

   component
     A buildable part of a :term:`package`: the :term:`main library`, a
     :term:`sublibrary`, a :term:`foreign library`, an :term:`executable`, a
     :term:`test suite` or a :term:`benchmark`. Each is declared by its own
     :term:`stanza` in the :term:`package description`.

   component ID
     The identifier Cabal gives a configured :term:`component`. For a
     component that uses no :term:`Backpack` :term:`signatures <signature>` it
     is the same as the :term:`unit ID`.

   conditional
     An ``if``, ``elif`` or ``else`` block whose fields apply only when its
     condition holds. Conditions test the operating system, architecture,
     compiler and, in a :term:`package description`, :term:`flags <flag>`. See
     :ref:`conditional-blocks`.

   configuration file
     The user-wide settings file for :term:`cabal`, by default
     ``~/.config/cabal/config``. The documentation calls it the
     :term:`global` configuration because it applies to all of a user's
     projects. See :doc:`config`.

   conflict set
     The set of :term:`goals <goal>` that the :term:`solver` found to be
     jointly responsible for a conflict. It appears in the messages of a
     failed solver run.

   constraint
     A restriction given to the :term:`solver` from outside the
     :term:`package descriptions <package description>`, through the
     :cfg-field:`constraints` project field or ``--constraint``. It can limit
     a package's version, fix a :term:`flag`, or require the installed or
     source copy of a package, and unlike a :term:`preference` it must be
     satisfied. The documentation also says "version constraint" for the
     :term:`version range` of a dependency.

   cost centre
     A point in a program to which GHC's profiler attributes time and
     allocation. :cfg-field:`profiling-detail` controls where they are added
     automatically. See :term:`profiling`.

   custom field
     A field whose name begins with ``x-``. Cabal accepts and ignores it, so
     other tools can keep their own data in a :term:`package description`.

   data files
     Files listed in :pkg-field:`data-files` that are installed with the
     package and found at run time through the :term:`Paths module`. See
     :ref:`accessing-data-files`.

   dependency
     A package, library or tool that a :term:`component` needs in order to be
     built. Dependencies are declared in the :term:`package description`,
     mainly in :pkg-field:`build-depends`.

   dependency resolution
     Choosing, for every direct and transitive :term:`dependency`, a version
     and :term:`flag assignment` that satisfy all :term:`version ranges
     <version range>` and :term:`constraints <constraint>` at once. The
     :term:`solver` does this.

   executable
     A :term:`component` that produces a program, declared in an
     :pkg-section:`executable` stanza.

   exposed module
     A library :term:`module` listed in :pkg-field:`library:exposed-modules`,
     which code depending on the library can import. Compare :term:`other
     module`.

   external command
     An executable named ``cabal-<name>`` on the :term:`PATH`, which
     :term:`cabal` runs when invoked as ``cabal <name>``. See
     :doc:`external-commands`.

   external package
     A package that is not a :term:`local package` of the project. It usually
     comes from :term:`Hackage`, and where possible it is built once and kept
     in the :term:`store`.

   extra source files
     Files listed in :pkg-field:`extra-source-files`. They are included in the
     :term:`sdist` and watched for changes, but Cabal does nothing else with
     them. :pkg-field:`extra-doc-files` does the same for documentation.

   FFI
     Haskell's Foreign Function Interface, for calling code written in other
     languages, usually C, and for being called from it.

   field
     A ``name: value`` entry in a :term:`package description`, :term:`project
     file` or :term:`configuration file`.

   flag
     A boolean declared in a :pkg-section:`flag` stanza of a :term:`package
     description` and tested in :term:`conditionals <conditional>`, to switch
     dependencies or features on and off. Set it with ``--flags`` or the
     :cfg-field:`flags` project field. Command line options such as
     ``--enable-tests`` and compiler options such as ``-Wall`` are also called
     flags, but are unrelated.

   flag assignment
     The value chosen for each of a package's :term:`flags <flag>` in a
     :term:`build plan`. See :ref:`controlling flag assignments`.

   foreign library
     A :term:`component`, declared in a :pkg-section:`foreign-library` stanza,
     that produces a shared or static library for linking into programs not
     written in Haskell. Some older parts of the documentation use the phrase
     for the system C libraries a package links against, which are given in
     :pkg-field:`extra-libraries`.

   GHC
     The Glasgow Haskell Compiler, the compiler :term:`cabal` uses by default.

   GHC package
     A library as :term:`GHC` sees it: a built library :term:`unit` registered
     in a :term:`package database`. Compare :term:`package`, which is source
     code.

   GHCi
     GHC's interactive environment. ``cabal repl`` starts it with a
     :term:`component` loaded.

   GHCJS
     A Haskell to JavaScript compiler based on :term:`GHC`, which Cabal treats
     as a :term:`compiler flavor` of its own.

   ghc-pkg
   hc-pkg
     The program that reads and changes a :term:`package database`. Cabal uses
     "hc-pkg" as the compiler-neutral name for it, as in the ``with-hc-pkg``
     option.

   GHCup
     An installer for :term:`GHC`, :term:`cabal`, :term:`HLS` and
     :term:`Stack`.

   global
     A word with several meanings in this guide. The global
     :term:`configuration file` and the global :term:`store` are shared by all
     of one user's projects. The global :term:`package database` is the one
     that ships with :term:`GHC`. Global options are those given before the
     command name.

   goal
     Something the :term:`solver` still has to decide: which version of a
     package to use, or the value of one of its :term:`flags <flag>`.

   Hackage
     The Haskell community's central package :term:`repository`, at
     `hackage.haskell.org <https://hackage.haskell.org/>`__, and the one
     :term:`cabal` uses by default.

   Haddock
     The documentation generator for Haskell code, run by ``cabal haddock``.

   HLS
     The Haskell Language Server, which gives editors features such as type
     information and jump to definition.

   home unit
     A :term:`unit` that :term:`GHC` is compiling in the current session, as
     opposed to the already built units it depends on. GHC 9.4 and later can
     have several home units at once, which :term:`multi-repl` relies on.

   Hoogle
     A search engine for Haskell APIs that finds functions by name or by type.

   hpc
     Haskell Program Coverage, GHC's code coverage tool. Setting
     :cfg-field:`coverage` builds with it.

   HsColour
     A source code colouriser once used for the source links in
     :term:`Haddock` output. Haddock's own hyperlinked source has replaced it
     and ``cabal hscolour`` is deprecated.

   import
     In a :term:`package description`, ``import: name`` brings the fields of a
     :term:`common stanza` into a :term:`component`. In a :term:`project
     file`, ``import: path-or-URL`` includes another project file. See
     :ref:`conditionals and imports`.

   indefinite package
     In :term:`Backpack`, a library with at least one :term:`signature` that
     has not been filled. It can be type checked, but not compiled to code
     until :term:`instantiation`.

   index state
     A point in time in the history of a :term:`package index`. Setting
     :cfg-field:`index-state` makes the :term:`solver` see only what the
     repository held at that time, which keeps a :term:`build plan` from
     changing as new versions are published.

   in-place
     Built inside the project's :term:`build directory` and registered in a
     :term:`package database` there, instead of being installed into the
     :term:`store`. :term:`Local packages <local package>`, and any
     :term:`external packages <external package>` that depend on them, are
     built in place. Also written "inplace".

   install directory
     The directory where ``cabal install`` puts executables, set by
     :cfg-field:`installdir` and ``~/.local/bin`` by default. ``cabal
     install`` installs executables only; to make a library available to GHC
     outside a project, see :term:`package environment`.

   installed package ID
     The identifier of a library registered in a :term:`package database`,
     abbreviated IPID. With current versions of GHC it is the same as the
     library's :term:`unit ID`.

   instantiation
     In :term:`Backpack`, filling the :term:`signatures <signature>` of an
     :term:`indefinite package` with modules that implement them.

   interface stability
     How far :term:`cabal` promises not to change one of its interfaces, such
     as a command or a file format. See :doc:`cabal-interface-stability`.

   internal library
     A :term:`sublibrary` with private :term:`visibility`, usable only by
     components of its own package. Before version 3.0 of the :term:`Cabal
     specification` every sublibrary was internal, so older text uses the term
     for any sublibrary.

   language
     The edition of Haskell a :term:`component` is written in, such as
     ``Haskell2010``, ``GHC2021`` or ``GHC2024``, set by
     :pkg-field:`default-language`.

   language extension
     A named change to the Haskell language that the compiler can switch on
     or off, such as ``OverloadedStrings``. :pkg-field:`default-extensions`
     switches extensions on for every module of a :term:`component`.

   legacy
     Usually, the :term:`v1- commands` and the way they built packages. The
     documentation also applies the word to older forms of several other
     things, such as repositories that are not :term:`secure repositories
     <secure repository>` and license identifiers that predate :term:`SPDX
     license expressions <SPDX license expression>`.

   local no-index repository
     A :term:`repository` that is just a directory of :term:`sdist` tarballs,
     declared with a ``file+noindex://`` URL. :term:`cabal` builds the
     :term:`package index` for it. See :doc:`config`.

   local package
   project package
     A package that belongs to the :term:`project`: one listed in its
     :cfg-field:`packages`, :cfg-field:`optional-packages` or
     :cfg-field:`extra-packages` field. Local packages are built
     :term:`in-place`, and build options given on the command line apply to
     them alone. "Local" is about membership of the project, not about where
     the source code is. Compare :term:`external package`.

   main library
     The :term:`component` declared by a :pkg-section:`library` stanza with no
     name. It takes the name of its package and is always public. A package
     has at most one.

   manual flag
     A :term:`flag` declared with ``manual: True``. The :term:`solver` never
     changes it: it keeps its default unless the user sets it. Compare
     :term:`automatic flag`.

   mixin
     An entry in the :pkg-field:`mixins` field, which chooses the modules of a
     dependency that a :term:`component` can see and the names it sees them
     under. With :term:`Backpack` it also says how :term:`signatures
     <signature>` are filled.

   module
     Haskell's unit of source code and of namespace, normally one to a file. A
     :term:`component` lists its modules as :term:`exposed modules <exposed
     module>` or :term:`other modules <other module>`.

   MSYS2
     A distribution of Unix-style tools and libraries for Windows. Packages
     with a ``./configure`` script or a :term:`pkg-config dependency` need it
     there. See :doc:`how-to-run-in-windows`.

   multi-repl
     A :term:`GHCi` session with several :term:`components <component>`
     loaded at once, started with ``cabal repl --enable-multi-repl``. It needs
     GHC 9.4 or later.

   Nix
     A package manager that builds each package in isolation and stores it
     under a path containing a hash of its inputs. :term:`Nix-style local
     builds` borrow that idea and do not use Nix itself.

   Nix-style local builds
     The way :term:`cabal` builds. Every build happens within a
     :term:`project`; :term:`external packages <external package>` are built
     once and kept in the :term:`store` under an identifier that hashes their
     inputs; :term:`local packages <local package>` are built
     :term:`in-place`. No use is made of :term:`Nix`. Older text also says
     "new-style" or "v2" builds. See :doc:`nix-local-build`.

   offline mode
     Running with ``--offline`` or :cfg-field:`offline` set, in which
     :term:`cabal` makes no network requests and uses only what has already
     been downloaded.

   optimization level
     How hard the compiler works to optimize code, from 0 to 2, set with
     :cfg-field:`optimization` or ``-O``.

   other module
     A :term:`module` that is part of a :term:`component` but cannot be
     imported by code that depends on it, listed in
     :pkg-field:`other-modules`. Compare :term:`exposed module`.

   package
     The unit of distribution of Haskell code: a :term:`package description`
     together with the source code it describes. A package has a name and a
     version and contains one or more :term:`components <component>`. In this
     guide "package" on its own means a :term:`source package`; for what GHC
     calls a package see :term:`GHC package`.

   package candidate
     An upload to :term:`Hackage` that is not yet published, so it can be
     inspected but is not in the :term:`package index`. ``cabal upload``
     creates a candidate unless given ``--publish``.

   package database
   package db
     A directory in which :term:`GHC` keeps the registrations of built
     libraries. GHC ships with a global one; :term:`cabal` adds one in the
     :term:`store` for :term:`external packages <external package>` and one in
     the :term:`build directory` for packages built :term:`in-place`. See
     :cfg-field:`package-dbs`.

   package description
   .cabal file
   Cabal file
     The file ``<package-name>.cabal`` at the root of a :term:`package`,
     giving its metadata, its :term:`components <component>` and their
     :term:`dependencies <dependency>`. See
     :doc:`cabal-package-description-file`.

   package environment
     A GHC environment file: a list of :term:`package databases <package
     database>` and :term:`units <unit>` that ``ghc`` and ``ghci`` treat as
     visible by default. ``cabal install --lib`` adds libraries to one (see
     :ref:`adding-libraries`), and :cfg-field:`write-ghc-environment-files`
     has :term:`cabal` write one for a project.

   package ID
     A package name and version joined by a hyphen, such as ``HUnit-1.1``. It
     identifies a :term:`source package`, not any particular build of it;
     compare :term:`unit ID`.

   package index
     The list of every package version that a :term:`repository` offers,
     with their :term:`package descriptions <package description>`. ``cabal
     update`` downloads it, and the :term:`solver` reads the downloaded copy
     instead of contacting the repository.

   package location
     An entry in the :cfg-field:`packages` field saying where to find a
     :term:`local package`: a directory, a ``.cabal`` file, a glob matching
     either, or a tarball.

   package stanza
     A ``package <name>`` section of a :term:`project file` holding options
     for that one package. ``package *`` applies to every package, including
     :term:`external packages <external package>`, whereas options outside
     any stanza apply to :term:`local packages <local package>` only. See
     :ref:`package-configuration-options`.

   PackageInfo module
     The :term:`autogen module` ``PackageInfo_<pkgname>``, which gives a
     program access to its package's name, version and other metadata. See
     :ref:`package-related-info`.

   PATH
     The environment variable listing the directories searched for
     executables. The :term:`install directory` needs to be on it.

   path variable
     A placeholder such as ``$prefix``, ``$bindir`` or ``$pkgid`` that Cabal
     expands in installation paths and some other options. See
     :ref:`setup-configure`.

   Paths module
     The :term:`autogen module` ``Paths_<pkgname>``, which lets a program find
     its :term:`data files` at run time and read its package's version. See
     :ref:`accessing-data-files`.

   per-component build
     Configuring and building each :term:`component` of a package on its own,
     so that only the components needed are built. :term:`cabal` does this
     for every package except those of :term:`build type` ``Custom``.

   pkg-config
     A tool that reports the compiler and linker options needed to use an
     installed C library.

   pkg-config dependency
     A :term:`dependency` on a system library, named in
     :pkg-field:`pkgconfig-depends`, that Cabal satisfies by asking
     :term:`pkg-config`.

   plan.json
     The file ``dist-newstyle/cache/plan.json``, a JSON rendering of the
     :term:`build plan` for other tools to read.

   preference
     A soft :term:`constraint`: the :term:`solver` follows it when it can and
     drops it when it must. Set with the :cfg-field:`preferences` project
     field.

   preferred version
     A version inside the range that a :term:`repository` publishes as
     preferred for a package. The :term:`solver` picks versions outside that
     range, known as deprecated versions, only when nothing else will do.

   prefix independence
     The property of an installed package that it can be moved to another
     directory and still find its files. See :ref:`prefix independence`.

   profiling
     Building so that a program can report where it spends its time and
     memory. Every dependency has to be built for profiling too, which is a
     separate :term:`way`. See :doc:`how-to-enable-profiling`.

   program options
     Settings that choose the external programs :term:`cabal` runs and the
     arguments it gives them, such as :cfg-field:`with-compiler` and
     ``ghc-options``. See :ref:`program_options`.

   project
     One or more :term:`packages <package>` that are developed and built
     together, described by a :term:`cabal.project` file. When there is no
     project file, :term:`cabal` treats the package in the current directory
     as a project of its own. Every build command runs within a project.

   public library
     A library that other packages can depend on: a :term:`main library`, or
     a :term:`sublibrary` whose :term:`visibility` is ``public``. A public
     sublibrary is depended on as ``package:sublibrary``.

   PVP
     The Haskell `Package Versioning Policy <https://pvp.haskell.org/>`__. In
     a version ``A.B.C`` it makes ``A.B`` the major version, which changes
     when the API breaks, and ``C`` the minor version. This differs from
     SemVer, where the major version is the first number alone.

   reexported module
     A :term:`module` that a library makes importable without defining it, by
     passing on a module from one of its dependencies, optionally under a new
     name. See :pkg-field:`library:reexported-modules`.

   REPL
     A read-eval-print loop, an interactive session. ``cabal repl`` starts
     :term:`GHCi`.

   repository
   package archive
     A collection of packages, with a :term:`package index`, that
     :term:`cabal` can download from. :term:`Hackage` is the default. See
     :doc:`config`.

   resolver
     Stack's former name for a :term:`snapshot`.

   response file
     A file of command line arguments, passed to a program as ``@file`` to
     get round limits on the length of a command line.

   revision
     On :term:`Hackage`, an edit to the :term:`package description` of a
     version that is already published, usually to correct dependency bounds,
     made without releasing a new version. The word is also used for a commit
     in a :term:`VCS`.

   sdist
   source distribution
     A ``.tar.gz`` archive of a :term:`source package`, made by ``cabal
     sdist``. It is the form in which packages are uploaded to
     :term:`Hackage`. See :ref:`cabal-sdist`.

   secure repository
     A :term:`repository` whose :term:`package index` and packages are signed,
     and checked by :term:`cabal` using :term:`TUF`. Hackage is one. See
     :doc:`config`.

   setup dependency
     A :term:`dependency` of a package's :term:`setup script`, declared in the
     :pkg-field:`custom-setup:setup-depends` field. See :ref:`custom-setup`.

   setup script
   Setup.hs
     The program ``Setup.hs`` at the root of a package, which offers the
     low-level command line interface for building that one package with the
     ``Cabal`` library. Its content is fixed for every :term:`build type`
     except ``Custom``. See :doc:`setup-commands`.

   SetupHooks.hs
     The module at the root of a package of :term:`build type` ``Hooks`` that
     adds steps to the standard build. See :ref:`setup-hooks`.

   shared library
   dynamic library
     A library that is loaded when a program runs instead of being copied into
     the executable. Building them is controlled by :cfg-field:`shared`.
     Compare :term:`static linking`.

   signature
     In :term:`Backpack`, the interface of a :term:`module` without an
     implementation, written in an ``.hsig`` file and listed in
     :pkg-field:`library:signatures`.

   snapshot
     :term:`Stackage`'s name for a set of package versions, one of each, that
     are known to build together with a particular GHC. See :ref:`how
     reproducible <how reproducible>`.

   solver
     The part of :term:`cabal` that performs :term:`dependency resolution`,
     producing either a :term:`build plan` or an account of why there is
     none.

   source package
     A :term:`package` as source code, before it is built: what is in a
     package directory, in an :term:`sdist` and on :term:`Hackage`. One source
     package can be built in many ways, each giving different :term:`units
     <unit>`.

   source-repository
     A stanza in a :term:`package description` recording where the package's
     :term:`VCS` repository is. It is information for readers and tools and
     has no effect on the build. Compare :term:`source-repository-package`.
     See :ref:`pkg-author-source`.

   source-repository-package
     A stanza in a :term:`project file` naming a :term:`VCS` repository and
     commit from which :term:`cabal` fetches a package to use in the build.
     Compare :term:`source-repository`. See :ref:`pkg-consume-source`.

   SPDX license expression
     A standard notation for licenses, such as ``BSD-3-Clause`` or ``MIT OR
     Apache-2.0``, used in the :pkg-field:`license` field.

   Stack
     Another build tool for Haskell. It uses the ``Cabal`` library and
     :term:`package descriptions <package description>`, and takes dependency
     versions from a :term:`snapshot` where :term:`cabal` uses its
     :term:`solver`.

   Stackage
     A distribution of :term:`Hackage` packages published as :term:`snapshots
     <snapshot>`.

   stanza
   section
     A group of :term:`fields <field>` under a heading line, such as
     ``library`` or ``executable name`` in a :term:`package description` and
     ``package name`` in a :term:`project file`. The documentation uses both
     words for the same thing.

   static linking
     Copying library code into the executable when it is linked, which is what
     GHC does with Haskell libraries by default. Compare :term:`shared
     library`.

   store
     The directory, shared by all of a user's projects, where :term:`cabal`
     keeps built :term:`external packages <external package>`, each under its
     :term:`unit ID` so that different builds of one package can coexist.
     ``cabal path --store-dir`` shows where it is, and it is safe to delete.

   sublibrary
   named library
     A library :term:`component` declared by a :pkg-section:`library` stanza
     with a name: a library a package has in addition to, or instead of, its
     :term:`main library`. It is private unless given public
     :term:`visibility`. See :ref:`sublibrary examples <sublibs>`.

   tarball
     A ``.tar.gz`` archive. In this guide it is nearly always an
     :term:`sdist`.

   target
     What a command such as ``cabal build`` or ``cabal run`` acts on: a
     package, a :term:`component`, a module, a file or a :term:`cabal script`.

   target form
     The syntax for naming a :term:`target`, such as ``pkg:test:name`` or
     ``all:exes``. See :ref:`target-forms`.

   test suite
     A :term:`component` that tests a package, declared in a
     :pkg-section:`test-suite` stanza and run with ``cabal test``.

   test suite interface
     The protocol by which a :term:`test suite` is run and reports its
     result, chosen by the :pkg-field:`test-suite:type` field.
     ``exitcode-stdio-1.0`` is an executable whose exit code reports success
     or failure; ``detailed-0.9`` is a module that exports its tests to Cabal.

   toolchain
     The compiler together with the programs used alongside it, such as
     :term:`ghc-pkg`, :term:`Haddock`, the C compiler and the linker. "Haskell
     toolchain" is also used more loosely for what :term:`GHCup` installs.

   TUF
     `The Update Framework <https://theupdateframework.io/>`__, a
     specification for protecting software repositories with signed metadata.

   unit
     One :term:`component` built in one particular configuration. Library
     units are registered in a :term:`package database`, where GHC finds them.
     A single :term:`source package` can give rise to many units: one for
     each component, and again for each combination of dependency versions,
     flags and options it is built with.

   unit ID
     The identifier of a :term:`unit`. For a unit in the :term:`store` it
     contains a hash of everything that influenced the build; for one built
     :term:`in-place` it has ``inplace`` where the hash would be. Compare
     :term:`package ID`.

   v1- commands
     Commands such as ``v1-build`` and ``v1-install`` from before
     :term:`Nix-style local builds`. They build one package at a time using
     libraries installed in a shared :term:`package database`.

   v2- commands
     The commands for :term:`Nix-style local builds`. Since version 3.0 of
     :term:`cabal` they are the default, so ``v2-build`` is just another name
     for ``build``. ``new-`` is an earlier spelling of the prefix.

   vanilla
     The plain :term:`way`: code built without :term:`profiling` and for
     :term:`static linking`.

   VCS
     A version control system, such as Git. See :doc:`version-control-fields`.

   vendoring
     Copying the source of a :term:`dependency` into your project and listing
     it as a :term:`local package`, so that it is used in place of the version
     from a :term:`repository`. See :doc:`how-to-source-packages`.

   version bound
     One end of a :term:`version range`: a lower bound such as ``>=1.2`` or an
     upper bound such as ``<1.3``.

   version range
     An expression denoting a set of versions, such as ``>=1.2 && <1.3``,
     ``==1.2.*`` or ``^>=1.2``. It follows the name of a :term:`dependency` in
     fields such as :pkg-field:`build-depends`. The documentation also calls
     it a version constraint; compare :term:`constraint`.

   visibility
     Whether a :term:`sublibrary` can be depended on from other packages
     (``public``) or only from its own (``private``, the default). Set with
     :pkg-field:`library:visibility`.

   way
     GHC's word for a variant of compiled code, such as :term:`vanilla`,
     profiling or dynamic. Code built in different ways cannot be linked
     together, so a program needs all of its dependencies in the way it is
     built in. This is why turning on :term:`profiling` rebuilds them.

   wired-in package
     A :term:`boot package` that GHC itself refers to by name, such as
     ``base`` and ``ghc-prim``. It cannot be replaced by another version.

   XDG Base Directory Specification
     A convention for where programs keep their configuration, cache and
     state under the user's home directory. :term:`cabal` follows it unless a
     :term:`cabal directory` is in use. See :ref:`directories`.
