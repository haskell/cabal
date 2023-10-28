How to package Haskell code
===========================

.. TIP::
    If this is your first time using `cabal` you should check out the :doc:`Getting Started guide <getting-started>`.

Starting from scratch, we're going to walk you through creating a simple
Haskell application.

**TL;DR;** ``mkdir proglet && cd proglet && cabal init --simple --exe && cabal run proglet``


Introduction
------------

Every application needs a name, we'll call ours "proglet" and start by
creating an empty directory.

.. highlight:: console

::

    $ mkdir proglet
    $ cd proglet/


.. _init quickstart:

Using ``cabal init``
--------------------

The ``cabal init`` command creates the necessary files for a Cabal package,
it has both an ``--interactive`` (default) and ``--non-interactive``
mode. The interactive mode will walk you through many of the package
options and metadata, the non-interactive mode will simply pick reasonable
defaults which is sufficient if you're just trying something out.

.. highlight:: console

::

    $ cabal init --non-interactive
    # You can also use -n which is the short version of --non-interactive

If you want, you can also try out the interactive mode, for now chose
"Executable" when asked what type of package you want to build.

.. highlight:: console

::

    $ cabal init
    ...
    What does the package build:
       1) Executable
       2) Library
       3) Library and Executable
       4) Test suite
    Your choice?

One of the important questions is whether the package contains a library
and/or an executable. Libraries are collections of Haskell modules that
can be re-used by other Haskell libraries and programs, while executables
are standalone programs. Test suites can both depend on a library or be
standalone.

For the moment these are the only choices. For more complex packages
(e.g. a library and multiple executables) the ``.cabal``
file can be edited afterwards.

After you make your selection (executable; library; library
and executable; or: test suite) cabal asks us a number of questions starting with
which version of the cabal specification to use, our package's name
(for example, "proglet"), and our package's version.

::

    Generating CHANGELOG.md...
    Generating Main.hs...
    Generating proglet.cabal...

Use the ``ls`` command to see the created files:

::

   $ ls
   CHANGELOG.md  Main.hs  proglet.cabal


Running the program
-------------------

Now that we have our Haskell code and the extra files that Cabal needs, we
can build and run our application.

::

   $ cabal build
   Resolving dependencies...
   ...
   Linking /path/to/proglet ...

   $ cabal run proglet
   ...
   Hello, Haskell!

Since we have an executable we can use ``cabal run proglet`` which will build
our executable (and re-build it if we've made any changes) and then run the
binary. The ``cabal run`` command works for any ``component-name`` (tests for
example), not just the main executable.


About the Cabal package structure
---------------------------------

It is assumed that all the files that make up a package live under a common
root directory (apart from external dependencies). This simple example has
all the package files in one directory, but most packages use one or more
subdirectories.

Cabal needs one extra file in the package's root directory:

-  ``proglet.cabal``: contains package metadata and build information.


Editing the .cabal file
-----------------------

.. highlight:: cabal

Load up the ``.cabal`` file in a text editor. The first part of the
``.cabal`` file has the package metadata and towards the end of the file
you will find the :pkg-section:`executable` or :pkg-section:`library`
section.

You will see that the fields that have yet to be filled in are commented
out. Cabal files use "``--``" Haskell-style comment syntax.

.. NOTE::
   Comments are only allowed on lines on their own. Trailing comments on
   other lines are not allowed because they could be confused with program
   options.


::

    executable proglet
      main-is: Main.hs
      -- other-modules:
      -- other-extensions:
      build-depends: base >=4.11 && <4.12
      -- hs-source-dirs:
      default-language: Haskell2010


If you selected earlier to create a library package then your ``.cabal``
file will have a section that looks like this:

::

    library
      exposed-modules: MyLib
      -- other-modules:
      -- build-depends:
      build-depends: base >=4.11 && <4.12
      -- hs-source-dirs:
      default-language: Haskell2010


The build information fields listed (but commented out) are just the few
most important and common fields. There are many others that are covered
later in this chapter.

Most of the build information fields are the same between libraries and
executables. The difference is that libraries have a number of "exposed"
modules that make up the public interface of the library, while
executables have a file containing a ``Main`` module.

The name of a library always matches the name of the package, so it is
not specified in the library section. Executables often follow the name
of the package too, but this is not required and the name is given
explicitly.


Modules included in the package
-------------------------------

For an executable, ``cabal init`` creates the ``Main.hs`` file which
contains your program's ``Main`` module. It will also fill in the
:pkg-field:`executable:main-is` field with the file name of your program's
``Main`` module, including the ``.hs`` (or ``.lhs``) extension. Other
modules included in the executable should be listed in the
:pkg-field:`other-modules` field.

For a library, ``cabal init`` looks in the project directory for files
that look like Haskell modules and adds all the modules to the
:pkg-field:`library:exposed-modules` field. For modules that do not form part
of your package's public interface, you can move those modules to the
:pkg-field:`other-modules` field. Either way, all modules in the library need
to be listed.


Modules imported from other packages
------------------------------------

While your library or executable may include a number of modules, it
almost certainly also imports a number of external modules from the
standard libraries or other pre-packaged libraries. (These other
libraries are of course just Cabal packages that contain one or more libraries.)

You have to list all of the library packages that your library or
executable imports modules from. Or to put it another way: you have to
list all the other packages that your package depends on.

For example, suppose the example ``Proglet`` module imports the module
``Data.Map``. The ``Data.Map`` module comes from the ``containers``
package, so we must list it:

::

    library
      exposed-modules:     Proglet
      other-modules:
      build-depends:       containers, base >=4.11 && <4.12

In addition, almost every package also depends on the ``base`` library
package because it exports the standard ``Prelude`` module plus other
basic modules like ``Data.List``.

You will notice that we have listed ``base >=4.11 && <4.12``. This gives a
constraint on the version of the base package that our package will work
with. The most common kinds of constraints are:

-  ``pkgname >=n``
-  ``pkgname ^>=n``
-  ``pkgname >=n && <m``
-  ``pkgname ==n.*``

The last is just shorthand, for example ``base ==4.*`` means exactly
the same thing as ``base >=4 && <5``. Please refer to the documentation
on the :pkg-field:`build-depends` field for more information.

Also, you can factor out shared ``build-depends`` (and other fields such
as ``ghc-options``) into a ``common`` stanza which you can ``import`` in
your libraries and executable sections. For example:

::

    common shared-properties
      default-language: Haskell2010
      build-depends:
        base == 4.*
      ghc-options:
        -Wall

    library
      import: shared-properties
      exposed-modules:
        Proglet

Note that the ``import`` **must** be the first thing in the stanza. For more
information see the :ref:`common-stanzas` section.

.. _building-packages:

Building the package
--------------------

For simple packages that's it! We can now try building the package,
which also downloads and builds all required dependencies:

.. code-block:: console

    $ cabal build

If the package contains an executable, you can run it with:

.. code-block:: console

   $ cabal run

and the executable can also be installed for convenience:

.. code-block:: console

    $ cabal install

When installed, the executable program lands in a special directory
for binaries that may or may not already be on your system's ``PATH``.
If it is, the executable can be run by typing its filename on commandline.
For installing libraries see the :ref:`adding-libraries` section.
