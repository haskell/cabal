Getting Started
===============

Installing Cabal
----------------

The easiest and recommended way to install the ``cabal`` command-line tool
on Linux, macOS, FreeBSD or Windows is through `ghcup <https://www.haskell.org/ghcup/>`__.
It installs the “Haskell toolchain”, which includes Cabal,
the Haskell compiler `GHC <https://www.haskell.org/ghc/>`__
and optionally other useful Haskell tools.

Creating a new application
--------------------------

We create a minimal Haskell application to get a quick overview
of the ``cabal`` command-line tool:

1. How to initialize a Haskell package.
2. How files are organized inside a package.
3. How to compile Haskell files and run a resulting executable.
4. How to manage external dependencies.

Initializing an application
^^^^^^^^^^^^^^^^^^^^^^^^^^^

To initialize a new Haskell application, run

.. code-block:: console

    $ cabal init myapp --non-interactive

in a terminal. This generates the following files in a new ``myapp`` directory:

.. code-block:: console

    $ tree
    .
    └── myapp
        ├── app
        │   └── Main.hs
        ├── CHANGELOG.md
        └── myapp.cabal

The ``myapp.cabal`` file is a package description file, commonly referred to as a “Cabal file”:

.. code-block:: cabal

    cabal-version:      3.0
    name:               myapp
    version:            0.1.0.0
    -- ...

    executable myapp
        import:           warnings
        main-is:          Main.hs
        build-depends:    base ^>=4.19.0.0
        hs-source-dirs:   app
        default-language: Haskell2010

It contains metadata (package name and version, author name, license, etc.) and sections
to define package components. Components can be used to split large codebases into smaller,
more managable building blocks.
A component can be of one of several types (executable, library, etc.) and describes,
among other things, the location of source files and its dependencies.
The ``myapp.cabal`` file above defines a single component named ``myapp`` of the executable type.
Inside the ``executable`` section, the ``build-depends`` field lists the dependencies of this component.


The ``app/Main.hs`` file is where your executable's code lives:

.. code-block:: haskell

    module Main where

    main :: IO ()
    main = putStrLn "Hello, Haskell!"


To run the executable, switch into the application directory with ``cd myapp`` and run

.. code-block:: console

     $ cabal run myapp
     ...
     Hello, Haskell!

This command automatically determines if the executable needs to be (re)built
before running the executable. With only one executable component in the package,
``cabal run`` (without a component name) is smart enough to infer it, so the name can be omitted.

If you just want to build the executable without running it, run:

.. code-block:: console

    $ cabal build
    Resolving dependencies...
    ...
    Building executable 'myapp' for myapp-0.1.0.0..
    [1 of 1] Compiling Main             ( app/Main.hs, /home/.../myapp/dist-newstyle/build/.../myapp-tmp/Main.o )
    Linking /home/.../myapp/dist-newstyle/build/.../myapp


Adding dependencies
^^^^^^^^^^^^^^^^^^^

Next we'll add an external dependency to our application. `Hackage
<https://hackage.haskell.org/>`__ is the Haskell community's central `package`
archive of open source software.

In our application, we'll use a package called `haskell-say
<https://hackage.haskell.org/package/haskell-say>`__ to print text to the
terminal with some embellishment.

.. TIP::
   If you installed ``cabal`` a while ago but haven't used it recently you may
   need to update the package index, you can do this by running ``cabal
   update``.

In our ``myapp.cabal`` file, we will update the ``build-depends`` field of
the executable section to include ``haskell-say``:

.. code-block:: cabal

   executable myapp
       import: warnings
       main-is: Main.hs
       build-depends:
           base ^>=4.19.0.0,
           haskell-say ^>=1.0.0.0
       hs-source-dirs: app
       default-language: Haskell2010


.. NOTE::
   ``^>=1.0.0.0`` means use version 1.0.0.0 of the library or any more recent
   minor release with the same major version. To put it simply, this means
   use the latest version of the library that starts with ``1.0``.

Next we'll update ``app/Main.hs`` to use the ``HaskellSay`` library:

.. code-block:: haskell

   module Main where

   import HaskellSay (haskellSay)

   main :: IO ()
   main = haskellSay "Hello, Haskell!"

``import HaskellSay (haskellSay)`` brings the ``haskellSay`` function from the
module named ``HaskellSay`` into scope. The ``HaskellSay`` module is defined in
the ``haskell-say`` package that we added as a dependency above.

Now you can build and re-run your code to see the new output:

.. code-block:: console

   $ cabal run myapp
       ________________________________________________________
      /                                                        \
     | Hello, Haskell!                                          |
      \____       _____________________________________________/
           \    /
            \  /
             \/
       _____   _____
       \    \  \    \
        \    \  \    \
         \    \  \    \
          \    \  \    \  \-----------|
           \    \  \    \  \          |
            \    \  \    \  \---------|
            /    /  /     \
           /    /  /       \  \-------|
          /    /  /    ^    \  \      |
         /    /  /    / \    \  \ ----|
        /    /  /    /   \    \
       /____/  /____/     \____\

Running a single-file Haskell script
------------------------------------

Cabal also supports running single-file Haskell scripts like
the following file named ``myscript``:

.. code-block:: haskell

    #!/usr/bin/env cabal
    {- cabal:
    build-depends:
      base ^>=4.19.0.0,
      haskell-say ^>=1.0.0.0
    -}

    import HaskellSay (haskellSay)

    main :: IO ()
    main = haskellSay "Hello, Haskell!"

The necessary sections of a ``.cabal`` file are placed
directly into the script as a comment.

Use the familiar ``cabal run`` command to execute this script:

.. code-block:: console

    $ cabal run myscript

On Unix-like systems, a Haskell script starting with ``#!/usr/bin/env cabal``, like the one above,
can be run directly after setting the execute permission (+x):

.. code-block:: console

    $ chmod +x myscript
    $ ./myscript
       ________________________________________________________
      /                                                        \
     | Hello, Haskell!                                          |
      \____        ____________________________________________/
           \ ... /

See more in the documentation for :ref:`cabal run`.

What Next?
----------

Now that you know how to set up a simple Haskell package using Cabal, check out
some of the resources on the Haskell website's `documentation page
<https://www.haskell.org/documentation/>`__ or read more about packages and
Cabal on the :doc:`What Cabal does <cabal-context>` page.
