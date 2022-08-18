Getting Started with Haskell and Cabal
======================================

Installing the Haskell toolchain
--------------------------------

To install the Haskell toolchain follow the `ghcup instructions
<https://www.haskell.org/ghcup/>`__.


Creating a new application
--------------------------

Let's start by creating a simple Haskell application from scratch where we'll
learn about a Haskell package's directory structure, how to run the executable,
and how to add external dependencies.


Initializing the application
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Start by initialising our ``myfirstapp`` project, these instructions work in
unix shells and PowerShell (if you're on Windows).

.. code-block:: console

    $ cabal init myfirstapp -n

.. note:: ``myfirstapp`` stands for the directory (or path) where the project
          will reside in, if omitted, ``cabal init`` will do its proceedings
          in the directory it's called in.

.. note:: ``-n`` stands for ``--non-interactive``, which means that cabal will try to guess
          how to set up the project for you and use the default settings, which will serve us
          well for the purpose of this tutorial.
          When setting up your projects in the future, you will likely want to omit ``-n``
          and do just ``cabal init``, so that cabal will interactively ask you
          for the details on how the project should be set up
          (while still offering reasonable defaults on every step).
          Also, you can run ``cabal init --help`` to get more info on how ``cabal init`` can be used.

This will generate the following files:

.. code-block:: console

    $ tree
    .
    └── myfirstapp
        ├── app
        │   └── Main.hs
        ├── CHANGELOG.md
        └── myfirstapp.cabal

``app/Main.hs`` is where your package's code lives.

``myfirstapp.cabal`` is Cabal's metadata file which describes your package,
how it is built and its dependencies. We'll be updating this file in a
little bit when we add an external dependency to our package.


Running the application
^^^^^^^^^^^^^^^^^^^^^^^

When we ran ``cabal init myfirstapp -n`` above, it generated a package with a single
executable named same as the package (in this case ``myfirstapp``) that prints
``"Hello, Haskell!"`` to the terminal. To run the executable enter the project's
directory and run it, by inputting the following commands:

.. code-block:: console

    cd myfirstapp
    cabal run myfirstapp

You should see the following output in the terminal:

.. code-block:: console

     $ cabal run myfirstapp
     ...
     Hello, Haskell!

Notice that we didn't need to run a `build` command before we ran ``cabal run``.
This is because ``cabal run`` automatically determines if the code needs to be (re)built
before running the executable.
If you just want to build a target without running it, you can do so with ``cabal build``:

``cabal build myfirstapp``


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

In our ``myfirstapp.cabal`` file we'll update the ``build-depends`` attribute of
the ``executable myfirstapp`` section to include ``haskell-say``:

.. code-block:: cabal

   executable myfirstapp
       import: warnings
       main-is: Main.hs
       build-depends:
           base ^>=4.14.3.0,
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
   main =
     haskellSay "Hello, Haskell! You're using a function from another package!"

``import HaskellSay (haskellSay)`` brings the ``haskellSay`` function from the
module named ``HaskellSay`` into scope. The ``HaskellSay`` module is defined in
the ``haskell-say`` package that we added as a dependency above.

Now you can build and re-run your code to see the new output:

.. code-block:: console

   $ cabal run
       ________________________________________________________
      /                                                        \
     | Hello, Haskell! You're using a function from another     |
     | package!                                                 |
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


What Next?
----------

Now that you know how to set up a simple Haskell package using Cabal, check out
some of the resources on the Haskell website's `documentation page
<https://www.haskell.org/documentation/>`__ or read more about packages and
Cabal on the :doc:`introduction <intro>` page.
