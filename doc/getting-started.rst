Getting Started with Haskell and Cabal
======================================

Installing the Haskell toolchain
--------------------------------

To install the Haskell toolchain follow the `ghcup instructions
<https://www.haskell.org/ghcup/>`__ if you're using Linux or Mac, or follow
`this guide <https://hub.zhox.com/posts/introducing-haskell-dev/>`__ if you're
using Windows.


Creating a new application
--------------------------

Let's start by creating a simple Haskell application from scratch where we'll
learn about a Haskell package's directory structure, how to run the executable,
and how to add external dependencies.


Initializing the application
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Start by creating a ``myfirstapp`` directory to hold the project files, these
instructions work in unix shells and PowerShell (if you're on Windows).

.. code-block:: console

    $ mkdir myfirstapp
    $ cd myfirstapp

Once you have an empty directory we can initialize our package:

.. code-block:: console

    $ cabal init --cabal-version=2.4 --license=NONE -p myfirstapp

.. note:: ``cabal-version`` refers to the
          `version of the .cabal file format specification <file-format-changelog.html>`__,
          that can be different from the versions of the cabal library and tool
          in use. It is common to use a slightly older cabal-version, to strike
          a compromise between feature availability and backward compatibility.

This will generate the following files:

.. code-block:: console

    $ ls
    CHANGELOG.md
    Main.hs
    myfirstapp.cabal
    Setup.hs


``Main.hs`` is where your package's code lives. By default ``cabal init``
creates an executable with the same name as the package ``myfirstapp`` in this
case, you can instruct ``cabal init`` to generate just a library (with
``--lib``) or both a library and executable with (``--libandexe``); for the full
set of options see ``cabal init --help``.

``myfirstapp.cabal`` is Cabal's metadata file which describes your package and
its dependencies. We'll be updating this file in a little bit when we add an
external dependency to our package.


Running the application
^^^^^^^^^^^^^^^^^^^^^^^

As mentioned above, ``cabal init`` with no arguments generates a package with a
single executable that prints ``"Hello, Haskell!"`` to the terminal. To run the
executable enter the following command:

``cabal run :myfirstapp``

You should see the following output in the terminal:

.. code-block:: console

     $ cabal run :myfirstapp
     ...
     Hello, Haskell!

.. note:: The ``:`` prefix in ``:myfirstapp`` signifies that the
	  ``myfirstapp`` target is part of the current package.

Notice that we didn't need to run a `build` command before ``cabal run``, this
is because ``cabal run`` first determines if the code needs to be re-built
before running the executable. If you just want to build a target you can do so
with ``cabal build``:

``cabal build :myfirstapp``


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
       main-is: Main.hs
       build-depends:
           base >=4.11 && <4.12,
           haskell-say ^>=1.0.0.0

.. NOTE::
   ``^>=1.0.0.0`` means use version 1.0.0.0 of the library or any more recent
   minor release with the same major version.

Next we'll update ``Main.hs`` to use the ``HaskellSay`` library:

.. code-block:: haskell

   module Main where

   import HaskellSay (haskellSay)

   main :: IO ()
   main =
     haskellSay "Hello, Haskell! You're using a function from another package!"

``import HaskellSay (haskellSay)`` brings the ``haskellSay`` function from the
module named ``HaskellSay`` into scope. The ``HaskellSay`` module is defined in
the ``haskell-say`` packages that we added a dependency on above.

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
Cabal on the `introduction <intro.html>`__ page.
