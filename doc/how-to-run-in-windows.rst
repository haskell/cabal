How to use Cabal in Windows
===========================

This document describes how to use Cabal in a Windows system. See the
:ref:`Further reading` section for some other references that might provide some
more explanations. For a TL;DR, jump to the :ref:`Complete configuration`.

Install the Haskell environment
-------------------------------

Haskell development on Windows makes use of the `MSYS2 <https://www.msys2.org/>`_ 
tools.

The recommended way of setting up a Haskell environment in Windows is by using
`GHCup <https://www.haskell.org/ghcup/>`_. Follow the steps outlined in its
webpage to install at least GHC and Cabal. GHCup will install its own MSYS2
system in your computer unless told not to do so: refer to `its documentation
<https://www.haskell.org/ghcup/install/#windows_1>`_ for more information.

.. NOTE::
   Stack is another tool you can use to set up a Haskell environment on Windows. Stack
   can be installed on its own or via GHCup. See 
   `Stack's webpage <https://docs.haskellstack.org/en/stable/>`_ and/or 
   `GHCup's section on Stack integration <https://www.haskell.org/ghcup/guide/#stack-integration>`_,
   in particular the `Windows related subsection <https://www.haskell.org/ghcup/guide/#windows>`_.

Ensure that Cabal can call the tools it needs
---------------------------------------------

Cabal sometimes needs to call tools that do not come with Windows (such as
``make`` or even ``git``). The MSYS2 project makes many of them available on
Windows. The directories where those are located need to be made visible in the
``PATH`` when executing ``cabal``. For that, Cabal provides the
``extra-prog-path`` configuration option. Your :ref:`global configuration
<config-file-discovery>` should include this option:

::

   extra-prog-path: <msys-dir>\<environment>\bin
                    <msys-dir>\usr\bin

Where ``<msys-dir>`` points to the location of your MSYS2 installation. Refer to
GHCup's documentation on the default location of this directory.
``<environment>`` has to be one of the environments of MSYS2, which for GHCup is
``mingw64``. You can learn more about the different environments in the `MSYS2
documentation <https://www.msys2.org/docs/environments/>`_.

.. note::

   Unless told otherwise, the GHCup bootstrap script already adds these directories to `extra-prog-path`
   by default.

Ensure that Cabal can use system libraries
------------------------------------------

Third-party libraries can be installed using the ``pacman`` package manager on
the MSYS2 installation. When installing a third party package its libraries and
header files will (usually) be placed in
``<msys-dir>\<environment>\{lib,include}`` respectively. These directories need
to be specified in the ``extra-lib-dirs`` and ``extra-include-dirs``
respectively. Your :ref:`global configuration <config-file-discovery>` should
include these options:

::

   extra-include-dirs: <msys-dir>\<environment>\include
   extra-lib-dirs: <msys-dir>\<environment>\lib


.. note::

   Unless told otherwise, the GHCup bootstrap script already adds these directories to `extra-include-dirs` and `extra-lib-dirs`
   by default.

.. warning::

   Packages in the ``msys/`` repo are not native Windows libraries and will
   probably not work when one tries to link to them. Install the packages for
   your selected environment, which for GHCup is ``mingw64/``. Refer to `MSYS2's
   package management documentation
   <https://www.msys2.org/docs/package-management/>`_ for more information.

Ensure that Cabal can call Haskell tools
----------------------------------------

Haskell tools are located in two places:

- ``<ghcup-dir>\bin`` for standard Haskell tools such as GHC, Cabal, Haddock, ``hsc2hs``...

- The ``installdir`` that Cabal is configured with for user-installed Haskell tools.

For Cabal to be able to invoke these tools, those directories need to be made
visible in the ``PATH``. Your :ref:`global configuration <config-file-discovery>` should
include these options:

::

   installdir: <installdir>
   extra-prog-path: ...
                    <ghcup-dir>\bin
                    <installdir>

.. note::

   Unless told otherwise, the GHCup bootstrap script already adds these directories to `extra-prog-path`
   by default.

.. _Complete configuration:

Complete configuration
----------------------

The complete :ref:`global configuration <config-file-discovery>` should finally
look like this:

::

   installdir: <installdir>
   extra-include-dirs: <msys-dir>\<environment>\include
   extra-lib-dirs: <msys-dir>\<environment>\lib
   extra-prog-path: <ghcup-dir>\bin
                    <installdir>
                    <msys-dir>\<environment>\bin
                    <msys-dir>\usr\bin

.. note::

   Unless told otherwise, the GHCup bootstrap script already sets this configuration file to the right
   values by default.

.. _Further reading:

Further reading
---------------

- MSYS2 homepage: https://www.msys2.org
- MinGW-W64 homepage: https://www.mingw-w64.org/
- Setting up Windows to build GHC:
  https://gitlab.haskell.org/ghc/ghc/-/wikis/building/preparation/windows
- Some definitions and useful tools:
  https://gitlab.haskell.org/ghc/ghc/-/wikis/surviving-windows

Outdated links
~~~~~~~~~~~~~~

These links are outdated but still useful to understand the overall picture:

- GHC's wiki about the Windows platform (outdated, GHC now uses MSYS2):
  https://gitlab.haskell.org/ghc/ghc/-/wikis/building/platforms/windows
- The Windows toolchain (outdated, GHC now uses the ``CLANG64`` environment):
  https://gitlab.haskell.org/ghc/ghc/-/wikis/working-conventions/windows-toolchain
- Haskell Wiki on Windows (outdated, it talks about MSYS and old tools such as
  the Haskell platform): https://wiki.haskell.org/Windows
