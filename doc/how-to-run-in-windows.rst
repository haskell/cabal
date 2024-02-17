How to use Cabal in Windows
===========================

This document describes how to use Cabal in a Windows system. See the
:ref:`Further reading` section for some other references that might provide some
more explanations. For a TL;DR, jump to the :ref:`Complete configuration`.

Install the Haskell environment
-------------------------------

The recommended way of setting up a Haskell environment in Windows is by using
`GHCup <https://www.haskell.org/ghcup/>`_. Follow the steps outlined in its
webpage to install at least GHC and Cabal. GHCup will install its own MSYS2
system in your computer unless told not to do so, refer to `its documentation
<https://www.haskell.org/ghcup/install/#windows_1>`_ for more information.

Ensure that Cabal can call UNIX-like tools
------------------------------------------

As Cabal needs sometimes to call UNIX-like tools that come with MSYS2 (such as
``make`` or even ``git``), the directories where those are located need to be
made visible in the ``PATH``. For that, Cabal provides the ``extra-prog-path``
configuration option. Your :ref:`global configuration <config-file-discovery>`
should include this option:

::

   extra-prog-path: <msys-dir>\usr\bin
                    <msys-dir>\<environment>\bin

Where ``<msys-dir>`` points to the location of your MSYS2 installation. Refer to
GHCup's documentation on where this directory is located by default.
``<environment>`` has to be one of the environments of MSYS2, which for GHCup is
``mingw64``. You can learn more about the different environments in the `MSYS2
documentation <https://www.msys2.org/docs/environments/>`_.

.. note::

   Currently this step is already done by the GHCup installation on your behalf
   by default unless told otherwise.

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

   Currently this step is already done by the GHCup installation on your behalf
   by default unless told otherwise.

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

   Currently this step is already done by the GHCup installation on your behalf
   by default unless told otherwise.

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
                    <msys-dir>\usr\bin
                    <msys-dir>\<environment>\bin

.. note::

   Currently this is already done by the GHCup installation on your behalf by
   default unless told otherwise.

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
