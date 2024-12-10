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

MSYS2 environments and packages
-------------------------------

A particular environment has to be chosen when using MSYS2. By default GHCup will
use ``MINGW64``. You can learn more about the different environments in the `MSYS2
documentation <https://www.msys2.org/docs/environments/>`_.

GHCs before 9.4.1 are shipped with a minimal set of packages based on the
``MINGW64`` environment, and GHC 9.4.1 and newer are shipped with a minimal set
of packages based on the ``CLANG64`` environment. It is in general advisable to
work inside the same environment as your GHC uses, but (with some exceptions)
it shouldn't matter whether environments are mixed. Stay warned that it can
sometimes lead to undecipherable errors.

We will refer to the chosen environment as ``<environment>`` through this
documentation.

Third-party libraries and tools can be installed using the ``pacman`` package
manager on the MSYS2 installation
(`see <https://www.msys2.org/docs/package-management/>`_). If MSYS2 was
installed via GHCup, check GHCup's documentation on how to call ``pacman``. Note
that installing a package ``mingw-w64-<environment>-x86_64-<pkg>`` will install
it in the ``<msys-dir>\<environment>`` tree of directories, and might not be
visible if working on a different environment than ``<environment>``. In
general, it is advisable to install only packages for the environment that was
chosen above.

Apart from these environments, there is the ``msys`` environment which is based
on Cygwin. Some tools only exist for this environment. Tools from this environment
are callable when working in any other environment. It is in general not possible
to link to libraries installed in the ``msys`` environment.

Ensure that Cabal can call the tools it needs
---------------------------------------------

Cabal sometimes needs to call tools that do not come with Windows (such as
``make`` or even ``git``). The MSYS2 project makes many of them available on
Windows. The directories where those are located need to be made visible in the
``PATH`` when executing :term:`cabal`. For that, Cabal provides the
``extra-prog-path`` configuration option. Your :ref:`global configuration
<config-file-discovery>` should include this option:

::

   extra-prog-path: <msys-dir>\<environment>\bin
                    <msys-dir>\usr\bin

Where ``<msys-dir>`` points to the location of your MSYS2 installation. If MSYS2
was installed via GHCup, refer to GHCup's documentation on the default location
of this directory. If MSYS2 was installed system-wide this is usually
``C:\msys64``.

.. note::

   Unless told otherwise, the GHCup bootstrap script already adds these directories to `extra-prog-path`
   by default.

Ensure that Cabal can use system libraries
------------------------------------------

When installing a third party package its libraries and
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

   GHCs older than 9.4.1 will crash if a recent
   ``mingw-w64-<environment>-x86_64-crt-git`` is installed for whichever ``<environment>`` and
   these directories are set globally .

   Effectively this means that if you have installed ``mingw-w64-<environment>-x86_64-crt-git``
   (which you probably have if you are using ``clang`` in the ``CLANG64``
   environment or ``gcc`` in the ``UCRT64`` or ``MINGW64`` environments outside of
   Haskell, as this package is part of the ``mingw-w64-<environment>-x86_64-toolchain``
   meta-packages) and are using a GHC older than 9.4.1, you cannot simply depend on system
   libraries by adding these paths to the global config, and instead you will
   have to go through some other method to depend on those libraries like
   :pkg-field:`pkgconfig-depends`.

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
