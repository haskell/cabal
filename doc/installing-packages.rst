Configuration
=============

.. highlight:: cabal

Overview
--------

The global configuration file for ``cabal-install`` is
``~/.cabal/config``. If you do not have this file, ``cabal`` will create
it for you on the first call to ``cabal update``. Alternatively, you can
explicitly ask ``cabal`` to create it for you using

.. code-block:: console

    $ cabal user-config update

You can change the location of the global configuration file by specifying
either ``--config-file=FILE`` on the command line or by setting the
``CABAL_CONFIG`` environment variable.

Most of the options in this configuration file are also available as
command line arguments, and the corresponding documentation can be used
to lookup their meaning. The created configuration file only specifies
values for a handful of options. Most options are left at their default
value, which it documents; for instance,

::

    -- executable-stripping: True

means that the configuration file currently does not specify a value for
the ``executable-stripping`` option (the line is commented out), and
that the default is ``True``; if you wanted to disable stripping of
executables by default, you would change this line to

::

    executable-stripping: False

You can also use ``cabal user-config update`` to migrate configuration
files created by older versions of ``cabal``.

Environment variables
---------------------

Various environment variables affect ``cabal-install``.

``CABAL_CONFIG``
   The variable to find global configuration file.

``CABAL_DIR``
   Default content directory for ``cabal-install`` files.
   Default value is ``getAppUserDataDirectory "cabal"``, which is
   ``$HOME/.cabal`` on unix systems and ``%APPDATA%\cabal`` in Windows.

   .. note::

       The CABAL_DIR might be dropped in the future, when
       ``cabal-install`` starts to use XDG Directory specification.

``CABAL_BUILDDIR``
    The override for default ``dist`` build directory.
    Note, the nix-style builds build directory (``dist-newstyle``)
    is not affected by this environment variable.

Configuration file discovery
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

1. If ``$CABAL_CONFIG`` is set use it,
2. otherwise if ``$CABAL_DIR`` is set use ``$CABAL_DIR/config``
3. otherwise use ``getAppUserDirectory "cabal"``

If the configuration file doesn't exist ``cabal-install``
will generate the default one, with directories based on
``$CABAL_DIR`` (if set) or ``getAppUserDirectory "cabal"`` prefix.

.. note:

    If ``$CABAL_CONFIG`` is set, but the file doesn't exist,
    one will be generated with ``$CABAL_DIR`` or ``getAppUserDirectory "cabal"``
    based prefixes. In other words not the prefixes based on a
    directory part of ``$CABAL_CONFIG`` path.

Repository specification
------------------------

An important part of the configuration is the specification of the
repository. When ``cabal`` creates a default config file, it configures
the repository to be the central Hackage server:

::

    repository hackage.haskell.org
      url: http://hackage.haskell.org/

The name of the repository is given on the first line, and can be
anything; packages downloaded from this repository will be cached under
``~/.cabal/packages/hackage.haskell.org`` (or whatever name you specify;
you can change the prefix by changing the value of
:cfg-field:`remote-repo-cache`). If you want, you can configure multiple
repositories, and ``cabal`` will combine them and be able to download
packages from any of them.

Using secure repositories
^^^^^^^^^^^^^^^^^^^^^^^^^

For repositories that support the TUF security infrastructure (this
includes Hackage), you can enable secure access to the repository by
specifying:

::

    repository hackage.haskell.org
      url: http://hackage.haskell.org/
      secure: True
      root-keys: <root-key-IDs>
      key-threshold: <key-threshold>

The ``<root-key-IDs>`` and ``<key-threshold>`` values are used for
bootstrapping. As part of the TUF infrastructure the repository will
contain a file ``root.json`` (for instance,
http://hackage.haskell.org/root.json) which the client needs to do
verification. However, how can ``cabal`` verify the ``root.json`` file
*itself*? This is known as bootstrapping: if you specify a list of root
key IDs and a corresponding threshold, ``cabal`` will verify that the
downloaded ``root.json`` file has been signed with at least
``<key-threshold>`` keys from your set of ``<root-key-IDs>``.

You can, but are not recommended to, omit these two fields. In that case
``cabal`` will download the ``root.json`` field and use it without
verification. Although this bootstrapping step is then unsafe, all
subsequent access is secure (provided that the downloaded ``root.json``
was not tampered with). Of course, adding ``root-keys`` and
``key-threshold`` to your repository specification only shifts the
problem, because now you somehow need to make sure that the key IDs you
received were the right ones. How that is done is however outside the
scope of ``cabal`` proper.

More information about the security infrastructure can be found at
https://github.com/haskell/hackage-security.

Local no-index repositories
^^^^^^^^^^^^^^^^^^^^^^^^^^^

It's possible to use a directory of `.tar.gz` package files as a local package
repository.

::

    repository my-local-repository
      url: file+noindex:///absolute/path/to/directory

``cabal`` will construct the index automatically from the
``package-name-version.tar.gz`` files in the directory, and will use optional
corresponding ``package-name-version.cabal`` files as new revisions.

For example, if ``/absolute/path/to/directory`` looks like
::

    /absolute/path/to/directory/
        foo-0.1.0.0.tar.gz
        bar-0.2.0.0.tar.gz
        bar-0.2.0.0.cabal

then ``cabal`` will create an index with two packages:

- ``foo-0.1.0.0`` using the source and ``.cabal`` file inside
  ``foo-0.1.0.0.tar.gz``
- ``bar-0.2.0.0`` using the source inside ``bar-0.2.0.0.tar.gz``
  and ``bar-0.2.0.0.cabal``

The index is cached inside the given directory. If the directory is not
writable, you can append ``#shared-cache`` fragment to the URI,
then the cache will be stored inside the :cfg-field:`remote-repo-cache` directory.
The part of the path will be used to determine the cache key part.

.. note::
    ``cabal-install`` creates a ``.cache`` file, and will aggressively use
    its contents if it exists. Therefore if you change the contents of
    the directory, remember to wipe the cache too.

.. note::
    The URI scheme ``file:`` is interpreted as a remote repository,
    as described in the previous sections, thus requiring manual construction
    of ``01-index.tar`` file.

Legacy repositories
^^^^^^^^^^^^^^^^^^^

Currently ``cabal`` supports single kind of “legacy” repositories.
It is specified using

::

    remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive

This is just syntactic sugar for

::

    repository hackage.haskell.org
      url: http://hackage.haskell.org/packages/archive

although, in (and only in) the specific case of Hackage, the URL
``http://hackage.haskell.org/packages/archive`` will be silently
translated to ``http://hackage.haskell.org/``.

Secure local repositories
^^^^^^^^^^^^^^^^^^^^^^^^^

If you want to use repositories on your local file system, it is
recommended instead to use a *secure* local repository:

::

    repository my-local-repo
      url: file:/path/to/local/repo
      secure: True
      root-keys: <root-key-IDs>
      key-threshold: <key-threshold>

The layout of these secure local repos matches the layout of remote
repositories exactly; the :hackage-pkg:`hackage-repo-tool`
can be used to create and manage such repositories.

.. _installing-packages:

Building and installing packages
================================

To be written

Installing packages from Hackage
--------------------------------

The ``cabal`` tool also can download, configure, build and install a
`Hackage`_ package and all of its
dependencies in a single step. To do this, run:

::

   $ cabal install [PACKAGE...]

To browse the list of available packages, visit the `Hackage`_ web site.

.. _Hackage: https://hackage.haskell.org/