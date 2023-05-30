Configuration
=============

.. highlight:: cabal

Overview
--------

The global configuration file for ``cabal-install`` is by default
``$XDG_CONFIG_HOME/cabal/config``. If you do not have this file, ``cabal`` will create
it for you on the first call to ``cabal update``
(details see `configuration file discovery`_).
Alternatively, you can explicitly ask ``cabal`` to create it for you using

.. code-block:: console

    $ cabal user-config update

You can change the location of the global configuration file by specifying
either ``--config-file=FILE`` on the command line or by setting the
``CABAL_CONFIG`` or ``CABAL_DIR`` environment variable.

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

   If set, *all* ``cabal-install`` content files will be stored as
   subdirectories of this directory, including the configuration file
   if ``CABAL_CONFIG`` is unset.  If ``CABAL_DIR`` is unset, Cabal
   will store data files according to the XDG Base Directory
   Specification (see `directories`_).

   .. note::

       For backwards compatibility, if the directory ``~/.cabal`` on
       Unix or ``%APPDATA%\cabal`` on Windows exists, and
       ``$XDG_CONFIG_HOME/cabal/config`` does not exist, and
       ``CABAL_DIR`` is unset, ``cabal-install`` will behave as if
       ``CABAL_DIR`` was set to point at this directory.

``CABAL_BUILDDIR``

    The override for default ``dist`` build directory.
    Note, the nix-style builds build directory (``dist-newstyle``)
    is not affected by this environment variable.

.. _config-file-discovery:

Configuration file discovery
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The configuration file location is determined as follows:

1. If option ``--config-file`` is given, use it;
2. otherwise, if ``$CABAL_CONFIG`` is set use it;
3. otherwise, if ``$CABAL_DIR`` is set use ``$CABAL_DIR/config``;
4. otherwise use ``config`` in ``$XDG_CONFIG_HOME/cabal``, which
   defaults to ``~/.config/cabal`` on Unix.

If the configuration file does not exist, and it was not given
explicitly via ``--config-file`` or ``$CABAL_CONFIG``, then
``cabal-install`` will generate the default one, with directories
based on ``$CABAL_DIR`` (if set) or according to the XDG Base
Directory Specification, as listed below.

.. _directories:

Directories
-----------

Unless the ``CABAL_DIR`` environment variable is set or a ``~/.cabal``
directory exists, Cabal will by default store data in directories
according to the XDG Base Directory Specification.  The following
directories are used unless otherwise specified in the configuration
file:

* ``$XDG_CONFIG_HOME/cabal`` for the main configuration file.
  Defaults to ``~/.config/cabal`` on Unix, and ``%APPDATA%/cabal`` on
  Windows.  Overridden by the ``CABAL_CONFIG`` environment variable if
  set.

* ``$XDG_CACHE_HOME/cabal`` for downloaded packages and script
  executables.  Defaults to ``~/.cache/cabal`` on Unix, and
  ``%LOCALAPPDATA%/cabal`` on Windows.  You can delete this directory
  and expect that its contents will be reconstructed as needed.

* ``$XDG_STATE_HOME/cabal`` for compiled libraries and other stateful
  artifacts, including the Cabal store.  Defaults to
  ``~/.local/state/cabal`` on Unix and ``%LOCALAPPDATA%/cabal`` on
  Windows.  Deleting this directory might cause installed programs to
  stop working.

* ``~/.local/bin`` for executables installed with ``cabal install``.

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
``$XDG_CACHE_HOME/cabal/packages/hackage.haskell.org`` (or whatever name you specify;
you can change the prefix by changing the value of
:cfg-field:`remote-repo-cache`). If you want, you can configure multiple
repositories, and ``cabal`` will combine them and be able to download
packages from any of them.

Using secure repositories
^^^^^^^^^^^^^^^^^^^^^^^^^

When interacting with ``hackage.haskell.org``, Cabal always runs in secure mode
with standard root keys, so it is not necessary to specify ``secure`` or
``root-keys``. If no repositories are listed, Cabal will default to
``hackage.haskell.org``.

For non-Hackage repositories that support the TUF security infrastructure you
can enable secure access to the repository by specifying:

::

    repository packages.example.org
      url: http://packages.example.org/
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

It is possible to define ``preferred-versions``, containing additional version constraints
for deprecating or preferring certain package versions, in the given directory.

For example, if ``/absolute/path/to/directory`` looks like
::

    /absolute/path/to/directory/
        foo-0.1.0.0.tar.gz
        bar-0.2.0.0.tar.gz
        preferred-versions

then package deprecations and preferences will be taken into account by the solver.

The contents of ``preferred-versions`` is a list of package version constraints, e.g.
::

    binary < 0.8.0.0 || > 0.8.0.0
    text == 1.2.0.0

thus, looks similar to a ``package-name.cabal``'s ``build-depends`` section.

.. note::
    The ``preferred-versions`` file can be used to restrict the package set from Hackage, by preferring
    certain versions or marking a specific version as deprecated. To achieve this, add a
    local no-index repository to your :ref:`configuration file <config-file-discovery>`,
    where the directory contains your custom
    ``preferred-versions``. After running ``cabal update``, all ``cabal`` operations will honour the
    configuration.

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
