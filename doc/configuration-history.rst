.. _configuration-history:

Configuration History
=====================

``cabal`` is configured in two places. The :ref:`configuration file
<config-file-discovery>` applies to everything ``cabal`` does and a
:ref:`project <cabal-project-file>` applies to the packages of that project.
Most fields can be used in either.

A package description gives the version of its format with
:pkg-field:`cabal-version`. The configuration file and a project have no such
field, so ``cabal`` reads them with whatever fields it has at the time. This
page lists the fields that have been renamed, replaced or removed, to help with
reading and updating older files.

Fields with a newer name
------------------------

These fields are still accepted, without a warning, and do the same as the
field that replaces them.

.. list-table::
    :header-rows: 1

    * - Older field
      - Use
    * - ``remote-repo``
      - A :ref:`repository <repository-specification>` section
    * - ``repos``
      - ``repository`` sections
    * - ``cachedir``
      - :cfg-field:`remote-repo-cache`
    * - ``hackage-username``
      - ``username``
    * - ``hackage-password``
      - ``password``
    * - ``hackage-password-command``
      - ``password-command``
    * - ``hackage-token``
      - ``token``
    * - ``user-prefix``, ``user-bindir`` and the other ``user-`` fields
      - ``prefix``, ``bindir`` and so on in an ``install-dirs user`` section
    * - ``global-prefix``, ``global-bindir`` and the other ``global-`` fields
      - ``prefix``, ``bindir`` and so on in an ``install-dirs global`` section
    * - :cfg-field:`library-coverage` [#both]_
      - :cfg-field:`coverage`
    * - :cfg-field:`prefer-oldest` [#both]_
      - :cfg-field:`prefer-version` with the value ``oldest``, since
        ``cabal-install-3.20``
    * - :cfg-field:`symlink-bindir` [#both]_
      - :cfg-field:`installdir`

.. [#both] Accepted in a project too. The other fields are accepted in the
   configuration file only.

.. admonition:: The remote-repo field
    :class: note

    This older, one-line field is equivalent to the newer ``repository``
    section with a ``url`` field:

    ::

        remote-repo: packages.example.org:http://packages.example.org/

        repository packages.example.org
          url: http://packages.example.org/

    Configuration files old enough to have this field often give Hackage's
    URL as ``http://hackage.haskell.org/packages/archive``. ``cabal`` reads
    that as ``http://hackage.haskell.org/``.

Removed fields
--------------

.. list-table::
    :header-rows: 1

    * - Removed field
      - Removed in ``cabal-install``
    * - ``local-repo``
      - 3.4
    * - ``require-sandbox`` and ``ignore-sandbox``
      - 3.4, along with sandboxes
    * - ``project-file`` and ``ignore-project``
      - 3.4
    * - ``world-file`` and ``one-shot``
      - 3.8
    * - ``nix``
      - 3.18, along with the Nix integration

A removed field is ignored with a warning that the field is unrecognised,
except for ``world-file``, which is ignored without one.

In place of ``local-repo``, use a no-index repository or a secure repository at
a ``file:`` URL. See :ref:`repository-specification`. The other removed fields
have no replacement.

Project-related field gotchas
-----------------------------

``cabal`` makes most configuration fields from the options of its commands,
which is why ``--jobs`` on the command line is ``jobs`` in the configuration
file. An option added to one of those commands becomes a field of the
configuration file too. Two options to do with projects became fields this way.

Those two and a third are now accepted in a project, where they have no effect:
``cabal`` has found the project file, and decided not to ignore it, before
reading it. All three are still :ref:`options <cmdoption-project-file>` on the
command line.

project-file
^^^^^^^^^^^^

The ``--project-file`` option was added among the options of ``cabal install``,
all of which are fields, so up to ``cabal-install-3.2`` this line was accepted
in the configuration file:

::

    project-file: other.project

It made ``cabal`` look for ``other.project`` in place of ``cabal.project`` in
every project on the machine. In ``cabal-install-3.4`` the project options were
moved to a group of their own, one that is not made into fields of the
configuration file, and the field went with them. Since that release it has
been accepted in a project.

ignore-project
^^^^^^^^^^^^^^

The ``--ignore-project`` option was a field of the configuration file in
``cabal-install-3.2`` only. Since that release it has been accepted in a
project.

project-dir
^^^^^^^^^^^

The ``--project-dir`` option has never been a field of the configuration file.
Since ``cabal-install-3.12`` it has been accepted in a project.

Relative imports
----------------

A project can :ref:`import <conditionals and imports>` other files since
``cabal-install-3.8``. What a relative path in an ``import`` is relative to has
changed since then.

.. list-table::
    :header-rows: 1

    * - ``cabal-install``
      - A relative ``import`` path is relative to
    * - 3.8
      - The directory ``cabal`` is run from
    * - 3.10
      - The directory of the importing file. An import in an imported file is
        only found when ``cabal`` is run from the project's directory.
    * - 3.12 and later
      - The directory of the importing file

The change shows in a file that is imported and has an import of its own. With
these files, ``one.config`` has to say where ``two.config`` is.

.. code-block:: text

    cabal.project
    configs/
        one.config
        two.config

In ``cabal-install-3.8``, run from the project's directory, that is
``import: configs/two.config``. From ``cabal-install-3.10`` it is
``import: two.config``. Neither line works in both versions.

Only the path of an ``import`` changed. A path in another field of an imported
file, such as ``packages``, is relative to the project's directory in all of
these releases.

The configuration file has no ``import``.

The project file parser
-----------------------

Since ``cabal-install-3.18`` a project is read with a new parser, falling back
to the old one if the new one fails. The old parser is to be removed in a
future release. The :ref:`--project-file-parser
<cmdoption-project-file-parser>` option picks between them.
