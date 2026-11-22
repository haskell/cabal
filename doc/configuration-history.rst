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

.. note::
    The ``remote-repo`` field is an older way of specifying a repository. This
    field,

    ::

        remote-repo: packages.example.org:http://packages.example.org/

    is the same as this section:

    ::

        repository packages.example.org
          url: http://packages.example.org/

    For Hackage alone, the URL ``http://hackage.haskell.org/packages/archive``
    is read as ``http://hackage.haskell.org/``.

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
    * - ``world-file``
      - 3.8
    * - ``nix``
      - 3.18, along with the Nix integration

A removed field is ignored with a warning that the field is unrecognised,
except for ``world-file``, which is ignored without one.

In place of ``local-repo``, use a no-index repository or a secure repository at
a ``file:`` URL. See :ref:`repository-specification`. The other removed fields
have no replacement.

The project file parser
-----------------------

Since ``cabal-install-3.18`` a project is read with a new parser, falling back
to the old one if the new one fails. The old parser is to be removed in a
future release. The :ref:`--project-file-parser
<cmdoption-project-file-parser>` option picks between them.
