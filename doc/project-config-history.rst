.. _project-config-history:

Project and Configuration History
=================================

Unlike a package description, with its :pkg-field:`cabal-version`, a project
and the configuration file don't say which version of their format they are
written in. ``cabal`` reads them with whatever fields it has at the time. This
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
      - Accepted in
    * - ``remote-repo: NAME:URL``
      - A ``repository NAME`` section with that ``url``. See
        :ref:`repository-specification`. For Hackage alone, the URL
        ``http://hackage.haskell.org/packages/archive`` is read as
        ``http://hackage.haskell.org/``.
      - Configuration file
    * - ``repos``
      - ``repository`` sections
      - Configuration file
    * - ``cachedir``
      - :cfg-field:`remote-repo-cache`
      - Configuration file
    * - ``hackage-username``
      - ``username``
      - Configuration file
    * - ``hackage-password``
      - ``password``
      - Configuration file
    * - ``hackage-password-command``
      - ``password-command``
      - Configuration file
    * - ``hackage-token``
      - ``token``
      - Configuration file
    * - ``user-prefix``, ``user-bindir`` and the other ``user-`` fields
      - ``prefix``, ``bindir`` and so on in an ``install-dirs user`` section
      - Configuration file
    * - ``global-prefix``, ``global-bindir`` and the other ``global-`` fields
      - ``prefix``, ``bindir`` and so on in an ``install-dirs global`` section
      - Configuration file
    * - :cfg-field:`library-coverage`
      - :cfg-field:`coverage`
      - Both
    * - :cfg-field:`prefer-oldest`
      - :cfg-field:`prefer-version` with the value ``oldest``, since
        ``cabal-install-3.20``
      - Both
    * - :cfg-field:`symlink-bindir`
      - :cfg-field:`installdir`
      - Both

Removed fields
--------------

.. list-table::
    :header-rows: 1

    * - Removed field
      - Removed in
      - What happens now
      - Use
    * - ``local-repo``
      - ``cabal-install-3.4``
      - Ignored with a warning that the field is unrecognised
      - A no-index repository or a secure repository at a ``file:`` URL. See
        :ref:`repository-specification`.
    * - ``require-sandbox`` and ``ignore-sandbox``
      - ``cabal-install-3.4``, along with sandboxes
      - Ignored with a warning that the field is unrecognised
      - Nothing. Every project build is now kept apart from the others.
    * - ``world-file``
      - ``cabal-install-3.8``
      - Ignored without a warning
      - Nothing
    * - ``nix``
      - ``cabal-install-3.18``, along with the Nix integration
      - Ignored with a warning that the field is unrecognised
      - Nothing

The project file parser
-----------------------

Since ``cabal-install-3.18`` a project is read with a new parser, falling back
to the old one if the new one fails. The old parser is to be removed in a
future release. The :ref:`--project-file-parser
<cmdoption-project-file-parser>` option picks between them.
