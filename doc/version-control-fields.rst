Version Control System Fields
=============================

.. _vcs-fields:

Most of the version control system (VCS) fields types are common to both
``source-repository`` and ``source-repository-package`` stanzas.

.. list-table::
    :header-rows: 1
    :widths: 30 30 40

    * - Field Name
      - source-repository (head|this)
      - source-repository-package
    * - type
      - [x]
      - [x]
    * - location
      - [x]
      - [x]
    * - branch
      - [x]
      - [x]
    * - tag
      - [x]
      - [x]
    * - subdir
      - [x] (0 or 1)
      - [x] (0 or 1 for each dependency)
    * - module (CVS only)
      - [x]
      - [_]
    * - post-checkout-command
      - [_]
      - [x]

.. _vcs-kind:

VCS kind
^^^^^^^^

Cabal supports specifying different information for various common source
control systems. This is the name of the source control system used for a
repository. The currently recognised types are:

-  ``darcs``
-  ``git``
-  ``svn``
-  ``cvs``
-  ``mercurial`` (or alias ``hg``)
-  ``bazaar`` (or alias ``bzr``)
-  ``arch``
-  ``monotone``
-  ``pijul``

The VCS kind will determine what other fields are appropriate to specify for a
particular version control system.

VCS location
^^^^^^^^^^^^

The location of the repository, usually a URL but the exact form of this field
depends on the repository type. For example:

-  for Darcs: ``http://code.haskell.org/foo/``
-  for Git: ``git://github.com/foo/bar.git``
-  for CVS: ``anoncvs@cvs.foo.org:/cvs``

VCS branch
^^^^^^^^^^

Many source control systems support the notion of a branch, as a distinct
concept from having repositories in separate locations. For example CVS, SVN and
git use branches while darcs uses different locations for different branches. If
you need to specify a branch to identify a your repository then specify it in
this field.

VCS tag
^^^^^^^

A tag identifies a particular state of a source repository.  The exact form of
the tag depends on the repository type.

VCS subdirectory
^^^^^^^^^^^^^^^^

A field of this type is always optional because it defaults to empty, which
corresponds to the root directory of the repository and is the same as
specifying ``.`` explicitly.

Some projects put the sources for multiple packages inside a single VCS
repository. This field lets you specify the relative path from the root of the
repository to the top directory for the package, i.e. the directory containing
the package's ``.cabal`` file.
