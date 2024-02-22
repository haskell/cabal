Source Repositories
===================

Source repositories are a way to specify where to find the source code for a
package, both for :ref:`package authors<pkg-authors>` and maintainers and for
:ref:`package consumers<pkg-consumers>`.

A relatively structured set of fields, that vary depending on the VCS kind,
enables cabal commands and other tools to interpret and make effective use of
this information.

- ``source-repository`` says where to find the source for a package description
- ``source-repository-package`` locates source for project dependencies

.. _pkg-authors:

As a Package Author
-------------------

If you are authoring or maintaining a package, you will be dealing with
``source-repository``, a package description stanza that typically specifies
where upstream development of the package is happening.  A package published to
hackage will be displayed with a link to the source repository if a
``source-repository`` stanza is present in the package description.

.. _pkg-consumers:

As a Package Consumer
---------------------

When working on a project you may need one or more ``source-repository-package``
stanzas if taking dependencies via a version control system (VCS) such as git.
This is an alternative way of taking dependencies, instead of getting them from
a package repository such as hackage. It may be the only practical way of taking
dependencies on packages not published to hackage. For a package on hackage, a
``source-repository`` stanza is not needed but if used, the downloaded source
will become the only version of the package available to the project.

A dependency taken this way, effectively adds a local package to the project
much like listing the package's source directory in the packages field, except
that the source code is downloaded by cabal using the version control system.
Cabal commands that need the source code will do this as needed, command like
``cabal build`` or ``cabal freeze``. This is also the easiest way to work with a
fork, much easier than using a git submodule.

.. list-table::
    :header-rows: 1
    :widths: 30 30 40

    * - Field Name
      - source-repository (head|this)
      - source-repository-package
    * - type
      - 🗹
      - 🗹
    * - location
      - 🗹
      - 🗹
    * - branch
      - 🗹
      - 🗹
    * - tag
      - 🗹
      - 🗹
    * - subdir
      - 🗹 (0 or 1)
      - 🗹 (0 or 1 for each dependency)
    * - module (CVS only)
      - 🗹
      - ☐
    * - post-checkout-command
      - ☐
      - 🗹

.. _source-repository-fields:

Source Repository Fields
------------------------
..
  data SourceRepo = SourceRepo
    { repoKind :: RepoKind
    , repoType :: Maybe RepoType
    , repoLocation :: Maybe String
    , repoModule :: Maybe String
    , repoBranch :: Maybe String
    , repoTag :: Maybe String
    , repoSubdir :: Maybe FilePath
    }

.. pkg-field:: type: VCS kind

    This field is required.

.. pkg-field:: location: VCS location

    This field is required.

.. pkg-field:: module: token

    CVS requires a named module, as each CVS server can host multiple
    named repositories.

    This field is required for the CVS repository type and should not be
    used otherwise.

.. pkg-field:: branch: VCS branch

    This field is optional.

.. pkg-field:: tag: VCS tag

    This field is required for the ``this`` repository kind.

    This might be used to indicate what sources to get if someone needs to fix a
    bug in an older branch that is no longer an active head branch.

.. pkg-field:: subdir: VCS subdirectory

    This field is optional but, if given, specifies a single subdirectory.

.. _source-repository-package-fields:

Source Repository Package Fields
--------------------------------

..
  data SourceRepositoryPackage f = SourceRepositoryPackage
    { srpType :: !RepoType
    , srpLocation :: !String
    , srpTag :: !(Maybe String)
    , srpBranch :: !(Maybe String)
    , srpSubdir :: !(f FilePath)
    , srpCommand :: ![String]
    }

.. cfg-field:: type: VCS kind

    This field is required.

.. cfg-field:: location: VCS location

    This field is required.

.. cfg-field:: branch: VCS branch

    This field is optional.

.. cfg-field:: tag: VCS tag

    This field is optional.

.. cfg-field:: subdir: VCS subdirectory list

    Look in one or more subdirectories of the repository for cabal files, rather
    than the root. This field is optional.

.. cfg-field:: post-checkout-command: command

    Run command in the checked out repository, prior sdisting.

VCS Field Types
---------------

Version Control Systems (VCS) fields types common to both ``source-repository``
and ``source-repository-package`` stanzas.

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

-  for darcs: ``http://code.haskell.org/foo/``
-  for git: ``git://github.com/foo/bar.git``
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

Some projects put the sources for multiple packages under a single source
repository. This field lets you specify the relative path from the root of the
repository to the top directory for the package, i.e. the directory containing
the package's ``.cabal`` file.
