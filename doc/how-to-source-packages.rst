How to deal with package *source code*
======================================

We can leave a **"source"** marker in the package description that shows where
to find the *source code*. This marker is the ``source-repository`` field and is
described in the :ref:`package author<pkg-authors>` section.

We can direct Cabal to get (or **"source"**) the package *source code* for
dependencies from a **source code repository** by adding a
``source-repository-package`` stanza to a project as explained in the
:ref:`package consumer<pkg-consumers>` section.

Getting package *source code* without Cabal
-------------------------------------------

There are two ways to grab the *source code* for a package manually; from a
**source code repository** such as GitHub [#]_ or from a **package repository**
such as Hackage.  Cabal automates these two ways of obtaining the *source code*.
This is described in the :ref:`package consumer<pkg-consumers>` section.

.. Note::

   The *source code* for a package is not always available from a source code
   repository. This will be the case for published packages where the
   ``source-repository`` source marker was not added to the package's ``.cabal``
   description before being published, when the source code repository no longer
   exists or when it was never made available.  In those cases the only
   available *source code* will be source archives on Hackage.

   Each package on Hackage is available for download as a ``.tar.gz`` source
   archive. Look for the "Downloads" section. One of these will be the "Package
   description". That's the ``.cabal`` file. The other will be the "Cabal source
   package". That's the ``.tar.gz`` archive containing all of the source for the
   package.  For example, here's the direct link to download
   `vector-0.13.1.0.tar.gz <vector-download_>`_.

   Conversely, package *source code* may be only available from a source code
   repository.  This is the case for all unpublished packages and for all
   unpublished commits.

.. Warning::

   Be careful with the term "revision". On Hackage this refers to an edited
   version of the package description. With version control systems, a revision
   is a commit. Thankfully, when telling Cabal about a "commit" or "revision",
   the field name is something completely different, a ``tag``.

Fields pointing to *source code* repositories
---------------------------------------------

Source [code] repositories are a way to specify where to find the source code
for a package, both for :ref:`package authors<pkg-authors>` and maintainers and
for :ref:`package consumers<pkg-consumers>`.

A relatively structured set of version control system (VCS) fields, that vary
depending on the :ref:`VCS kind<vcs-kind>`, enables Cabal commands and other
tools to interpret and make effective use of this information.

``source-repository``
   Says where to find the source for a package and is a metadata field of a
   **package description**. This is the source marker.

``source-repository-package``
   Locates the source for one or more **project dependencies** that live in the
   same **source code repository** but in separate folders. The root of the
   repository is assumed when the ``subdir`` field is omitted.

.. Warning::

   The ``source-repository`` field, the one without a "-package" suffix, belongs
   to a package!  This actually makes perfect sense as it's within the package
   description looking outwards.

   .. code-block:: text

       (package description)      source-repository         (Git repository)
      solver-benchmarks.cabal ──────────────────────────> github/haskell/cabal

      $ cat solver-benchmarks.cabal
      ...
      source-repository head
         type:     git
         location: https://github.com/haskell/cabal/
         subdir:   solver-benchmarks
      ...
      library
         build-depends: base, ..., vector

   The ``source-repository-package`` field, the one with a "-package" suffix,
   doesn't belong to a package! It's for a project dependency. This name is a
   bit misleading being singular because it can describe multiple dependency
   packages at the same location, each offset by a directory containing a
   package description, a ``.cabal`` file.

   This project stanza is like a redirect or override for dependencies of all
   the packages in the project saying "use this source code repository" for this
   package dependency, not Hackage.

   .. code-block:: text

        (project)       source-repository-package         (Git repository)
      cabal.project ──────────────────────────────────> github/haskell/vector

      $ tree
      ...
      └── solver-benchmarks
         └── solver-benchmarks.cabal

      $ cat cabal.project
      packages: solver-benchmarks
      ...
      source-repository-package
         type:     git
         location: https://github.com/haskell/vector.git
         subdir:   vector

.. Warning::

   Cabal project files (``cabal.project``) don't declare dependencies!

   The union of the dependencies of all project packages [#]_ declares the set
   of project dependencies.  While the project can tighten version constraint
   ranges with ``constraints`` or loosen them with ``allow-newer`` or
   ``allow-older``, it cannot add package names to or remove package names from
   the set of dependencies.

.. _pkg-consumers:

*Source code* when taking a package dependency
----------------------------------------------

Cabal commands that work with dependencies actually need to have the source code
of each dependency and will download it as needed. Example of commands like this
are ``cabal build`` or ``cabal freeze``.

Dependencies of a project are sourced, by default, from Hackage if they've been
uploaded and published to this package repository. Cabal will download the
*source code* ``.tar.gz`` archive for each dependency from Hackage. While we can
depend on an exact version of a package, more often we'll accept a range of
versions and the dependency solver picks the exact version from the range to
download.

We can also :ref:`take a dependency from a source code
repository<pkg-consume-source>`. These are accessed via a version control system
(VCS) such as Git and set up in the project with ``source-package-repository``
stanzas. With these, we can take dependencies on packages not published to
Hackage, on revisions not yet published or on forks.  This is the easiest way to
work with a fork, much easier than using a Git submodule.  A dependency taken
this way, effectively adds a local package to the project much like listing the
package's source directory in the packages field, except that the source code is
downloaded by Cabal using the version control system.

For example, with a project that depends on the ``vector`` package from source
we can see that the source code repository has been cloned by ``cabal build``:

.. code-block:: text

   $ cat cabal.project
   ...
   source-repository-package
      type:     git
      location: https://github.com/haskell/vector.git
      subdir:   vector

   $ cabal build all --dry-run
   Cloning into '/.../dist-newstyle/src/vector-51ba9353b7a850a'...
   remote: Enumerating objects: 9542, done.
   remote: Counting objects: 100% (1800/1800), done.
   remote: Compressing objects: 100% (652/652), done.
   remote: Total 9542 (delta 1010), reused 1599 (delta 917), pack-reused 7742
   Receiving objects: 100% (9542/9542), 2.52 MiB | 16.56 MiB/s, done.
   Resolving deltas: 100% (5273/5273), done.
   HEAD is now at 6b8bbc3 ...
   Resolving dependencies...

Cabal makes an archive from the cloned source code repository and uses this as
the source for the dependency.

.. code-block:: text

      $ find -name '*.tar.gz'
      ...
      ./dist-newstyle/src/vector-51ba9353b7a850a-vector-0.13.1.0.tar.gz


.. Warning::

   The hash in the directory name of the clone is not the commit hash of the
   cloned repository.

   .. code-block:: text

         $ cd dist-newstyle/src/vector-51ba9353b7a850a

         $ git rev-parse HEAD
         6b8bbc3a75b40451d8d225e30c576dfe89121c49

Setting up a *source code* dependency
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is easy to copy the fields of ``source-repository`` to set up a
``source-repository-package`` as they share many of the same :ref:`VCS
fields<vcs-fields>`. Looking at vector's `package description
<vector-pkg-desc_>`_ on Hackage we see the ``source-repository`` stanza:

.. code-block:: cabal

   source-repository head
      type:     git
      location: https://github.com/haskell/vector.git
      subdir:   vector

To turn that into a dependency we'd make the following changes to prepare a
``source-repository-package`` stanza for our project, using the Git commit hash
for the ``tag`` field:

.. code-block:: diff

   - source-repository head
   + source-repository-package
         type:     git
         location: https://github.com/haskell/vector.git
   +     tag:      79bdd2edcfaf6b07f7fabc43a7d9c5a2ff93d3ca
         subdir:   vector

.. code-block:: text

   $ git ls-remote --tags
   From git@github.com:haskell/vector.git
   ...
   79bdd2edcfaf6b07f7fabc43a7d9c5a2ff93d3ca     refs/tags/vector-0.13.1.0

.. Warning::

   Only a commit hash pins to an exact version of the *source code* for Git
   respositories.

   - If the ``tag`` field is omitted then the latest commit on the Git default branch is used.
   - If the ``tag`` field is a Git branch name then the latest commit on that branch is used.
   - If the ``tag`` field is a Git tag then the current commit that tag points to is used.


*Source code* when dependency vendoring
---------------------------------------

*Vendoring* is where you add the source code of an external package to your
project, either as a package ``.tar.gz`` archive or as unpacked package source
code.

Vendoring a *source code* archive
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

After manually downloading a source code archive from Hackage (or elsewhere),
you can add it to your project as a local package.

   .. code-block:: shell

      $ VER=0.13.1.0 \
         curl -sSL https://hackage.haskell.org/package/vector-{$VER}/vector-{$VER}.tar.gz \
         --output vector-{$VER}.tar.gz \
      $ VER=0.13.1.0 echo "packages: vector-$VER.tar.gz" >> cabal.project
      $ cabal build vector --dry-run
      ...
      Resolving dependencies...
      Build profile: -w ghc-9.8.2 -O1
      In order, the following would be built (use -v for more details):
      - vector-0.13.1.0 (lib) (requires build)

Vendoring unpacked *source code*
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can use :ref:`cabal get<cabal-get>` and then add the unpack directory to the
``packages`` field in the project's ``cabal.project`` file.  This downloads the
package's ``.tar.gz`` source code archive and extracts it to a directory named
after the package name and version.

- For an exact version of the package, use the package name and version.

   .. code-block:: shell

      $ VER=0.12.3.1 cabal get vector-{$VER}
      Unpacking to vector-0.12.3.1/
      $ VER=0.12.3.1 echo "packages: vector-$VER" >> cabal.project

- For the latest version of the package, use the package name only after a ``cabal update``.

   .. code-block:: shell

      $ cabal update
      $ cabal get vector
      Unpacking to vector-0.13.1.0/
      $ echo "packages: vector-0.13.1.0" >> cabal.project

You can vendor unpacked source code obtained by other means, by means that
replace the ``cabal get`` unpacking step, in the same way.

Fork, don't vendor
^^^^^^^^^^^^^^^^^^
There's no need to vendor packages on Hackage if you expect Hackage to always be
available, as packages cannot be deleted from Hackage. Source code repositories,
on the other hand, can disappear.

Rather than vendoring, it might be easier to take a
``source-repository-package`` dependency on a fork of the upstream source code
repository, on a fork that you control, especially if you're going to be making
contributions to the upstream repository.

.. _pkg-authors:

*Source code* as a package author or maintainer
-----------------------------------------------

If you are authoring or maintaining a package and want to link to the
:ref:`package source<pkg-author-source>` you will be dealing with
``source-repository``, a package description stanza that typically specifies
where upstream development of the package is happening.  A package published to
Hackage will be displayed with a link to the source repository if a
``source-repository`` stanza is present in the package description.

This helps users find the source code for the package if they want to contribute
to it or if they want to try out the latest changes before they're published.
The simplest way to try out the latest changes is to add a
``source-repository-package`` dependency on the package so that it will be
picked up from the source code repository instead of from Hackage.

*Source code* as a package publisher
------------------------------------

If you are publishing a package to Hackage, you'll first need to create a
``.tar.gz`` archive of your package's source code before `uploading it to
Hackage <hackage-upload_>`_. This is done with the :ref:`cabal-sdist` command
that names archives after package name and version.

.. [#] Also known as a version control system (VCS) repository.

.. [#] When the list of all packages is taken from the ``packages`` field(s) within a ``cabal.project``.

.. _vector-download: https://hackage.haskell.org/package/vector-0.13.1.0/vector-0.13.1.0.tar.gz

.. _vector-pkg-desc: https://hackage.haskell.org/package/vector-0.13.1.0/vector.cabal

.. _hackage-upload: https://hackage.haskell.org/upload
