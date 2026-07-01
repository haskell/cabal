.. _how-to-control-versions:

How to control versions
=======================

There are various ways and places to limit what versions the solver can pick for
dependencies. Ways to control versions are:

:ref:`Version ranges <version-ranges>`
    Within a package description, version ranges for dependencies can be tight,
    loose, or missing altogether.

:ref:`Version constraints <version-constraints>`
    Within a project, version constraints for dependencies limit the versions
    that the solver can pick. These can be :ref:`mandated <mandated-versions>`.

:ref:`Curated version sets <curated-versions>`
    A project can import curated sets of packages and versions that are known to
    work together.

:ref:`Capped repository versions <capped-versions>`
    Adding ``index-state`` to a project limits versions coming from Hackage to
    include only those that were available at the cutoff time.

:ref:`Frozen versions <frozen-versions>`
    Pins the versions picked by the solver for all dependencies. This is a way
    to preserve a set of versions found by the solver, a solver-curated set, if
    you will.

:ref:`Version exceptions <version-exceptions>`
    Allow newer or older dependencies.

Places to control versions are the files and command line options related to
versioning but how to use these also depend on what we're building. If we're
building a product for its own sake then we're free to manage versions wherever
and however we please. If we're publishing a package to Hackage then that
platform places restrictions on how loose we can be when specifying
dependencies. Before publishing to Hackage, ``cabal check`` will warn about
loose versioning that Hackage will reject.

:ref:`Hackage versions <hackage-versions>`
    Not all dependency version ranges are accepted by Hackage. After publishing,
    a package on Hackage can be revised to change version ranges of its
    dependencies.

:ref:`Project versions <project-versions>`
    Almost any way and any place imaginable for setting versions is available
    when configuring a project.

.. _version-ranges:

Version ranges
--------------

A dependency can have a version range specified in the package description.
Published packages are expected to attach version ranges to dependencies that
conform to a set of rules that ``cabal check`` command will check for and report
on:

- that lower bounds are inclusive
- that upper bounds are exclusive
- that upper bounds don't have trailing zeros
- that upper bounds are not missing
- that upper bounds are not missing for ``base``

.. Note::

    Missing an upper bound on ``base`` is more serious than it is for other
    packages:

    - when upper bounds are missing for ``base``:

        .. code-block:: text

            $ cabal check
            The following errors will cause portability problems on other environments:
            Error: [missing-bounds-important] The dependency 'build-depends: base' does
            not specify an upper bound on the version number. Each major release of the
            'base' package changes the API in various ways and most packages will need
            some changes to compile with it. The recommended practice is to specify an
            upper bound on the version of the 'base' package. This ensures your package
            will continue to build when a new major version of the 'base' package is
            released. If you are not sure what upper bound to use then use the next major
            version. For example if you have tested your package with 'base' version 4.5
            and 4.6 then use 'build-depends: base >= 4.5 && < 4.7'.
            Error: Hackage would reject this package.

    - when upper bounds are missing on ``array``:

        .. code-block:: text

            $ cabal check
            These warnings may cause trouble when distributing the package:
            Warning: [missing-upper-bounds] On library, these packages miss upper bounds:
            - array
            Please add them. There is more information at https://pvp.haskell.org/

For large projects with many packages, it would be a lot of work to keep all
package dependency version ranges up to date if each package was independently
maintained, especially using tight version ranges for dependencies in the
packages. For a dependency, we can avoid setting its version range in any
package, or we can set the same version range in each package of a project.

**Avoiding**
    Package-level version ranges can be avoided;

    - by using a curated set of packages and versions, or
    - by specifying version constraints only in the ``cabal.project``.

    Curated sets of version constrained packages, such as the ones supplied by
    Stackage, are project-level configuration, ready-made to be imported by a
    project.

**Setting**
    Package-level version ranges can be specified in one place and then copied
    to packages;

    - by using a package generator like `dhall-hpack-cabal <https://github.com/cabalism/hpack-dhall/blob/3d464cddea0aa0a7f268c45556e0daafa8ac06ff/hpack-dhall.cabal#L148>`_ with imports,
    - by using a package formatter with fragment pragmas, or
    - by using a bash script to replace or update version ranges in all package descriptions.

If a project has many packages and these are all in-house and not published,
then it will be easier to use the same version range for a dependency
everywhere.  When only some packages of a project are published, and moreover
when their publication times are staggered, it may not be possible to use the
same version range for a dependency in all packages of a project. In this latter
case we'll need to adjust the avoidance and shared setting techniques.  A
project-level version range for a dependency will still work as long as it lies
within (as long as it narrows) each package-level version range for the same
dependency.  For package-level generators, formatters or scripts, you may need
to use more than one set of imports, fragment pragmas or scripts.

.. _version-constraints:

Version constraints
-------------------

Version constraints can be applied to the project or command line.

.. Note::

    While they do not directly restrain dependency versions, flag constraints
    can alter the set of dependencies of a package. This can happen when a
    dependency is behind a flag. For example, the `directory
    <https://hackage.haskell.org/package/directory-1.3.10.1/directory.cabal>`_
    package description has a flag that brings in the `os-string
    <https://hackage.haskell.org/package/os-string>`_ as a dependency and
    changes the version range for `filepath
    <https://hackage.haskell.org/package/filepath>`_:

    .. code-block:: cabal

        name: directory

        flag os-string
          description: Use the new os-string package
          default: False
          manual: False

        library
          if flag(os-string)
            build-depends: filepath >= 1.5.0.0, os-string >= 2.0.0
          else
            build-depends: filepath >= 1.4.100.0 && < 1.5.0.0

.. Warning::

    Version constraints accumulate!

    These are often set in a ``.cabal`` file as a version range within the
    ``build-depends`` stanza.

    There are other ways to supply version constraints on a dependency; in a
    project file or one of its imports and on the command line. Constraints for
    the same dependency can be given multiple times in the project or on the
    command line.

    Adding a constraint via any of these methods does not remove or replace any
    of the constraints on the same dependency set elsewhere; no method or order
    of supply has priority and constraints given one way cannot and do not
    override other constraints.

    Before dependency solving begins, all version constraints for each
    dependency given by any method are combined with the ``&&`` operator into
    one effective version range for that dependency.

.. _mandated-versions:

Mandated version constraints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Version constraints can be mandated with
:cfg-field:`reject-unconstrained-dependencies`.  When this flag is set to
``all``, each dependency must have some version constraint, even if this is just
``>0``. This flag can be made stricter and set to ``eq``, requiring each
dependency to have an equality version constraint.

.. _curated-versions:

Curated version sets
--------------------

Stackage provides curated sets of packages and versions that are known to work
together and are updated regularly.  The latest resolver is the nightly and this
typically lags a bit behind the latest available GHC version. The long term
support (LTS) resolvers will each follow a specific GHC version and are updated
less frequently.

- https://www.stackage.org/lts-24.34/cabal.config
- https://www.stackage.org/nightly-2026-03-25/cabal.config

These ``*.config`` files set the compiler version and warn about cabal not
yet having revisions.

.. code-block::

    $ curl -s https://www.stackage.org/lts-24.34/cabal.config | grep "with-compiler"
    with-compiler: ghc-9.10.3

If importing directly causes a version conflict, we recommend downloading the
package set locally, committing it to source control, and then commenting out
conflicting package constraint lines. Each constraint is on its own line and can
be prefixed with ``--`` to comment it out.

.. _capped-versions:

Capped repository versions
--------------------------

.. _frozen-versions:

Frozen versions
---------------

The :ref:`cabal freeze <cabal-freeze>` command generates **freeze file** that
describes the exact dependency tree as it was resolved at that moment by Cabal's
dependency solver.  It captures exact versions and flags of every dependency,
including dependencies of dependencies, exhaustively and recursively.

In practice, freezing adds a version equality constraint for each package in the
set of project dependencies, explicit and transitive.  The ``cabal freeze``
command saves these to a file named the same as the whole of the project file
name but with a extra ``.freeze`` extension, so the freeze file for
``cabal.project`` is ``cabal.project.freeze``. Effectively a ``.freeze`` file is
an implicit project import, same as the ``.local`` file for projects.

.. Note::

    The order of imports of ``.local`` and ``.freeze`` files is important.  The
    ``.local`` file is imported last, after the ``.freeze`` file, giving the
    user a final say in the setting of any fields that have override semantics.

    The ``.local`` and ``.freeze`` extensions are specialized and quirky. In
    general, it is better to use a ``.project`` extension for any project and a
    ``.config`` extension for any project configuration that is imported. It is
    of course possible to have multiple ``.project`` files and for these to have
    import relationships.

.. Warning::

    A project or package that may build with a variety of compiler versions will
    be locked to a particular compiler version by freezing.  For instance, if
    ``cabal init`` is used to create an executable package,
    ``unconstrained-base-exe.cabal``, after removing version constraints on base
    it can be frozen with various compiler versions that will each bring in
    their set of boot libraries as dependencies.

    .. code-block:: cabal

        executable unconstrained-base-exe
        build-depends: base >0

    .. code-block:: diff

        $ cabal freeze --with-compiler=ghc-8.10.7
        $ mv cabal.project.freeze ghc-8.10.7.freeze

        $ cabal freeze --with-compiler=ghc-9.10.1
        $ mv cabal.project.freeze ghc-9.10.1.freeze

        $ cabal freeze --with-compiler=ghc-9.12.1
        $ mv cabal.project.freeze ghc-9.12.1.freeze

        $ diff ghc-8.10.7.freeze ghc-9.12.1.freeze --unified
        -constraints: any.base ==4.14.3.0,
        -             any.ghc-prim ==0.6.1,
        -             any.integer-gmp ==1.0.3.0,
        -             any.rts ==1.0.1
        +constraints: any.base ==4.21.0.0,
        +             any.ghc-bignum ==1.3,
        +             any.ghc-internal ==9.1201.0,
        +             any.ghc-prim ==0.13.0,
        +             any.rts ==1.0.2

        $ diff ghc-9.10.1.freeze ghc-9.12.1.freeze --unified
        -constraints: any.base ==4.20.0.0,
        +constraints: any.base ==4.21.0.0,
                      any.ghc-bignum ==1.3,
        -             any.ghc-internal ==9.1001.0,
        -             any.ghc-prim ==0.11.0,
        +             any.ghc-internal ==9.1201.0,
        +             any.ghc-prim ==0.13.0,
                      any.rts ==1.0.2

Do you need to freeze?
^^^^^^^^^^^^^^^^^^^^^^

Why would you want to freeze? Don't we want to get minor updates of our
dependencies, or at least patches, as soon as we can?  Well, although they
shouldn't, it is possible that any kind of update introduces new bugs,
performance issues, or some other kind of unexpected behaviour.  This is where
``cabal.project.freeze`` comes in, as it ensures that dependencies don't
unexpectedly change.  You can still update your dependencies, but you have to do
it on purpose, by modifying or by deleting and regenerating
``cabal.project.freeze`` file, and in the meantime you are guaranteed no
surprises will happen.

This consistency can be valuable as it ensures that all teammates, deployments,
and continuous integration are installing the exactly same dependencies.  So if
you are running and testing the code on your local machine, you are guaranteed
that your teammate and your continuous integration will be running the exact
same code, and that at the end that exact same code will get deployed.

A ``.freeze`` file can be good to have when developing for yourself or within a
private team.  If anyone using it can somehow have different inputs to
the solver then the ``.freeze`` file can be troublesome.  It can prevent the
solver from finding a different version of a dependency that would satisfy a
different architecture or a different compiler version and boot libraries.

.. Warning::

    If publishing a package to Hackage, no matter what kind of component it
    contains, don't include a ``.freeze`` file, don't add it to any field of the
    package description that would have ``cabal sdist`` include it in the
    ``.tar.gz``. In general, don't include anything in the package description
    that relates to the project environment, like ``cabal.project`` or
    ``cabal.project.local``.

Freezing workflows
^^^^^^^^^^^^^^^^^^

.. Warning::
    For each of these workflows, you may have to first delete the
    ``index-state`` line from ``cabal.project`` (and from
    ``cabal.project.freeze`` if it exists) and then run ``cabal update`` to
    ensure that cabal will have newer versions to re-resolve the dependencies
    with. Alternatively, you can run ``cabal update
    --ignore-project``.

For the versions of all dependencies at once:

- To check solver-chosen versions - **cold snap**

    With this workflow we freeze, inspect then discard ``cabal.project.freeze``,
    always leaving the solver free to choose other versions.  This is great way
    to see what versions of dependencies are currently being used.

- To update versions of all dependencies - **thaw and freeze**

    If you changed the version ranges of any of the dependencies in any of your
    project's package descriptions, in any ``.cabal`` file, then delete the
    ``cabal.project.freeze`` file if it already exists and run ``cabal freeze``
    to generate fresh version of ``cabal.project.freeze``.  The steps of this
    workflow are delete and freeze. The solver is left free to choose versions of
    all dependencies but once it has decided, those versions are immediately
    pinned.

    This "thaw and freeze" workflow is the simplest way to work with a
    ``cabal.project.freeze`` file committed to source control.

For the version of a single dependency:

.. _freeze-harder:

- To pin the version of a new dependency - **freeze harder**

    The steps of this workflow are add the new dependency and freeze. The solver
    is free to choose a version for the new dependency while retaining the
    already pinned versions for the rest of the dependencies.

- To update the version of one dependency - **partial thaw and freeze**

    If you want to a pick up a different version of a single dependency, you can
    delete its constraint from ``cabal.project.freeze`` and then run ``cabal
    freeze`` again.  The steps of this workflow are delete one line and freeze.
    It gives the solver the chance to choose another version for the unpinned
    dependency.

Ensuring everything is frozen
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. Note::

    If the ``.freeze`` file already has version equality constraints for every
    package that is a dependency of the project, then the solver will not be
    able to find a different version for any of them, the ``.freeze`` file
    cannot change and, at that point when every dependency is frozen, ``cabal
    freeze`` becomes an idempotent operation.

.. Warning::
    Setting :cfg-field:`reject-unconstrained-dependencies` to ``all`` will
    ensure that dependencies have version constraints but does not require these
    be equality constraints.

When adding a new dependency to a project that utilizes a freeze file
[#add-new-dep]_, regenerating the freeze file using one of the methods described
above ensures that the freeze file contains a constraint for the new dependency.

Not :ref:`freezing harder<freeze-harder>` leaves the newly added dependency
susceptible to getting updated unexpectedly when the solver can find a different
version for it.  Running ``cabal freeze`` will show this vulnerability to a
human or an automated check that notices a new version equality constraint in
the ``.freeze`` file, a constraint for a package that wasn't in the ``.freeze``
file before.

To automate this check, make it a part of continuous integration or make a
pre-commit hook for it. A simple check for this might be to compare the md5sum
of the ``.freeze`` file before and after running ``cabal freeze``.  If the
checksums are the same, then the ``.freeze`` file didn't change, and all
versions are frozen.

.. code-block:: bash

    [[ -f cabal.project.freeze ]] || exit 1
    OLD_FREEZE_SUM=$(md5sum cabal.project.freeze)
    cabal freeze || exit 1
    NEW_FREEZE_SUM=$(md5sum cabal.project.freeze)
    exit [[ "$NEW_FREEZE_SUM" == "$OLD_FREEZE_SUM" ]]

.. [#add-new-dep] By adding a dependency to one of the packages in the project.

.. _version-exceptions:

Version Exceptions
^^^^^^^^^^^^^^^^^^

Version constraints can be lifted with ``allow-newer`` and ``allow-older``.

Places for Versioning
---------------------

For end-user executables, it is recommended that you distribute the
``cabal.project.freeze`` file in your source repository so that all users see a
consistent set of dependencies. For libraries, this is not recommended: users
often need to build against different versions of libraries than what you
developed against.

.. _hackage-versions:

Hackage versions
^^^^^^^^^^^^^^^^

The unit of distribution to Hackage is the package but each package component
has their own dependencies. The Hackage website shows dependencies in summary
form, like those for `hpack-dhall-0.5.7
<https://hackage.haskell.org/package/hpack-dhall-0.5.7>`_, a package with
multiple executables.  Clicking on the "[details]" link at the end of the
summary of dependencies list will show the dependencies for each component:

.. raw:: html

    <div class="highlight" style="padding: 4px">
    <b>Dependencies</b><br/>
    <div style="padding-left: 0.8em">
    <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/aeson">aeson</a></span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/aeson-pretty">aeson-pretty</a></span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/base">base</a> (&gt;=4.13 &amp;&amp; &lt;5)</span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/bytestring">bytestring</a></span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/dhall">dhall</a> (&gt;=1.41.1)</span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/dhall-json">dhall-json</a> (&gt;=1.7.10)</span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/filepath">filepath</a></span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/hpack">hpack</a> (&gt;=0.35 &amp;&amp; &lt;0.36)</span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/megaparsec">megaparsec</a> (&gt;=9.2)</span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/microlens">microlens</a></span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/optparse-applicative">optparse-applicative</a></span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/prettyprinter">prettyprinter</a></span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/text">text</a></span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/transformers">transformers</a></span>, <span style="white-space: nowrap"><a href="https://hackage.haskell.org/package/yaml">yaml</a></span><span style="font-size: small"> [<a href="https://hackage.haskell.org/package/hpack-dhall-0.5.7/dependencies">details</a>]</span>
    <div>
    </div>

.. _project-versions:

Project versions
^^^^^^^^^^^^^^^^

Projects are shared by *source code* repository.

Since ``cabal`` reads ``cabal.project.freeze`` when present, and takes into
consideration the version constraints in it, this means that by producing
``cabal.project.freeze`` you are guaranteed that every future ``cabal`` call
will use the exact same set of dependencies, regardless of any updates (even
patches) that might get published for these dependencies in the meantime.
Therefore, we have effectively "frozen" the dependencies in place.

.. Tip::

    To freeze but also build your project with different versions of GHC; fix
    the compiler version using :cfg-field:`with-compiler` field and freeze each
    project separately using the :ref:`project-file<cmdoption-project-file>`
    option.

    .. code-block:: bash

        $ cabal freeze --project-file=cabal.ghc-9.10.3.project
        $ cabal freeze --project-file=cabal.ghc-9.12.4.project

    Common configuration can be shared between projects by using project
    imports.

Freezing limitations
^^^^^^^^^^^^^^^^^^^^

A freeze is specific for a single Cabal dependency solver run within the current
environment.  There's a lot to consider before deciding to add a
``cabal.project.freeze`` file to source control due to limitations of ``cabal
freeze``.

Environment Limitation
    The solver can only solve for the environment that it is running within so
    the generated ``cabal.project.freeze`` file has no ``os(name)``,
    ``arch(name)`` or ``impl(compiler)`` conditionals. This is why a ``.freeze``
    file is not a true lock file.

    A good many packages have platform-specific dependencies and will vary their
    dependencies based on the operating system.

Index State Limitation
    The index state of the local package index will also change the list of
    possible dependencies available to the solver.

Revision Limitation
    A version constraint cannot include a package revision, neither by revision
    number nor hash. These constraints do not have sufficient granularity to
    guarantee picking a specific revision. To work around this limitation a
    project file will often fix the index state and so hide future revisions
    that a freeze file's equality constraints cannot specify.

Index State Limitation
    Aside from revisions, the index state of the local package index will also
    change the list of possible dependency versions available to the solver.

Despite all of those limitations, for a software product that is always
developed on the same platform and environment, committing the freeze file to
source control may make sense. Even then, a production build would likely need
to avoid such a freeze file.
