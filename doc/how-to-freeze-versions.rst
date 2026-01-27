How to control versions
=======================

There are various ways and places to limit what versions the solver can pick for
dependencies.

Version ranges
    Within a package description, version ranges for dependencies can be; tight
    or loose or missing altogether.

Version constraints
    Within a project, version constraints for dependencies limit the versions
    that the solver can pick.

Mandatory version constraints
    With :cfg-field:`reject-unconstrained-dependencies` set to ``all``
    dependencies must have version constraints.

Curated version sets
    A project can import curated sets of packages and versions that are known to
    work together, such as those Stackage provides for Cabal's use.

Capped repository versions
    Adding ``index-state`` to a project limits versions coming from Hackage to
    include only those that were available at the cutoff time.

Frozen versions
    Pins the versions picked by the solver for all dependencies. This is a way
    to preserve a set of versions found by the solver, a solver-curated set, if
    you will.

Version exceptions
    Allow newer or older dependencies.

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

For large projects with many packages, it would be a lot of work to keep all
package dependency version ranges up to date. Ways of overcoming this are:

- use a bash script to replace or update version ranges in all package descriptions
- use a package generator like ``hpack-dhall-cabal`` to import sets of dependencies with version ranges
- use a ``cabal.project`` file to specify version constraints for some or all dependencies
- use a curated set of packages and versions and import these into the project

Version constraints
-------------------

Version constraints can be applied to the project or command line.

.. Note::

    While they do not directly restrain dependency versions, flag constraints
    can alter the set of dependencies of a package.

.. Warning::

    Version constraints are additive. If I add a constraint it doesn't remove or
    replace prior constraints on versions. Constraints don't have override
    semantics.

Curated version sets
--------------------

Stackage provides curated sets of packages and versions that are known to work
together and are updated regularly.  The latest resolver is the nightly and this
typically lags a bit behind the latest available GHC version. The LTS resolvers
will each follow a specific GHC version and are updated less frequently.

Capped repository versions
--------------------------

.. _freeze-versions:

Frozen versions
---------------

Pinning adds a version equality constraint for each package in the set of
project dependencies, explicit and transitive.  The ``cabal freeze`` command
saves these to a file named the same as the whole of the project file name but
with a extra ``.freeze`` extension, so the freeze file for ``cabal.project`` is
``cabal.project.freeze``. Effectively a ``.freeze`` file is an implicit project
import, same as the ``.local`` file for projects.

.. Warning::

    The order of imports of ``.local`` and ``.freeze`` files is important.  The
    ``.local`` file is imported last, after the ``.freeze`` file, giving the
    user a final say in the setting of any fields that have override semantics.

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

- To check solver-chosen versions - cold snap

    With this workflow we freeze, inspect then discard ``cabal.project.freeze``,
    always leaving the solver free to choose other versions.  This is great way
    to see what versions of dependencies are currently being used.

- To update versions of all dependencies - thaw and freeze

    If you changed the version ranges of any of the dependencies in any of your
    project's package descriptions, in any ``.cabal`` file, then delete the
    ``cabal.project.freeze`` file if it already exists and run ``cabal freeze``
    to generate fresh version of ``cabal.project.freeze``.  The steps of this
    workflow are delete and freeze. The solver is let free to choose versions of
    all dependencies but once it has decided, those versions are immediately
    pinned.

    This "thaw and freeze" workflow is the simplest way to work with a
    ``cabal.project.freeze`` file committed to source control.

For the version of a single dependency:

- To pin the version of a new dependency

    The steps of this workflow are add the new dependency and freeze. The solver
    is free to choose a version for the new dependency while retaining the
    already pinned versions for the rest of the dependencies.

- To update the version of one dependency

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

Adding a dependency to one of the packages in a project without freezing harder
leaves the newly added dependency susceptible to getting updated unexpectedly
when the solver can find a different version for it.  Running ``cabal freeze``
will show this vulnerability to a human or an automated check that notices a new
version equality constraint in the ``.freeze`` file, a constraint for a package
that wasn't in the ``.freeze`` file before.

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
