.. _freeze-versions:

How to freeze versions
======================

Freezing
    Pins the versions picked by the solver for all dependencies.
    
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

Do you need this?
^^^^^^^^^^^^^^^^^

Why would you want this? Don't we want to get minor updates of our dependencies,
or at least patches, as soon as we can?  Well, although they shouldn't, it is
possible that any kind of update introduces new bugs, performance issues, or
some other kind of unexpected behaviour.  This is where ``cabal.project.freeze``
comes in, as it ensures that dependencies don't unexpectedly change.  You can
still update your dependencies, but you have to do it on purpose, by modifying
or by deleting and regenerating ``cabal.project.freeze`` file, and in the
meantime you are guaranteed no surprises will happen.

This consistency can be valuable as it ensures that all teammates, deployments,
and continuous integration are installing the exactly same dependencies.  So if
you are running and testing the code on your local machine, you are guaranteed
that your teammate and your continuous integration will be running the exact same
code, and that at the end that exact same code will get deployed.

Usual use-case for using ``cabal freeze`` is when developing end-user code, for
example an executable that you will distribute.  On the other hand, if you are
developing a library, you will not want to distribute it together with the
``cabal.project.freeze`` file, as it would make it very hard for cabal to
resolve dependencies for users of the library.

Common workflows
^^^^^^^^^^^^^^^^

.. Warning::
    For each of these workflows, you may have to first delete the
    ``index-state`` line from ``cabal.project`` (and from
    ``cabal.project.freeze`` if it exists) and then run ``cabal update`` to
    ensure that cabal will have newer versions to re-resolve the dependencies
    with. Alternatively, you can run ``cabal update
    --ignore-project``.

Freeze
    If the ``cabal.project.freeze`` file doesn't exist, generating one is a
    great way to see what versions of dependencies are currently being used even
    if you choose to discard the ``.freeze`` file after inspecting it.

Thaw, Freeze
    If you changed the version ranges of any of the dependencies in any of your
    project's package descriptions, in any ``.cabal`` file, then delete the
    ``cabal.project.freeze`` file if it already exists and run ``cabal freeze``
    to generate fresh version of ``cabal.project.freeze``.

Freeze, Freeze (Freezing Harder)
    You might in some cases want to skip deletion of ``cabal.project.freeze``,
    but keep in mind that in that case ``cabal freeze`` will use existing
    ``cabal.project.freeze`` when resolving dependencies, therefore not updating
    any existing dependencies, only adding new ones.

Partial Thaw, Freeze
    If you want to a pick up a different version of a single dependency, you can
    delete its constraint from ``cabal.project.freeze`` and then run ``cabal
    freeze`` again.

.. Note::

    If not sure, pick the "thaw, freeze" workflow, as it is the safest, the
    simplest and the most common. Finally, you will always want to commit the
    changed ``cabal.project.freeze`` to version control.

Ensuring everything is frozen
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. Note::

    If the ``.freeze`` file already has version equality constraints for every
    package that is a dependency of the project, then the solver will not be
    able to find a different version for any of them, the ``.freeze`` file
    cannot change and, at that point when every dependency is frozen, ``cabal
    freeze`` becomes an idempotent operation.

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
