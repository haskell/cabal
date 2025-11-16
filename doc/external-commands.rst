External Commands
=================

``cabal-install`` provides a system for external commands, akin to the ones used by tools like ``git`` or ``cargo``.

If you execute ``cabal <cmd>``, ``cabal-install`` will search the path for an executable named ``cabal-<cmd>`` and execute it. An error will be thrown in case the custom command is not found. The exit code of cabal when calling an external command is the same as the exit code
of the command.

The name of the command is *not* passed as the first argument as is done in ``cargo``, instead you will have to figure out the name via `argv[0]` as
is the case in e.g. `git`.

The ``$CABAL_EXTERNAL_CABAL_PATH`` environment variable is set to the path of the ``cabal-install`` executable
which invoked the subcommand.

It is strongly recommended that you implement your custom commands by calling the
CLI via the ``$CABAL_EXTERNAL_CABAL_PATH`` variable rather than linking against the ``Cabal`` library.
There is no guarantee that the subcommand will link against the same version of the
``Cabal`` library as ``cabal-install`` so it would lead to unexpected results and
incompatibilities.

Historically, the `cabal-install` binary would pass the name of the executable which it is trying to invoke via the external command feature as
the first argument to the executable itself. The main difference was that ``$CABAL_EXTERNAL_CABAL_PATH`` was called ``$CABAL``, which means that
you can stay compatible with both versions, depending on which variable is set.

Mind that if you were implementing external commands previously, you will not need to skip the first argument (the executable name) anymore.

``cabal-install`` can also display the help message of the external command.
When ``cabal help <cmd>`` is invoked, then ``cabal-<cmd> <cmd> --help`` will be called so
your external command can display a help message.

For ideas or existing external commands, visit `this Discourse thread <https://discourse.haskell.org/t/an-external-command-system-for-cabal-what-would-you-do-with-it/7114>`_.
