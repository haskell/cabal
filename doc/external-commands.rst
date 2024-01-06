External Commands
=================

``cabal-install`` provides a system for external commands, akin to the ones used by tools like ``git`` or ``cargo``.

If you execute ``cabal <cmd>``, ``cabal-install`` will search the path for an executable named ``cabal-<cmd>`` and execute it. The name of the command is passed as the first argument and
the remaining arguments are passed afterwards. An error will be thrown in case the custom command is not found.

The ``$CABAL`` environment variable is set to the path of the ``cabal-install`` executable
which invoked the subcommand.

It is strongly recommended that you implement your custom commands by calling the
CLI via the ``$CABAL`` variable rather than linking against the ``Cabal`` library.
There is no guarantee that the subcommand will link against the same version of the
``Cabal`` library as ``cabal-install`` so it would lead to unexpected results and
incompatibilities.

``cabal-install`` can also display the help message of the external command.
When ``cabal help <cmd>`` is invoked, then ``cabal-<cmd> <cmd> --help`` will be called so
your external command can display a help message.

For ideas or existing external commands, visit `this Discourse thread <https://discourse.haskell.org/t/an-external-command-system-for-cabal-what-would-you-do-with-it/7114>`_.
