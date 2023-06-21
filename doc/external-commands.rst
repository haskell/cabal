External Commands
=================

Cabal provides a system for external commands, akin to the ones used by tools like ``git`` or ``cargo``.

If you execute ``cabal my-custom-command``, Cabal will search the path for an executable named ``cabal-my-custom-command`` and execute it, passing any remaining arguments to this external command. An error will be thrown in case the custom command is not found.

For ideas or existing external commands, visit `this Discourse thread <https://discourse.haskell.org/t/an-external-command-system-for-cabal-what-would-you-do-with-it/7114>`_.
