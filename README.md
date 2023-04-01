# Cabal

Cabal is a tool for building and packaging Haskell libraries and programs. It provides a common interface for package authors and distributors to easily build their applications in a portable way. Cabal is part of a larger infrastructure for distributing, organizing, and cataloging Haskell libraries and programs.

## Getting started

To use Cabal, you'll need to have Haskell installed on your system. Once you have Haskell installed, you can install Cabal using the following command:

$ cabal install cabal-install


After installing Cabal, you can use it to create a new Haskell project. To create a new project, run the following command:

$ cabal init


This command will prompt you for information about your project, such as its name, version, and dependencies. Once you've provided this information, Cabal will generate a `.cabal` file for your project, which describes its structure and dependencies.

You can then use Cabal to build and install your project. To build your project, run the following command:

$ cabal build


This command will compile your project and generate executable files in the `dist/` directory. To install your project, run the following command:

$ cabal install


This command will install your project and its dependencies to your system's global package database.

## Fish shell integration

Fish shell users can install the [cabal-fish-completion](https://github.com/oh-my-fish/plugin-cabal) plugin to enable command-line completion for Cabal commands. To install the plugin using [Oh My Fish](https://github.com/oh-my-fish/oh-my-fish), run the following command:

$ omf install cabal


After installing the plugin, you can use tab completion to easily enter Cabal commands and options.

## Documentation

For more information about Cabal, including detailed documentation and usage examples, see the [official Cabal documentation](https://www.haskell.org/cabal/) or the [Cabal User Guide](https://cabal.readthedocs.io/en/3.4/).

## Contributing

If you're interested in contributing to Cabal, you can find the source code on [GitHub](https://github.com/haskell/cabal). Contributions are welcome and appreciated!
