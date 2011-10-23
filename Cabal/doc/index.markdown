% Cabal User Guide

Cabal is package system for [Haskell] software.

Cabal specifies a standard way in which Haskell libraries and
applications can be packaged so that it is easy for consumers to use
them, or re-package them, regardless of the Haskell implementation or
installation platform.

Cabal defines a common interface -- the _Cabal package_ -- between
package authors, builders and users. There is a library to help package
authors implement this interface, and a tool to enable developers,
builders and users to work with Cabal packages.

# Contents #

  * [Introduction](#introduction)
      - [What's in a package](#whats-in-a-package)
      - [A tool for working with packages](#a-tool-for-working-with-packages)
  * [Developing packages](developing-packages.html)
      - [Package descriptions](developing-packages.html#package-descriptions)
          + [Package properties](developing-packages.html#package-properties)
          + [Library](developing-packages.html#library)
          + [Executables](developing-packages.html#executables)
          + [Test suites](developing-packages.html#test-suites)
          + [Build information](developing-packages.html#build-information)
          + [Configurations](developing-packages.html#configurations)
          + [Source Repositories](developing-packages.html#source-repositories)
      - [Accessing data files from package code](developing-packages.html#accessing-data-files-from-package-code)
          + [Accessing the package version](developing-packages.html#accessing-the-package-version)
      - [System-dependent parameters](developing-packages.html#system-dependent-parameters)
      - [Conditional compilation](developing-packages.html#conditional-compilation)
      - [More complex packages](developing-packages.html#more-complex-packages)
  * [Building and installing packages](installing-packages.html)
      - [Building and installing a system package](installing-packages.html#building-and-installing-a-system-package)
      - [Building and installing a user package](installing-packages.html#building-and-installing-a-user-package)
      - [Creating a binary package](installing-packages.html#creating-a-binary-package)
      - [setup configure](installing-packages.html#setup-configure)
          + [Programs used for building](installing-packages.html#programs-used-for-building)
          + [Installation paths](installing-packages.html#installation-paths)
          + [Controlling Flag Assignments](installing-packages.html#controlling-flag-assignments)
          + [Building Test Suites](installing-packages.html#building-test-suites)
          + [Miscellaneous options](installing-packages.html#miscellaneous-options)
      - [setup build](installing-packages.html#setup-build)
      - [setup haddock](installing-packages.html#setup-haddock)
      - [setup hscolour](installing-packages.html#setup-hscolour)
      - [setup install](installing-packages.html#setup-install)
      - [setup copy](installing-packages.html#setup-copy)
      - [setup register](installing-packages.html#setup-register)
      - [setup unregister](installing-packages.html#setup-unregister)
      - [setup clean](installing-packages.html#setup-clean)
      - [setup test](installing-packages.html#setup-test)
      - [setup sdist](installing-packages.html#setup-sdist)
  * [Reporting bugs and deficiencies](misc.html#reporting-bugs-and-deficiencies)
  * [Stability of Cabal interfaces](misc.html#stability-of-cabal-interfaces)
      - [Cabal file format](misc.html#cabal-file-format)
      - [Command-line interface](misc.html#command-line-interface)
          + [Very Stable Command-line interfaces](misc.html#very-stable-command-line-interfaces)
          + [Stable Command-line interfaces](misc.html#stable-command-line-interfaces)
          + [Unstable command-line](misc.html#unstable-command-line)
      - [Functions and Types](misc.html#functions-and-types)
          + [Very Stable API](misc.html#very-stable-api)
          + [Semi-stable API](misc.html#semi-stable-api)
          + [Unstable API](#unstable-api)
      - [Hackage](misc.html#hackage)

# Introduction #

Cabal is package system for Haskell software. The point of a packaging
system is to enable software developers and users to easily distribute,
use and reuse software. A good packaging system makes it easier for
developers to get their software into the hands of users, but equally
importantly it makes it easier for software developers to be able to
reuse software components written by other developers.

Packaging systems deal with packages and with Cabal we call them _Cabal
packages_. The Cabal package is the unit of distribution. Every Cabal
package has a name and a version number which are used to identify the
package, e.g. `filepath-1.0`.

Cabal packages are source based and are typically (but not necessarily)
portable to many platforms and Haskell implementations. The Cabal
package format is designed to make it possible to translate into other
formats, including binary packages for various systems.

When distributed, Cabal packages use the standard compressed tarball
format, with the file extension `.tar.gz`, e.g. `filepath-1.0.tar.gz`.

Note that packages are not part of the Haskell language, but most
Haskell implementations have some notion of package, and Cabal supports
most Haskell implementations.


## What's in a package ##

A Cabal package consists of:

  * Haskell software, including libraries, executables and tests
  * meta-data about the package in a standard human and machine
    readable format (the "`.cabal`" file)
  * a standard interface to build the package (the "`Setup.hs`" file)

The `.cabal` file contains information about the package, supplied by
the package author. Some of this information is used for identifying and
managing the package when it comes to distribution.

For the majority of packages it is possible to supply enough information
in the `.cabal` file so that it can be built without the package author
needing to write any extra build system scripts. For complex packages it
may be necessary to add code to the `Setup.hs` file.

Here is an example `foo.cabal` for a very simple Haskell library that
exposes one Haskell module called `Data.Foo`:

~~~~~~~~~~~~~~~~
name:              foo
version:           1.0
build-type:        Simple
cabal-version:     >= 1.2

library
  exposed-modules: Data.Foo
  build-depends:   base >= 3 && < 5
~~~~~~~~~~~~~~~~

For full details on what goes in the `.cabal` and `Setup.hs` files, and
for all the other features provided by the build system, see the section
on [developing packages](developing-packages.html).


## A tool for working with packages ##

There is a command line tool, called `cabal`, that users and developers
can use to install Cabal packages. It can be used for both local
packages and for packages available remotely over the network.

Developers can use the tool with packages in local directories, e.g.

~~~~~~~~~~~~~~~~
cd foo/
cabal install
~~~~~~~~~~~~~~~~

Developers and users can use the tool to install packages from remote
Cabal package archives. By default, the `cabal` tool is configured to
use the centeralised Haskell community archive called [Hackage] but it
is possible to use it with any other suitable archive.

~~~~~~~~~~~~~~~~
cabal install xmonad
~~~~~~~~~~~~~~~~

This will install the `xmonad` package plus all of its dependencies.

Cabal provides a number of ways for a user to customise how and where a
package is installed. They can decide where a package will be installed,
which Haskell implementation to use and whether to build optimised code
or build with the ability to profile code. It is not expected that users
will have to modify any of the information in the `.cabal` file.

For full details, see the section on [building and installing
packages](installing-packages.html).

Note that `cabal` is not the only tool for working with Cabal packages.
Due to the standardised format and a library for reading `.cabal` files,
there are several other special-purpose tools.

[Haskell]:  http://www.haskell.org/
[Hackage]:  http://hackage.haskell.org/
