Package concepts
================

Before diving into the details of writing packages it helps to
understand a bit about packages in the Haskell world and the particular
approach that Cabal takes.

The point of packages
---------------------

Packages are a mechanism for organising and distributing code. Packages
are particularly suited for "programming in the large", that is building
big systems by using and re-using code written by different people at
different times.

People organise code into packages based on functionality and
dependencies. Social factors are also important: most packages have a
single author, or a relatively small team of authors.

Packages are also used for distribution: the idea is that a package can
be created in one place and be moved to a different computer and be
usable in that different environment. There are a surprising number of
details that have to be got right for this to work, and a good package
system helps to simplify this process and make it reliable.

Packages come in two main flavours: libraries of reusable code, and
complete programs. Libraries present a code interface, an API, while
programs can be run directly. In the Haskell world, library packages
expose a set of Haskell modules as their public interface. Cabal
packages can contain a library or executables or both.

Some programming languages have packages as a builtin language concept.
For example in Java, a package provides a local namespace for types and
other definitions. In the Haskell world, packages are not a part of the
language itself. Haskell programs consist of a number of modules, and
packages just provide a way to partition the modules into sets of
related functionality. Thus the choice of module names in Haskell is
still important, even when using packages.

Package names and versions
--------------------------

All packages have a name, e.g. "HUnit". Package names are assumed to be
unique. Cabal package names may contain letters, numbers and hyphens,
but not spaces and may also not contain a hyphened section consisting of
only numbers. The namespace for Cabal packages is flat, not
hierarchical.

Packages also have a version, e.g "1.1". This matches the typical way in
which packages are developed. Strictly speaking, each version of a
package is independent, but usually they are very similar. Cabal package
versions follow the conventional numeric style, consisting of a sequence
of digits such as "1.0.1" or "2.0". There are a range of common
conventions for "versioning" packages, that is giving some meaning to
the version number in terms of changes in the package, such as
e.g. `SemVer <http://semver.org>`__; however, for packages intended to be
distributed via Hackage Haskell's `Package Versioning Policy <https://pvp.haskell.org/>`_ applies
(see also the `PVP/SemVer FAQ section <https://pvp.haskell.org/faq/#semver>`__).

The combination of package name and version is called the *package ID*
and is written with a hyphen to separate the name and version, e.g.
"HUnit-1.1".

For Cabal packages, the combination of the package name and version
*uniquely* identifies each package. Or to put it another way: two
packages with the same name and version are considered to *be* the same.

Strictly speaking, the package ID only identifies each Cabal *source*
package; the same Cabal source package can be configured and built in
different ways. There is a separate installed package ID that uniquely
identifies each installed package instance. Most of the time however,
users need not be aware of this detail.

Kinds of package: Cabal vs GHC vs system
----------------------------------------

It can be slightly confusing at first because there are various
different notions of package floating around. Fortunately the details
are not very complicated.

Cabal packages
    Cabal packages are really source packages. That is they contain
    Haskell (and sometimes C) source code.

    Cabal packages can be compiled to produce GHC packages. They can
    also be translated into operating system packages.

GHC packages
    This is GHC's view on packages. GHC only cares about library
    packages, not executables. Library packages have to be registered
    with GHC for them to be available in GHCi or to be used when
    compiling other programs or packages.

    The low-level tool ``ghc-pkg`` is used to register GHC packages and
    to get information on what packages are currently registered.

    You never need to make GHC packages manually. When you build and
    install a Cabal package containing a library then it gets registered
    with GHC automatically.

    Haskell implementations other than GHC have essentially the same
    concept of registered packages. For the most part, Cabal hides the
    slight differences.

Operating system packages
    On operating systems like Linux and Mac OS X, the system has a
    specific notion of a package and there are tools for installing and
    managing packages.

    The Cabal package format is designed to allow Cabal packages to be
    translated, mostly-automatically, into operating system packages.
    They are usually translated 1:1, that is a single Cabal package
    becomes a single system package.

    It is also possible to make Windows installers from Cabal packages,
    though this is typically done for a program together with all of its
    library dependencies, rather than packaging each library separately.

Unit of distribution
--------------------

The Cabal package is the unit of distribution. This means that
each Cabal package can be distributed on its own, in source or binary
form. There may be dependencies between packages, but there is
usually a degree of flexibility in which versions of packages can work
together so distributing them independently makes sense.

It is perhaps easiest to see what being "the unit of distribution"
means by contrast to an alternative approach. Many projects are made up
of several interdependent packages and during development these might
all be kept under one common directory tree and be built and tested
together. When it comes to distribution however, rather than
distributing them all together in a single tarball, it is required that
they each be distributed independently in their own tarballs.

Cabal's approach is to say that if you can specify a dependency on a
package then that package should be able to be distributed
independently. Or to put it the other way round, if you want to
distribute it as a single unit, then it should be a single package.

Explicit dependencies and automatic package management
------------------------------------------------------

Cabal takes the approach that all packages dependencies are specified
explicitly and specified in a declarative way. The point is to enable
automatic package management. This means tools like ``cabal`` can
resolve dependencies and install a package plus all of its dependencies
automatically. Alternatively, it is possible to mechanically (or mostly
mechanically) translate Cabal packages into system packages and let the
system package manager install dependencies automatically.

It is important to track dependencies accurately so that packages can
reliably be moved from one system to another system and still be able to
build it there. Cabal is therefore relatively strict about specifying
dependencies. For example Cabal's default build system will not even let
code build if it tries to import a module from a package that isn't
listed in the ``.cabal`` file, even if that package is actually
installed. This helps to ensure that there are no "untracked
dependencies" that could cause the code to fail to build on some other
system.

The explicit dependency approach is in contrast to the traditional
"./configure" approach where instead of specifying dependencies
declaratively, the ``./configure`` script checks if the dependencies are
present on the system. Some manual work is required to transform a
``./configure`` based package into a Linux distribution package (or
similar). This conversion work is usually done by people other than the
package author(s). The practical effect of this is that only the most
popular packages will benefit from automatic package management.
Instead, Cabal forces the original author to specify the dependencies
but the advantage is that every package can benefit from automatic
package management.

The "./configure" approach tends to encourage packages that adapt
themselves to the environment in which they are built, for example by
disabling optional features so that they can continue to work when a
particular dependency is not available. This approach makes sense in a
world where installing additional dependencies is a tiresome manual
process and so minimising dependencies is important. The automatic
package management view is that packages should just declare what they
need and the package manager will take responsibility for ensuring that
all the dependencies are installed.

Sometimes of course optional features and optional dependencies do make
sense. Cabal packages can have optional features and varying
dependencies. These conditional dependencies are still specified in a
declarative way however and remain compatible with automatic package
management. The need to remain compatible with automatic package
management means that Cabal's conditional dependencies system is a bit
less flexible than with the "./configure" approach.

.. note::
   `GNU autoconf places restrictions on paths, including the
   path that the user builds a package from.
   <https://www.gnu.org/software/autoconf/manual/autoconf.html#File-System-Conventions>`_
   Package authors using ``build-type: configure`` should be aware of
   these restrictions; because users may be unexpectedly constrained and
   face mysterious errors, it is recommended that ``build-type: configure``
   is only used where strictly necessary.

Portability
-----------

One of the purposes of Cabal is to make it easier to build packages on
different platforms (operating systems and CPU architectures), with
different compiler versions and indeed even with different Haskell
implementations. (Yes, there are Haskell implementations other than
GHC!)

Cabal provides abstractions of features present in different Haskell
implementations and wherever possible it is best to take advantage of
these to increase portability. Where necessary however it is possible to
use specific features of specific implementations.

For example a package author can list in the package's ``.cabal`` what
language extensions the code uses. This allows Cabal to figure out if
the language extension is supported by the Haskell implementation that
the user picks. Additionally, certain language extensions such as
Template Haskell require special handling from the build system and by
listing the extension it provides the build system with enough
information to do the right thing.

Another similar example is linking with foreign libraries. Rather than
specifying GHC flags directly, the package author can list the libraries
that are needed and the build system will take care of using the right
flags for the compiler. Additionally this makes it easier for tools to
discover what system C libraries a package needs, which is useful for
tracking dependencies on system libraries (e.g. when translating into
Linux distribution packages).

In fact both of these examples fall into the category of explicitly
specifying dependencies. Not all dependencies are other Cabal packages.
Foreign libraries are clearly another kind of dependency. It's also
possible to think of language extensions as dependencies: the package
depends on a Haskell implementation that supports all those extensions.

Where compiler-specific options are needed however, there is an "escape
hatch" available. The developer can specify implementation-specific
options and more generally there is a configuration mechanism to
customise many aspects of how a package is built depending on the
Haskell implementation, the operating system, computer architecture and
user-specified configuration flags.
