..
  VCS subdirectory

A field of this type is always optional because it defaults to empty, which
corresponds to the root directory of the repository and is the same as
specifying ``.`` explicitly.

Some projects put the sources for multiple packages inside a single VCS
repository. This field lets you specify the relative path from the root of the
repository to the top directory for the package, i.e. the directory containing
the package's ``.cabal`` file.
