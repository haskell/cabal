---
synopsis: Run `configure` scripts from UNC paths on Windows
packages: [Cabal]
prs: 12655
---

On Windows, Cabal rewrote the path to a package's `configure` script into
MSYS form itself, guessing the shell's mount table. For a package under a UNC
path such as `\\server\share\pkg` the result started with `/\/`, and the shell
could not run it. Cabal now gives the shell the package directory with `/`
separators and lets it resolve the path. Behaviour on POSIX systems is
unchanged.
