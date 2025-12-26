# githash

[![Build Status](https://dev.azure.com/snoyberg/githash/_apis/build/status/snoyberg.githash?branchName=master)](https://dev.azure.com/snoyberg/githash/_build/latest?definitionId=11&branchName=master)

Some handy Template Haskell splices for including the current git hash
and branch in the code of your project. Useful for including in panic
messages, `--version` output, or diagnostic info for more informative
bug reports.

Most of the complication in the `GitHash` module is due to the various
places the current git hash might be stored:

1. Detached HEAD: the hash is in `.git/HEAD`
2. On a branch or tag: the hash is in a file pointed to by `.git/HEAD`
in a location like `.git/refs/heads`
3. On a branch or tag but in a repository with packed refs: the hash is
in `.git/packed-refs`

These situations all arise under normal development workflows, but
there might be further scenarios that cause problems. Let me know if
you run into them!
