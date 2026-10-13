---
synopsis: Make the build tree relocatable
packages: [cabal-install]
significance: significant
prs: 12145
issues: 12137
---

The project-local paths recorded in `plan.json` are now relative to the build tree root (e.g. `dist-newstyle`), rather than absolute. The package configuration file monitor also compares these paths relative to the build tree root, so moving the tree no longer invalidates it.

In practice, this means that the build tree can be moved to another codebase, and re-used. This is useful when creating worktrees with `git`, to minimize the amount of re-builds of local packages. This is particularly useful for users leveraging agentic workflows.

For example:

```txt
$ cabal build
Up to date.
$ git worktree add ../new-feature
$ cp -R dist-newstyle ../new-feature
$ cd ../new-feature
$ cabal build
Up to date.
```
