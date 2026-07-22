---
synopsis: Make build tree relocatable with `--enable-relocatable`
packages: [Cabal, cabal-install]
significance: significant
prs: 12145
issues: 12137
---

When using the flag `--enable-relocatable` (or `relocatable: True` configuration in a project file), all paths in the build tree (e.g. `dist-newstyle`) are relativized to the build tree root.

In practice, this means that the build tree can be moved to another codebase, and re-used. This is useful when creating worktrees with `git`, to minimize the amount of re-builds of local packages. This is particularly useful for users leveraging agentic workflows.

For example:

```txt
$ cabal build --enable-relocatable
Up to date.
$ git worktree add ../new-feature
$ cp -R dist-newstyle ../new-feature
$ cd ../new-feature
$ cabal build --enable-relocatable
Up to date.
```
