---
synopsis: Add --watch flag to rerun commands on file changes
packages: [cabal-install]
prs: [12485]
significance: significant
---

`cabal build`, `install`, `haddock`, `repl`, `run`, `bench`, `test`, `exec`,
`check` and `sdist` accept a new `--watch` flag. With the flag, the command
runs once, then watches the files relevant to the command's targets (the
source files of the selected units and all `cabal.project` and `.cabal`
files) and re-runs the command whenever one of them changes, until
interrupted with Ctrl-C. A failing command is reported but does not stop
watching. Only supported on Linux.
