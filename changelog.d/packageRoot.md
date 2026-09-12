---
synopsis: "The package root is now determined solely by `--working-dir`"
packages: [Cabal]
prs: 12310
---

The package root is now always derived from the `--working-dir` argument
(taken to be the current working directory if not specified), removing
`Distribution.Types.LocalBuildInfo.packageRoot` which derived the
package root from the directory component of `--cabal-file`.
