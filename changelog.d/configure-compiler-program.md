---
synopsis: Keep the compiler and hc-pkg programs found when configuring the compiler
packages: [Cabal, cabal-install]
prs: [12340, 12624]
---

Configuring the compiler finds its program, and for GHC also `ghc-pkg`, but
used to return only the `Compiler` and a `ProgramDb`, so code that needed either
program had to look it up again by name. Now:

- the new module `Distribution.Simple.Program.Compiler`, re-exported from
  `Distribution.Simple.Program`, defines the abstract type `CompilerPrograms`.
  `compilerPath` and `compilerPkgPath` give the paths of the configured
  programs; `compilerProgramToRun` and `compilerPkgProgramToRun` give them
  ready to run, with the arguments and environment from a `ProgramDb`;
- `configCompilerEx` and `GHC.configure` also return a `CompilerPrograms`;
- `configCompiler`, `GHC.configureCompiler` and `UHC.configure` also return the
  compiler's `ConfiguredProgram`;
- `configCompilerProgDb` takes the compiler's program and returns a
  `CompilerPrograms`; `GHC.compilerProgramDb` takes the compiler's program and
  also returns the `hc-pkg` program;
- `configureFinal` takes a `CompilerPrograms`;
- `PackageBuildDescr` (hence `LocalBuildInfo`) records it in the new
  `compilerPrograms` field.
