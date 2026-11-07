---
synopsis: Give the dependency solver a notion of build machines
packages: [cabal-install-solver, cabal-install]
prs: 12356
---

- The modular solver gains a `Machine` (`Host` or `Build`) and a `PerMachine`
  container that holds one value per machine. A package's machine says which
  compiler and platform it is built for: the `Host` machine produces the
  artifacts the user asked for, the `Build` machine produces things consumed
  during the build itself, such as compiled `Setup.hs` scripts and
  `build-tool-depends` dependencies.
- `PackagePath` is renamed `GoalScope` and gains the machine as a first
  field, `GoalScope Machine Namespace Qualifier`. The old name suggested a
  route through the dependency tree; the type is a flat label for the scope
  a goal is solved in. The machine follows the qualifier (it changes only at
  build-tool and setup boundaries) and leaves the namespace and the
  qualifier logic unaffected.
- The solver's installed-package index is keyed by machine first, since
  installed packages are inherently per-machine and every goal knows its own
  machine.
- The resolver interface threads a `PerMachine` toolchain rather than a single
  compiler and platform.
- This is a behaviour-preserving refactor: every path is still constructed
  at the `Host` machine and only that machine's index is populated, so plans are
  unchanged. It is the foundation for cross-compilation (#11179), where the
  two machines are solved against different compilers.
