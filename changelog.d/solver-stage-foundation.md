---
synopsis: Give the dependency solver a notion of build stages
packages: [cabal-install-solver, cabal-install]
prs: 12356
---

- The modular solver gains a `Stage` (`Host` or `Build`) and a `Staged`
  container that holds one value per stage. A package's stage says which
  compiler and platform it is built for: the `Host` stage produces the
  artifacts the user asked for, the `Build` stage produces things consumed
  during the build itself, such as compiled `Setup.hs` scripts and
  `build-tool-depends` dependencies.
- `PackagePath` is renamed `GoalScope` and gains the stage as a first
  field, `GoalScope Stage Namespace Qualifier`. The old name suggested a
  route through the dependency tree; the type is a flat label for the scope
  a goal is solved in. The stage follows the qualifier (it changes only at
  build-tool and setup boundaries) and leaves the namespace and the
  qualifier logic unaffected.
- The solver's installed-package index is keyed by stage first, since
  installed packages are inherently per-stage and every goal knows its own
  stage.
- The resolver interface threads a `Staged` toolchain rather than a single
  compiler and platform.
- This is a behaviour-preserving refactor: every path is still constructed
  at the `Host` stage and only that stage's index is populated, so plans are
  unchanged. It is the foundation for cross-compilation (#11179), where the
  two stages are solved against different compilers.
