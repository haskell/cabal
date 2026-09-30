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
  `build-tools` dependencies.
- `PackagePath` carries the stage as a third field,
  `PackagePath Stage Namespace Qualifier`. Stage is orthogonal to the
  existing namespace and qualifier axes, so independent goals and the
  qualifier logic are unaffected.
- The solver's installed-package index is keyed by stage first, since
  installed packages are inherently per-stage and every goal knows its own
  stage.
- The resolver interface threads a `Staged` toolchain rather than a single
  compiler and platform.
- This is a behaviour-preserving refactor: every path is still constructed
  at the `Host` stage and only that stage's index is populated, so plans are
  unchanged. It is the foundation for cross-compilation (#11179), where the
  two stages are solved against different compilers.
