---
synopsis: Constraints can be qualified by build stage and by build-tool dependency
packages: [cabal-install, cabal-install-solver]
prs:
---

Package constraints gain two new qualifiers.

- A constraint may be restricted to the build stage with a `build:` prefix,
  for example

  ```
  constraints: build:any.rts installed
  ```

  which applies only to goals solved for the build stage — the compiler that
  builds `Setup.hs` scripts and `build-tools` dependencies when
  cross-compiling. An unprefixed constraint applies to every stage, as
  before. The host stage is the invisible default and is never printed, so
  output for ordinary (non-cross) builds is unchanged.

- A constraint may target a package as it is depended on via `build-tools`,
  alongside the existing setup qualifier:

  ```
  constraints: exe.alex ==3.4.0.0
  constraints: mypkg:alex:exe.containers ==0.6.7
  ```

  The short `exe.<pkg>` form matches any build-tool goal for that package;
  the fully-qualified `<pkg>:<tool>:exe.<dep>` form matches a dependency
  brought in by one particular tool of one particular package.

Existing constraints keep their current meaning: the stage is optional and
top-level goals are solved at the host stage.
