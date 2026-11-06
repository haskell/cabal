---
synopsis: Store per-component configuration in ComponentBuildDescr
packages: [Cabal, Cabal-hooks]
prs: 12562
issues: 12538
---

`PackageBuildDescr`, the information determined by configuring a package as a
whole, no longer contains information that can differ between components.

The following fields move to `ComponentBuildDescr`:

  - `componentConfigFlags`,
  - `componentEnabledSpec`,
  - `installDirTemplates`,
  - `withPackageDB`,
  - `extraCoverageFor`.

The `LocalBuildInfo` pattern synonym keeps all these fields.

`PreConfComponentInputs` gains a `componentConfigFlags` field, with the
component-level configure flags of the component being configured.

**Breaking change for packages with `build-type: Configure`** (and custom
setups using `autoconfUserHooks`): the `configure` script is no longer passed
the installation directories (`--prefix`, `--bindir`, `--libdir`,
`--dynlibdir`, `--datadir`, `--libexecdir`, `--sysconfdir`). It is still
passed `--with-compiler`, `--with-hc-pkg` and the `--configure-option`s.

This is because the `configure` script is run once for the whole package, but
the installation directories are fundamentally per-component.

`configure` scripts which use `@prefix@`, `@datadir@` or similar need to be
changed to obtain these locations at run time instead, e.g. by using the
`Paths` module.

