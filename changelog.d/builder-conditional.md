---
synopsis: "Add `builder(...)` conditional to test the build tool and its version"
packages: [Cabal-syntax, Cabal, cabal-install]
issues: #10386
significance: significant
---

Package descriptions can now use a `builder(<tool> [version])` conditional,
parallel to `impl(<compiler> [version])`, to branch on the tool building the
package and its version:

```cabal
cabal-version: 3.18

library
  if builder(cabal >= 3.18)
    cmm-sources: cbits/fast.cmm
```

The recognised tools are `cabal` (the Cabal library, versioned in lockstep
with `cabal-install` — note this is the Cabal *library* version, not the
`cabal-install` version) and `mcabal` (MicroHs). Any other tool name is
accepted but always evaluates to false, so a `builder` test naming an
unknown tool simply selects the `else` branch instead of failing to parse.

Using `builder(...)` requires `cabal-version: 3.18` or later.

The new `Distribution.Types.BuildTool.BuildTool` type and the
`Distribution.Types.ConfVar.ConfVar` constructor `Builder BuildTool
VersionRange` are exposed from Cabal-syntax.
