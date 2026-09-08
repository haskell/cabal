---
synopsis: plan.json entries record their build stage
packages: [cabal-install]
prs: 12356
---

Every entry of `install-plan` in `plan.json` now carries a `stage` field,
`"host"` or `"build"`. In an ordinary build every entry is on the host stage.
When cross-compiling (`--with-build-compiler`) the packages built for the
build machine (build tools, custom `Setup.hs` dependencies) are on the build
stage, and the same package can appear once per stage, possibly under the
same `id`; the `stage` field is what tells the two entries apart.
