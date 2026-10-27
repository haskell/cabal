---
synopsis: plan.json entries record their build machine
packages: [cabal-install]
prs: 12356
---

Every entry of `install-plan` in `plan.json` now carries a `machine` field,
`"host"` or `"build"`. In an ordinary build every entry is on the host machine.
When cross-compiling (`--with-build-compiler`) the packages built for the
build machine (build tools, custom `Setup.hs` dependencies) are on the build
machine, and the same package can appear once per machine, possibly under the
same `id`; the `machine` field is what tells the two entries apart.
