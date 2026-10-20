---
synopsis: Emit loader-relative rpaths for GHC-linked artifacts
packages: [Cabal]
prs: 0
significance: significant
---

Executables and shared libraries linked with GHC now get every rpath entry in
loader-relative form: absolute library directories are rewritten as
`$ORIGIN/...` (`@loader_path/...` on macOS) relative to the artifact's own
install directory, and the artifact's own directory is searched first. A
binary distribution assembled from a store therefore keeps working when moved
(macOS 15's dyld aborts on an unresolvable absolute rpath), and a library
installed next to its dependant is found on musl, which does not consult
`LD_LIBRARY_PATH` after process start. This is no longer gated on
`--enable-relocatable`.
