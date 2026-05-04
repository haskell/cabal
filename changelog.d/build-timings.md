synopsis: Add flag for build timings
packages: cabal-install
prs: #11769
description:{
Introduce `--build-timings` flag to `cabal-install`. This flag makes `cabal-install`
log timing information to stdout, in a format like:

```
[build-timings] configure aeson-2.2.3.0 0.042s
[build-timings] build     aeson-2.2.3.0 3.284s
[build-timings] install   aeson-2.2.3.0 0.123s
```
}
