---
synopsis: Enable hyperlinked source by default when building documentation
packages: [cabal-install]
prs: [12491]
---

`documentation: True` now enables hyperlinked source unless explicitly disabled
with `haddock-hyperlink-source: False`. Explicit settings in project files and
the global configuration take precedence over this default.

When documentation and hyperlinked source are enabled, normal builds with GHC
8.8 and later also generate `.hie` files using `-fwrite-ide-info`. Haddock can
reuse these files when generating hyperlinked source, avoiding an additional
compilation of the modules.
