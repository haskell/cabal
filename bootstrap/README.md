# Bootstrapping cabal-install

This utility is only intended for use in building cabal-install
on a new platform. If you already have a functional (if dated) cabal-install
please rather run `cabal v2-install`.

The typical usage is porting to a new linux architecture,
then the `linux-ghcvec.json` file is available in `bootstrap/` folder:

On a (linux) system you are boostrapping, run

    bootstrap.py -d linux-ghcver.json -w /path/to-ghc

To generate the `platform-ghcver` files for other platforms, do:

  1. On a system with functional cabal-install, install the same GHC version
     as you will use to bootstrap on the host system.

  2. Build a dependency description file (`platform-ghcver.json`, e.g. `linux-8.8.3.json`) by running:

       ```sh
       cabal v2-build --with-compiler=/path/to/ghc --dry-run cabal-install:exe:cabal
       cp dist-newstyle/cache/plan.json bootstrap/platform-ghcver.plan.json
       cabal v2-build               --builddir=dist-newstyle-bootstrap --project=cabal.project.bootstrap cabal-bootstrap-gen
       cabal v2-run -vnormal+stderr --builddir=dist-newstyle-bootstrap --project=cabal.project.bootstrap cabal-bootstrap-gen -- bootstrap/platform-ghcver.plan.json | tee bootstrap/platform-ghcver.json
       ```

  3. You may need to tweak `bootstrap/platform-ghcver.json` file manually,
     for example toggle flags.

There are rules in top-level `Makefile` for generation of these files.
