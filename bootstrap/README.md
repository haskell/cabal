# Bootstrapping cabal-install

This utility is only intended for use in building cabal-install
on a new platform. If you already have a functional (if dated) cabal-install
please rather run `cabal v2-install`.

The typical usage is porting to a new linux architecture,
then the `linux-$GHCVER.json` file is available in `bootstrap/` folder:

On a (linux) system you are bootstrapping, run

    ./bootstrap/bootstrap.py -d ./bootstrap/linux-$GHCVER.json -w /path/to-ghc

from the top directory of the source checkout.

To generate the `$PLATFORM-$GHCVER` files for other platforms, do:

  1. On a system with functional cabal-install, install the same GHC version
     as you will use to bootstrap on the host system.

  2. Build a dependency description file (`$PLATFORM-$GHCVER.json`, e.g. `linux-8.8.4.json`) by running:

       ```sh
       cabal v2-build --with-compiler=/path/to/ghc --dry-run cabal-install:exe:cabal
       cp dist-newstyle/cache/plan.json bootstrap/$PLATFORM-$GHCVER.plan.json
       cd bootstrap
       cabal v2-run -v0 cabal-bootstrap-gen -- $PLATFORM-$GHCVER.plan.json | tee $PLATFORM-$GHCVER.json
       ```

  3. You may need to tweak `bootstrap/$PLATFORM-$GHCVER.json` file manually,
     for example toggle flags.

There are rules in the top-level `Makefile` for generation of these files.
