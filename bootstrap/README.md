# Bootstrapping cabal-install

This utility is only intended for use in building `cabal-install`
on a new platform. If you already have a functional cabal-install, however old, then
please instead run `cabal install cabal-install`.

## Bootstrapping on Linux

The typical use case is porting to a new Linux architecture. In that case,
a `linux-${GHCVER}.json` file is available in the `bootstrap/` folder:

On a Linux system you are bootstrapping, run

    ./bootstrap/bootstrap.py -d ./bootstrap/linux-${GHCVER}.json -w /path/to-ghc

from the top directory of the source checkout.

### Offline build

For offline builds, you can first run

    ./bootstrap/bootstrap.py -d ./bootstrap/linux-${GHCVER}.json -w /path/to-ghc fetch

to fetch tarballs for all the dependencies. These can then be used by a further
bootstrap command by way of the `--bootstrap-sources` argument:

    ./bootstrap/bootstrap.py -w /path/to-ghc --bootstrap-sources bootstrap-sources.tar.gz

## Bootstrapping on other (non-Linux) platforms

You will need to generate a `${PLATFORM}-${GHCVER}.json` file for other platforms and then use it
in the same way as it is shown for Linux above. On a system with functional `cabal-install`, do:

1. Install the same GHC version as you will use to bootstrap on the host system.

2. Build a dependency description file (`$PLATFORM-$GHCVER.json`, e.g. `macosx-8.8.4.json`) by running:

   ```sh
   cabal build --with-compiler=/path/to/ghc --dry-run cabal-install:exe:cabal
   cp dist-newstyle/cache/plan.json bootstrap/$PLATFORM-$GHCVER.plan.json
   cd bootstrap
   cabal run -v0 cabal-bootstrap-gen -- $PLATFORM-$GHCVER.plan.json | tee $PLATFORM-$GHCVER.json
   ```

3. You may need to tweak `bootstrap/$PLATFORM-$GHCVER.json` file manually,
     for example, to toggle flags.

There are rules in the top-level `Makefile` for generation of these files.

