# cabal-validate

`cabal-validate` is a script that builds and tests `Cabal` and `cabal-install`.
`cabal-validate` can be run with `validate.sh` in the repository root;
arguments passed to `validate.sh` will be forwarded to `cabal-validate`.

Notable arguments include:

- `-v`/`--verbose` to display build and test output in real-time, instead of
  only if commands fail.
- `-s`/`--step` to run a specific step (e.g. `-s build -s lib-tests` will only
  run the `build` and `lib-tests` steps).
- `-p`/`--pattern` to filter tests by a pattern.

## Hacking on cabal-validate

Overview of important modules:

- `Main.hs` encodes all the commands that are run for each step.
- `Cli.hs` parses the CLI arguments and resolves default values from the
  environment, like determining which steps are run by default or the `--jobs`
  argument to pass to test suites.
- `Step.hs` lists the available steps.
