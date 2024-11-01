Actions
---

We use a number of local composite actions in `validate.yml`. Two of them simplify
`validate.yml` considerably; the other two less so, but will be useful in a future
tier-2 job to ensure that the main and tier 2 validate operations stay in sync.

We also use a central configuration file `.github/config.yml` containing the
various magic numbers/lists that bootstrap and validate use. The `Makefile`
also gets some information from it so it can stay in sync with `bootstrap.yml`.

The actions, found in `.github/actions` per convention, are:

- `cabal-setup`: does all the preliminary setup for a Cabal job
- `validate-build`: does `cabal-setup` and then builds cabal for validate
- `validate-old`: does `cabal-setup` and then validates a `validate-build` cabal against older GHCs
- `dogfooding`: does `cabal-setup` and then uses a `validate-build` cabal to build itself

As yet, there are no actions for the tests, because they're just a couple lines
of shell aside from `cabal-setup`. This may change in the future.

---

Workflows
---

The standard workflows are:

- `bootstrap.yml`: bootstrap a cabal from prepared JSONs (see `make bootstrap-jsons`)
- `validate.yml`: build a cabal with extra assertions and run the full test suite on it
- `changelogs.yml`: validate `changelog.d` files using [`changelog-d`]
- `dependabot.yml`: check `dependabot` configuration (sadly, not automatic; we lifted this from Ubuntu's CI)
- `lint.yml`: run `hlint` on cabal sources
- `format.yml`: check source formatting using Fourmolu v0.12
- `quick-jobs.yml`: various (formerly) quick checks
- `typos.yml`: look for typos in documentation
- `users-guide.yml`: generate the users guide, creating an artifact
- `whitespace.yml`: check for extraneous whitespace in various files
- `check-sdist.yml`: make sure cabal can be built against the `Cabal` bootlib (see e.g. #10931, #9863)

The validate workflow performs a number of tests on tier-1 platforms:

- on current GHCs (see `GHC_FOR_VALIDATE` and `GHC_FOR_VALIDATE_ONLY` in `config.yml`) it runs through the full suite of tests (`lib-tests`, `lib-suite`, `cli-tests`, and `cli-suite`)
- on older GHCs (see `GHC_FOR_VALIDATE_OLD`) it only runs `lib-suite-extras`, which is a cut-down test suite ("extras" refers to the fact that `validate.yml` historically referred to the old GHCs as `extra-ghc`)
- it builds but doesn't validate (for some reason) a static `cabal` on Alpine with MUSL
- it dogfoods `cabal` by having it build itself

You can use a manual dispatch on the validate workflow. It has two optional parameters:
- `allow-newer line` will add an `allow-newer:` entry to the project file. Don't include the prefix.
- `constraints line` will similarly add a `comnstraints:` entry.

The bootstrap workflow verifies that cabal can be built from pregenerated JSONs, for use in bootstrapping cabal on a new platform (since cabal is self-hosted).

---

Support tiers
---

Currently we support the following platforms as Tier 1:

- MacOS on AArch64
- X86-64 (aka AMD64)
- Windows (10 and 11)

Tier 2 platforms are:

- FreeBSD (AMD64 only)
- Alpine/MUSL static build
- MacOS on Intel
- X86 (deprecated)

We do not currently test on tier 2 platforms, but support for that is coming.
