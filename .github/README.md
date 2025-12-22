Cabal maintainer documentation
---

This document outlines some of the things that cabal maintainers should know. Contributors shouldn't need anything in here, unless they're working on the CI system. (If you find that you do, please open an issue pointing out what needs to be moved to the contributor documentation.)

This is a first draft; many things are as yet missing. Open an issue if you need something added here.

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
- `release.yml`: build release binaries, either as part of a release or for testing

The validate workflow performs a number of tests on tier-1 platforms:

- on current GHCs (see `GHC_FOR_VALIDATE` and `GHC_FOR_VALIDATE_ONLY` in `config.yml`) it runs through the full suite of tests (`lib-tests`, `lib-suite`, `cli-tests`, and `cli-suite`)
- on older GHCs (see `GHC_FOR_VALIDATE_OLD`) it only runs `lib-suite-extras`, which is a cut-down test suite ("extras" refers to the fact that `validate.yml` historically referred to the old GHCs as `extra-ghc`)
- it builds but doesn't validate (for some reason) a static `cabal` on Alpine with MUSL
- it dogfoods `cabal` by having it build itself

You can use a manual dispatch on the validate workflow. It has two optional parameters:
- `allow-newer line` will add an `allow-newer:` entry to the project file. Don't include the prefix.
- `constraints line` will similarly add a `comnstraints:` entry.

The bootstrap workflow verifies that cabal can be built from pregenerated JSONs, for use in bootstrapping cabal on a new platform (since cabal is self-hosted). Note that, while we test this on release branches currently, bootstrapping is only supported from `master`.

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

---

CI
---

Mergify requires 2 approvals and a 2-day cooldown period before merging on `master`. Release branches are different, because we don't normally commit directly to them except during a release.

The rules for PRs on release branches are:

- only 1 approval needed for backports via Mergify (`@mergifyio backport branch`), otherwise 2 as usual
- no cooldown period, since either it's a backport of a PR that already received scrutiny or we're in the middle of a release and need things to move along

Note that you should not make (or approve) a PR directly to a release branch, unless it's necessary for release (usually this would be changelogs, but occasionally is needed for manual backports with conflicts).

---

GPG keys
---

All maintainers who are authorized to make release binaries should have GPG keys cross-signed with other maintainers' keys. @f-a and @geekosaur can help with this if a new maintainer is onboarded.
