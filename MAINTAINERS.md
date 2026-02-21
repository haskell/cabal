# Cabal maintainer documentation

This document outlines some of the things that cabal maintainers should know. Contributors shouldn't need anything in here, unless they're working on the CI system or need an executive decision from a repo maintainer below. (If you find that you do, please open an issue pointing out what needs to be moved to the contributor documentation.)

This is a first draft; many things are as yet missing. Open an issue if you need something added here.


## Maintainers

The Cabal Maintainers Team consists of:

* Mikolaj Konarski ([`@Mikolaj`](https://github.com/Mikolaj), mikolaj@well-typed.com, [`ce1ed8ae0b011d8c`](https://keyserver.ubuntu.com/pks/lookup?op=vindex&search=0xce1ed8ae0b011d8c))

* Francesco Ariis ([`@ffaf1`](https://github.com/ffaf1), fa-ml@ariis.it, [`a9af0aaa6b87ec51`](https://keyserver.ubuntu.com/pks/lookup?op=vindex&search=0xa9af0aaa6b87ec51))

* Artem Pelenitsyn ([`@ulysses4ever`](https://github.com/ulysses4ever), a@pelenitsyn.top, [`a31c1bfa09b1f47d`](https://keyserver.ubuntu.com/pks/lookup?op=vindex&search=0xa31c1bfa09b1f47d))

* Brandon Allbery ([`@geekosaur`](https://github.com/geekosaur), allbery.b@gmail.com, [`227ee1942b0bdb95`](https://keyserver.ubuntu.com/pks/lookup?op=vindex&search=0x227ee1942b0bdb95))

* Matthew Pickering ([`@mpickering`](https://github.com/mpickering), matthew@well-typed.com)

The main goal of the team is to ensure that Cabal is keeping up with the ever-evolving Haskell ecosystem. In practical terms this means producing releases of the packages in this repository on a regular basis: we usually have to release at least as often as does GHC due to an intimate connection between the compiler and the build system. Hence, the people listed above (in chronological order by when they joined the team) are those who are currently available for carrying out the release procedures.

Successful maintenance requires coordination, and the team engages in three main ways:

- attending to issues and PRs on GitHub;

- discussing Cabal on the [Matrix channel](https://matrix.to/#/#hackage:matrix.org);

- meeting biweekly in video calls with agenda prepared asynchronously in a [Markdown document](https://hackmd.io/ytXS6xrAS2mTyPVxdUS6OA?both), which also holds the meeting notes.

Worth noting that the meetings are open to everyone interested in Cabal, especially aspiring and returning Cabal contributors. Ask on Matrix how to join.

Most of the current team are volunteers, and we are happy to receive any help. If you want to participate in Cabal maintenance as defined above (e.g. take on some release tasks), get in touch: open a GitHub discussion or send a message on Matrix.


## Workflows

The standard workflows are:

- `bootstrap.yml`: bootstrap a cabal from prepared JSONs (see `make bootstrap-jsons`)
- `validate.yml`: build a cabal with extra assertions and run the full test suite on it
- `changelogs.yml`: validate `changelog.d` files using [`changelog-d`]
- `dependabot.yml`: check `dependabot` configuration (sadly, not automatic; we lifted this from Ubuntu's CI)
- `lint.yml`: run `hlint` on cabal sources
- `format.yml`: check source formatting using Fourmolu v0.12
- `quick-jobs.yml`: various (formerly) quick checks
- `release.yaml`: build devel / release binaries for multiple platforms
- `typos.yml`: look for typos in documentation
- `users-guide.yml`: generate the users guide, creating an artifact
- `whitespace.yml`: check for extraneous whitespace in various files
- `check-sdist.yml`: make sure cabal can be built against the `Cabal` bootlib (see e.g. #10931, #9863)
- `release.yml`: build release binaries, either as part of a release or for testing

The validate workflow performs a number of tests on tier-1 platforms:

- on current GHCs (see the list of ghc versions in the jobs `matrix` in `validate.yml`) it runs through the full suite of tests (`lib-tests`, `lib-suite`, `cli-tests`, and `cli-suite`)
- on older GHCs (see the `extra-ghc` entries in `validate-old-ghcs`) it only runs `lib-suite-extras`, which is a cut-down test suite
- it builds but doesn't validate (for some reason) a static `cabal` on Alpine with MUSL
- it dogfoods `cabal` by having it build itself

You can use a manual dispatch on the validate workflow. It has two optional parameters:
- `allow-newer line` will add an `allow-newer:` entry to the project file. Don't include the prefix.
- `constraints line` will similarly add a `constraints:` entry.

The bootstrap workflow verifies that cabal can be built from pregenerated JSONs, for use in bootstrapping cabal on a new platform (since cabal is self-hosted). Note that, while we test this on release branches currently, bootstrapping is only supported from `master`.

The release workflow tests that PRs result in releasable `cabal`s, and is also used to produce `cabal` for releases. It can be dispatched manually or via a label `run release build`. It also performs daily draft releases starting at 00:00 UTC. It builds, tests, and releases for a wide variety of platforms, not all of which are considered Tier I for cabal development. (See list of tiers below.)


## Actions

Currently there is only one local action:

- `reusable-release.yaml`: the actual guts of `release.yaml` above.

The `validate-actions` branch in development will add more reusable actions for `validate.yml` in order to reduce duplication and make it more maintainable, and at that time `reusable-release` will likely be moved with the other reusable actions.


## Support tiers

Currently we support the following platforms as Tier 1:

- MacOS on AArch64
- X86-64 (aka AMD64)
- Windows (10 and 11)

Tier 2 platforms are:

- FreeBSD (AMD64 only)
- Alpine/MUSL static build
- MacOS on Intel
- X86 (deprecated)
- ARM Linux (Debian and Alpine)

We do not currently test on tier 2 platforms, but support for that is coming.


## CI

Mergify requires 2 approvals and a 2-day cooldown period before merging on `master`. Release branches are different, because we don't normally commit directly to them except during a release.

The rules for PRs on release branches are:

- only 1 approval needed for backports via Mergify (`@mergifyio backport branch`), otherwise 2 as usual
- no cooldown period, since either it's a backport of a PR that already received scrutiny or we're in the middle of a release and need things to move along

Note that you should not make (or approve) a PR directly to a release branch, unless it's necessary for release (usually this would be changelogs, but occasionally is needed for manual backports with conflicts).


## GPG keys

All maintainers who are authorized to make release binaries should have GPG keys cross-signed with other maintainers' keys. @f-a and @geekosaur can help with this if a new maintainer is onboarded.


## Releases

Notes for how to make a release are at the
wiki page ["Making a release"](https://github.com/haskell/cabal/wiki/Making-a-release).
Currently, [@emilypi](https://github.com/emilypi), [@fgaz](https://github.com/fgaz) and [@Mikolaj](https://github.com/Mikolaj) have access to
`haskell.org/cabal`, and [@Mikolaj](https://github.com/Mikolaj) is the point of contact for getting
permissions.


## Hackage Revisions

We have a CI setup to test that our main pipeline ("Validate") accepts a proposed revision. To use
it, go to the
[Validate workflow page](https://github.com/haskell/cabal/actions/workflows/validate.yml)
and dispatch it manually by clicking "Run workflow". As noted above in ["Workflows"](#workflows),
you can specify `allow-newer:` and `constraints:` entries reflecting the proposed revision.

For example, imagine that Cabal only allows `tar` or version less then
or equal to 0.6, and you want to bump it to 0.6. Then, to show that Validate
succeeds with `tar` 0.6, you should input

- `tar` for the `allow-newer line`
- `tar ==0.6` for the `constraints line`

Hopefully, running the Validate pipeline with these inputs succeeds, and you
should link to the run in the ticket about bumping the bound and making a revision.

If you are interested in the technical details, refer to the parts of `validate.yml` that
mention `hackage-revisions`.
