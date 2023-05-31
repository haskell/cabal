# Contributing to Cabal

Building Cabal for hacking
--------------------------

The current recommended way of developing Cabal is to use the
`v2-build` feature which [shipped in cabal-install-1.24](http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/).
If you use the latest version of cabal published on Hackage, it is sufficient to run:

```
cabal v2-build cabal
```

If not, you aren't able to build the testsuite, so you need to disable the default `cabal.project` that implies configuring the testsuite, e.g., with:

```
cabal v2-build --project-file=cabal.project.release cabal
```

The location of your build products will vary depending on which version of
cabal-install you use to build; see the documentation section
[Where are my build products?](http://cabal.readthedocs.io/en/latest/nix-local-build.html#where-are-my-build-products)
to find the binary (or just run `find -type f -executable -name cabal`).

Here are some other useful variations on the commands:

```
cabal v2-build Cabal # build library only
cabal v2-build Cabal-tests:unit-tests # build Cabal's unit test suite
cabal v2-build cabal-tests # etc...
```

Running tests
-------------

**Using Github Actions.**
If you are not in a hurry, the most convenient way to run tests on Cabal
is to make a branch on GitHub and then open a pull request; our
continuous integration service on Github Actions builds and
tests your code.  Title your PR with WIP so we know that it does not need
code review.

Some tips for using Github Actions effectively:

* Github Actions builds take a long time.  Use them when you are pretty
  sure everything is OK; otherwise, try to run relevant tests locally
  first.

* Watch over your jobs on the [Github Actions website](http://github.org/haskell/cabal/actions).
  If you know a build of yours is going to fail (because one job has
  already failed), be nice to others and cancel the rest of the jobs,
  so that other commits on the build queue can be processed.

**How to debug a failing CI test.**
One of the annoying things about running tests on CI is when they
fail, there is often no easy way to further troubleshoot the broken
build.  Here are some guidelines for debugging continuous integration
failures:

1. Can you tell what the problem is by looking at the logs?  The
   `cabal-testsuite` tests run with `-v` logging by default, which
   is dumped to the log upon failure; you may be able to figure out
   what the problem is directly this way.

2. Can you reproduce the problem by running the test locally?
   See the next section for how to run the various test suites
   on your local machine.

3. Is the test failing only for a specific version of GHC, or
   a specific operating system?  If so, try reproducing the
   problem on the specific configuration.

4. Is the test failing on a Github Actions per-GHC build.
   In this case, if you click on "Branch", you can get access to
   the precise binaries that were built by Github Actions that are being
   tested.  If you have an Ubuntu system, you can download
   the binaries and run them directly.

If none of these let you reproduce, there might be some race condition
or continuous integration breakage; please file a bug.

**Running tests locally.**
To run tests locally with `v2-build`, you will need to know the
name of the test suite you want.  Cabal and cabal-install have
several.  Also, you'll want to read [Where are my build products?](http://cabal.readthedocs.io/en/latest/nix-local-build.html#where-are-my-build-products)

The most important test suite is `cabal-testsuite`: most user-visible
changes to Cabal should come with a test in this framework.  See
[cabal-testsuite/README.md](cabal-testsuite/README.md) for more
information about how to run tests and write new ones.  Quick
start: use `cabal-tests` to run `Cabal` tests, and `cabal-tests
--with-cabal=/path/to/cabal` to run `cabal-install` tests
(don't forget `--with-cabal`! Your cabal-install tests won't
run without it).

There are also other test suites:

* `Cabal-tests:unit-tests` are small, quick-running unit tests
  on small pieces of functionality in Cabal.  If you are working
  on some utility functions in the Cabal library you should run this
  test suite.

* `cabal-install:unit-tests` are small, quick-running unit tests on
  small pieces of functionality in cabal-install.  If you are working
  on some utility functions in cabal-install you should run this test
  suite.

* `cabal-install:long-tests` are QuickCheck tests on
  cabal-install's dependency solver, VCS, and file monitoring code.
  If you are working on the solver you should run this test suite.

* `cabal-install:integration-tests2` are integration tests on some
  top-level API functions inside the `cabal-install` source code.

For these test executables, `-p` which applies a regex filter to the test
names. When running `cabal-install` test suites, one need only use `cabal test` or
`cabal run <test-target>` in order to test locally.

QA Notes
--------

Manual Quality Assurance (QA) is performed to ensure that the changes impacting
the command-line interface, whether adding or modifying a behaviour,
are tested before being released. This allows us to catch UX regressions and put
a human perspective into testing.

Contributions that touch `cabal-install` are expected to include notes for the QA team.
They are a description of an expected result upon calling `cabal-install` with certain parameters.

For instance:

> ## QA Notes
> Calling `cabal haddock-project` should produce documentation for the whole cabal project with the following defaults enabled:
> * Documentation lives in ./haddocks
> * The file `./haddocks/index.html` should exist

Manual QA is not expected to find every possible bug, but to really challenge the assumptions of the contributor, and to verify that their own testing
of their patch is not influenced by their setup or implicit knowledge of the system.


Code Style
---------------

We use automated formatting with Fourmolu to enforce a unified style across the code bases. It is checked in the CI process.
You can automatically format the code bases with `make style` at the top level of the project.

Other Conventions
-----------------

* Format your commit messages [in the standard way](https://chris.beams.io/posts/git-commit/#seven-rules).

* A lot of Cabal does not have top-level comments.  We are trying to
  fix this.  If you add new top-level definitions, please Haddock them;
  and if you spend some time understanding what a function does, help
  us out and add a comment.  We'll try to remind you during code review.

* If you do something tricky or non-obvious, add a comment.

* For local imports (Cabal module importing Cabal module), import lists
  are NOT required (although you may use them at your discretion.)  For
  third-party and standard library imports, please use either qualified imports
  or explicit import lists.

* You can use basically any GHC extension supported by a GHC in our
  support window, except Template Haskell, which would cause
  bootstrapping problems in the GHC compilation process.

* Our GHC support window is five years for the Cabal library and three
  years for cabal-install: that is, the Cabal library must be
  buildable out-of-the-box with the dependencies that shipped with GHC
  for at least five years.  The Travis CI checks this, so most
  developers submit a PR to see if their code works on all these
  versions of GHC.  `cabal-install` must also be buildable on all
  supported GHCs, although it does not have to be buildable
  out-of-the-box. Instead, the `cabal-install/bootstrap.sh` script
  must be able to download and install all of the dependencies (this
  is also checked by CI). Also, self-upgrade to the latest version
  (i.e. `cabal install cabal-install`) must work with all versions of
  `cabal-install` released during the last three years.

* `Cabal` has its own Prelude, in `Distribution.Compat.Prelude`,
  that provides a compatibility layer and exports some commonly
  used additional functions. Use it in all new modules.

* As far as possible, please do not use CPP. If you must use it,
  try to put it in a `Compat` module, and minimize the amount of code
  that is enclosed by CPP.  For example, prefer:
  ```
  f :: Int -> Int
  #ifdef mingw32_HOST_OS
  f = (+1)
  #else
  f = (+2)
  #endif
  ```

  over:
  ```
  #ifdef mingw32_HOST_OS
  f :: Int -> Int
  f = (+1)
  #else
  f :: Int -> Int
  f = (+2)
  #endif
  ```

GitHub Ticket Conventions
-------------------

Each major `Cabal`/`cabal-install` release (e.g. 3.4, 3.6, etc.) has a
corresponding GitHub Project and milestone. A ticket is included in a release's
project if the release managers are tenatively planning on including a fix for
the ticket in the release, i.e. if they are actively seeking someone to work on
the ticket.

By contrast, a ticket is milestoned to a given release if we are open to
accepting a fix in that release, i.e. we would very much appreciate someone
working on it, but are not committing to actively sourcing someone to work on
it.

GitHub Pull Request Conventions
-------------------

Every (non-backport) pull request has to go through a review and get 2
approvals. After this is done, the author of the pull request is expected to add
any final touches they deem important and put the `merge me` label on the pull
request. If the author lacks permissions to apply labels, they are welcome to
explicitly signal the merge intent on the discussion thread of the pull request,
at which point others (e.g., reviewers) apply the label. Merge buttons are
reserved for exceptional situations, e.g., CI fixes being iterated on or
backports/patches that need to be expedited for a release.

Currently there is a 2 day buffer for potential extra feedback between the last
update of a pull request (e.g. a commit, a rebase, an addition of the `merge me`
label) and the moment the Mergify bot picks up the pull request for a merge.

If your pull request consists of several commits, consider using `squash+merge
me` instead of `merge me`: the Mergify bot will squash all the commits into one
and concatenate the commit messages of the commits before merging.

Changelog
---------

When opening a pull request, you should write a changelog entry
(or more in case of multiple independent changes).
This is done by adding files in the `changelog.d` directory.

The files follow a simple key-value format similar to the one for .cabal files.

Here's an exhaustive example:

```cabal
synopsis: Add feature xyz
packages: cabal-install
prs: #0000
issues: #0000 #0000
significance: significant

description: {

- Detail number 1
- Detail number 2

}
```

Only the `synopsis` field is actually required, but you should also set the others where applicable.

| Field          | Description                                                                                                        |
| -----          | -----------                                                                                                        |
| `synopsis`     | Brief description of the change. Often just the pr title.                                                          |
| `description`  | Longer description, with a list of sub-changes. Not needed for small/atomic changes.                               |
| `packages`     | Packages affected by the change (`cabal-install`, `Cabal`...). Omit if it's an overarching or non-package change.  |
| `prs`          | Space-separated hash-prefixed pull request numbers containing the change (usually just one).                       |
| `issues`       | Space-separated hash-prefixed issue numbers that the change fixes/closes/affects.                                  |
| `significance` | Set to `significant` if the change is significant, that is if it warrants being put near the top of the changelog. |

You can find a large number of real-world examples of changelog files
[here](https://github.com/haskell/cabal/tree/bc83de27569fda22dbe1e10be1a921bebf4d3430/changelog.d).

At release time, the entries will be merged with
[this tool](https://github.com/fgaz/changelog-d).

In addition, if you're changing the .cabal file format specification you should
add an entry in `doc/file-format-changelog.rst`.

Communicating
-------------

There are a few main venues of communication:

* Most developers subscribe to receive messages from [all issues](https://github.com/haskell/cabal/issues); issues can be used to [open discussion](https://github.com/haskell/cabal/issues?q=is%3Aissue+is%3Aopen+custom+label%3A%22type%3A+discussion%22).  If you know someone who should hear about a message, CC them explicitly using the @username GitHub syntax.

* For more organizational concerns, the [mailing
  list](http://www.haskell.org/mailman/listinfo/cabal-devel) is used.

* Many developers idle on `#hackage` on [`irc.libera.chat`](https://libera.chat). The `#ghc` channel is also a decently good bet.
  * You can join the channel using a web client, even anonymously: https://web.libera.chat/#hackage
  * Alternatively you can join it using [matrix](https://matrix.org/): https://matrix.to/#/#hackage:libera.chat

Releases
--------

Notes for how to make a release are at the
wiki page ["Making a release"](https://github.com/haskell/cabal/wiki/Making-a-release).
Currently, [@emilypi](https://github.com/emilypi), [@fgaz](https://github.com/fgaz) and [@Mikolaj](https://github.com/Mikolaj) have access to
`haskell.org/cabal`, and [@Mikolaj](https://github.com/Mikolaj) is the point of contact for getting
permissions.

API Documentation
-----------------

Auto-generated API documentation for the `master` branch of Cabal is automatically uploaded here: http://haskell.github.io/cabal-website/doc/html/Cabal/.

## Issue triage [![Open Source Helpers](https://www.codetriage.com/haskell/cabal/badges/users.svg)](https://www.codetriage.com/haskell/cabal)

You can contribute by triaging issues which may include reproducing bug reports or asking for vital information, such as version numbers or reproduction instructions. If you would like to start triaging issues, one easy way to get started is to [subscribe to cabal on CodeTriage](https://www.codetriage.com/haskell/cabal).
