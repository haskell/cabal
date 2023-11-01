Cabal and Cabal-syntax 3.10.2.1 changelog and release notes
---

## Release 3.10.2.1 is strictly a bug-fix release, with the fixes listed below

- Relax extension .c requirement for c-sources [#9285](https://github.com/haskell/cabal/pull/9285)

We will be tightening the behaviour of Cabal in the future, when users list files ending with extensions other than `.c` in the `c-sources` field of their cabal file. These files were never processed properly.
This PR displays more warnings and prepares the transition.
