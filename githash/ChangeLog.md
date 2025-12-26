# ChangeLog for githash

## 0.1.7.0

* Expose giFiles to users.

## 0.1.6.3

* Specify protocol.file.allow=always for latest git [#28](https://github.com/snoyberg/githash/pull/28)

## 0.1.6.2

* Fixed bugs; now this library's Template Haskell functions are much more
  likely on recompilation to detect Git update that doesn't affect workspace:
  e.g. `git switch -c <new_branch>` (equivalently
  `git checkout -b <new_branch>`) and `git commit --amend --only`.
  Implemented in [#23](https://github.com/snoyberg/githash/pull/23).

## 0.1.6.1

* [Support template-haskell 2.17](https://github.com/snoyberg/githash/pull/22)

## 0.1.6.0

* Always include patchlevel and hash in git-describe output

* Don't let user's configured initial branch name break tests

## 0.1.5.0

* Add git tag output via git-describe

## 0.1.4.0

* Add git-describe output

## 0.1.3.3

* Add git-worktree support [#13](https://github.com/snoyberg/githash/issues/13)

## 0.1.3.2

* Test suite works outside of a Git repo [#12](https://github.com/snoyberg/githash/issues/12)

## 0.1.3.1

* Clean up some warnings (addresses [fpco/optparse-simple#11](https://github.com/fpco/optparse-simple/issues/11))

## 0.1.3.0

* Catch exceptions thrown by `readCreateProcessWithExitCode` to deal
  with missing `git` executable
  [#7](https://github.com/snoyberg/githash/issues/7)

## 0.1.2.0

* Add `tGitInfoTry` and `tGitInfoCwdTry`

## 0.1.1.0

* Add message of the most recent commit

## 0.1.0.1

* Update the test suite

## 0.1.0.0

* Initial release
