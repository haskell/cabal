#!/bin/sh
git status > /dev/null # See https://github.com/haskell/cabal/pull/3088#commitcomment-15818452
git diff-files -p --exit-code
