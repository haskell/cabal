#!/bin/sh

set -x

# Read out ACCOUNT and REPO from the slug
# Cribbed from http://unix.stackexchange.com/a/53323/118117
ACCOUNT=${TRAVIS_REPO_SLUG%%"/"*}
REPO=${TRAVIS_REPO_SLUG#*"/"}

# TAG will be used to uniquely identify a matrix entry; we
# need to push each matrix entry to a separate branch.
TAG="$TRAVIS_OS_NAME-$GHCVER$TAGSUFFIX"

# This is the commit for which we want a GitHub status update
# ping to go to.  Note that it is NOT TRAVIS_COMMIT unconditionally,
# since if we have a pull request, this commit will be a merge
# commit which no one from GitHub will be able to see.
COMMIT=${TRAVIS_PULL_REQUEST_SHA:-$TRAVIS_COMMIT}

# Git will complain if these fields don't work when committing,
# so set them up.
git config --global user.name "Pushbot"
git config --global user.email "pushbot@$(hostname)"
git config --global push.default simple

cd travis

# Setup SSH key we will use to push to binaries repository
cp id_rsa $HOME/.ssh/id_rsa
chmod 0600 $HOME/.ssh/id_rsa

# Setup SSH keys
ssh-keyscan github.com >> $HOME/.ssh/known_hosts

cd binaries

# Setup binaries repository for pushing
git init
# TODO: Update this
git remote add origin git@github.com:haskell-pushbot/cabal-binaries.git

# Make some final modifications to .travis.yml based so
# that downstream builds with the correct configuration
echo "env: GHCVER=$GHCVER UPSTREAM_BUILD_DIR=$TRAVIS_BUILD_DIR CABAL_LIB_ONLY=$CABAL_LIB_ONLY TEST_OTHER_VERSIONS=$TEST_OTHER_VERSIONS PARSEC=$PARSEC" >> .travis.yml
echo "os: $TRAVIS_OS_NAME" >> .travis.yml
if [ "x$GHCVER" = "x7.8.4" ] && [ "x$TRAVIS_OS_NAME" = "xosx" ]; then
    echo "osx_image: xcode6.4" >> .travis.yml
fi

# Install all of the necessary files for testing
cp $TRAVIS_BUILD_DIR/travis-install.sh .
cp $TRAVIS_BUILD_DIR/travis-common.sh .
cp -R $HOME/.cabal .
# Index files are too big for Git
rm -fv .cabal/packages/hackage.haskell.org/00-index*
rm -fv .cabal/packages/hackage.haskell.org/01-index*
rm -fv .cabal/packages/hackage.haskell.org/*.json
cp -R $TRAVIS_BUILD_DIR/dist-newstyle .
# Test files for test suites that rely on them
cp -R $TRAVIS_BUILD_DIR/cabal-testsuite .
mkdir Cabal
cp -R $TRAVIS_BUILD_DIR/Cabal/tests Cabal
mkdir cabal-install
cp -R $TRAVIS_BUILD_DIR/cabal-install/tests cabal-install

# Add, commit, push
git add .
# The JSON in the commit message is used by the webhook listening
# on the downstream repo to figure out who to communicate the
# status update back to
git commit -m '{"account":"'$ACCOUNT'", "repo":"'$REPO'", "commit": "'$COMMIT'", "tag":"'$TAG'"}'
git push -f origin "HEAD:$TAG/$COMMIT"
