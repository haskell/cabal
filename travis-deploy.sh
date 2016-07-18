#!/bin/sh
set -ex

deploy() {
    git config --global user.email "builds@travis-ci.org"
    git config --global user.name "Travis CI User"
    git clone https://github.com/haskell/cabal-website.git ../cabal-website
    cd ../cabal-website
    git checkout --track -b gh-pages origin/gh-pages
    cd -
    mkdir -p ../cabal-website/doc/html
    mv Cabal/dist/doc/html/Cabal ../cabal-website/doc/html/Cabal
    cd ../cabal-website
    git add --all .
    git commit --amend --reset-author -m "Deploy to GitHub ($(date))."
    git push --force git@github.com:haskell/cabal-website.git gh-pages:gh-pages
}

if [ "x$TRAVIS_PULL_REQUEST" = "xfalse" -a "x$TRAVIS_BRANCH" = "xmaster" \
                             -a "x$DEPLOY_DOCS" = "xYES" ]
then
    deploy
fi
